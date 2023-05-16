#' Variable Contribution Breakdown for Single Observation
#'
#' Calculates sequential additive variable contributions (approximate SHAP) to
#' the prediction of a single observation, see Gosiewska and Biecek (see reference)
#' and the details below.
#'
#' The breakdown algorithm works as follows: First, the visit order
#' \eqn{(x_1, ..., x_m)} of the variables `v` is specified.
#' Then, in the query `data`, the column \eqn{x_1} is set to the value of \eqn{x_1}
#' of the single observation `new_obs` to be explained.
#' The change in the (weighted) average prediction on `data` measures the
#' contribution of \eqn{x_1} on the prediction of `new_obs`.
#' This procedure is iterated over all \eqn{x_i} until eventually, all rows
#' in `data` are identical to `new_obs`.
#'
#' A complication with this approach is that the visit order is relevant,
#' at least for non-additive models. Ideally, the algorithm could be repeated
#' for all possible permutations of `v` and its results averaged per variable.
#' This is basically what SHAP values do, see the reference below for an explanation.
#' Unfortunately, there is no efficient way to do this in a model agnostic way.
#'
#' We offer two visit strategies to approximate SHAP:
#' 1. "importance": Using the short-cut described in the reference below:
#' The variables are sorted by the size of their contribution in the same way as the
#' breakdown algorithm but without iteration, i.e., starting from the original query
#' data for each variable \eqn{x_i}.
#' 2. "permutation": Averages contributions from a small number of random permutations
#' of `v`.
#'
#' Note that the minimum required elements in the (multi-)flashlight are a
#' "predict_function", "model", and "data". The latter can also directly be passed to
#' [light_breakdown()]. Note that by default, no retransformation function is applied.
#'
#' @param x An object of class "flashlight" or "multiflashlight".
#' @param new_obs One single new observation to calculate variable attribution for.
#'   Needs to be a `data.frame` of same structure as `data`.
#' @param data An optional `data.frame`.
#' @param by An optional vector of column names used to filter `data`
#'   for rows with equal values in "by" variables as `new_obs`.
#' @param v Vector of variable names to assess contribution for.
#'   Defaults to all except those specified by "y", "w" and "by".
#' @param visit_strategy In what sequence should variables be visited?
#'   By "importance", by `n_perm` "permutation" or as "v" (see Details).
#' @param n_max Maximum number of rows in `data` to consider in the reference data.
#'   Set to lower value if `data` is large.
#' @param n_perm Number of permutations of random visit sequences.
#'   Only used if `visit_strategy = "permutation"`.
#' @param seed An integer random seed used to shuffle rows if `n_max`
#'   is smaller than the number of rows in `data`.
#' @param use_linkinv Should retransformation function be applied? Default is `FALSE`.
#' @param description Should descriptions be added? Default is `TRUE`.
#' @param digits Passed to [prettyNum()] to format numbers in description text.
#' @param ... Further arguments passed to [prettyNum()] to format numbers
#'   in description text.
#' @returns
#'   An object of class "light_breakdown" with the following elements:
#'   - `data` A tibble with results.
#'   - `by` Same as input `by`.
#' @export
#' @references
#'   A. Gosiewska and P. Biecek (2019). IBREAKDOWN: Uncertainty of model explanations
#'     for non-additive predictive models. ArXiv.
#' @examples
#' fit_part <- stats::lm(Sepal.Length ~ Species + Petal.Length, data = iris)
#' fl_part <- flashlight(
#'   model = fit_part, label = "part", data = iris, y = "Sepal.Length"
#' )
#' plot(light_breakdown(fl_part, new_obs = iris[1, ]))
#'
#' # Second model
#' fit_full <- stats::lm(Sepal.Length ~ ., data = iris)
#' fl_full <- flashlight(
#'   model = fit_full, label = "full", data = iris, y = "Sepal.Length"
#' )
#' fls <- multiflashlight(list(fl_part, fl_full))
#' plot(light_breakdown(fls, new_obs = iris[1, ]))
#' @seealso [plot.light_breakdown()]
light_breakdown <- function(x, ...) {
  UseMethod("light_breakdown")
}

#' @describeIn light_breakdown Default method not implemented yet.
#' @export
light_breakdown.default <- function(x, ...) {
  stop("light_breakdown method is only available for objects of class flashlight or multiflashlight.")
}

#' @describeIn light_breakdown Variable attribution to single observation
#' for a flashlight.
#' @export
light_breakdown.flashlight <- function(x, new_obs, data = x$data, by = x$by,
                                       v = NULL,
                                       visit_strategy = c("importance",
                                                          "permutation", "v"),
                                       n_max = Inf, n_perm = 20,
                                       seed = NULL, use_linkinv = FALSE,
                                       description = TRUE, digits = 2, ...) {
  visit_strategy <- match.arg(visit_strategy)

  stopifnot(
    "No data!" = is.data.frame(data) && nrow(data) >= 1L,
    "'by' not in 'data'!" = by %in% colnames(data),
    "Not all 'v' in 'data'" = v %in% colnames(data),
    "'new_obs' has to consist of one row" = nrow(new_obs) == 1L,
    "'new_obs' not consistent with 'data'" =
      sort(colnames(new_obs)) == sort(colnames(data)),
    !any(c("after_", "before_", "description_", "label_", "step_", "variable_") %in% by)

  )
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Subset data to the correct "by" values -> only place where we need "by"
  if (length(by)) {
    data <- dplyr::semi_join(data, new_obs, by = by)
  }
  n <- nrow(data)
  stopifnot(n >= 1L)

  # Subsample to n_max
  if (n > n_max) {
    data <- data[sample(n, min(n_max, n)), , drop = FALSE]
  }

  # Update flashlight with info on linkinv
  x <- flashlight(x, linkinv = if (use_linkinv) x$linkinv else function(z) z)

  # Baseline prediction and target prediction
  baseline <- .mean_pred(x, data = data)
  prediction <- unname(stats::predict(x, data = new_obs))

  # Determine v
  if (is.null(v)) {
    v <- setdiff(colnames(data), c(x$y, by, x$w))
  }

  m <- length(v)
  stopifnot(
    m >= 1L,
    !(c("baseline", "prediction") %in% v)
  )

  # If visit_strategy is not "permutation" or "v", choose order by importance
  if (visit_strategy == "importance") {
    ind_impact <- vapply(
      v,
      function(vi) {
        data[[vi]] <- new_obs[[vi]]; (.mean_pred(x = x, data = data) - baseline)^2
      },
      FUN.VALUE = numeric(1)
    )
    v <- names(sort(-ind_impact))
  }

  # Calculate contributions
  core_func <- function(X, perm = FALSE) {
    out <- numeric(m)
    vv <- if (perm) sample(v) else v
    for (i in 1:m) {
      X[[vv[i]]] <- new_obs[[vv[i]]]
      out[i] <- .mean_pred(x, data = X)
    }
    if (perm) cumsum(c(baseline, diff(c(baseline, out))[match(v, vv)]))[-1L] else out
  }
  if (visit_strategy != "permutation") {
    mean_pred_vector <- core_func(data)
  } else {
    mean_pred_vector <- rowMeans(
      replicate(n_perm, core_func(data, perm = TRUE)), na.rm = TRUE
    )
  }

  # Combine results
  out <- tibble::tibble(
    step_ = 0:(m + 1L),
    variable_ = c("baseline", v, "prediction"),
    after_ = c(baseline, mean_pred_vector, prediction),
    before_ = dplyr::lag(after_, default = baseline),
    label_ = x$label
  )

  if (description) {
    # Helper function
    .pretty_num <- function(z) {
      if (is.numeric(z)) {
        return(prettyNum(z, preserve.width = "individual", digits = digits, ...))
      }
      as.character(z)
    }
    # Add description text
    formatted_input <- vapply(
      new_obs[, v, drop = FALSE], .pretty_num, FUN.VALUE = character(1)
    )
    formatted_input <- c(
      "average in data", paste(v, formatted_input, sep = " = "), "prediction"
    )
    formatted_impact <- out$after_ - ifelse(out$step_ > 0, out$before_, 0)
    plus_sign <- formatted_impact >= 0 & out$step_ > 0
    formatted_impact <- paste0(
      ifelse(plus_sign, "+", ""), .pretty_num(formatted_impact)
    )
    out$description_ <- paste(formatted_input, formatted_impact, sep = ": ")
  } else {
    out$description_ <- ""
  }

  # Organize output
  if (length(by)) {
    out[, by] <- new_obs[rep(1L, nrow(out)), by, drop = FALSE]
  }
  add_classes(list(data = out, by = by), c("light_breakdown", "light"))
}

#' @describeIn light_breakdown Variable attribution to single observation
#' for a multiflashlight.
#' @export
light_breakdown.multiflashlight <- function(x, ...) {
  light_combine(lapply(x, light_breakdown, ...),
                new_class = "light_breakdown_multi")
}

#' Visualize Variable Contribution Breakdown for Single Observation
#'
#' Minimal visualization of an object of class "light_breakdown" as waterfall plot.
#' The object returned is of class "ggplot" and can be further customized.
#'
#' The waterfall plot is to be read from top to bottom.
#' The first line describes the (weighted) average prediction in the query data
#' used to start with. Then, each additional line shows how the prediction changes
#' due to the impact of the corresponding variable.
#' The last line finally shows the original prediction of the selected observation.
#' Multiple flashlights are shown in different facets.
#' Positive and negative impacts are visualized with different colors.
#'
#' @importFrom rlang .data
#'
#' @inheritParams plot.light_performance
#' @param x An object of class "light_breakdown".
#' @param facet_ncol `ncol` argument passed to [ggplot2::facet_wrap()].
#' @param ... Further arguments passed to [ggplot2::geom_label()].
#' @returns An object of class "ggplot".
#' @export
#' @seealso [light_breakdown()]
plot.light_breakdown <- function(x, facet_scales = "free",
                                 facet_ncol = 1, rotate_x = FALSE, ...) {
  stopifnot(!(c("fill_", "xmin_", "xmax_", "y_") %in% colnames(x$data)))
  data <- transform(
    x$data,
    fill_ = (after_ - before_) > 0,
    ymin_ = step_ - 0.5,
    ymax_ = step_ + 0.5,
    x_ = pmin(before_, after_)
  )

  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = x_, y = step_, xmin = before_, xmax = after_, ymin = ymin_, ymax = ymax_
    )
  ) +
    ggplot2::geom_rect(
      ggplot2::aes(fill = fill_), color = "black", show.legend = FALSE
    ) +
    ggplot2::labs(x = "Prediction", y = ggplot2::element_blank()) +
    ggplot2::geom_label(ggplot2::aes(label = description_), hjust = -0.05, ...) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.67))) +
    ggplot2::scale_y_reverse() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )

  if (is.light_breakdown_multi(x)) {
    p <- p + ggplot2::facet_wrap(~label_, scales = facet_scales, ncol = facet_ncol)
  }
  if (rotate_x) {
    p <- p + rotate_x()
  }
  p
}

# Helper function
.mean_pred <- function(x, data, w = if (!is.null(x$w)) data[[x$w]]) {
  MetricsWeighted::weighted_mean(stats::predict(x, data = data), w = w, na.rm = TRUE)
}
