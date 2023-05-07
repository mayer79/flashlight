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
#'   - `data` A tibble with results. Can be used to build fully customized
#'     visualizations. Column names can be controlled by `options(flashlight.column_name)`.
#'   - `by` Same as input `by`.
#' @export
#' @references
#'   A. Gosiewska and P. Biecek (2019). IBREAKDOWN: Uncertainty of model explanations
#'     for non-additive predictive models. ArXiv.
#' @examples
#' fit <- lm(Sepal.Length ~ . + Petal.Length:Species, data = iris)
#' fl <- flashlight(model = fit, label = "lm", data = iris, y = "Sepal.Length")
#' light_breakdown(fl, new_obs = iris[1, ])
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

  warning_on_names(
    c(
      "after_name",
      "before_name",
      "description_name",
      "variable_name"
    ),
    ...
  )

  after_name <- getOption("flashlight.after_name")
  before_name <- getOption("flashlight.before_name")
  description_name <- getOption("flashlight.description_name")
  label_name <- getOption("flashlight.label_name")
  step_name <- getOption("flashlight.step_name")
  variable_name <- getOption("flashlight.variable_name")

  stopifnot(
    "No data!" = is.data.frame(data) && nrow(data) >= 1L,
    "'by' not in 'data'!" = by %in% colnames(data),
    "Not all 'v' in 'data'" = v %in% colnames(data),
    "'new_obs' has to consist of one row" = nrow(new_obs) == 1L,
    "'new_obs' not consistent with 'data'" =
      sort(colnames(new_obs)) == sort(colnames(data))
  )
  check_unique(
    opt_names = c(
      after_name, before_name, label_name, variable_name, description_name
    )
  )

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Subset data to the correct "by" values -> only place where we need "by"
  if (length(by)) {
    data <- dplyr::semi_join(data, new_obs, by = by)
  }
  stopifnot((n <- nrow(data)) >= 1L)

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
  stopifnot(
    (m <- length(v)) >= 1L,
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
    0:(m + 1L),
    c("baseline", v, "prediction"),
    c(baseline, mean_pred_vector, prediction)
  )
  colnames(out) <- c(step_name, variable_name, after_name)
  out[[before_name]] <- dplyr::lag(out[[after_name]], default = baseline)
  out[[label_name]] <- x$label

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
    formatted_impact <-
      out[[after_name]] - ifelse(out[[step_name]] > 0, out[[before_name]], 0)
    plus_sign <- formatted_impact >= 0 & out[[step_name]] > 0
    formatted_impact <- paste0(
      ifelse(plus_sign, "+", ""), .pretty_num(formatted_impact)
    )
    out[[description_name]] <- paste(formatted_input, formatted_impact, sep = ": ")
  } else {
    out[[description_name]] <- ""
  }

  # Organize output
  if (length(by)) {
    out[, by] <- new_obs[rep(1, nrow(out)), by, drop = FALSE]
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

# Helper function
.mean_pred <- function(x, data, w = if (!is.null(x$w)) data[[x$w]]) {
  MetricsWeighted::weighted_mean(stats::predict(x, data = data), w = w, na.rm = TRUE)
}
