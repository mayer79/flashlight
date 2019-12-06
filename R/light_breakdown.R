#' Variable Contribution Breakdown for Single Observation
#'
#' Calculates sequential additive variable contributions (approximate SHAP) to the prediction of a single observation, see Gosiewska and Biecek [1] and the details below.
#'
#' The breakdown algorithm works as follows: First, the visit order (x_1, ..., x_m) of the variables \code{v} is specified. Then, in the query \code{data}, the column x_1 is set to the value of x_1 of the single observation \code{new_obs} to be explained. The change in the (weighted) average prediction on \code{data} measures the contribution of x_1 on the prediction of \code{new_obs}. This procedure is iterated over all x_i until eventually, all rows in \code{data} are identical to \code{new_obs}.
#' A complication with this approach is that the visit order is relevant, at least for non-additive models. Ideally, the algorithm could be repeated for all possible permutations of \code{v} and its results averaged per variable. This is basically what SHAP values do, see [1] for an explanation. Unfortunately, there is no efficient way to do this in a model agnostic way. We offer two visit strategies to approximate SHAP. The first one uses
#' the short-cut described in [1]: The variables are sorted by the size of their contribution in the same way as the breakdown algorithm but without iteration, i.e. starting from the original query data for each variable $x_i$. We call this visit strategy "importance". The second strategy "permutation" averages contributions from a small number of random permutations of v.
#' Note that the minimum required elements in the (multi-) flashlight are a "predict_function", "model", and "data". The latter can also directly be passed to \code{light_breakdown}. Note that by default, no retransformation function is applied.
#'
#' @importFrom dplyr semi_join lag tibble
#' @importFrom MetricsWeighted weighted_mean
#' @param x An object of class \code{flashlight} or \code{multiflashlight}.
#' @param new_obs One single new observation to calculate variable attribution for. Needs to be a \code{data.frame} of same structure as \code{data}.
#' @param data An optional \code{data.frame}.
#' @param by An optional vector of column names used to filter \code{data} for rows with equal values in "by" variables as \code{new_obs}.
#' @param v Vector of variables to assess contribution for. Defaults to all except those specified by "y", "w" and "by".
#' @param visit_strategy In what sequence should variables be visited? By "importance", by \code{n_perm} "permutation" or as "v" (see Details).
#' @param n_max Maximum number of rows in \code{data} to consider in the reference data. Set to lower value if \code{data} is large.
#' @param n_perm Number of permutations of random visit sequences. Only used if \code{visit_strategy = "permutation"}.
#' @param seed An integer random seed used to shuffle rows if \code{n_max} is smaller than the number of rows in \code{data}.
#' @param use_linkinv Should retransformation function be applied? Default is \code{FALSE}.
#' @param after_name Column name in resulting \code{data} containing prediction after the step in \code{step_name}. Defaults to "after".
#' @param before_name Column name in resulting \code{data} containing prediction before the step in \code{step_name}. Defaults to "before".
#' @param label_name Column name in resulting \code{data} containing the label of the flashlight. Defaults to "label".
#' @param variable_name Column name in resulting \code{data} containing the variable names. Defaults to "variable".
#' @param step_name Column name in resulting \code{data} containing the step of the prediction process. Defaults to "step".
#' @param description_name Column name in resulting \code{data} containing the description text to be visualized. Defaults to "description".
#' @param description Should descriptions be added? Default is \code{TRUE}.
#' @param digits Passed to \code{prettyNum} to format numbers in description text.
#' @param ... Further arguments passed to \code{prettyNum} to format numbers in description text.
#' @return An object of class \code{light_breakdown}, \code{light} (and a list) with the following elements.
#' \itemize{
#'   \item \code{data} A tibble with results. Can be used to build fully customized visualizations.
#'   \item \code{by} Same as input \code{by}.
#'   \item \code{after_name} Same as input \code{after_name}.
#'   \item \code{before_name} Same as input \code{before_name}.
#'   \item \code{label_name} Same as input \code{label_name}.
#'   \item \code{variable_name} Same as input \code{variable_name}.
#'   \item \code{step_name} Same as input \code{step_name}.
#'   \item \code{description_name} Same as input \code{description_name}.
#' }
#' @export
#' @references [1] A. Gosiewska and P. Biecek (2019). IBREAKDOWN: Uncertainty of model explanations for non-additive predictive models. ArXiv <arxiv.org/abs/1903.11420>.
#' @examples
#' fit <- lm(Sepal.Length ~ . + Petal.Length:Species, data = iris)
#' fl <- flashlight(model = fit, label = "lm", data = iris, y = "Sepal.Length")
#' light_breakdown(fl, new_obs = iris[1, ])
#' @seealso \code{\link{plot.light_breakdown}}.
light_breakdown <- function(x, ...) {
  UseMethod("light_breakdown")
}

#' @describeIn light_breakdown Default method not implemented yet.
#' @export
light_breakdown.default <- function(x, ...) {
  stop("light_breakdown method is only available for objects of class flashlight or multiflashlight.")
}

#' @describeIn light_breakdown Variable attribution to single observation for a flashlight.
#' @export
light_breakdown.flashlight <- function(x, new_obs, data = x$data, by = x$by,
                                       v = NULL,
                                       visit_strategy = c("importance", "permutation", "v"),
                                       n_max = Inf, n_perm = 20,
                                       seed = NULL, use_linkinv = FALSE,
                                       after_name = "after", before_name = "before",
                                       label_name = "label", variable_name = "variable",
                                       step_name = "step", description_name = "description",
                                       description = TRUE, digits = 3, ...) {
  visit_strategy <- match.arg(visit_strategy)
  stopifnot(nrow(new_obs) == 1L, nrow(data) >= 1L,
            sort(colnames(new_obs)) == sort(colnames(data)),
            !anyDuplicated(c(after_name, before_name, label_name,
                             variable_name, description_name)))

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Subset data to the correct "by" values -> only place where we need "by"
  if (length(by)) {
    data <- semi_join(data, new_obs, by = by)
  }
  stopifnot((n <- nrow(data)) >= 1L)
  # Subsample to n_max
  if (n > n_max) {
    data <- data[sample(n, min(n_max, n)), , drop = FALSE]
  }

  # Update flashlight with info on linkinv
  x <- flashlight(x, linkinv = if (use_linkinv) x$linkinv else function(z) z)

  # Helper function
  mean_pred <- function(x, data, w = if (!is.null(x$w)) data[[x$w]]) {
    weighted_mean(predict(x, data = data), w = w, na.rm = TRUE)
  }

  # Baseline prediction and target prediction
  baseline <- mean_pred(x, data = data)
  prediction <- unname(predict(x, data = new_obs))

  # Determine v
  if (is.null(v)) {
    v <- setdiff(colnames(data), c(x$y, by, x$w))
  }
  stopifnot((m <- length(v)) >= 1L,
            !(c("baseline", "prediction") %in% v))

  # If visit_strategy is not "permutation" or "v", choose order by importance
  if (visit_strategy == "importance") {
    ind_impact <- vapply(v, function(vi) {
      data[[vi]] <- new_obs[[vi]];
      (mean_pred(x = x, data = data) - baseline)^2
    }, FUN.VALUE = numeric(1))
    v <- names(sort(-ind_impact))
  }

  # Calculate contributions
  core_func <- function(X, perm = FALSE) {
    out <- numeric(m)
    vv <- if (perm) sample(v) else v
    for (i in 1:m) {
      X[[vv[i]]] <- new_obs[[vv[i]]]
      out[i] <- mean_pred(x, data = X)
    }
    if (perm) cumsum(c(baseline, diff(c(baseline, out))[match(v, vv)]))[-1] else out
  }
  if (visit_strategy != "permutation") {
    mean_pred_vector <- core_func(data)
  } else {
    mean_pred_vector <- rowMeans(replicate(n_perm, core_func(data, perm = TRUE)), na.rm = TRUE)
  }

  # Combine results
  out <- tibble(0:(m + 1),
                c("baseline", v, "prediction"),
                c(baseline, mean_pred_vector, prediction))
  colnames(out) <- c(step_name, variable_name, after_name)
  out[[before_name]] <- lag(out[[after_name]], default = baseline)
  out[[label_name]] <- x$label

  if (description) {
    # Add description text
    pretty_num <- function(z) {
      if (is.numeric(z)) {
        return(prettyNum(z, preserve.width = "individual", digits = digits, ...))
      }
      as.character(z)
    }
    formatted_input <- vapply(new_obs[, v, drop = FALSE], pretty_num, character(1))
    formatted_input <- c("average in data", paste(v, formatted_input, sep = " = "), "prediction")
    formatted_impact <- out[[after_name]] - ifelse(out[[step_name]] > 0, out[[before_name]], 0)
    plus_sign <- formatted_impact >= 0 & out[[step_name]] > 0
    formatted_impact <- paste0(ifelse(plus_sign, "+", ""), pretty_num(formatted_impact))
    out[[description_name]] <- paste(formatted_input, formatted_impact, sep = ": ")
  } else {
    out[[description_name]] <- ""
  }

  # Add "by" variables
  if (length(by)) {
    out[, by] <- new_obs[rep(1, nrow(out)), by, drop = FALSE]
  }

  # Organize output
  out <- list(data = out, by = by,
              after_name = after_name, before_name = before_name,
              label_name = label_name, variable_name = variable_name,
              step_name = step_name, description_name = description_name)
  class(out) <- c("light_breakdown", "light", "list")
  out
}

#' @describeIn light_breakdown Variable attribution to single observation for a multiflashlight.
#' @export
light_breakdown.multiflashlight <- function(x, ...) {
  light_combine(lapply(x, light_breakdown, ...),
                new_class = "light_breakdown_multi")
}
