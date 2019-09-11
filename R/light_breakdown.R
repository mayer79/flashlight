#' Variable Contribution Breakdown for Single Observation
#'
#' Calculates sequential variable contribution to the prediction of a single observation, see Gosiewska and Biecek [1]. For non-additive models, the order of the variables visited is relevant and is determined either by the specified order in \code{v} or the non-sequential contribution of each \code{v}. In the first (non-default) case, complexity in the length p of \code{v} is \code{min(n_max, p)}, while in the second case its \code{p + min(n_max, p)}.
#'
#' The breakdown algorithm assigns the following attribution to the first variable (x): In the query \code{data}, all values in x are set to the value of x in the new observation. The change in the (weighted) average predicted value on \code{data} is the contribution of x on the prediction. This procedure is iterated over \code{v}. To determine the order of the sequence, the same approach is calculated first without iteration, i.e. starting with the original \code{data} for each variable. This approach is described in detail in [1]. If the interative algorithm would be repeated for all possible permutations of the input variables, the resulting attributions would equal SHAP values. Note that the minimum required elements in the (multi-) flashlight are "y", "predict_function", "model", and "data". The latter can also directly be passed to \code{light_breakdown}. Note that by default, no retransformation function is applied.
#'
#' @importFrom dplyr semi_join lag
#' @importFrom MetricsWeighted weighted_mean
#' @param x An object of class \code{flashlight} or \code{multiflashlight}.
#' @param new_obs One single new observation to calculate variable attribution for. Needs to be a \code{data.frame} of same structure as \code{data}.
#' @param data An optional \code{data.frame}.
#' @param by An optional vector of column names used to filter \code{data} for rows with equal values in "by" variables like \code{new_obs}.
#' @param v Vector of variables to assess contribution for. Defaults to all except variables specified by "y", "w" and "by".
#' @param order_by_importance Logical flag indicated if \code{v} should be ordered automatically based on individual contribution (see description).
#' @param top_m Maximum number of steps/variable contributions to be calculated.
#' @param n_max Maximum number of rows in \code{data} to consider. Set to lower value if \code{data} is large.
#' @param seed An integer random seed used to shuffle rows if \code{n_max} is smaller than the number of rows in \code{data}. Furthermore passed to \code{light_im}
#' @param use_linkinv Should retransformation function be applied? Default is \code{FALSE}.
#' @param after_name Column name in resulting \code{data} containing prediction after the step in \code{step_name}. Defaults to "after".
#' @param before_name Column name in resulting \code{data} containing prediction before the step in \code{step_name}. Defaults to "before".
#' @param label_name Column name in resulting \code{data} containing the label of the flashlight. Defaults to "label".
#' @param variable_name Column name in resulting \code{data} containing the variable names. Defaults to "variable".
#' @param description_name Column name in resulting \code{data} containing the description text to be visualized. Defaults to "description".
#' @param step_name Column name in resulting \code{data} containing the step of the prediction process. Defaults to "step".
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
#' fit_part <- lm(Sepal.Length ~ Petal.Length, data = iris)
#' fit_full <- lm(Sepal.Length ~ ., data = iris)
#' mod_full <- flashlight(model = fit_full, label = "full", data = iris, y = "Sepal.Length")
#' mod_part <- flashlight(model = fit_part, label = "part", data = iris, y = "Sepal.Length")
#' mods <- multiflashlight(list(mod_full, mod_part), by = "Species")
#' light_breakdown(mod_full, new_obs = iris[1, ])
#' light_breakdown(mods, new_obs = iris[1, ])
#' light_breakdown(mods, new_obs = iris[1, ], top_m = 2)
#'
#' ir <- iris
#' ir$log_sl <- log(ir$Sepal.Length)
#' fit_lm <- lm(log_sl ~ Petal.Length, data = ir)
#' fit_glm <- glm(Sepal.Length ~ Petal.Length, data = ir, family = Gamma(link = log))
#' fl_lm <- flashlight(model = fit_lm, label = "lm", y = "log_sl", linkinv = exp)
#' fl_glm <- flashlight(model = fit_glm, label = "glm", y = "Sepal.Length",
#'   predict_function = function(m, X) predict(m, X, type = "response"))
#' fls <- multiflashlight(list(fl_lm, fl_glm), data = ir)
#' light_breakdown(fls, new_obs = ir[1, ])$data
#' light_breakdown(fls, new_obs = ir[1, ], use_linkinv = TRUE)$data
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
                                       v = NULL, order_by_importance = TRUE, top_m = Inf,
                                       n_max = Inf, seed = NULL, use_linkinv = FALSE,
                                       after_name = "after", before_name = "before",
                                       label_name = "label", variable_name = "variable",
                                       step_name = "step", description_name = "description",
                                       digits = 3, ...) {
  stopifnot(nrow(new_obs) == 1L, nrow(data) >= 1L,
            sort(colnames(new_obs)) == sort(colnames(data)),
            !anyDuplicated(c(after_name, before_name, label_name,
                             variable_name, description_name)))

  # Subset data to the correct "by" values -> only place where we need "by"
  if (length(by)) {
    data <- semi_join(data, new_obs, by = by)
  }
  stopifnot((n <- nrow(data)) >= 1L)

  # Subsample to n_max
  if (n > n_max) {
    if (!is.null(seed)) {
      set.seed(seed)
    }
    data <- data[sample(n, min(n_max, n)), , drop = FALSE]
  }
  # Update flashlight
  x <- flashlight(x, data = data, by = NULL,
                  linkinv = if (use_linkinv) x$linkinv else function(z) z)

  mean_pred <- function(x, data = x$data, w = x$w) {
    weighted_mean(predict(x, data = data), w, na.rm = TRUE)
  }

  baseline <- mean_pred(x)
  prediction <- unname(predict(x, data = new_obs))

  # Determine v and its order
  if (is.null(v)) {
    v <- setdiff(colnames(data), c(x$y, by, x$w))
  }
  if (order_by_importance) {
    ind_impact <- vapply(v, function(vi) {
      data[[vi]] <- new_obs[[vi]];
      (mean_pred(x = x, data = data) - baseline)^2
    }, FUN.VALUE = numeric(1))
    v <- names(sort(-ind_impact))
  }
  if (top_m < length(v)) {
    v <- v[seq_len(top_m)]
  }

  # Calculate contributions
  stopifnot((m <- length(v)) >= 1L,
            !(c("baseline", "prediction") %in% v))
  mean_pred_vector <- numeric(m)
  for (i in 1:m) {
    data[[v[i]]] <- new_obs[[v[i]]]
    mean_pred_vector[i] <- mean_pred(x, data = data)
  }

  # Combine results
  out <- tibble(0:(m + 1),
                 c("baseline", v, "prediction"),
                 c(baseline, mean_pred_vector, prediction))
  colnames(out) <- c(step_name, variable_name, after_name)
  out[[before_name]] <- lag(out[[after_name]], default = baseline)
  out[[label_name]] <- x$label

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

  # Add by variables
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
