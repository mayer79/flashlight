#' Variable Contribution Breakdown for Single Observations.
#'
#' Calculates variable contribution breakdown for a single observation, see Gosiewska and Biecek [1]. For non-additive models, the order of the variables visited is relevant and is determined based on variable importance by default.
#'
#' The breakdown algorithm assigns the following attribution to the first variable (x): In \code{data}, all values in x are set to its value in the new observation. This will change the (weighted) average of predicitons on \code{data} by a certain amount. This amount measures the contribution of x on the prediction. If this algorithm would be repeated for all possible permutations of the input variables, the resulting attributions would equal SHAP values. Note that the minimum required elements in the (multi-) flashlight are "y", "predict_function", "model", "data" and "metrics". The latter two can also directly be passed to \code{light_breakdown}. Note that by default, no retransformation function is applied.
#'
#' @importFrom dplyr left_join bind_rows
#' @importFrom stats ave
#' @param x An object of class \code{flashlight} or \code{multiflashlight}.
#' @param new_obs One single new observation to calculate variable attribution for. Needs to be of same structure as \code{data}.
#' @param data An optional \code{data.frame}.
#' @param metric An optional named list of length one with a metric as element used to evaluate variable importance if \code{order_by_importance = TRUE}. Defaults to the first metric in the flashlight. The metric needs to be a function with at least four arguments: actual, predicted, case weights w and \code{...}.
#' @param v Vector of variables to assess contribution for. Defaults to all variables in \code{data}.
#' @param order_by_importance Logical flag indicated if \code{v} should be ordered by variable importance.
#' @param n_max Maximum number of rows to consider. Use if \code{data} is large.
#' @param seed An integer random seed used to shuffle rows if \code{n_max} is smaller than the number of rows in \code{data}.
#' @param lower_is_better Logical flag indicating if lower values in the metric are better or not in order to assess variable importance (only used if \code{order_by_importance = TRUE}). If set to FALSE, the increase in metric is multiplied by -1.
#' @param use_linkinv Should retransformation function be applied? Default is FALSE.
#' @param value_name Column name in resulting \code{data} containing the variable attribution. Defaults to "value".
#' @param label_name Column name in resulting \code{data} containing the label of the flashlight. Defaults to "label".
#' @param variable_name Column name in resulting \code{data} containing the variable names. Defaults to "variable".
#' @param ... Further arguments passed to \code{light_performance} (only used if \code{order_by_importance = TRUE}).
#' @return An object of class \code{light_breakdown}, \code{light} (and a list) with the following elements.
#' \itemize{
#'   \item \code{data} A tibble with results. Can be used to build fully customized visualizations.
#'   \item \code{by} Same as input \code{by}.
#'   \item \code{value_name} Same as input \code{value_name}.
#'   \item \code{label_name} Same as input \code{label_name}.
#'   \item \code{variable_name} Same as input \code{variable_name}.
#' }
#' @export
#' @references [1] A. Gosiewska and P. Biecek (2019). IBREAKDOWN: Uncertainty of model explanations for non-additive predictive models. ArXiv. <https://arxiv.org/abs/1903.11420>.
#' @examples
#' fit_part <- lm(Sepal.Length ~ Petal.Length, data = iris)
#' fit_full <- lm(Sepal.Length ~ ., data = iris)
#' mod_full <- flashlight(model = fit_full, label = "full", data = iris, y = "Sepal.Length")
#' mod_part <- flashlight(model = fit_part, label = "part", data = iris, y = "Sepal.Length")
#' mods <- multiflashlight(list(mod_full, mod_part), by = "Species")
#' light_importance(mod_full)
#' light_importance(mods)
#'
#' ir <- iris
#' ir$log_sl <- log(ir$Sepal.Length)
#' fit_lm <- lm(log_sl ~ Petal.Length, data = ir)
#' fit_glm <- glm(Sepal.Length ~ Petal.Length, data = ir, family = Gamma(link = log))
#' fl_lm <- flashlight(model = fit_lm, label = "lm", y = "log_sl", linkinv = exp)
#' fl_glm <- flashlight(model = fit_glm, label = "glm", y = "Sepal.Length",
#'   predict_function = function(m, X) predict(m, X, type = "response"))
#' fls <- multiflashlight(list(fl_lm, fl_glm), data = ir)
#' light_importance(fls, v = "Petal.Length", seed = 45)
#' light_importance(fls, v = "Petal.Length", seed = 45, use_linkinv = TRUE)
#' @seealso \code{\link{light_importance}}, \code{\link{plot.light_breakdown}}.
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
light_breakdown.flashlight <- function(x, new_obs, data = x$data, metric = x$metrics[1],
                                       v = NULL, order_by_importance = TRUE,
                                       n_max = Inf, seed = NULL,
                                       lower_is_better = TRUE, use_linkinv = FALSE,
                                       value_name = "value", label_name = "label",
                                       variable_name = "variable", ...) {
  stopifnot(!is.null(new_obs),
            !is.null(metric), length(metric) == 1L,
            (n <- nrow(data)) >= 1L,
            !anyDuplicated(c(by, "metric", value_name, label_name, variable_name)))

  if (!is.null(seed)) {
    set.seed(seed)
  }
  # Subsample to n_max
  if (n > n_max) {
    data <- data[sample(n, min(n_max, n)), , drop = FALSE]
  }

  if (is.null(v)) {
    v <- setdiff(colnames(data), c(x$y, x$by))
  }

  out <- list(data = imp, by = by, value_name = value_name,
              label_name = label_name, variable_name = variable_name)
  class(out) <- c("light_breakdown", "light", "list")
  out
}

#' @describeIn light_breakdown Variable attribution to single observation for a multiflashlight.
#' @export
light_breakdown.multiflashlight <- function(x, ...) {
  light_combine(lapply(x, light_breakdown, ...),
                new_class = "light_breakdown_multi")
}
