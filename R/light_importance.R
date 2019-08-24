#' Permutation Importance.
#'
#' Calculates performance per variable with respect to a performance measure before and after permuting its values. The difference is a measure of importance, see Fisher et al. 2018 [1].
#'
#' The minimum required elements in the (multi-) flashlight are "y", "predict_function", "model", "data" and "metrics". The latter two can also directly be passed to \code{light_importance}. Note that by default, no retransformation function is applied.
#'
#' @importFrom dplyr left_join bind_rows
#' @importFrom stats ave
#' @param x An object of class \code{flashlight} or \code{multiflashlight}.
#' @param data An optional \code{data.frame}.
#' @param by An optional vector of column names used to additionally group the results.
#' @param metric An optional named list of length one with a metric as element. Defaults to the first metric in the flashlight. The metric needs to be a function with at least four arguments: actual, predicted, case weights w and \code{...}.
#' @param v Vector of variables to assess importance for. Defaults to all variables in \code{data}.
#' @param n_max Maximum number of rows to consider. Use if \code{data} is large.
#' @param seed An integer random seed used to shuffle rows if \code{n_max} is smaller than the number of rows in \code{data}.
#' @param lower_is_better Logical flag indicating if lower values in the metric are better or not. If set to FALSE, the increase in metric is multiplied by -1.
#' @param use_linkinv Should retransformation function be applied? Default is FALSE.
#' @param metric_name Name of the resulting column containing the name of the metric. Defaults to "metric".
#' @param value_name Column name in resulting \code{data} containing the drop in performance. Defaults to "value".
#' @param label_name Column name in resulting \code{data} containing the label of the flashlight. Defaults to "label".
#' @param variable_name Column name in resulting \code{data} containing the variable name. Defaults to "variable".
#' @param ... Arguments passed from or to other methods.
#' @return An object of class \code{light_importance}, \code{light} (and a list) with the following elements.
#' \itemize{
#'   \item \code{data} A tibble with results. Can be used to build fully customized visualizations. The columns "value_original" and "value_shuffled" provide the performance before and after shuffling.
#'   \item \code{by} Same as input \code{by}.
#'   \item \code{metric_name} Column name representing the name of the metric. For information only.
#'   \item \code{value_name} Same as input \code{value_name}.
#'   \item \code{label_name} Same as input \code{label_name}.
#'   \item \code{variable_name} Same as input \code{variable_name}.
#' }
#' @export
#' @references [1] Fisher A., Rudin C., Dominici F. (2018). All Models are Wrong but many are Useful: Variable Importance for Black-Box, Proprietary, or Misspecified Prediction Models, using Model Class Reliance. ArXiv. <https://arxiv.org/abs/1801.01489>.
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
#' @seealso \code{\link{most_important}}, \code{\link{plot.light_importance}}.
light_importance <- function(x, ...) {
  UseMethod("light_importance")
}

#' @describeIn light_importance Default method not implemented yet.
#' @export
light_importance.default <- function(x, ...) {
  stop("light_importance method is only available for objects of class flashlight or multiflashlight.")
}

#' @describeIn light_importance Variable importance for a flashlight.
#' @export
light_importance.flashlight <- function(x, data = x$data, by = x$by,
                                        metric = x$metrics[1],
                                        v = NULL, n_max = Inf, seed = NULL,
                                        lower_is_better = TRUE, use_linkinv = FALSE,
                                        metric_name = "metric",
                                        value_name = "value", label_name = "label",
                                        variable_name = "variable", ...) {
  stopifnot(!is.null(metric), length(metric) == 1L,
            (n <- nrow(data)) >= 1L,
            !anyDuplicated(c(by, metric_name, value_name, label_name, variable_name)),
            !(c("value_original", "value_shuffled", variable_name) %in% by))

  # Update flashlight with everything except data and linkinv
  x <- flashlight(x, by = by, metrics = metric)

  if (is.null(v)) {
    v <- setdiff(colnames(data), c(x$y, x$by))
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }
  # Subsample to n_max
  if (n > n_max) {
    data <- data[sample(n, min(n_max, n)), , drop = FALSE]
  }
  # Performance before shuffling
  metric_full <- light_performance(x, data = data, use_linkinv = use_linkinv,
                                   metric_name = metric_name,
                                   value_name = "value_original",
                                   label_name = label_name, ...)$data
  # Performance difference after shuffling
  imp <- lapply(v, function(z) {
    shuffled <- data
    if (length(by)) {
      shuffled[[z]] <- ave(shuffled[[z]], shuffled[, by, drop = FALSE], FUN = sample)
    } else {
      shuffled[[z]] <- sample(shuffled[[z]])
    }
    light_performance(x, data = shuffled, use_linkinv = use_linkinv,
                      metric_name = metric_name,
                      value_name = "value_shuffled",
                      label_name = label_name, ...)$data
  })
  names(imp) <- v
  imp <- bind_rows(imp, .id = variable_name)
  imp <- left_join(imp, metric_full, by = c(label_name, metric_name, by))
  imp[[value_name]] <- (imp[["value_shuffled"]] - imp[["value_original"]]) *
    if (lower_is_better) 1 else -1
  out <- list(data = imp, by = by,
              metric_name = metric_name, value_name = value_name,
              label_name = label_name, variable_name = variable_name)
  class(out) <- c("light_importance", "light", "list")
  out
}

#' @describeIn light_importance Variable importance for a multiflashlight.
#' @export
light_importance.multiflashlight <- function(x, ...) {
  light_combine(lapply(x, light_importance, ...),
                new_class = "light_importance_multi")
}
