#' Create or Update a flashlight
#'
#' Creates or updates a "flashlight" object. If a flashlight is to be created,
#' all arguments are optional except `label`. If a flashlight is to be updated,
#' all arguments are optional up to `x` (the flashlight to be updated).
#'
#' @param x An object of class "flashlight". If not provided, a new flashlight is
#'   created based on further input. Otherwise, `x` is updated based on further input.
#' @param model A fitted model of any type. Most models require a customized
#'   `predict_function`.
#' @param data A `data.frame` or `tibble` used as basis for calculations.
#' @param y Variable name of response.
#' @param predict_function A real valued function with two arguments:
#'   A model and a data of the same structure as `data`.
#'   Only the order of the two arguments matter, not their names.
#' @param linkinv An inverse transformation function applied after `predict_function`.
#' @param w A variable name of case weights.
#' @param by A character vector with names of grouping variables.
#' @param metrics A named list of metrics. Here, a metric is a function with exactly
#'   four arguments: actual, predicted, w (case weights) and `...`
#'   like those in package {MetricsWeighted}.
#' @param label Name of the flashlight. Required.
#' @param shap An optional shap object. Typically added by calling [add_shap()].
#' @param check When updating the flashlight: Should internal checks be performed?
#'   Default is `TRUE`.
#' @param ... Arguments passed from or to other functions.
#' @returns An object of class "flashlight" (and `list`) containing each
#'   input (except `x`) as element.
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' (fl <- flashlight(model = fit, data = iris, y = "Sepal.Length", label = "ols"))
#' (fl_updated <- flashlight(fl, linkinv = exp))
#' @seealso [multiflashlight()]
flashlight <- function(x, ...) {
  UseMethod("flashlight")
}

#' @describeIn flashlight Used to create a flashlight object.
#' No `x` has to be passed in this case.
#' @export
flashlight.default <- function(x, model = NULL, data = NULL, y = NULL,
                               predict_function = stats::predict,
                               linkinv = function(z) z,
                               w = NULL, by = NULL,
                               metrics = list(rmse = MetricsWeighted::rmse),
                               label = NULL, shap = NULL, ...) {
  x <- c(
    list(
      model = model,
      data = data,
      y = y,
      predict_function = predict_function,
      linkinv = linkinv,
      w = w,
      by = by,
      metrics = metrics,
      label = label,
      shap = shap
    ),
    list(...)
  )
  class(x) <- c("flashlight", "list")
  light_check(x)
}

#' @describeIn flashlight Used to update an existing flashlight object.
#' @export
flashlight.flashlight <- function(x, check = TRUE, ...) {
  args <- list(...)
  x[names(args)] <- args
  if (check) light_check(x) else invisible(x)
}
