#' Model Performance of Flashlight
#'
#' Calculates performance of a flashlight with respect to one or more
#' performance measure.
#'
#' The minimal required elements in the (multi-) flashlight are "y", "predict_function",
#' "model", "data" and "metrics". The latter two can also directly be passed to
#' [light_performance()]. Note that by default, no retransformation function is applied.
#'
#' @param x An object of class "flashlight" or "multiflashlight".
#' @param data An optional `data.frame`.
#' @param by An optional vector of column names used to additionally group the results.
#'   Will overwrite `x$by`.
#' @param metrics An optional named list with metrics. Each metric takes at least
#'   four arguments: actual, predicted, case weights w and `...`.
#' @param use_linkinv Should retransformation function be applied? Default is `FALSE`.
#' @param ... Arguments passed from or to other functions.
#' @returns
#'   An object of class "light_performance" with the following elements:
#'   - `data`: A tibble containing the results. Can be used to build fully customized
#'     visualizations. Column names can be controlled by
#'     `options(flashlight.column_name)`.
#'   - `by` Same as input `by`.
#' @export
#' @examples
#' fit_part <- stats::lm(Sepal.Length ~ Species + Petal.Length, data = iris)
#' fl_part <- flashlight(
#'   model = fit_part, label = "part", data = iris, y = "Sepal.Length"
#' )
#' fit_full <- stats::lm(Sepal.Length ~ ., data = iris)
#' fl_full <- flashlight(
#'   model = fit_full, label = "full", data = iris, y = "Sepal.Length"
#' )
#' fls <- multiflashlight(list(fl_part, fl_full))
#' plot(light_performance(fls), fill = "chartreuse4")
#' plot(light_performance(fls, by = "Species"))
#' plot(light_performance(fls, by = "Species"), swap_dim = TRUE)
#' @seealso [plot.light_performance()]
light_performance <- function(x, ...) {
  UseMethod("light_performance")
}

#' @describeIn light_performance Default method not implemented yet.
#' @export
light_performance.default <- function(x, ...) {
  stop("light_performance method is only available for objects of class flashlight or multiflashlight.")
}

#' @describeIn light_performance Model performance of flashlight object.
#' @export
light_performance.flashlight <- function(x, data = x$data, by = x$by,
                                         metrics = x$metrics,
                                         use_linkinv = FALSE, ...) {
  stopifnot(
    "No data!" = is.data.frame(data) && nrow(data) >= 1L,
    "'by' not in 'data'!" = by %in% colnames(data),
    "No metric!" = !is.null(metrics),
    "No 'y' defined in flashlight!" = !is.null(x$y),
    !any(c("metric_", "value_", "label_", "pred_") %in% by)
  )

  # Update flashlight
  x <- flashlight(
    x, data = data, by = by, linkinv = if (use_linkinv) x$linkinv else function(z) z
  )

  # Calculate predictions
  data$pred_ <- stats::predict(x)
  data[[x$y]] <- response(x)  # Applies linkinv

  # Aggregate the result within by groups
  core_fun <- function(X) {
    MetricsWeighted::performance(
      X,
      actual = x$y,
      predicted = "pred_",
      w = x$w,
      metrics = metrics,
      key = "metric_",
      value = "value_",
      ...
    )
  }
  agg <- Reframe(data, FUN = core_fun, .by = by)
  agg$label_ <- x$label

  # Organize output
  add_classes(list(data = agg, by = by), classes = c("light_performance", "light"))
}

#' @describeIn light_performance Model performance of multiflashlight object.
#' @export
light_performance.multiflashlight <- function(x, ...) {
   light_combine(
     lapply(x, light_performance, ...), new_class = "light_performance_multi"
    )
}
