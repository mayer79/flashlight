#' Model Performance of Flashlight
#'
#' Calculates performance of a flashlight with respect to one or more performance measure.
#'
#' The minimal required elements in the (multi-) flashlight are "y", "predict_function", "model", "data" and "metrics". The latter two can also directly be passed to \code{light_performance}. Note that by default, no retransformation function is applied.
#'
#' @importFrom MetricsWeighted performance
#' @importFrom dplyr group_by summarize across cur_data
#' @importFrom tidyselect all_of
#' @param x An object of class \code{flashlight} or \code{multiflashlight}.
#' @param data An optional \code{data.frame}.
#' @param by An optional vector of column names used to additionally group the results. Will overwrite \code{x$by}.
#' @param metrics An optional named list with metrics. Each metric takes at least four arguments: actual, predicted, case weights w and \code{...}.
#' @param use_linkinv Should retransformation function be applied? Default is FALSE.
#' @param metric_name Column name in resulting \code{data} containing the name of the metric. Defaults to "metric".
#' @param value_name Column name in resulting \code{data} containing the value of the metric. Defaults to "value".
#' @param label_name Column name in resulting \code{data} containing the label of the flashlight. Defaults to "label".
#' @param ... Arguments passed from or to other functions.
#' @return An object of class \code{light_performance}, \code{light} (and a list) with the following elements.
#' \itemize{
#'   \item \code{data} A tibble containing the results. Can be used to build fully customized visualizations.
#'   \item \code{by} Same as input \code{by}.
#'   \item \code{metric_name} Same as input \code{metric_name}.
#'   \item \code{value_name} Same as input \code{value_name}.
#'   \item \code{label_name} Same as input \code{label_name}.
#' }
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' fl <- flashlight(model = fit, label = "lm", data = iris, y = "Sepal.Length")
#' light_performance(fl)
#' light_performance(fl, by = "Species")
#' @seealso \code{\link{plot.light_performance}}.
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
light_performance.flashlight <- function(x, data = x$data, by = x$by, metrics = x$metrics,
                                         use_linkinv = FALSE, metric_name = "metric",
                                         value_name = "value", label_name = "label", ...) {
  stopifnot(!anyDuplicated(c(metric_name, value_name, label_name, "pred_", by)),
            !is.null(metrics))

  # Update flashlight
  x <- flashlight(x, data = data, by = by,
                  linkinv = if (use_linkinv) x$linkinv else function(z) z)

  # Calculate predictions
  data[["pred_"]] <- predict(x)
  data[[x$y]] <- response(x)

  # Aggregate the result within by groups
  core_fun <- function(X) {
    performance(X, actual = x$y, predicted = "pred_", w = x$w,
                metrics = metrics, key = metric_name, value = value_name, ...)
  }
  data[[label_name]] <- x$label
  data <- group_by(data, across(all_of(c(label_name, by))))
  agg <- summarize(data, core_fun(cur_data()), .groups = "drop")

  # Collect results
  out <- list(data = agg, by = by, metric_name = metric_name,
              value_name = value_name, label_name = label_name)
  class(out) <- c("light_performance", "light", "list")
  out
}

#' @describeIn light_performance Model performance of multiflashlight object.
#' @export
light_performance.multiflashlight <- function(x, ...) {
   light_combine(lapply(x, light_performance, ...),
                 new_class = "light_performance_multi")
}
