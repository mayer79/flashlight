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
#' @param ... Arguments passed from or to other functions.
#' @return An object of class \code{light_performance} with the following elements.
#' \itemize{
#'   \item \code{data} A tibble containing the results. Can be used to build fully customized visualizations. Column names can be controlled by \code{options(flashlight.column_name)}.
#'   \item \code{by} Same as input \code{by}.
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
light_performance.flashlight <- function(x, data = x$data, by = x$by,
                                         metrics = x$metrics,
                                         use_linkinv = FALSE, ...) {
  warning_on_names(c("metric_name", "value_name", "label_name"), ...)

  metric_name <- getOption("flashlight.metric_name")
  value_name <- getOption("flashlight.value_name")
  label_name <- getOption("flashlight.label_name")

  stopifnot(
    "No data!" = is.data.frame(data) && nrow(data) >= 1L,
    "'by' not in 'data'!" = by %in% colnames(data),
    "No metric!" = !is.null(metrics),
    "No 'y' defined in flashlight!" = !is.null(x$y)
  )
  check_unique(by, c(metric_name, value_name, label_name), temp_names = "pred_")

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

  # Organize output
  add_classes(list(data = agg, by = by),
              classes = c("light_performance", "light"))
}

#' @describeIn light_performance Model performance of multiflashlight object.
#' @export
light_performance.multiflashlight <- function(x, ...) {
   light_combine(lapply(x, light_performance, ...),
                 new_class = "light_performance_multi")
}
