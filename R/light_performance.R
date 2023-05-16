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
#'   - `data`: A tibble containing the results.
#'   - `by` Same as input `by`.
#' @export
#' @examples
#' fit_part <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
#' fl_part <- flashlight(
#'   model = fit_part, label = "part", data = iris, y = "Sepal.Length"
#' )
#' plot(light_performance(fl_part, by = "Species"), fill = "chartreuse4")
#'
#' # Second model
#' fit_full <- lm(Sepal.Length ~ ., data = iris)
#' fl_full <- flashlight(
#'   model = fit_full, label = "full", data = iris, y = "Sepal.Length"
#' )
#' fls <- multiflashlight(list(fl_part, fl_full))
#'
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

#' Visualize Model Performance
#'
#' Minimal visualization of an object of class "light_performance" as
#' [ggplot2::geom_bar()]. The object returned has class "ggplot",
#' and can be further customized.
#'
#' The plot is organized as a bar plot as follows:
#' For flashlights without "by" variable specified, a single bar is drawn.
#' Otherwise, the "by" variable (or the flashlight label if there is no "by" variable)
#' is represented by the "x" aesthetic.
#'
#' The flashlight label (in case of one "by" variable) is represented by dodged bars.
#' This strategy makes sure that performance of different flashlights can
#' be compared easiest. Set "swap_dim = TRUE" to revert the role of dodging and x
#' aesthetic. Different metrics are always represented by facets.
#'
#' @importFrom rlang .data
#' @param x An object of class "light_performance".
#' @param swap_dim Should representation of dimensions
#'   (either two "by" variables or one "by" variable and multiflashlight)
#'   of x aesthetic and dodge fill aesthetic be swapped? Default is `FALSE`.
#' @param geom Geometry of plot (either "bar" or "point")
#' @param facet_scales Scales argument passed to [ggplot2::facet_wrap()].
#' @param rotate_x Should x axis labels be rotated by 45 degrees?
#' @param ... Further arguments passed to [ggplot2::geom_bar()] or
#'   [ggplot2::geom_point()].
#' @returns An object of class "ggplot".
#' @export
#' @seealso [light_performance()]
plot.light_performance <- function(x, swap_dim = FALSE, geom = c("bar", "point"),
                                   facet_scales = "free_y", rotate_x = FALSE, ...) {
  geom <- match.arg(geom)
  data <- x$data
  nby <- length(x$by)
  multi <- is.light_performance_multi(x)
  ndim <- nby + multi
  if (ndim > 2L) {
    stop("Plot method not defined for single flashlights with more than two by variables or
         a multiflashlight with more than one by variable.")
  }

  # Differentiate some plot cases
  if (ndim <= 1L) {
    xvar <- if (nby) x$by[1L] else "label_"
    p <- ggplot2::ggplot(data, ggplot2::aes(y = value_, x = .data[[xvar]]))
    if (geom == "bar") {
      p <- p + ggplot2::geom_bar(stat = "identity", ...)
    } else if (geom == "point") {
      p <- p + ggplot2::geom_point(...)
    }
  } else {
    second_dim <- if (multi) "label_" else x$by[2L]
    x_var <- if (!swap_dim) x$by[1L] else second_dim
    dodge_var <- if (!swap_dim) second_dim else x$by[1L]

    p <- ggplot2::ggplot(data, ggplot2::aes(y = value_, x = .data[[x_var]]))
    if (geom == "bar") {
      p <- p + ggplot2::geom_bar(
        ggplot2::aes(fill = .data[[dodge_var]]), stat = "identity", position = "dodge",
        ...
      )
    } else if (geom == "point") {
      p <- p + ggplot2::geom_point(
        ggplot2::aes(group = .data[[dodge_var]], color = .data[[dodge_var]]), ...
      )
    }
  }

  # Multiple metrics always go into a facet due to different y scales
  if (length(unique(data$metric_)) >= 2L) {
    p <- p + ggplot2::facet_wrap(~ metric_, scales = facet_scales)
  }
  if (rotate_x) {
    p <- p + rotate_x()
  }
  p + ggplot2::ylab("Value")
}
