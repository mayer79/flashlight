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
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' fl <- flashlight(model = fit, label = "ols", data = iris, y = "Sepal.Length")
#' plot(light_performance(fl, by = "Species"), fill = "darkred")
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
