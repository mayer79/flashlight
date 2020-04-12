#' Visualize Model Performance
#'
#' Minimal visualization of an object of class \code{light_performance} as \code{geom_bar}. The object returned has class \code{ggplot} and can be further customized.
#'
#' The plot is organized as a bar plot as follows: For flashlights without "by" variable specified, a single bar is drawn. Otherwise, the "by" variable (or the flashlight label if there is no "by" variable) is represented by the "x" aesthetic. The flashlight label (in case of one "by" variable) is represented by dodged bars. This strategy makes sure that performance of different flashlights can be compared easiest. Set "swap_dim = TRUE" to revert the role of dodging and x aesthetic. Different metrics are always represented by facets.
#'
#' @import ggplot2
#' @importFrom stats reformulate
#' @method plot light_performance
#' @param x An object of class \code{light_performance}.
#' @param swap_dim Should representation of dimensions (either two "by" variables or one "by" variable and multiflashlight) of x aesthetic and dodge fill aesthetic be swapped? Default is FALSE.
#' @param geom Geometry of plot (either "bar" or "point")
#' @param facet_scales Scales argument passed to \code{facet_wrap}.
#' @param rotate_x Should x axis labels be rotated by 45 degrees? Default is FALSE.
#' @param ... Further arguments passed to \code{geom_bar} or \code{geom_point}.
#' @return An object of class \code{ggplot2}.
#' @export
#' @examples
#' fit_full <- lm(Sepal.Length ~ ., data = iris)
#' fit_part <- lm(Sepal.Length ~ Petal.Length, data = iris)
#' mod_full <- flashlight(model = fit_full, label = "full", data = iris, y = "Sepal.Length")
#' mod_part <- flashlight(model = fit_part, label = "part", data = iris, y = "Sepal.Length")
#' mods <- multiflashlight(list(mod_full, mod_part))
#' plot(light_performance(mods), fill = "darkred")
#' plot(light_performance(mods, by = "Species"))
#' plot(light_performance(mods, by = "Species"), swap_dim = TRUE)
#' @seealso \code{\link{light_performance}}.
plot.light_performance <- function(x, swap_dim = FALSE,
                                   geom = c("bar", "point"),
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
    p <- ggplot(data, aes_string(y = x$value_name, x = if (nby) x$by[1] else x$label_name))
    if (geom == "bar") {
      p <- p + geom_bar(stat = "identity", ...)
    } else if (geom == "point") {
      p <- p + geom_point(...)
    }
  } else {
    second_dim <- if (multi) x$label_name else x$by[2]
    x_var <- if (!swap_dim) x$by[1] else second_dim
    dodge_var <- if (!swap_dim) second_dim else x$by[1]

    p <- ggplot(data, aes_string(y = x$value_name, x = x_var))
    if (geom == "bar") {
      p <- p + geom_bar(aes_string(fill = dodge_var),
                        stat = "identity", position = "dodge", ...)
    } else if (geom == "point") {
      p <- p + geom_point(aes_string(group = dodge_var, color = dodge_var), ...)
    }
  }

  # Multiple metrics always go into a facet due to different y scales
  if (length(unique(data[[x$metric_name]])) >= 2L) {
    p <- p + facet_wrap(reformulate(x$metric_name), scales = facet_scales)
  }
  if (rotate_x) {
    p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  }
  p
}
