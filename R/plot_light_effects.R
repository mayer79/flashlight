#' Visualize Multiple Types of Profiles Together
#'
#' Visualizes response-, prediction-, partial dependence, and/or ALE profiles of a (multi-)flashlight with respect to a covariable \code{v}. Different flashlights or a single flashlight with one "by" variable are separated by a facet wrap.
#'
#' @import ggplot2
#' @importFrom stats reformulate
#' @importFrom dplyr semi_join bind_rows
#' @method plot light_effects
#' @param x An object of class \code{light_effects}.
#' @param use A vector of elements to show. Any subset of ("response", "predicted", "pd", "ale") or "all". Defaults to all except "ale"
#' @param zero_counts Logical flag if 0 count levels should be shown on the x axis.
#' @param size_factor Factor used to enlarge default \code{size} in \code{geom_point} and \code{geom_line}.
#' @param facet_scales Scales argument passed to \code{facet_wrap}.
#' @param facet_nrow Number of rows in \code{facet_wrap}. Must be 1 if \code{plot_counts} should be used.
#' @param rotate_x Should x axis labels be rotated by 45 degrees?
#' @param show_points Should points be added to the line (default is \code{TRUE}).
#' @param ... Further arguments passed to geoms.
#' @return An object of class \code{ggplot2}.
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' fl <- flashlight(model = fit, label = "iris", data = iris, y = "Sepal.Length")
#' plot(light_effects(fl, v = "Species"))
#' @seealso \code{\link{light_effects}}, \code{\link{plot_counts}}.
plot.light_effects <- function(x, use = c("response", "predicted", "pd"),
                               zero_counts = TRUE, size_factor = 1,
                               facet_scales = "free_x",
                               facet_nrow = 1L, rotate_x = TRUE,
                               show_points = TRUE, ...) {
  # Checks
  stopifnot(length(use) >= 1L)
  if ("all" %in% use) {
    use <- c("response", "predicted", "pd", "ale")
  }
  nby <- length(x$by)
  multi <- is.light_effects_multi(x)
  if (nby + multi > 1L) {
    stop("Plot method unavailable for multiple 'by' variables or a multiflashlight and a 'by' variable.")
  }

  # Combine data for plotting points and lines
  data <- bind_rows(x[setdiff(use, if (x$stats == "quartiles") "response")])

  # Remove 0 count entries in "data"
  n <- nrow(data)
  if (!zero_counts && n) {
    data <- semi_join(data, x$response, by = c(x$label_name, x$by, x$v))
  }

  # Prepare crossbar if required
  crossbar_required <- x$stats == "quartiles" && "response" %in% use
  if (crossbar_required) {
    crossbar <- geom_crossbar(data = x$response, aes_string(ymin = x$q1_name, ymax = x$q3_name),
                              width = 0.3, fill = "darkblue", colour = "black", alpha = 0.1, ...)
  }

  # Put together the plot
  if (n) {
    tp <- x$type_name
    p <- ggplot(data, aes_string(y = x$value_name, x = x$v)) +
      geom_line(aes_string(color = tp, group = tp), size = size_factor / 3, ...)
    if (show_points) {
      p <- p + geom_point(aes_string(color = tp, group = tp), size = size_factor, ...)
    }
    if (crossbar_required) {
      p <- p + crossbar
    }
  } else {
    p <- ggplot(x$response, aes_string(y = x$value_name, x = x$v)) +
      crossbar
  }
  if (multi || nby) {
    p <- p + facet_wrap(reformulate(if (multi) x$label_name else x$by[1]),
                        scales = facet_scales, nrow = facet_nrow)
  }
  p <- p +
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank())
  if (rotate_x) {
    p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  }
  p
}
