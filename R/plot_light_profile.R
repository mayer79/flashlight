#' Visualize Profiles, e.g. of Partial Dependence
#'
#' Minimal visualization of an object of class \code{light_profile}. The object returned is of class \code{ggplot} and can be further customized.
#'
#' Either lines and points are plotted (if stats = "mean") or quartile boxes. If there is a "by" variable or a multiflashlight, this first dimension is taken care by color (or if \code{swap_dim = TRUE} by facets). If there are two "by" variables or a multiflashlight with one "by" variable, the first "by" variable is visualized as color, the second one or the multiflashlight via facet (change with \code{swap_dim}).
#'
#' @import ggplot2
#' @importFrom stats reformulate
#' @method plot light_profile
#' @param x An object of class \code{light_profile}.
#' @param swap_dim If multiflashlight and one "by" variable or single flashlight with two "by" variables, swap the role of dodge/fill variable and facet variable. If multiflashlight or one "by" variable, use facets instead of colors.
#' @param facet_scales Scales argument passed to \code{facet_wrap}.
#' @param rotate_x Should x axis labels be rotated by 45 degrees? TRUE, except for type "partial dependence".
#' @param show_points Should points be added to the line (default is \code{TRUE}).
#' @param ... Further arguments passed to \code{geom_point} and \code{geom_line}.
#' @return An object of class \code{ggplot2}.
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' fl <- flashlight(model = fit, label = "iris", data = iris, y = "Sepal.Length")
#' plot(light_profile(fl, v = "Species"))
#' plot(light_profile(fl, v = "Petal.Width", by = "Species", evaluate_at = 2:4))
#' plot(light_profile(fl, v = "Petal.Width", type = "predicted"))
#' @seealso \code{\link{light_profile}}, \code{\link{plot.light_effects}}.
plot.light_profile <- function(x, swap_dim = FALSE, facet_scales = "free_x",
                               rotate_x = x$type != "partial dependence",
                               show_points = TRUE, ...) {
  value_name <- getOption("flashlight.value_name")
  label_name <- getOption("flashlight.label_name")
  q1_name <- getOption("flashlight.q1_name")
  q3_name <- getOption("flashlight.q3_name")
  type_name <- getOption("flashlight.type_name")

  data <- x$data
  nby <- length(x$by)
  multi <- is.light_profile_multi(x)
  ndim <- nby + multi
  if (ndim > 2L) {
    stop("Plot method not defined for more than two by variables or
         multiflashlight with more than one by variable.")
  }
  if (length(x$v) >= 2L) {
    stop("No plot method defined for two or higher dimensional grids.")
  }
  # Distinguish some cases
  if (x$stats == "quartiles") {
    p <- ggplot(x$data, aes_string(y = value_name, x = x$v,
                                   ymin = q1_name, ymax = q3_name))
  } else {
    p <- ggplot(x$data, aes_string(y = value_name, x = x$v))
  }
  if (ndim == 0L) {
    if (x$stats == "quartiles") {
      p <- p + geom_crossbar(...)
    }
    else {
      p <- p + geom_line(aes(group = 1), ...)
      if (show_points) {
        p <- p + geom_point(...)
      }
    }
  } else if (ndim == 1L) {
    first_dim <- if (multi) label_name else x$by[1]
    if (!swap_dim) {
      if (x$stats == "quartiles") {
        p <- p +
          geom_crossbar(aes_string(color = first_dim), position = "dodge", ...)
      } else {
        p <- p +
          geom_line(aes_string(color = first_dim, group = first_dim), ...)
        if (show_points) {
          p <- p + geom_point(aes_string(color = first_dim), ...)
        }
      }
    } else {
      p <- p +
        facet_wrap(reformulate(first_dim), scales = facet_scales)
      if (x$stats == "quartiles") {
        p <- p + geom_crossbar(...)
      } else {
        p <- p + geom_line(aes(group = 1), ...)
        if (show_points) {
          p <- p + geom_point(...)
        }
      }
    }
  } else {
    second_dim <- if (multi) label_name else x$by[2]
    wrap_var <- if (swap_dim) x$by[1] else second_dim
    col_var <- if (swap_dim) second_dim else x$by[1]

    if (x$stats == "quartiles") {
      p <- p +
        geom_crossbar(aes_string(color = col_var), position = "dodge", ...)
    } else {
      p <- p + geom_line(aes_string(color = col_var, group = col_var), ...)
      if (show_points) {
        p <- p + geom_point(aes_string(color = col_var), ...)
      }
    }
    p <- p + facet_wrap(wrap_var, scales = facet_scales)
  }
  if (rotate_x) {
    p <- p +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  }
  p + ylab(x$type)
}

