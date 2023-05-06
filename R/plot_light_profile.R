#' Visualize Profiles, e.g. Partial Dependence
#'
#' Minimal visualization of an object of class "light_profile".
#' The object returned is of class "ggplot" and can be further customized.
#'
#' Either lines and points are plotted (if stats = "mean") or quartile boxes.
#' If there is a "by" variable or a multiflashlight, this first dimension
#' is represented by color (or if `swap_dim = TRUE` by facets).
#' If there are two "by" variables or a multiflashlight with one "by" variable,
#' the first "by" variable is visualized as color, while the second one
#' or the multiflashlight is shown via facet (change with `swap_dim`).
#'
#' @importFrom rlang .data
#'
#' @inheritParams plot.light_performance
#' @param x An object of class "light_profile".
#' @param swap_dim If multiflashlight and one "by" variable or
#'   single flashlight with two "by" variables, swap the role of dodge/fill variable
#'   and facet variable. If multiflashlight or one "by" variable,
#'   use facets instead of colors.
#' @param show_points Should points be added to the line (default is `TRUE`).
#' @param ... Further arguments passed to [ggplot2::geom_point()] or
#'   [ggplot2::geom_line()].
#' @returns An object of class "ggplot".
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' fl <- flashlight(model = fit, label = "iris", data = iris, y = "Sepal.Length")
#' plot(light_profile(fl, v = "Species"))
#' plot(light_profile(fl, v = "Petal.Width", by = "Species", evaluate_at = 2:4))
#' plot(light_profile(fl, v = "Petal.Width", type = "predicted"))
#' @seealso [light_profile()], [plot.light_effects()]
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
    p <- ggplot2::ggplot(
      x$data,
      ggplot2::aes(
        y = .data[[value_name]],
        x = .data[[x$v]],
        ymin = .data[[q1_name]],
        ymax = .data[[q3_name]]
      )
    )
  } else {
    p <- ggplot2::ggplot(
      x$data, ggplot2::aes(y = .data[[value_name]], x = .data[[x$v]])
    )
  }
  if (ndim == 0L) {
    if (x$stats == "quartiles") {
      p <- p + ggplot2::geom_crossbar(...)
    }
    else {
      p <- p + ggplot2::geom_line(ggplot2::aes(group = 1), ...)
      if (show_points) {
        p <- p + ggplot2::geom_point(...)
      }
    }
  } else if (ndim == 1L) {
    first_dim <- if (multi) label_name else x$by[1L]
    if (!swap_dim) {
      if (x$stats == "quartiles") {
        p <- p + ggplot2::geom_crossbar(
          ggplot2::aes(color = .data[[first_dim]]), position = "dodge", ...
        )
      } else {
        p <- p + ggplot2::geom_line(
          ggplot2::aes(color = .data[[first_dim]], group = .data[[first_dim]]), ...
        )
        if (show_points) {
          p <- p + ggplot2::geom_point(ggplot2::aes(color = .data[[first_dim]]), ...)
        }
      }
    } else {
      p <- p + ggplot2::facet_wrap(first_dim, scales = facet_scales)
      if (x$stats == "quartiles") {
        p <- p + ggplot2::geom_crossbar(...)
      } else {
        p <- p + ggplot2::geom_line(ggplot2::aes(group = 1), ...)
        if (show_points) {
          p <- p + ggplot2::geom_point(...)
        }
      }
    }
  } else {
    second_dim <- if (multi) label_name else x$by[2L]
    wrap_var <- if (swap_dim) x$by[1L] else second_dim
    col_var <- if (swap_dim) second_dim else x$by[1L]

    if (x$stats == "quartiles") {
      p <- p + ggplot2::geom_crossbar(
        ggplot2::aes(color = .data[[col_var]]), position = "dodge", ...
      )
    } else {
      p <- p + ggplot2::geom_line(
        ggplot2::aes(color = .data[[col_var]], group = .data[[col_var]]), ...
      )
      if (show_points) {
        p <- p + ggplot2::geom_point(ggplot2::aes(color = .data[[col_var]]), ...)
      }
    }
    p <- p + ggplot2::facet_wrap(wrap_var, scales = facet_scales)
  }
  if (rotate_x) {
    p <- p + ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
      )
  }
  p + ggplot2::ylab(x$type)
}

