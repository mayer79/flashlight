#' Scatter Plot
#'
#' Values are plotted against a variable. The object returned is of class "ggplot"
#' and can be further customized. To avoid overplotting, try `alpha = 0.2` or
#' `position = "jitter"`.
#'
#' @importFrom rlang .data
#'
#' @inheritParams plot.light_performance
#' @param x An object of class "light_scatter".
#' @param swap_dim If multiflashlight and one "by" variable, or single flashlight
#'   with two "by" variables, swap the role of color variable and facet variable.
#'   If multiflashlight or one "by" variable, use colors instead of facets.
#' @param ... Further arguments passed to [ggplot2::geom_point()]. Typical arguments
#'   would be `alpha = 0.2` or `position = "jitter"` to avoid overplotting.
#' @returns An object of class "ggplot".
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' fl <- flashlight(model = fit, label = "ols", data = iris)
#' plot(light_scatter(fl, v = "Petal.Length", by = "Species"), alpha = 0.2)
#' @seealso [light_scatter()]
plot.light_scatter <- function(x, swap_dim = FALSE, facet_scales = "free_x",
                              rotate_x = FALSE, ...) {
  value_name <- getOption("flashlight.value_name")
  label_name <- getOption("flashlight.label_name")

  data <- x$data
  nby <- length(x$by)
  multi <- is.light_scatter_multi(x)
  ndim <- nby + multi
  if (ndim > 2L) {
    stop("Plot method not defined for more than two by variables or
         multiflashlight with more than one by variable.")
  }
  # Distinguish some cases
  p <- ggplot2::ggplot(
    x$data, ggplot2::aes(x = .data[[x$v]], y = .data[[value_name]])
  )
  if (ndim == 0L) {
    p <- p + ggplot2::geom_point(...)
  } else if (ndim == 1L) {
    first_dim <- if (multi) label_name else x$by[1L]
    if (swap_dim) {
      p <- p +
        ggplot2::geom_point(ggplot2::aes(color = .data[[first_dim]]), ...) +
        ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(alpha = 1)))
    } else {
      p <- p +
        ggplot2::geom_point(...) +
        ggplot2::facet_wrap(first_dim, scales = facet_scales)
     }
  } else {
    second_dim <- if (multi) label_name else x$by[2L]
    wrap_var <- if (swap_dim) x$by[1L] else second_dim
    col_var <- if (swap_dim) second_dim else x$by[1L]
    p <- p +
      ggplot2::geom_point(ggplot2::aes(color = .data[[col_var]]), ...) +
      ggplot2::facet_wrap(wrap_var, scales = facet_scales) +
      ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(alpha = 1)))
  }
  if (rotate_x) {
    p <- p +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
      )
  }
  p + ggplot2::ylab(x$type)
}
