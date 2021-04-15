#' Visualize 2D-Profiles, e.g. of Partial Dependence
#'
#' Minimal visualization of an object of class \code{light_profile2d}. The object returned is of class \code{ggplot} and can be further customized.
#'
#' The main geometry is geom_tile. If there is a "by" variable or a multiflashlight, this first dimension is taken care by color (or if \code{swap_dim = TRUE} by facets). If there are two "by" variables or a multiflashlight with one "by" variable, the first "by" variable is visualized as color, the second one or the multiflashlight via facet (change with \code{swap_dim}).
#'
#' @import ggplot2
#' @importFrom stats reformulate
#' @method plot light_profile2d
#' @param x An object of class \code{light_profile}.
#' @param swap_dim Swap the `facet_grid` dimensions.
#' @param rotate_x Should x axis labels be rotated by 45 degrees?
#' @param rotate_y Should y axis labels be rotated by 45 degrees?
#' @param ... Further arguments passed to \code{geom_tile}.
#' @return An object of class \code{ggplot2}.
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' fl <- flashlight(model = fit, label = "iris", data = iris, y = "Sepal.Length")
#' grid <- expand_grid(Species = unique(iris$Species), Petal.Length = c(2:5))
#' plot(light_profile2d(fl, pd_grid = grid))
#' plot(light_profile2d(fl, pd_grid = grid, type = "response"))
#' @seealso \code{\link{light_profile}}, \code{\link{plot.light_effects}}.
plot.light_profile2d <- function(x, swap_dim = FALSE,
                               rotate_x = FALSE, rotate_y = FALSE, ...) {
  value_name <- getOption("flashlight.value_name")
  label_name <- getOption("flashlight.label_name")
  type_name <- getOption("flashlight.type_name")

  multi <- is.light_profile_multi(x)
  ndim <- length(x$by) + multi
  if (ndim > 2L) {
    stop("Plot method not defined for more than two by variables or
         multiflashlight with more than one by variable.")
  }

  # Build plot
  p <- ggplot(x$data, aes_string(x = x$v[1], y = x$v[2], fill = value_name)) +
    geom_tile(...)
  if (ndim == 1L) {
    first_dim <- if (multi) label_name else x$by[1]
    p <- p + facet_wrap(reformulate(first_dim))
  } else if (ndim == 2L) {
    d1 <- if (multi) label_name else x$by[1]
    d2 <- if (multi) x$by[1] else x$by[2]
    form <- if (!swap_dim) reformulate(d1, d2) else reformulate(d2, d1)
    p <- p + facet_grid(form)
  }
  if (rotate_x || rotate_y) {
    ele <- element_text(angle = 45, hjust = 1, vjust = 1)
    if (rotate_x) {
      p <- p + theme(axis.text.x = ele)
    }
    if (rotate_y) {
      p <- p + theme(axis.text.y = ele)
    }
  }
  p
}

