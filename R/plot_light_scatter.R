#' Scatter Plot
#'
#' Values are plotted against a variable. The object returned is of class \code{ggplot} and can be further customized.
#'
#' @import ggplot2
#' @importFrom stats reformulate
#' @method plot light_scatter
#' @param x An object of class \code{light_scatter}.
#' @param swap_dim If multiflashlight and one "by" variable or single flashlight with two "by" variables, swap the role of color variable and facet variable. If multiflashlight or one "by" variable, use colors instead of facets.
#' @param facet_scales Scales argument passed to \code{facet_wrap}.
#' @param rotate_x Should x axis labels be rotated by 45 degrees? Default is \code{FALSE}.
#' @param position Position argument passed to \code{geom_point}.
#' @param ... Further arguments passed to \code{geom_point}.
#' @return An object of class \code{ggplot2}.
#' @export
#' @examples
#' \dontrun{
#' fit_a <- lm(Sepal.Length ~ . + Petal.Length:Species, data = iris)
#' fit_b <- lm(Sepal.Length ~ . + Petal.Length, data = iris)
#' fl_a <- flashlight(model = fit_a, label = "a")
#' fl_b <- flashlight(model = fit_b, label = "b")
#' fls <- multiflashlight(list(fl_a, fl_b), data = iris, y = "Sepal.Length")
#' pr <- light_scatter(fls, "Petal.Length")
#' plot(pr, alpha = 0.2)
#' plot(light_scatter(fls, "Petal.Length", by = "Species"), alpha = 0.2)
#' }
#' @seealso \code{\link{light_scatter}}.
plot.light_scatter <- function(x, swap_dim = FALSE, facet_scales = "free_x",
                              rotate_x = FALSE, position = "jitter", ...) {
  data <- x$data
  nby <- length(x$by)
  multi <- is.light_scatter_multi(x)
  ndim <- nby + multi
  if (ndim > 2L) {
    stop("Plot method not defined for more than two by variables or
         multiflashlight with more than one by variable.")
  }
  # Distinguish some cases
  p <- ggplot(x$data, aes_string(y = x$value, x = x$v))
  if (ndim == 0L) {
    p <- p + geom_point(position = position, ...)
  } else if (ndim == 1L) {
    first_dim <- if (multi) x$label_name else x$by[1]
    if (swap_dim) {
      p <- p + geom_point(aes_string(color = first_dim), position = position, ...)
    } else {
      p <- p + geom_point(position = position, ...) +
        facet_wrap(reformulate(first_dim), scales = facet_scales)
     }
  } else {
    second_dim <- if (multi) x$label_name else x$by[2]
    wrap_var <- if (swap_dim) x$by[1] else second_dim
    col_var <- if (swap_dim) second_dim else x$by[1]
    p <- p + geom_point(aes_string(color = col_var), position = position, ...) +
      facet_wrap(wrap_var, scales = facet_scales)
  }
  if (rotate_x) {
    p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  }
  p
}

