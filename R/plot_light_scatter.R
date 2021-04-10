#' Scatter Plot
#'
#' Values are plotted against a variable. The object returned is of class \code{ggplot} and can be further customized. To avoid overplotting, pass e.g. \code{alpha = 0.2} or \code{position = "jitter"}.
#'
#' @import ggplot2
#' @importFrom stats reformulate
#' @method plot light_scatter
#' @param x An object of class \code{light_scatter}.
#' @param swap_dim If multiflashlight and one "by" variable or single flashlight with two "by" variables, swap the role of color variable and facet variable. If multiflashlight or one "by" variable, use colors instead of facets.
#' @param facet_scales Scales argument passed to \code{facet_wrap}.
#' @param rotate_x Should x axis labels be rotated by 45 degrees? Default is \code{FALSE}.
#' @param ... Further arguments passed to \code{geom_point}. Typical arguments would be \code{alpha = 0.2} or \code{position = "jitter"} to avoid overplotting.
#' @return An object of class \code{ggplot2}.
#' @export
#' @examples
#' fit_a <- lm(Sepal.Length ~ . -Petal.Length, data = iris)
#' fit_b <- lm(Sepal.Length ~ ., data = iris)
#' fl_a <- flashlight(model = fit_a, label = "without Petal.Length")
#' fl_b <- flashlight(model = fit_b, label = "all")
#' fls <- multiflashlight(list(fl_a, fl_b), data = iris, y = "Sepal.Length")
#' pr <- light_scatter(fls, v = "Petal.Length")
#' plot(pr, alpha = 0.2)
#' plot(light_scatter(fls, "Petal.Length", by = "Species"), alpha = 0.2)
#' @seealso \code{\link{light_scatter}}.
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
  p <- ggplot(x$data, aes_string(y = value_name, x = x$v))
  if (ndim == 0L) {
    p <- p + geom_point(...)
  } else if (ndim == 1L) {
    first_dim <- if (multi) label_name else x$by[1]
    if (swap_dim) {
      p <- p + geom_point(aes_string(color = first_dim), ...)
    } else {
      p <- p + geom_point(...) +
        facet_wrap(reformulate(first_dim), scales = facet_scales)
     }
  } else {
    second_dim <- if (multi) label_name else x$by[2]
    wrap_var <- if (swap_dim) x$by[1] else second_dim
    col_var <- if (swap_dim) second_dim else x$by[1]
    p <- p + geom_point(aes_string(color = col_var), ...) +
      facet_wrap(wrap_var, scales = facet_scales)
  }
  if (rotate_x) {
    p <- p +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  }
  p + ylab(x$type)
}

