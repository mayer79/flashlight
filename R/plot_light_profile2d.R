#' Visualize 2D-Profiles, e.g. of Partial Dependence
#'
#' Minimal visualization of an object of class \code{light_profile2d}. The object returned is of class \code{ggplot} and can be further customized.
#'
#' The main geometry is \code{geom_tile}. Additional dimensions ("by" variable(s) and/or multiflashlight) are represented by \code{facet_wrap/grid}.
#' For all types of profiles except "partial dependence", it is natural to see empty parts in the plot. These are combinations of the \code{v} variables that do not appear in the data.
#' Even for type "partial dependence", gaps can occur, e.g. for \code{cut_type = "quantile"} or if \code{n_bins} are larger than the number of distinct values of a \code{v} variable.
#' Such gaps can be suppressed by setting \code{numeric_as_factor = TRUE} or by using the arguments \code{breaks}, \code{pd_evaluate_at} or \code{pd_grid} in \code{light_profile2d()}.
#'
#' @import ggplot2
#' @importFrom stats reformulate
#' @method plot light_profile2d
#' @param x An object of class \code{light_profile2d}.
#' @param swap_dim Swap the `facet_grid` dimensions.
#' @param rotate_x Should x axis labels be rotated by 45 degrees? Default is \code{TRUE}.
#' @param numeric_as_factor Should numeric x and y values be converted to factors first? Default is \code{FALSE}. Useful if \code{cut_type} was not set to "equal".
#' @param ... Further arguments passed to \code{geom_tile}.
#' @return An object of class \code{ggplot2}.
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' fl <- flashlight(model = fit, label = "iris", data = iris, y = "Sepal.Length")
#' plot(light_profile2d(fl, v = c("Petal.Length", "Species")))
#' pr <- light_profile2d(fl, v = c("Petal.Length", "Sepal.Width"),
#'   type = "predicted", by = "Species", n_bins=c(2, 3), sep = ";")
#' plot(pr)
#' @seealso \code{\link{light_profile2d}}.
plot.light_profile2d <- function(x, swap_dim = FALSE, rotate_x = TRUE,
                                 numeric_as_factor = FALSE, ...) {
  value_name <- getOption("flashlight.value_name")
  label_name <- getOption("flashlight.label_name")
  type_name <- getOption("flashlight.type_name")

  multi <- is.light_profile2d_multi(x)
  ndim <- length(x$by) + multi
  if (ndim > 2L) {
    stop("Plot method not defined for more than two by variables or
         multiflashlight with more than one by variable.")
  }
  data <- x$data
  if (isTRUE(numeric_as_factor)) {
    data[x$v] <- lapply(data[x$v], as.factor)
  }

  # Build plot
  p <- ggplot(data, aes_string(x = x$v[1], y = x$v[2], fill = value_name)) +
    geom_tile(...)
  if (ndim == 1L) {
    p <- p + facet_wrap(reformulate(if (multi) label_name else x$by[1]))
  } else if (ndim == 2L) {
    d1 <- if (multi) label_name else x$by[1]
    d2 <- if (multi) x$by[1] else x$by[2]
    form <- if (!swap_dim) reformulate(d1, d2) else reformulate(d2, d1)
    p <- p + facet_grid(form)
  }
  if (rotate_x) {
    p <- p +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  }
  p
}

