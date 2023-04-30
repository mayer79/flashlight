#' Visualize 2D-Profiles, e.g., of Partial Dependence
#'
#' Minimal visualization of an object of class \code{light_profile2d}.
#' The object returned is of class "ggplot" and can be further customized.
#'
#' The main geometry is \code{ggplot2::geom_tile()}. Additional dimensions
#' ("by" variable(s) and/or multiflashlight) are represented by \code{facet_wrap/grid}.
#' For all types of profiles except "partial dependence", it is natural to see
#' empty parts in the plot. These are combinations of the \code{v} variables that
#' do not appear in the data. Even for type "partial dependence", such gaps can occur,
#' e.g. for \code{cut_type = "quantile"} or if \code{n_bins} are larger than the number
#' of distinct values of a \code{v} variable.
#' Such gaps can be suppressed by setting \code{numeric_as_factor = TRUE}
#' or by using the arguments \code{breaks}, \code{pd_evaluate_at} or \code{pd_grid} in
#' \code{light_profile2d()}.
#'
#' @importFrom rlang .data
#' @param x An object of class "light_profile2d".
#' @param swap_dim Swap the \code{ggplot2::facet_grid()} dimensions.
#' @param rotate_x Should the x axis labels be rotated by 45 degrees?
#' Default is \code{TRUE}.
#' @param numeric_as_factor Should numeric x and y values be converted to factors first?
#' Default is \code{FALSE}. Useful if \code{cut_type} was not set to "equal".
#' @param ... Further arguments passed to \code{ggplot2::geom_tile()}.
#' @return An object of class "ggplot".
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' fl <- flashlight(model = fit, label = "iris", data = iris, y = "Sepal.Length")
#' plot(light_profile2d(fl, v = c("Petal.Length", "Species")))
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
  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data[[x$v[1L]]], y = .data[[x$v[2L]]], fill = .data[[value_name]])
  ) +
    ggplot2::geom_tile(...)
  if (ndim == 1L) {
    p <- p + ggplot2::facet_wrap(if (multi) label_name else x$by[1L])
  } else if (ndim == 2L) {
    d1 <- if (multi) label_name else x$by[1L]
    d2 <- if (multi) x$by[1L] else x$by[2L]
    form <- if (!swap_dim) stats::reformulate(d1, d2) else stats::reformulate(d2, d1)
    p <- p + ggplot2::facet_grid(form)
  }
  if (rotate_x) {
    p <- p + ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
    )
  }
  p
}

