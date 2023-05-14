#' Visualize ICE profiles
#'
#' Minimal visualization of an object of class "light_ice" as [ggplot2::geom_line()].
#' The object returned is of class "ggplot" and can be further customized.
#'
#' Each observation is visualized by a line. The first "by" variable is represented
#' by the color, a second "by" variable or a multiflashlight by facets.
#'
#' @importFrom rlang .data
#'
#' @inheritParams plot.light_performance
#' @param x An object of class "light_ice".
#' @param ... Further arguments passed to [ggplot2::geom_line()].
#' @returns An object of class "ggplot".
#' @export
#' @examples
#' fit_full <- stats::lm(Sepal.Length ~ ., data = iris)
#' fit_part <- stats::lm(Sepal.Length ~ Petal.Length, data = iris)
#' mod_full <- flashlight(model = fit_full, label = "full", data = iris)
#' mod_part <- flashlight(model = fit_part, label = "part", data = iris)
#' mods <- multiflashlight(list(mod_full, mod_part))
#' plot(light_ice(mod_full, v = "Species"), alpha = 0.2)
#' indices <- (1:15) * 10
#' plot(light_ice(mods, v = "Species", indices = indices))
#' plot(light_ice(mods, v = "Species", indices = indices, center = "first"))
#' plot(light_ice(mods, v = "Petal.Width", by = "Species", n_bins = 5, indices = indices))
#' @seealso [light_ice()]
plot.light_ice <- function(x, facet_scales = "fixed", rotate_x = FALSE, ...) {
  nby <- length(x$by)
  multi <- is.light_ice_multi(x)

  if (nby + multi > 2L) {
    stop("Plot method not defined for more than two by variables or
         multiflashlight with more than one by variable.")
  }
  if (length(x$v) >= 2L) {
    stop("No plot method defined for two or higher dimensional grids.")
  }

  data <- x$data

  # Distinguish cases
  if (nby == 0L) {
    p <- ggplot2::ggplot(
      data, ggplot2::aes(y = value_, x = .data[[x$v]], group = id_)
    ) +
      ggplot2::geom_line(...)
  } else {
    stopifnot(!("temp_" %in% colnames(data)))
    data <- transform(data, temp_ = interaction(id_, x$by[1L]))
    p <- ggplot2::ggplot(
      data, ggplot2::aes(y = value_, x = .data[[x$v]], group = temp_)
    ) +
      ggplot2::geom_line(ggplot2::aes(color = .data[[x$by[1L]]]), ...) +
      override_alpha()
  }
  if (nby > 1L || multi) {
    p <- p + ggplot2::facet_wrap(
      if (multi) "label_" else x$by[2L], scales = facet_scales
    )
  }
  if (rotate_x) {
    p <- p + rotate_x()
  }
  p
}

