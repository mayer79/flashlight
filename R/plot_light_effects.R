#' Visualize Multiple Types of Profiles Together
#'
#' Visualizes response-, prediction-, partial dependence, and/or ALE profiles
#' of a (multi-)flashlight with respect to a covariable `v`.
#' Different flashlights or a single flashlight with one "by" variable are separated
#' by a facet wrap.
#'
#' @importFrom rlang .data
#'
#' @inheritParams plot.light_performance
#' @param x An object of class "light_effects".
#' @param use A vector of elements to show. Any subset of ("response", "predicted",
#'   "pd", "ale") or "all". Defaults to all except "ale"
#' @param zero_counts Logical flag if 0 count levels should be shown on the x axis.
#' @param size_factor Factor used to enlarge default `size/linewidth` in
#'   [ggplot2::geom_point()] and [ggplot2::geom_line()].
#' @param facet_nrow Number of rows in [ggplot2::facet_wrap()].
#'   Must be 1 if [plot_counts()] should be used.
#' @param show_points Should points be added to the line (default is `TRUE`).
#' @param ... Further arguments passed to geoms.
#' @returns An object of class "ggplot".
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' fl <- flashlight(model = fit, label = "iris", data = iris, y = "Sepal.Length")
#' plot(light_effects(fl, v = "Species"))
#' @seealso [light_effects()], [plot_counts()]
plot.light_effects <- function(x, use = c("response", "predicted", "pd"),
                               zero_counts = TRUE, size_factor = 1,
                               facet_scales = "free_x", facet_nrow = 1L,
                               rotate_x = TRUE, show_points = TRUE, ...) {
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
  data <- dplyr::bind_rows(x[use])

  # Remove 0 count entries in "data"
  n <- nrow(data)
  if (!zero_counts && n) {
    data <- dplyr::semi_join(data, x$response, by = c("label_", x$by, x$v))
  }

  # Put together the plot
  if (n) {
    p <- ggplot2::ggplot(data, ggplot2::aes(y = value_, x = .data[[x$v]])) +
      ggplot2::geom_line(
        ggplot2::aes(color = type_, group = type_), linewidth = size_factor/3, ...
      )
    if (show_points) {
      p <- p + ggplot2::geom_point(
        ggplot2::aes(color = type_, group = type_), size = size_factor, ...
      )
    }
  } else {
    p <- ggplot2::ggplot(x$response, ggplot2::aes(y = value_, x = .data[[x$v]]))
  }
  if (multi || nby) {
    p <- p + ggplot2::facet_wrap(
      if (multi) "label_" else x$by[1L], scales = facet_scales, nrow = facet_nrow
    )
  }
  p <- p +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom", legend.title = ggplot2::element_blank())
  if (rotate_x) {
    p <- p + rotate_x()
  }
  p + ylab("Value")
}
