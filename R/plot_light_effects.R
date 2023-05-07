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

  value_name <- getOption("flashlight.value_name")
  label_name <- getOption("flashlight.label_name")
  q1_name <- getOption("flashlight.q1_name")
  q3_name <- getOption("flashlight.q3_name")
  type_name <- getOption("flashlight.type_name")

  nby <- length(x$by)
  multi <- is.light_effects_multi(x)
  if (nby + multi > 1L) {
    stop("Plot method unavailable for multiple 'by' variables or a multiflashlight and a 'by' variable.")
  }

  # Combine data for plotting points and lines
  data <- dplyr::bind_rows(x[setdiff(use, if (x$stats == "quartiles") "response")])

  # Remove 0 count entries in "data"
  n <- nrow(data)
  if (!zero_counts && n) {
    data <- dplyr::semi_join(data, x$response, by = c(label_name, x$by, x$v))
  }

  # Prepare crossbar if required
  crossbar_required <- x$stats == "quartiles" && "response" %in% use
  if (crossbar_required) {
    crossbar <- ggplot2::geom_crossbar(
      data = x$response,
      ggplot2::aes(ymin = .data[[q1_name]], ymax = .data[[q3_name]]),
      width = 0.3,
      fill = "darkblue",
      colour = "black",
      alpha = 0.1,
      ...
    )
  }

  # Put together the plot
  if (n) {
    tp <- type_name
    p <- ggplot2::ggplot(
      data, ggplot2::aes(y = .data[[value_name]], x = .data[[x$v]])
    ) +
      ggplot2::geom_line(
        ggplot2::aes(color = .data[[tp]], group = .data[[tp]]),
        linewidth = size_factor / 3,
        ...
      )
    if (show_points) {
      p <- p + ggplot2::geom_point(
        ggplot2::aes(color = .data[[tp]], group = .data[[tp]]), size = size_factor, ...)
    }
    if (crossbar_required) {
      p <- p + crossbar
    }
  } else {
    p <- ggplot2::ggplot(
      x$response, ggplot2::aes(y = .data[[value_name]], x = .data[[x$v]])
    ) +
      crossbar
  }
  if (multi || nby) {
    p <- p + ggplot2::facet_wrap(
      if (multi) label_name else x$by[1L], scales = facet_scales, nrow = facet_nrow)
  }
  p <- p +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom", legend.title = ggplot2::element_blank())
  if (rotate_x) {
    p <- p + ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
    )
  }
  p
}
