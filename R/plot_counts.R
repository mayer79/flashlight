#' DEPRECATED - Add Counts to Effects Plot
#'
#' Add counts as labelled bar plot on top of light_effects plot.
#'
#' Experimental. Uses package {ggpubr} to rearrange the figure.
#' Thus, the resulting plot cannot be easily modified.
#' Furthermore, adding counts only works if the legend in [plot.light_effects()]
#' is not placed on the left or right side of the plot.
#' It has to be placed inside or at the bottom.
#'
#' @importFrom rlang .data
#'
#' @param p The result of [plot.light_effects()].
#' @param x An object of class "light_effects".
#' @param text_size Size of count labels.
#' @param facet_scales Scales argument passed to [ggplot2::facet_wrap()].
#' @param show_labels Should count labels be added as text?
#' @param big.mark Parameter passed to [format()] the labels. Default is "'".
#' @param scientific Parameter passed to [format()] the labels. Default is `FALSE`.
#' @param digits Used to round the labels. Default is 0.
#' @param ... Further arguments passed to [ggplot2::geom_bar()].
#' @returns An object of class "ggplot".
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' fl <- flashlight(model = fit, label = "iris", data = iris, y = "Sepal.Length")
#' x <- light_effects(fl, v = "Species")
#' plot_counts(plot(x), x, width = 0.3, alpha = 0.2)
#' @seealso [plot.light_effects()]
#' @export
plot_counts <- function(p, x, text_size = 3, facet_scales = "free_x",
                        show_labels = TRUE, big.mark = "'",
                        scientific = FALSE, digits = 0, ...) {
  message("Deprecated. Will be removed in flashlight 1.0.0.")

  # Checks
  stopifnot(
    ggplot2::is.ggplot(p),
    is.light_effects(x),
    !("lab_" %in% colnames(x$response))
  )

  label_name <- getOption("flashlight.label_name")
  counts_name <- getOption("flashlight.counts_name")

  multi <- is.light_effects_multi(x)

  # Deal with zero counts
  key <- c(x$by, x$v, label_name)
  x$response <- dplyr::right_join(
    x$response, unique(p$data[, key, drop = FALSE]), by = key
  )
  if (any((bad <- is.na(x$response[[counts_name]])))) {
    x$response[[counts_name]][bad] <- 0
  }

  # Prepare for plotting
  if (show_labels) {
    x$response[["lab_"]] <- format(
      round(x$response[[counts_name]], digits),
      big.mark = big.mark, scientific = scientific
    )
  }
  ct <- ggplot2::ggplot(
    x$response, ggplot2::aes(x = .data[[x$v]], y = .data[[counts_name]])
  ) +
    ggplot2::geom_bar(stat = "identity", ...) +
    ggplot2::theme_void() +
    ggplot2::theme(
      strip.text.x = ggplot2::element_blank(), panel.grid = ggplot2::element_blank()
    )
  if (show_labels) {
    ct <- ct +
      ggplot2::geom_text(
        ggplot2::aes(y = 0, label = lab_),
        angle = 90,
        hjust = -0.1,
        size = text_size
      )
  }
  if (multi || length(x$by)) {
    ct <- ct + ggplot2::facet_wrap(
      if (multi) label_name else x$by[1L], scales = facet_scales, nrow = 1L
    )
  }
  # Arrange
  cowplot::plot_grid(ct, p, rel_heights = c(0.2, 1), ncol = 1, nrow = 2, align = "v")
}
