#' Add Counts to Effects Plot
#'
#' Add counts as labelled bar plot on top of light_effects plot.
#'
#' Experimental. Uses package \code{ggpubr} to rearrange the figure. Thus, the resulting plot cannot be easily modified. Furthermore, adding counts only works if the legend in \code{plot.light_effects} is not placed on the left or right side of the plot. It has to be placed inside or at the bottom.
#'
#' @import ggplot2
#' @importFrom ggpubr ggarrange
#' @importFrom dplyr right_join
#' @param p The result of \code{plot.light_effects}.
#' @param x An object of class \code{light_effects}.
#' @param text_size Size of count labels.
#' @param facet_scales Scales argument passed to \code{facet_wrap}.
#' @param show_labels Should count labels be added as text?
#' @param big.mark Parameter passed to \code{format} the labels. Default is "'".
#' @param scientific Parameter passed to \code{format} the labels. Default is FALSE.
#' @param digits Used to round the labels. Default is 0.
#' @param ... Further arguments passed to \code{geom_bar}.
#' @return An object of class \code{ggplot2}.
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' fl <- flashlight(model = fit, label = "iris", data = iris, y = "Sepal.Length")
#' x <- light_effects(fl, v = "Species")
#' plot_counts(plot(x), x, width = 0.3, alpha = 0.2)
#' @seealso \code{\link{plot.light_effects}}.
#' @export
plot_counts <- function(p, x, text_size = 3, facet_scales = "free_x", show_labels = TRUE,
                        big.mark = "'", scientific = FALSE, digits = 0, ...) {
  # Checks
  stopifnot(is.ggplot(p), is.light_effects(x),
            !("lab_" %in% colnames(x$response)))

  multi <- is.light_effects_multi(x)

  # Deal with zero counts
  key <- c(x$by, x$v, x$label_name)
  x$response <- right_join(x$response, unique(p$data[, key, drop = FALSE]), by = key)
  if (any((bad <- is.na(x$response[[x$counts_name]])))) {
    x$response[[x$counts_name]][bad] <- 0
  }

  # Prepare for plotting
  if (show_labels) {
    x$response[["lab_"]] <- format(round(x$response[[x$counts_name]], digits),
                                   big.mark = big.mark, scientific = scientific)
  }
  ct <- ggplot(x$response, aes_string(x = x$v, y = x$counts_name)) +
    geom_bar(stat = "identity", ...) +
    theme_void() +
    theme(strip.text.x = element_blank(), panel.grid = element_blank())
  if (show_labels) {
    ct <- ct + geom_text(aes_string(y = 0, label = "lab_"),
                         angle = 90, hjust = -0.1, size = text_size)
  }
  if (multi || length(x$by)) {
    ct <- ct + facet_wrap(reformulate(if (multi) x$label_name else x$by[1]),
                          scales = facet_scales, nrow = 1L)
  }
  # Arrange
  ggarrange(ct, p, heights = c(0.2, 1), ncol = 1, nrow = 2, align = "v")
}
