#' Discretizes a Vector
#'
#' This function takes a vector `x` and returns a list with information on
#' disretized version of `x`. The construction of level names can be controlled
#' by passing `...` arguments to [formatC()].
#'
#' @param x A vector.
#' @param breaks An optional vector of breaks. Only relevant for numeric `x`.
#' @param n_bins If `x` is numeric and no breaks are provided,
#'   this is the maximum number of bins allowed or to be created (approximately).
#' @param cut_type For the default type "equal", bins of equal width are created
#'   by [pretty()]. Choose "quantile" to create quantile bins.
#' @param x_name Column name with the values of `x` in the output.
#' @param level_name Column name with the bin labels of `x` in the output.
#' @param ... Further arguments passed to [cut3()].
#' @returns
#'   A list with the following elements:
#'   - `data`: A `data.frame` with colums `x_name` and
#'     `level_name` each with the same length as `x`.
#'     The column `x_name` has values in output `bin_means`
#'     while the column `level_name` has values in `bin_labels`.
#'   - `breaks`: A vector of increasing and unique breaks used to cut a
#'     numeric `x` with too many distinct levels. `NULL` otherwise.
#'   - `bin_means`: The midpoints of subsequent breaks, or if there are no
#'     `breaks` in the output, factor levels or distinct values of `x`.
#'   - `bin_labels`: Break labels of the form "(low, high]" if there are `breaks`
#'     in the output, otherwise the same as `bin_means`. Same order as `bin_means`.
#' @export
#' @examples
#' auto_cut(1:10, n_bins = 3)
#' auto_cut(c(NA, 1:10), n_bins = 3)
#' auto_cut(1:10, breaks = 3:4, n_bins = 3)
#' auto_cut(1:10, n_bins = 3, cut_type = "quantile")
#' auto_cut(LETTERS[4:1], n_bins = 2)
#' auto_cut(factor(LETTERS[1:4], LETTERS[4:1]), n_bins = 2)
#' auto_cut(990:1100, n_bins = 3, big.mark = "'", format = "fg")
#' auto_cut(c(0.0001, 0.0002, 0.0003, 0.005), n_bins = 3, format = "fg")
auto_cut <- function(x, breaks = NULL, n_bins = 27L,
                     cut_type = c("equal", "quantile"),
                     x_name = "value", level_name = "level", ...) {
  cut_type <- match.arg(cut_type)
  bin_means <- if (is.factor(x)) levels(x) else sort(unique(x))
  if (!is.numeric(x) || (is.null(breaks) && length(bin_means) <= n_bins)) {
      if (!is.numeric(x)) {
        breaks <- NULL  # ignored for non-numeric
      }
      data <- data.frame(x, x)
      if (anyNA(x)) {
        bin_means <- c(bin_means, NA)
      }
      if (is.factor(x)) {
        bin_means <- factor(bin_means, bin_means)
      }
      bin_labels <- bin_means
  } else {
    if (is.null(breaks)) {
      if (cut_type == "equal") {
        breaks <- pretty(x, n = n_bins)
      } else {
        breaks <- stats::quantile(
          x,
          probs = seq(0, 1, length.out = n_bins + 1L),
          na.rm = TRUE,
          names = FALSE,
          type = 1
        )
      }
    }
    breaks <- sort(unique(breaks))
    bin_means <- midpoints(breaks)
    cuts <- cut3(x, breaks = breaks, include.lowest = TRUE, ...)
    bin_labels <- levels(cuts)
    if (anyNA(cuts)) {
      bin_labels <- c(bin_labels, NA)
      bin_means <- c(bin_means, NA)
    }
    bin_labels <- factor(bin_labels, levels(cuts))
    stopifnot(length(bin_labels) == length(bin_means))
    int_cuts <- as.integer(cuts)
    data <- data.frame((breaks[int_cuts] + breaks[int_cuts + 1L]) / 2, cuts)
  }
  list(
    data = stats::setNames(data, c(x_name, level_name)),
    breaks = breaks,
    bin_means = bin_means,
    bin_labels = bin_labels
  )
}

