#' Discretizes a Vector
#'
#' This function takes a vector \code{x} and returns a \code{data.frame} with two columns. In case \code{x} is not numeric, both columns are identical to the input \code{x}. The same holds if \code{x} is numeric, no \code{breaks} are provided and the number of disjoint values is not larger than \code{n_bins}. Otherwise, \code{x} is discretized by \code{breaks} or by \code{n_bins} quantile cuts. In these cases, the first column of the resulting \code{data.frame} contains the break average (high+low)/2 and the second column the level names of the form "(low, high)". The construction of the level names can be controlled by passing ... arguments to \code{formatC}.
#'
#' @importFrom stats ave quantile setNames
#' @param x A vector.
#' @param breaks An optional vector of breaks. Only relevant for numeric \code{x}.
#' @param n_bins If \code{x} is numeric and no breaks are provided, this is the maximum number of bins allowed or to be created.
#' @param return_distinct If TRUE, the resulting \code{data.frame} is deduplicated.
#' @param x_name Column name with the values of \code{x} in the output.
#' @param level_name Column name with the bin labels of \code{x} in the output.
#' @param ... Further arguments passed to \code{cut3}.
#' @return A \code{data.frame} with colums \code{x_name} and \code{level_name} and an attribute "breaks" with cut breaks or NULL if none were used.
#' @export
#' @examples
#' out <- auto_cut(1:10, n_bins = 3)
#' auto_cut(LETTERS[1:4], n_bins = 2)
#' auto_cut(990:1100, n_bins = 3, big.mark = "'", format = "fg")
#' auto_cut(990:1100, n_bins = 3, big.mark = "'", format = "fg", return_distinct = TRUE)
#' auto_cut(c(0.0001, 0.0002, 0.0003, 0.005), n_bins = 3, format = "fg")
auto_cut <- function(x, breaks = NULL, n_bins = 27, return_distinct = FALSE,
                     x_name = "value", level_name = "level", ...) {
  un <- unique(x)
  if (!is.numeric(x) || (is.null(breaks) && length(un) <= n_bins)) {
    if (return_distinct) {
      out <- data.frame(un, un)
    } else {
      out <- data.frame(x, x)
    }
  } else {
    if (is.null(breaks)) {
      breaks <- quantile(x, probs = seq(0, 1, length.out = n_bins + 1),
                         na.rm = TRUE, names = FALSE)
    }
    breaks <- sort(unique(breaks))
    cuts <- cut3(x, breaks = breaks, include.lowest = TRUE, ...)
    int_cuts <- as.integer(cuts)
    cut_means <- (breaks[int_cuts + 1L] + breaks[int_cuts]) / 2
    out <- data.frame(cut_means, cuts)
    if (return_distinct) {
      out <- out[!duplicated(out[[2]]), , drop = FALSE]
    }
  }
  attr(out, "breaks") <- breaks
  setNames(out, c(x_name, level_name))
}

