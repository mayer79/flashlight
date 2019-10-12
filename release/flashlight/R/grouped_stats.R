#' Grouped Weighted Means or Quartiles
#'
#' Calculates weighted means or quartiles (and counts) of a variable grouped by optional columns.
#'
#' @importFrom dplyr group_by_at do ungroup
#' @importFrom stats setNames
#' @importFrom MetricsWeighted weighted_mean weighted_quantile
#' @importFrom rlang .data
#' @param data A \code{data.frame}.
#' @param x Variable name in \code{data} to summarize.
#' @param w Optional name of the column in \code{data} with case weights.
#' @param by An optional vector of column names in \code{data} used to group the results.
#' @param stats Statistic to calculate: "mean" or "quartiles".
#' @param counts Should group counts be added?
#' @param counts_weighted Should counts be weighted by the case weights? If TRUE, the sum of \code{w} is returned by group.
#' @param counts_name Name of column in the resulting \code{data.frame} containing the counts.
#' @param value_name Name of the resulting column with mean or median.
#' @param q1_name Name of the resulting column with first quartile values. Only relevant for \code{stats} "quartiles".
#' @param q3_name Name of the resulting column with third quartile values. Only relevant for \code{stats} "quartiles".
#' @param ... Additional arguments passed to \code{MetricsWeighted::weighted_mean} or \code{weighted_quartiles}.
#' @return A \code{data.frame} with columns \code{by}, \code{x} and optionally \code{counts_name}.
#' @export
#' @examples
#' grouped_stats(iris, "Sepal.Width")
#' grouped_stats(iris, "Sepal.Width", stats = "quartiles")
#' grouped_stats(iris, "Sepal.Width", w = "Petal.Width")
#' grouped_stats(iris, "Sepal.Width", w = "Petal.Width", counts_weighted = TRUE)
#'
#' grouped_stats(iris, "Sepal.Width", by = "Species")
#' grouped_stats(iris, "Sepal.Width", stats = "quartiles", by = "Species")
#' grouped_stats(iris, "Sepal.Width", w = "Petal.Width", by = "Species")
#' grouped_stats(iris, "Sepal.Width", w = "Petal.Width",
#'   counts_weighted = TRUE, by = "Species")
#'
#' grouped_stats(iris, "Sepal.Width", counts = FALSE)
#' grouped_stats(iris, "Sepal.Width", counts_name = "n",
#'   stats = "quartiles", q1_name = "p25", q3_name = "p75")
grouped_stats <- function(data, x, w = NULL, by = NULL, stats = c("mean", "quartiles"),
                          counts = TRUE, counts_weighted = FALSE,
                          counts_name = "counts", value_name = x,
                          q1_name = "q1", q3_name = "q3", ...) {
  # Initial checks
  stats <- match.arg(stats)
  if (counts_weighted && is.null(w)) {
    counts_weighted <- FALSE
  }
  stopifnot(c(x, w, by) %in% colnames(data),
            !anyDuplicated(c(value_name, by, counts_name, q1_name, q3_name)),
            nrow(data) >= 1L)

  # Function that does the ungrouped calculation
  core_fun <- function(X) {
    xx <- X[[x]]
    ww <- if (!is.null(w)) X[[w]] # else NULL
    if (stats == "mean") {
      val <- setNames(data.frame(weighted_mean(xx, ww, ...)), value_name)
    } else {
      val <- t(weighted_quantile(xx, ww, probs = (1:3) / 4, names = FALSE, ...))
      val <- setNames(data.frame(val), c(q1_name, value_name, q3_name))
    }
    if (!counts) {
      return(val)
    }
    cnt <- if (counts_weighted) sum(ww) else length(xx)
    cnt <- setNames(data.frame(cnt), counts_name)
    cbind(cnt, val)
  }

  # Apply core_fun
  if (!length(by)) {
    return(core_fun(data))
  }
  ungroup(do(group_by_at(data, by), core_fun(.data)))
}
