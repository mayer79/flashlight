#' Grouped Weighted Means, Quartiles, or Variances
#'
#' Calculates weighted means, quartiles, or variances (and counts) of a
#' variable grouped by optional columns. By default, counts are not weighted,
#' even if there is a weighting variable.
#'
#' @param data A `data.frame`.
#' @param x Variable name in `data` to summarize.
#' @param w Optional name of the column in `data` with case weights.
#' @param by An optional vector of column names in `data` used to group the results.
#' @param stats Statistic to calculate: "mean", "quartiles", or "variance".
#' @param counts Should group counts be added?
#' @param counts_weighted Should counts be weighted by the case weights?
#'   If `TRUE`, the sum of `w` is returned by group.
#' @param counts_name Name of column in the resulting `data.frame` containing the counts.
#' @param value_name Name of the resulting column with mean, median, or variance.
#' @param q1_name Name of the resulting column with first quartile values.
#'   Only relevant if `stats = "quartiles"`.
#' @param q3_name Name of the resulting column with third quartile values.
#'   Only relevant if `stats = "quartiles"`.
#' @param ... Additional arguments passed to corresponding `weighted_*()` functions in
#'   {MetricsWeighted}.
#' @returns A `data.frame` with columns `by`, `x`, and optionally `counts_name`.
#' @export
#' @examples
#' grouped_stats(iris, "Sepal.Width")
#' grouped_stats(iris, "Sepal.Width", stats = "quartiles")
#' grouped_stats(iris, "Sepal.Width", stats = "variance")
#' grouped_stats(iris, "Sepal.Width", w = "Petal.Width", counts_weighted = TRUE)
#' grouped_stats(iris, "Sepal.Width", by = "Species")
grouped_stats <- function(data, x, w = NULL, by = NULL,
                          stats = c("mean", "quartiles", "variance"),
                          counts = TRUE, counts_weighted = FALSE,
                          counts_name = "counts", value_name = x,
                          q1_name = "q1", q3_name = "q3", ...) {
  # Initial checks
  stats <- match.arg(stats)
  if (counts_weighted && is.null(w)) {
    counts_weighted <- FALSE
  }
  stopifnot(
    is.data.frame(data),
    c(x, w, by) %in% colnames(data),
    !anyDuplicated(c(value_name, by, counts_name, q1_name, q3_name)),
    nrow(data) >= 1L
  )

  # Function that does the ungrouped calculation
  core_fun <- function(X) {
    xx <- X[[x]]
    ww <- if (!is.null(w)) X[[w]] # else NULL
    if (stats == "mean") {
      val <- stats::setNames(
        data.frame(MetricsWeighted::weighted_mean(xx, ww, ...)), value_name
      )
    } else if (stats == "quartiles") {
      val <- t(
        MetricsWeighted::weighted_quantile(
          xx, ww, probs = (1:3) / 4, names = FALSE, ...
        )
      )
      val <- stats::setNames(data.frame(val), c(q1_name, value_name, q3_name))
    } else {
      val <- stats::setNames(
        data.frame(MetricsWeighted::weighted_var(xx, ww, ...)), value_name
      )
    }
    if (!counts) {
      return(val)
    }
    cnt <- if (counts_weighted) sum(ww) else length(xx)
    cnt <- stats::setNames(data.frame(cnt), counts_name)
    cbind(cnt, val)
  }

  # Apply core_fun
  Reframe(data, FUN = core_fun, .by = by, as_tib = FALSE)
}
