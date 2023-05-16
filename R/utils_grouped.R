#' Fast Grouped Weighted Mean
#'
#' Fast version of `grouped_stats(..., counts = FALSE)`.
#' Works if there is at most one "by" variable.
#'
#' @noRd
#' @param data A `data.frame`.
#' @param x Variable name in `data` to summarize.
#' @param w Optional name of the column in `data` with case weights.
#' @param by One column name in `data` used to group the results.
#' @param na.rm Should missing values in `x` be removed?
#' @param value_name Name of the resulting column with means.
#' @returns A `data.frame` with grouped weighted means.
#' @examples
#' n <- 100
#' data <- data.frame(
#'   x = rnorm(n),
#'   w = runif(n),
#'   group = factor(sample(1:3, n, TRUE))
#' )
#' grouped_weighted_mean(data, x = "x", w = "w", by = "group")
grouped_weighted_mean <- function(data, x, w = NULL, by = NULL,
                                  na.rm = TRUE, value_name = x) {
  if (is.null(by)) {
    if (is.null(w)) {
      out <- mean(data[[x]], na.rm = na.rm)
    } else {
      out <- stats::weighted.mean(data[[x]], w = data[[w]], na.rm = na.rm)
    }
    return(stats::setNames(as.data.frame(out), value_name))
  }
  if (length(by) > 1L) {
    stop("At most one 'by' variable is supported.")
  }
  xx <- data[[x]]
  gg <- data[[by]]
  if (!is.numeric(gg) && !is.character(gg) && !is.factor(gg)) {
    stop("'by' should be character, numeric or factor.")
  }
  if (fac <- is.factor(gg)) {
    lev <- levels(gg)
  }
  num <- is.numeric(gg)

  ww <- if (is.null(w)) rep(1, length(xx)) else data[[w]]
  if (na.rm && any(na <- is.na(xx))) {
    ww <- ww[!na]
    xx <- xx[!na]
    gg <- gg[!na]
  }
  if (!all(pos <- ww > 0)) {
    ww <- ww[pos]
    xx <- xx[pos]
    gg <- gg[pos]
  }
  out <- rowsum(xx * ww, gg) / rowsum(ww * 1, gg)
  rn <- rownames(out)
  if (num || fac) {
    rn <- if (fac) factor(rn, levels = lev) else if (num) as.numeric(rn)
  }
  stats::setNames(data.frame(rn, out, row.names = NULL), c(by, value_name))
}

# library(collapse)
#
# gwmean <- function(data, x, w = NULL, by = NULL, na.rm = TRUE, value_name = x, ...) {
#   out <- fmean(
#     data[[x]],
#     g = if (!is.null(by)) data[[by]],
#     w = if (!is.null(w)) data[[w]],
#     na.rm = na.rm,
#     ...
#   )
#   out
# }

#' Grouped Weighted Means, Quartiles, or Variances
#'
#' Calculates weighted means, quartiles, or variances (and counts) of a
#' variable grouped by optional columns. By default, counts are not weighted,
#' even if there is a weighting variable.
#'
#' @noRd
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
#' @examples
#' grouped_stats(iris, "Sepal.Width")
#' grouped_stats(iris, "Sepal.Width", stats = "quartiles")
#' grouped_stats(iris, "Sepal.Width", stats = "variance")
#' grouped_stats(iris, "Sepal.Width", w = "Petal.Width", counts_weighted = TRUE)
#' grouped_stats(iris, "Sepal.Width", by = "Species")
grouped_stats <- function(data, x, w = NULL, by = NULL,
                          stats = c("mean", "quartiles", "variance"),
                          counts = TRUE, counts_weighted = FALSE,
                          counts_name = "counts_", value_name = x,
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

#' Grouped count
#'
#' Calculates weighted counts grouped by optional columns.
#'
#' @noRd
#' @param data A `data.frame`.
#' @param by An optional vector of column names in `data` used to group the results.
#' @param w Optional name of the column in `data` with case weights.
#' @param value_name Name of the resulting column with counts.
#' @param ... Arguments passed to [sum()] (only if weights are provided).
#' @returns A `data.frame` with columns `by` and `value_name`.
#' @examples
#' grouped_counts(iris)
#' grouped_counts(iris, by = "Species")
#' grouped_counts(iris, w = "Petal.Length")
#' grouped_counts(iris, by = "Species", w = "Petal.Length")
grouped_counts <- function(data, by = NULL, w = NULL, value_name = "n", ...) {
  # Initial checks
  stopifnot(
    is.data.frame(data),
    c(w, by) %in% colnames(data),
    !anyDuplicated(c(value_name, by))
  )

  # Function that does the ungrouped calculation
  core_fun <- function(X) {
    val <- if (!is.null(w)) sum(X[[w]], ...) else nrow(X)
    stats::setNames(data.frame(val), value_name)
  }

  # Apply core_fun
  Reframe(data, FUN = core_fun, .by = by, as_tib = FALSE)
}

#' Grouped, weighted mean centering
#'
#' Centers a numeric variable within optional groups and optional weights.
#' The order of values is unchanged.
#'
#' @noRd
#' @param data A `data.frame`.
#' @param x Variable name in `data` to center.
#' @param w Optional name of the column in `data` with case weights.
#' @param by An optional vector of column names in `data` used to group the results.
#' @param ... Additional arguments passed to mean calculation (e.g. `na.rm = TRUE`).
#' @returns A numeric vector with centered values in column `x`.
#' @examples
#' ir <- data.frame(iris, w = 1)
#' mean(grouped_center(ir, "Sepal.Width"))
#' rowsum(grouped_center(ir, "Sepal.Width", by = "Species"), ir$Species)
#' mean(grouped_center(ir, "Sepal.Width", w = "w"))
#' rowsum(grouped_center(ir, "Sepal.Width", by = "Species", w = "w"), ir$Species)
grouped_center <- function(data, x, w = NULL, by = NULL, ...) {
  stopifnot(
    is.data.frame(data),
    !anyDuplicated(c(x, w, by)),
    c(x, w, by) %in% colnames(data)
  )
  if (is.null(by) && is.null(w)) {
    return(data[[x]] - mean(data[[x]], ...))
  }
  if (is.null(by)) {
    return(data[[x]] - MetricsWeighted::weighted_mean(data[[x]], w = data[[w]], ...))
  }
  if (is.null(w)) {
    return(stats::ave(data[[x]], by = data[, by], FUN = function(z) z - mean(z, ...)))
  }
  stopifnot(!"global_mean__" %in% colnames(data))
  M <- grouped_stats(
    data,
    x = x,
    w = w,
    by = by,
    counts = FALSE,
    value_name = "global_mean__",
    ...
  )

  combined <- dplyr::left_join(data, M, by = by)
  combined[[x]] - combined[["global_mean__"]]
}
