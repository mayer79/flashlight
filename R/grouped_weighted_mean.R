#' Fast Grouped Weighted Mean
#'
#' Fast version of grouped_stats(..., counts = FALSE). Works if there is at most one "by" variable.
#'
#' @importFrom stats weighted.mean
#' @param data A \code{data.frame}.
#' @param x Variable name in \code{data} to summarize.
#' @param w Optional name of the column in \code{data} with case weights.
#' @param by An optional vector of column names in \code{data} used to group the results.
#' @param na.rm Should missing values in \code{x} be removed?
#' @param value_name Name of the resulting column with means.
#' @export
#' @examples
#' n <- 100
#' data <- data.frame(x = rnorm(n), w = runif(n), group = factor(sample(1:3, n, TRUE)))
#' grouped_weighted_mean(data, x = "x", w = "w", by = "group")
grouped_weighted_mean <- function(data, x, w = NULL, by = NULL, na.rm = TRUE, value_name = x) {
  if (is.null(by)) {
    if (is.null(w)) {
      out <- mean(data[[x]], na.rm = na.rm)
    } else {
      out <- weighted.mean(data[[x]], w = data[[w]], na.rm = TRUE)
    }
    return(setNames(as.data.frame(out), value_name))
  }
  if (length(by) > 1L) {
    stop("At most one 'by' variable is supported.")
  }
  xx <- data[[x]]
  gg <- data[[by]]
  if (fac <- is.factor(gg)) {
    lev <- levels(gg)
  }
  ww <- if (is.null(w)) rep(1, length(xx)) else data[[w]]
  if (na.rm && !all(ok <- !is.na(xx))) {
    ww <- ww[ok]
    xx <- xx[ok]
    gg <- gg[ok]
  }
  if (!all(pos <- ww > 0)) {
    ww <- ww[pos]
    xx <- xx[pos]
    gg <- gg[pos]
  }
  out <- rowsum(xx * ww, gg) / rowsum(ww, gg)
  rn <- if (fac) factor(rownames(out), levels = lev) else rownames(out)
  setNames(data.frame(rn, out, row.names = NULL), c(by, value_name))
}
