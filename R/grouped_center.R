#' Grouped, weighted mean centering
#'
#' Centers a numeric variable within optional groups and optional weights.
#' The order of values is unchanged.
#'
#' @param data A `data.frame`.
#' @param x Variable name in `data` to center.
#' @param w Optional name of the column in `data` with case weights.
#' @param by An optional vector of column names in `data` used to group the results.
#' @param ... Additional arguments passed to mean calculation (e.g. `na.rm = TRUE`).
#' @returns A numeric vector with centered values in column `x`.
#' @export
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
