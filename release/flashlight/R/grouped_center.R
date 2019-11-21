#' Grouped, weighted mean centering
#'
#' Centers a numeric variable within optional groups and optional weights. The order of values is unchanged.
#'
#' @importFrom dplyr left_join
#' @importFrom MetricsWeighted weighted_mean
#' @importFrom stats ave
#' @param data A \code{data.frame}.
#' @param x Variable name in \code{data} to center.
#' @param w Optional name of the column in \code{data} with case weights.
#' @param by An optional vector of column names in \code{data} used to group the results.
#' @param ... Additional arguments passed to mean calculation (e.g. \code{na.rm = TRUE}).
#' @return A numeric vector with centered values in column \code{x}.
#' @export
#' @examples
#' ir <- data.frame(iris, w = 1)
#' mean(grouped_center(ir, "Sepal.Width"))
#' rowsum(grouped_center(ir, "Sepal.Width", by = "Species"), ir$Species)
#' mean(grouped_center(ir, "Sepal.Width", w = "w"))
#' rowsum(grouped_center(ir, "Sepal.Width", by = "Species", w = "w"), ir$Species)
grouped_center <- function(data, x, w = NULL, by = NULL, ...) {
  stopifnot(!anyDuplicated(c(x, w, by)),
            c(x, w, by) %in% colnames(data))
  if (is.null(by) && is.null(w)) {
    return(data[[x]] - mean(data[[x]], ...))
  }
  if (is.null(by)) {
    return(data[[x]] - weighted_mean(data[[x]], w = data[[w]], ...))
  }
  if (is.null(w)) {
    return(ave(data[[x]], by = data[, by], FUN = function(z) z - mean(z, ...)))
  }
  stopifnot(!"global_mean__" %in% colnames(data))
  M <- grouped_stats(data, x = x, w = w,
                     by = by, counts = FALSE,
                     value_name = "global_mean__", ...)

  combined <- left_join(data, M, by = by)
  combined[[x]] - combined[["global_mean__"]]
}
