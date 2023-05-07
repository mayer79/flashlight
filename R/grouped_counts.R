#' Grouped count
#'
#' Calculates weighted counts grouped by optional columns.
#'
#' @param data A `data.frame`.
#' @param by An optional vector of column names in `data` used to group the results.
#' @param w Optional name of the column in `data` with case weights.
#' @param value_name Name of the resulting column with counts.
#' @param ... Arguments passed to [sum()] (only if weights are provided).
#' @returns A `data.frame` with columns `by` and `value_name`.
#' @export
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
