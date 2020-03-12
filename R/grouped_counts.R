#' Grouped count
#'
#' Calculates weighted counts grouped by optional columns.
#'
#' @importFrom dplyr group_by_at do ungroup
#' @importFrom stats setNames
#' @importFrom rlang .data
#' @param data A \code{data.frame}.
#' @param by An optional vector of column names in \code{data} used to group the results.
#' @param w Optional name of the column in \code{data} with case weights.
#' @param value_name Name of the resulting column with counts.
#' @param ... Arguments passed to \code{sum} (only if weights are provided).
#' @return A \code{data.frame} with columns \code{by} and \code{value_name}.
#' @export
#' @examples
#' grouped_counts(iris)
#' grouped_counts(iris, by = "Species")
#' grouped_counts(iris, w = "Petal.Length")
#' grouped_counts(iris, by = "Species", w = "Petal.Length")
grouped_counts <- function(data, by = NULL, w = NULL, value_name = "n", ...) {
  # Initial checks
  stopifnot(c(w, by) %in% colnames(data),
            !anyDuplicated(c(value_name, by)))

  # Function that does the ungrouped calculation
  core_fun <- function(X) {
    val <- if (!is.null(w)) sum(X[[w]], ...) else nrow(X)
    setNames(data.frame(val), value_name)
  }

  # Apply core_fun
  if (!length(by)) {
    return(core_fun(data))
  }
  ungroup(do(group_by_at(data, by), core_fun(.data)))
}
