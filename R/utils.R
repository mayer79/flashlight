# Helper functions

# Add vector of classes upfront existing ones
add_classes <- function(x, classes) {
  class(x) <- union(classes, class(x))
  x
}

# Renames one column of a data.frame by passing character strings for old, new
rename_one <- function(x, old, new) {
  colnames(x)[colnames(x) == old] <- new
  x
}

# Organize binning strategy per variable for 2d partial dependence
fix_strategy <- function(v, n_bins, cut_type, breaks, pd_evaluate_at) {
  stopifnot(
    "breaks must be NULL or a named list" =
      is.null(breaks) || is.list(breaks),
    "pd_evaluate_at must be NULL or a named list" =
      is.null(pd_evaluate_at) || is.list(pd_evaluate_at),
    "n_bins should be a numeric vector of length <=2" =
      length(n_bins) <= 2L && is.numeric(n_bins),
    "cut_type should be a character vector of length <=2" =
      length(cut_type) <= 2L && all(cut_type %in% c("equal", "quantile"))
  )
  strategy <- list()
  for (i in 1:2) {
    vv <- v[i]
    strategy[[vv]] <- list(
      breaks = if (vv %in% names(breaks)) breaks[[vv]],
      pd_evaluate_at = if (vv %in% names(pd_evaluate_at)) pd_evaluate_at[[vv]],
      n_bins = n_bins[min(i, length(n_bins))],
      cut_type = cut_type[min(i, length(cut_type))]
    )
  }
  return(strategy)
}

# Applies df-valued FUN to X grouped by BY
Reframe <- function(X, FUN, .by = NULL, as_tib = TRUE) {
  if (is.null(.by)) {
    out <- FUN(X)
  } else {
    X_grouped <- dplyr::group_by(X, dplyr::across(tidyselect::all_of(.by)))
    out <- dplyr::reframe(X_grouped, FUN(dplyr::pick(dplyr::everything())))
  }
  if (as_tib && !tibble::is_tibble(out)) {
    out <- tibble::as_tibble(out)
  }
  if (!as_tib && tibble::is_tibble(out)) {
    out <- as.data.frame(out)
  }
  out
}

#' all_identical
#'
#' Checks if an aspect is identical for all elements in a nested list.
#' The aspect is specified by `fun`, e.g., `[[`, followed by the element
#' name to compare.
#'
#' @noRd
#' @param x A nested list of objects.
#' @param fun Function used to extract information of each element of `x`.
#' @param ... Further arguments passed to `fun()`.
#' @returns A logical vector of length one.
#' @examples
#' x <- list(a = 1, b = 2)
#' y <- list(a = 1, b = 3)
#' all_identical(list(x, y), `[[`, "a")
#' all_identical(list(x, y), `[[`, "b")
all_identical <- function(x, fun, ...) {
  if ((m <- length(x)) <= 1L) {
    return(TRUE)
  }
  subs <- lapply(x, fun, ...)
  all(vapply(subs[2:m], FUN = identical, FUN.VALUE = TRUE, subs[[1L]]))
}
