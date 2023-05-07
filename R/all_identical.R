#' all_identical
#'
#' Checks if an aspect is identical for all elements in a nested list.
#' The aspect is specified by `fun`, e.g., `[[`, followed by the element
#' name to compare.
#'
#' @param x A nested list of objects.
#' @param fun Function used to extract information of each element of `x`.
#' @param ... Further arguments passed to `fun()`.
#' @returns A logical vector of length one.
#' @export
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
