#' zap_small
#'
#' Internal function used to clip small values.
#'
#' @param x A vector.
#' @param eps Values closer to 0 than this value are being replaced by \code{val}.
#' @param val Value to replace bad values.
#' @return A vector of the same length as \code{x}.
# Function to safely replace bad values by 0
zap_small <- function(x, eps = 1e-12, val = 0) {
  .bad <- abs(x) < eps | !is.finite(x)
  if (any(.bad)) {
    x[.bad] <- val
  }
  x
}
