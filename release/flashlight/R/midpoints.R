#' Midpoints
#'
#' Internal function that takes a vector of breaks and calculates midpoints of subsequent unique breaks.
#'
#' @param breaks Numeric vector of cut points or a single number specifying the number of intervals desired.
#' @return Vector of the same length as x minus 1 with midpoints of breaks.
midpoints <- function(breaks) {
  # to do: deal with missings
  stopifnot(is.numeric(breaks))
  breaks <- sort(unique(breaks))
  stopifnot((m <- length(breaks)) >= 2L)
  (breaks[-m] + breaks[-1]) / 2
}



