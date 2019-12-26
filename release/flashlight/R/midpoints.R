#' Midpoints
#'
#' Takes a vector of breaks and calculates midpoints of subsequent unique breaks.
#'
#' @param breaks Numeric vector of cut points or a single number specifying the number of intervals desired.
#' @return Vector of the same length as x minus 1 with midpoints of breaks.
#' @export
#' @examples
#' midpoints(1:4)
#' midpoints(c(4, 4:1))
midpoints <- function(breaks) {
  stopifnot(is.numeric(breaks))
  breaks <- sort(unique(breaks))
  stopifnot((m <- length(breaks)) >= 2L)
  (breaks[-m] + breaks[-1]) / 2
}



