#' Common Breaks for multiflashlight
#'
#' Internal function used to find common breaks from different flashlights.
#'
#' @param x An object of class \code{multiflashlight}.
#' @param v The variable to be profiled.
#' @param data A \code{data.frame}.
#' @param n_bins Maxmium number of unique values to evaluate for numeric \code{v}.
#' @param cut_type Cut type
#' @return A vector of breaks
common_breaks <- function(x, v, data = NULL, n_bins, cut_type) {
  if (is.null(data)) {
    # Stack v from all data in flashlights
    stopifnot(
      all(vapply(x, function(z) nrow(z$data) >= 1L, FUN.VALUE = TRUE)),
      all(vapply(x, function(z) v %in% colnames(z$data), FUN.VALUE = TRUE))
    )
    v_vec <- unlist(lapply(x, function(z) z$data[[v]]), use.names = FALSE)
  } else {
    stopifnot(nrow(data) >= 1L, v %in% colnames(data))
    v_vec <- data[[v]]
  }
  auto_cut(v_vec, n_bins = n_bins, cut_type = cut_type)$breaks
}

