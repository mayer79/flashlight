#' Discretizes a Vector
#'
#' This function takes a vector `x` and returns a list with information on
#' disretized version of `x`. The construction of level names can be controlled
#' by passing `...` arguments to [formatC()].
#'
#' @noRd
#' @param x A vector.
#' @param breaks An optional vector of breaks. Only relevant for numeric `x`.
#' @param n_bins If `x` is numeric and no breaks are provided,
#'   this is the maximum number of bins allowed or to be created (approximately).
#' @param cut_type For the default type "equal", bins of equal width are created
#'   by [pretty()]. Choose "quantile" to create quantile bins.
#' @param x_name Column name with the values of `x` in the output.
#' @param level_name Column name with the bin labels of `x` in the output.
#' @param ... Further arguments passed to [cut3()].
#' @returns
#'   A list with the following elements:
#'   - `data`: A `data.frame` with colums `x_name` and
#'     `level_name` each with the same length as `x`.
#'     The column `x_name` has values in output `bin_means`
#'     while the column `level_name` has values in `bin_labels`.
#'   - `breaks`: A vector of increasing and unique breaks used to cut a
#'     numeric `x` with too many distinct levels. `NULL` otherwise.
#'   - `bin_means`: The midpoints of subsequent breaks, or if there are no
#'     `breaks` in the output, factor levels or distinct values of `x`.
#'   - `bin_labels`: Break labels of the form "(low, high]" if there are `breaks`
#'     in the output, otherwise the same as `bin_means`. Same order as `bin_means`.
#' @examples
#' auto_cut(1:10, n_bins = 3)
#' auto_cut(c(NA, 1:10), n_bins = 3)
#' auto_cut(1:10, breaks = 3:4, n_bins = 3)
#' auto_cut(1:10, n_bins = 3, cut_type = "quantile")
#' auto_cut(LETTERS[4:1], n_bins = 2)
#' auto_cut(factor(LETTERS[1:4], LETTERS[4:1]), n_bins = 2)
#' auto_cut(990:1100, n_bins = 3, big.mark = "'", format = "fg")
#' auto_cut(c(0.0001, 0.0002, 0.0003, 0.005), n_bins = 3, format = "fg")
auto_cut <- function(x, breaks = NULL, n_bins = 27L,
                     cut_type = c("equal", "quantile"),
                     x_name = "value", level_name = "level", ...) {
  cut_type <- match.arg(cut_type)
  bin_means <- if (is.factor(x)) levels(x) else sort(unique(x))
  if (!is.numeric(x) || (is.null(breaks) && length(bin_means) <= n_bins)) {
    if (!is.numeric(x)) {
      breaks <- NULL  # ignored for non-numeric
    }
    data <- data.frame(x, x)
    if (anyNA(x)) {
      bin_means <- c(bin_means, NA)
    }
    if (is.factor(x)) {
      bin_means <- factor(bin_means, bin_means)
    }
    bin_labels <- bin_means
  } else {
    if (is.null(breaks)) {
      if (cut_type == "equal") {
        breaks <- pretty(x, n = n_bins)
      } else {
        breaks <- stats::quantile(
          x,
          probs = seq(0, 1, length.out = n_bins + 1L),
          na.rm = TRUE,
          names = FALSE,
          type = 1
        )
      }
    }
    breaks <- sort(unique(breaks))
    bin_means <- midpoints(breaks)
    cuts <- cut3(x, breaks = breaks, include.lowest = TRUE, ...)
    bin_labels <- levels(cuts)
    if (anyNA(cuts)) {
      bin_labels <- c(bin_labels, NA)
      bin_means <- c(bin_means, NA)
    }
    bin_labels <- factor(bin_labels, levels(cuts))
    stopifnot(length(bin_labels) == length(bin_means))
    int_cuts <- as.integer(cuts)
    data <- data.frame((breaks[int_cuts] + breaks[int_cuts + 1L]) / 2, cuts)
  }
  list(
    data = stats::setNames(data, c(x_name, level_name)),
    breaks = breaks,
    bin_means = bin_means,
    bin_labels = bin_labels
  )
}

#' Modified cut
#'
#' Slightly modified version of [cut.default()]. Both modifications refer
#' to the construction of break labels. Firstly, `...` arguments are passed to
#' [formatC()] in formatting the numbers in the labels.
#' Secondly, a separator between the two numbers can be specified with default ", ".
#'
#' @noRd
#' @param x Numeric vector.
#' @param breaks Numeric vector of cut points or a single number
#'   specifying the number of intervals desired.
#' @param labels Labels for the levels of the final categories.
#' @param include.lowest Flag if minimum value should be added to intervals
#'   of type "(,]" (or maximum for "[,)").
#' @param right Flag if intervals should be closed to the right or left.
#' @param dig.lab Number of significant digits passed to [formatC()].
#' @param ordered_result Flag if resulting output vector should be ordered.
#' @param sep Separater between from-to labels.
#' @param ... Arguments passed to [formatC()].
#' @returns Vector of the same length as x.
#' @examples
#' x <- 998:1001
#' cut3(x, breaks = 2)
#' cut3(x, breaks = 2, big.mark = "'", sep = ":")
cut3 <- function(x, breaks, labels = NULL, include.lowest = FALSE, right = TRUE,
                 dig.lab = 3L, ordered_result = FALSE, sep = ", ", ...) {
  # Modified version of base::cut.default()
  if (!is.numeric(x))
    stop("'x' must be numeric")
  if (length(breaks) == 1L) {
    if (is.na(breaks) || breaks < 2L)
      stop("invalid number of intervals")
    nb <- as.integer(breaks + 1)
    dx <- diff(rx <- range(x, na.rm = TRUE))
    if (dx == 0) {
      dx <- if (rx[1L] != 0)
        abs(rx[1L])
      else 1
      breaks <- seq.int(rx[1L] - dx / 1000, rx[2L] + dx / 1000, length.out = nb)
    }
    else {
      breaks <- seq.int(rx[1L], rx[2L], length.out = nb)
      breaks[c(1L, nb)] <- c(rx[1L] - dx / 1000, rx[2L] + dx / 1000)
    }
  }
  else nb <- length(breaks <- sort.int(as.double(breaks)))
  if (anyDuplicated(breaks))
    stop("'breaks' are not unique")
  codes.only <- FALSE
  if (is.null(labels)) {
    for (dig in dig.lab:max(12L, dig.lab)) {
      ch.br <- formatC(0L + breaks, digits = dig, width = 1L, ...)
      if (ok <- all(ch.br[-1L] != ch.br[-nb]))
        break
    }
    labels <- if (ok)
      paste0(
        if (right) "(" else "[", ch.br[-nb], sep, ch.br[-1L], if (right) "]" else ")"
      )
    else paste0("Range_", seq_len(nb - 1L))
    if (ok && include.lowest) {
      if (right)
        substr(labels[1L], 1L, 1L) <- "["
      else substring(labels[nb - 1L], nchar(labels[nb - 1L], "c")) <- "]"
    }
  }
  else if (is.logical(labels) && !labels)
    codes.only <- TRUE
  else if (length(labels) != nb - 1L)
    stop("lengths of 'breaks' and 'labels' differ")
  code <- .bincode(x, breaks, right, include.lowest)
  if (codes.only)
    code
  else factor(code, seq_along(labels), labels, ordered = ordered_result)
}

#' Common Breaks for multiflashlight
#'
#' Internal function used to find common breaks from different flashlights.
#'
#' @noRd
#' @param x An object of class "multiflashlight".
#' @param v The variable to be profiled.
#' @param data A `data.frame`.
#' @param n_bins Maxmium number of unique values to evaluate for numeric `v`.
#' @param cut_type Cut type
#' @returns A vector of breaks
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

# Calculates midpoints of subsequent unique breaks
midpoints <- function(breaks) {
  # to do: deal with missings
  stopifnot(is.numeric(breaks))
  breaks <- sort(unique(breaks))
  stopifnot((m <- length(breaks)) >= 2L)
  (breaks[-m] + breaks[-1L]) / 2
}
