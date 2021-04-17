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

# Deprecation warning
warning_on_names <- function(to_check, ...) {
  if (any(to_check %in% names(list(...)))) {
    warning("Using 'xyz_name' arguments is deprecated. Use 'options(flashlight.xyz_name = ...)' instead.")
  }
}

# Calculates midpoints of subsequent unique breaks
midpoints <- function(breaks) {
  # to do: deal with missings
  stopifnot(is.numeric(breaks))
  breaks <- sort(unique(breaks))
  stopifnot((m <- length(breaks)) >= 2L)
  (breaks[-m] + breaks[-1]) / 2
}

# Organize binning strategy per variable for 2d partial dependence
fix_strategy <- function(v, n_bins, cut_type, breaks, pd_evaluate_at) {
  stopifnot(
    "Breaks must be NULL or a named list" = is.null(breaks) ||
      (is.list(breaks) && length(breaks) <= 2L && all(names(breaks) %in% v)),
    "pd_evaluate_at must be NULL or a named list" = is.null(pd_evaluate_at) ||
      (is.list(pd_evaluate_at) && length(pd_evaluate_at) <= 2L &&
         all(names(pd_evaluate_at) %in% v)),
    "n_bins should be a numeric vector of length <=2" =
      length(n_bins) %in% 1:2 && is.numeric(n_bins),
    "cut_type should be a character vector of length <=2" =
      length(cut_type) %in% 1:2 && all(cut_type %in% c("equal", "quantile"))
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

