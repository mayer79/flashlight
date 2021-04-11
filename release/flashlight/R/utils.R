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



