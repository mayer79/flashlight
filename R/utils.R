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
  (breaks[-m] + breaks[-1L]) / 2
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

# Test for non-unique column names (passed as variables and/or options)
check_unique <- function(var_names = NULL, opt_names = NULL, temp_names = NULL) {
  if (!anyDuplicated(c(var_names, opt_names, temp_names))) {
    return(TRUE)
  }

  # Consistency of variable names
  if (anyDuplicated(z <- var_names)) {
    what <- z[duplicated(z)][1L]
    stop("The variable '", what, "' seems to be used multiple times. Please fix.")
  }

  # Consistency of options names
  if (anyDuplicated(z <- opt_names)) {
    what <- z[duplicated(z)][1L]
    stop("flashlight uses 'options(flashlight.*)' to set column names ",
         "in 'data' objects returned by light_*() functions. ",
         "The name '", what, "' seems to be used in multiple options. ",
         "Please change these.")
  }

  # temp names can't overlap

  # Consistency of variable and option names
  if (anyDuplicated(z <- c(var_names, opt_names))) {
    what <- z[duplicated(z)][1L]
    stop("Variable name '", what, "' coincides with a column name specified ",
         "in options(flashlight.*). ",
         "To solve the problem, please change the value of that option.")
  }

  # Otherwise, there must be a problem with a temp name
  z <- c(var_names, opt_names, temp_names)
  what <- z[duplicated(z)][1L]
  stop("Variable name '", what, "' is internally used and cannot be used ",
       "as flashlight option or as variable name. Please avoid this name.")
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
