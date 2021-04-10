# Helper functions

# Add vector of classes upfront existing ones
add_classes <- function(x, classes) {
  class(x) <- union(classes, class(x))
  x
}

# Deprecation warning
warning_on_names <- function(...) {
  if (!all(vapply(list(...), is.null, FUN.VALUE = logical(1)))) {
    warning("The use of 'xyz_name' arguments is deprecated.
              Use 'options(flashlight.xyz_name = ...)' instead.")
  }
}
