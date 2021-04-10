.onLoad <- function(libname, pkgname) {
  op <- options()
  op.flashlight <- list(
    flashlight.metric_name = "metric",
    flashlight.value_name = "value",
    flashlight.label_name = "label",
    flashlight.error_name = "error",
    flashlight.variable_name = "variable",
    flashlight.tree_name = "tree"
  )
  toset <- !(names(op.flashlight) %in% names(op))
  if (any(toset)) {
    options(op.flashlight[toset])
  }
  invisible()
}
