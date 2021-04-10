.onLoad <- function(libname, pkgname) {
  op <- options()
  op.flashlight <- list(
    flashlight.after_name = "after",
    flashlight.before_name = "before",
    flashlight.description_name = "description",
    flashlight.error_name = "error",
    flashlight.label_name = "label",
    flashlight.metric_name = "metric",
    flashlight.step_name = "step",
    flashlight.tree_name = "tree",
    flashlight.value_name = "value",
    flashlight.variable_name = "variable"
  )
  toset <- !(names(op.flashlight) %in% names(op))
  if (any(toset)) {
    options(op.flashlight[toset])
  }
  invisible()
}


