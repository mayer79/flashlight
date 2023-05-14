# .onLoad <- function(libname, pkgname) {
#   op <- options()
#   op.flashlight <- list(
#     flashlight.after_name = "after",
#   )
#   toset <- !(names(op.flashlight) %in% names(op))
#   if (any(toset)) {
#     options(op.flashlight[toset])
#   }
#   invisible()
# }

utils::globalVariables(
  c(
    "after_",
    "before_",
    "cal_xx",
    "counts_",
    "description_",
    "error_",
    "fill_",
    "high_",
    "id_",
    "lab_",
    "label_",
    "low_",
    "metric_",
    "shift_xx",
    "step_",
    "temp_",
    "tree_",
    "type_",
    "value_",
    "value_i_",
    "value_j_",
    "value_2_",
    "variable_",
    "ymax_",
    "ymin_",
    "x_")
  )
