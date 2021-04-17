#' Check functions for flashlight Classes
#'
#' Checks if an object inherits specific class relevant for the flashlight package.
#'
#' @param x Any object.
#' @return A logical vector of length one.
#' @export
#' @examples
#' a <- flashlight(label = "a")
#' is.flashlight(a)
#' is.flashlight("a")
is.flashlight <- function(x) {
  inherits(x, "flashlight")
}

#' @describeIn is.flashlight Check for multiflashlight object.
#' @export
is.multiflashlight <- function(x) {
  inherits(x, "multiflashlight")
}

#' @describeIn is.flashlight Check for light object.
#' @export
is.light <- function(x) {
  inherits(x, "light")
}

#' @describeIn is.flashlight Check for light_performance object.
#' @export
is.light_performance <- function(x) {
  inherits(x, "light_performance")
}

#' @describeIn is.flashlight Check for light_performance_multi object.
#' @export
is.light_performance_multi <- function(x) {
  inherits(x, "light_performance_multi")
}

#' @describeIn is.flashlight Check for light_importance object.
#' @export
is.light_importance <- function(x) {
  inherits(x, "light_importance")
}

#' @describeIn is.flashlight Check for light_importance_multi object.
#' @export
is.light_importance_multi <- function(x) {
  inherits(x, "light_importance_multi")
}

#' @describeIn is.flashlight Check for light_breakdown object.
#' @export
is.light_breakdown <- function(x) {
  inherits(x, "light_breakdown")
}

#' @describeIn is.flashlight Check for light_breakdown_multi object.
#' @export
is.light_breakdown_multi <- function(x) {
  inherits(x, "light_breakdown_multi")
}

#' @describeIn is.flashlight Check for light_ice object.
#' @export
is.light_ice <- function(x) {
  inherits(x, "light_ice")
}

#' @describeIn is.flashlight Check for light_ice_multi object.
#' @export
is.light_ice_multi <- function(x) {
  inherits(x, "light_ice_multi")
}

#' @describeIn is.flashlight Check for light_profile object.
#' @export
is.light_profile <- function(x) {
  inherits(x, "light_profile")
}

#' @describeIn is.flashlight Check for light_profile_multi object.
#' @export
is.light_profile_multi <- function(x) {
  inherits(x, "light_profile_multi")
}

#' @describeIn is.flashlight Check for light_profile2d object.
#' @export
is.light_profile2d <- function(x) {
  inherits(x, "light_profile2d")
}

#' @describeIn is.flashlight Check for light_profile2d_multi object.
#' @export
is.light_profile2d_multi <- function(x) {
  inherits(x, "light_profile2d_multi")
}

#' @describeIn is.flashlight Check for light_effects object.
#' @export
is.light_effects <- function(x) {
  inherits(x, "light_effects")
}

#' @describeIn is.flashlight Check for light_effects_multi object.
#' @export
is.light_effects_multi <- function(x) {
  inherits(x, "light_effects_multi")
}

#' @describeIn is.flashlight Check for shap object.
#' @export
is.shap <- function(x) {
  inherits(x, "shap")
}

#' @describeIn is.flashlight Check for light_scatter object.
#' @export
is.light_scatter <- function(x) {
  inherits(x, "light_scatter")
}

#' @describeIn is.flashlight Check for light_scatter_multi object.
#' @export
is.light_scatter_multi <- function(x) {
  inherits(x, "light_scatter_multi")
}

#' @describeIn is.flashlight Check for light_global_surrogate object.
#' @export
is.light_global_surrogate <- function(x) {
  inherits(x, "light_global_surrogate")
}

#' @describeIn is.flashlight Check for light_global_surrogate_multi object.
#' @export
is.light_global_surrogate_multi <- function(x) {
  inherits(x, "light_global_surrogate_multi")
}
