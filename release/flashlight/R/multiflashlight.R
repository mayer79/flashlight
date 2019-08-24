#' Create or Update a multiflashlight
#'
#' Combines a list of flashlights to an object of class \code{multiflashlight} and/or updates a multiflashlight.
#'
#' @param x An object of class \code{multiflashlight}, \code{flashlight} or a list of flashlights.
#' @param ... Optional arguments in the flashlights to update, see examples.
#' @return An object of class \code{multiflashlight}. This is a named list of flashlight objects.
#' @export
#' @examples
#' fit_lm <- lm(Sepal.Length ~ ., data = iris)
#' fit_glm <- glm(Sepal.Length ~ ., family = Gamma(link = log), data = iris)
#' mod_lm <- flashlight(model = fit_lm, label = "lm")
#' mod_glm <- flashlight(model = fit_glm, label = "glm")
#' (mods <- multiflashlight(list(mod_lm, mod_glm)))
#'
#' mods <- multiflashlight(list(mod_lm, mod_glm),
#'   data = iris, by = "Species", y = "Sepal.Length")
#' mod_lm <- mods$lm
#' mod_lm
#' @seealso \code{\link{flashlight}}.
multiflashlight <- function(x, ...) {
  UseMethod("multiflashlight")
}

#' @describeIn multiflashlight Used to create a flashlight object. No \code{x} has to be passed in this case.
#' @export
multiflashlight.default <- function(x, ...) {
  stop("No default method available yet.")
}

#' @describeIn multiflashlight Updates an existing flashlight object and turns into a multiflashlight.
#' @export
multiflashlight.flashlight <- function(x, ...) {
  multiflashlight(list(x), ...)
}

#' @describeIn multiflashlight Creates (and updates) a multiflashlight from a list of flashlights.
#' @export
multiflashlight.list <- function(x, ...) {
  stopifnot(vapply(x, is.flashlight, FUN.VALUE = TRUE))
  out <- lapply(x, flashlight, ...)
  names(out) <- lapply(x, `[[`, "label")
  class(out) <- c("multiflashlight", "list")
  light_check(out)
}

#' @describeIn multiflashlight Updates an object of class \code{multiflashlight}.
#' @export
multiflashlight.multiflashlight <- function(x, ...) {
  multiflashlight(lapply(x, flashlight, ...))
}

