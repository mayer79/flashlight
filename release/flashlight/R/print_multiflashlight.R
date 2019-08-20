#' Prints a multiflashlight
#'
#' Print method for an object of class \code{multiflashlight}.
#'
#' @param x An object of class \code{multiflashlight}.
#' @param ... Further arguments passed to \code{print.flashlight}.
#' @return Invisibly, the input is returned.
#' @method print multiflashlight
#' @export
#' @examples
#' fit_lm <- lm(Sepal.Length ~ ., data = iris)
#' fit_glm <- glm(Sepal.Length ~ ., family = Gamma(link = log), data = iris)
#' fl_lm <- flashlight(model = fit_lm, label = "lm")
#' fl_glm <- flashlight(model = fit_glm, label = "glm")
#' multiflashlight(list(fl_lm, fl_glm), data = iris)
#' @seealso \code{\link{multiflashlight}}.
print.multiflashlight <- function(x, ...) {
  lapply(x, print.flashlight, ...)
  invisible(x)
}

