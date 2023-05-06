#' Prints a multiflashlight
#'
#' Print method for an object of class "multiflashlight".
#'
#' @param x An object of class "multiflashlight".
#' @param ... Further arguments passed to [print.flashlight()].
#' @returns Invisibly, the input is returned.
#' @export
#' @examples
#' fit_lm <- lm(Sepal.Length ~ ., data = iris)
#' fit_glm <- glm(Sepal.Length ~ ., family = Gamma(link = log), data = iris)
#' fl_lm <- flashlight(model = fit_lm, label = "lm")
#' fl_glm <- flashlight(model = fit_glm, label = "glm")
#' multiflashlight(list(fl_lm, fl_glm), data = iris)
#' @seealso [multiflashlight()]
print.multiflashlight <- function(x, ...) {
  lapply(x, print.flashlight, ...)
  invisible(x)
}

