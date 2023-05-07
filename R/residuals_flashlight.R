#' Residuals for flashlight
#'
#' Residuals method for an object of class "flashlight".
#' Pass additional elements to update the flashlight before calculation of residuals.
#'
#' @param object An object of class "flashlight".
#' @param ... Arguments used to update the flashlight before calculating the residuals.
#' @returns A numeric vector with residuals.
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' x <- flashlight(model = fit, data = iris, y = "Sepal.Length", label = "ols")
#' residuals(x)[1:5]
residuals.flashlight <- function(object, ...) {
  object <- flashlight(object, check = FALSE, ...)
  response(object) - stats::predict(object)
}
