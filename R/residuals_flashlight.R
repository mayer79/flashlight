#' Residuals for flashlight
#'
#' Residuals method for an object of class \code{flashlight}. Pass additional elements to update the flashlight before calculation of residuals.
#'
#' @importFrom stats residuals
#' @method residuals flashlight
#' @param object An object of class \code{flashlight}.
#' @param ... Arguments used to update the flashlight before calculating the residuals.
#' @return A numeric vector with residuals.
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' (fl <- flashlight(model = fit, data = iris, y = "Sepal.Length", label = "ols"))
#' residuals(fl)[1:5]
#' residuals(fl, data = iris[1:5, ])
#' residuals(fl, data = iris[1:5, ], linkinv = exp)
#' resid(fl)[1:5]
residuals.flashlight <- function(object, ...) {
  object <- flashlight(object, check = FALSE, ...)
  response(object) - predict(object)
}
