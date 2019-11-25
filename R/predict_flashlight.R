#' Predictions for flashlight
#'
#' Predict method for an object of class \code{flashlight}. Pass additional elements to update the flashlight, typically \code{data}.
#'
#' @importFrom stats predict
#' @param object An object of class \code{flashlight}.
#' @param ... Arguments used to update the flashlight.
#' @return A vector with predictions.
#' @export
#' @method predict flashlight
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' (fl <- flashlight(model = fit, data = iris, y = "Sepal.Length", label = "ols"))
#' predict(fl)[1:5]
#' predict(fl, data = iris[1:5, ])
#' predict(fl, data = iris[1:5, ], linkinv = exp)
predict.flashlight <- function(object, ...) {
  object <- flashlight(object, ...)
  required <- c("predict_function", "linkinv", "model", "data")
  stopifnot(sapply(with(object, required), Negate(is.null)))
  with(object, linkinv(predict_function(model, data)))
}
