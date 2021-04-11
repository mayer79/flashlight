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
  object <- flashlight(object, check = FALSE, ...)
  if (is.null(object[["data"]])) {
    stop("No 'data' to predict.")
  }
  if (!is.data.frame(object[["data"]])) {
    stop("'data' needs to be a data.frame.")
  }
  pred <- with(object, linkinv(predict_function(model, data)))
  if (!is.numeric(pred) && !is.logical(pred)) {
    stop("Non-numeric/non-logical predictions detected. Please modify 'predict_function' accordingly.")
  }
  pred
}
