#' Predictions for multiflashlight
#'
#' Predict method for an object of class \code{multiflashlight}. Pass additional elements to update the flashlight, typically \code{data}.
#'
#' @importFrom stats predict
#' @param object An object of class \code{multiflashlight}.
#' @param ... Arguments used to update the multiflashlight.
#' @return A named list of prediction vectors.
#' @export
#' @method predict multiflashlight
#' @examples
#' fit_part <- lm(Sepal.Length ~ Petal.Length, data = iris)
#' fit_full <- lm(Sepal.Length ~ ., data = iris)
#' mod_full <- flashlight(model = fit_full, label = "full")
#' mod_part <- flashlight(model = fit_part, label = "part")
#' mods <- multiflashlight(list(mod_full, mod_part), data = iris, y = "Sepal.Length")
#' predict(mods, data = iris[1:5, ])
predict.multiflashlight <- function(object, ...) {
  lapply(object, predict, ...)
}
