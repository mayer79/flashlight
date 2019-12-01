#' Residuals for multiflashlight
#'
#' Residuals method for an object of class \code{multiflashlight}. Pass additional elements to update the multiflashlight before calculation of residuals.
#'
#' @importFrom stats residuals
#' @method residuals multiflashlight
#' @param object An object of class \code{multiflashlight}.
#' @param ... Arguments used to update the multiflashlight before calculating the residuals.
#' @return A named list with residuals per flashlight.
#' @export
#' @examples
#' fit_part <- lm(Sepal.Length ~ Petal.Length, data = iris)
#' fit_full <- lm(Sepal.Length ~ ., data = iris)
#' mod_full <- flashlight(model = fit_full, label = "full")
#' mod_part <- flashlight(model = fit_part, label = "part")
#' mods <- multiflashlight(list(mod_full, mod_part), data = iris, y = "Sepal.Length")
#' residuals(mods, data = head(iris))
residuals.multiflashlight <- function(object, ...) {
  lapply(object, residuals, ...)
}
