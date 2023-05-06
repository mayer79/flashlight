#' Prints a flashlight
#'
#' Print method for an object of class "flashlight".
#'
#' @param x A on object of class "flashlight".
#' @param ... Further arguments passed from other methods.
#' @returns Invisibly, the input is returned.
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' x <- flashlight(model = fit, label = "lm", y = "Sepal.Length", data = iris)
#' x
#' @seealso [flashlight()]
print.flashlight <- function(x, ...) {
  cat("\nFlashlight", x$label, "\n")
  cat("\nModel:\t\t\t", .yn(x$model, "Yes"))
  cat("\ny:\t\t\t", .yn(x$y))
  cat("\nw:\t\t\t", .yn(x$w))
  cat("\nby:\t\t\t", .yn(x$by))
  cat("\ndata dim:\t\t", .yn(dim(x$data)))
  cat("\npredict_fct default:\t", isTRUE(all.equal(stats::predict, x$predict_function)))
  cat("\nlinkinv default:\t", isTRUE(all.equal(function(z) z, x$linkinv)))
  cat("\nmetrics:\t\t", .yn(x[["metrics"]], names(x$metrics)))
  cat("\nSHAP:\t\t\t", .yn(x$shap, "Yes"))
  cat("\n")
  invisible(x)
}

# Helper function
.yn <- function(z, ret = z) {
  if (!is.null(z)) ret else "No"
}
