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
  cat("\nmetrics:\t\t", .yn(x[["metrics"]], names(x$metrics)))
  cat("\n")
  invisible(x)
}

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

#' Prints light Object
#'
#' Print method for an object of class "light".
#'
#' @param x A on object of class "light".
#' @param ... Further arguments passed from other methods.
#' @returns Invisibly, the input is returned.
#' @method print light
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' fl <- flashlight(model = fit, label = "lm", y = "Sepal.Length", data = iris)
#' light_performance(fl, v = "Species")
print.light <- function(x, ...) {
  cat("\nI am an object of class", class(x)[1L], "\n")
  x_cs <- x[vapply(x, FUN = is.data.frame, FUN.VALUE = TRUE)]
  if (length(x_cs)) {
    cat("\ndata.frames:\n")
    for (nm in names(x_cs)) {
      cat("\n", nm, "\n")
      print(x_cs[[nm]])
    }
  }
  invisible(x)
}

#' Predictions for flashlight
#'
#' Predict method for an object of class "flashlight".
#' Pass additional elements to update the flashlight, typically `data`.
#'
#' @param object An object of class "flashlight".
#' @param ... Arguments used to update the flashlight.
#' @returns A vector with predictions.
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' fl <- flashlight(model = fit, data = iris, y = "Sepal.Length", label = "ols")
#' predict(fl)[1:5]
#' predict(fl, data = iris[1:5, ])
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

#' Predictions for multiflashlight
#'
#' Predict method for an object of class "multiflashlight".
#' Pass additional elements to update the flashlight, typically `data`.
#'
#' @param object An object of class "multiflashlight".
#' @param ... Arguments used to update the multiflashlight.
#' @returns A named list of prediction vectors.
#' @export
#' @examples
#' fit_part <- lm(Sepal.Length ~ Petal.Length, data = iris)
#' fit_full <- lm(Sepal.Length ~ ., data = iris)
#' mod_full <- flashlight(model = fit_full, label = "full")
#' mod_part <- flashlight(model = fit_part, label = "part")
#' mods <- multiflashlight(list(mod_full, mod_part), data = iris, y = "Sepal.Length")
#' predict(mods, data = iris[1:5, ])
predict.multiflashlight <- function(object, ...) {
  lapply(object, stats::predict, ...)
}

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

#' Residuals for multiflashlight
#'
#' Residuals method for an object of class "multiflashlight".
#' Pass additional elements to update the multiflashlight before calculation of
#' residuals.
#'
#' @param object An object of class "multiflashlight".
#' @param ... Arguments used to update the multiflashlight before
#'   calculating the residuals.
#' @returns A named list with residuals per flashlight.
#' @export
#' @examples
#' fit_part <- lm(Sepal.Length ~ Petal.Length, data = iris)
#' fit_full <- lm(Sepal.Length ~ ., data = iris)
#' mod_full <- flashlight(model = fit_full, label = "full")
#' mod_part <- flashlight(model = fit_part, label = "part")
#' mods <- multiflashlight(list(mod_full, mod_part), data = iris, y = "Sepal.Length")
#' residuals(mods, data = head(iris))
residuals.multiflashlight <- function(object, ...) {
  lapply(object, stats::residuals, ...)
}

#' Response of multi/-flashlight
#'
#' Extracts response from object of class "flashlight".
#'
#' @param object An object of class "flashlight".
#' @param ... Arguments used to update the flashlight before extracting the response.
#' @returns A numeric vector of responses.
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' (fl <- flashlight(model = fit, data = iris, y = "Sepal.Length", label = "ols"))
#' response(fl)[1:5]
#' response(fl, data = iris[1:5, ])
#' response(fl, data = iris[1:5, ], linkinv = exp)
response <- function(object, ...) {
  UseMethod("response")
}

#' @describeIn response Default method not implemented yet.
#' @export
response.default <- function(object, ...) {
  stop("No default method available yet.")
}

#' @describeIn response Extract response from flashlight object.
#' @export
response.flashlight <- function(object, ...) {
  object <- flashlight(object, check = FALSE, ...)
  required <- c("y", "linkinv", "data")
  stopifnot(sapply(with(object, required), Negate(is.null)))
  with(object, linkinv(data[[y]]))
}

#' @describeIn response Extract responses from multiflashlight object.
#' @export
response.multiflashlight <- function(object, ...) {
  lapply(object, response, ...)
}



# Helper functions
.yn <- function(z, ret = z) {
  if (!is.null(z)) ret else "No"
}
