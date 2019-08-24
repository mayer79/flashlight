#' Response of flashlight
#'
#' Extracts response from object of class \code{flashlight}.
#'
#' @param object An object of class \code{flashlight}.
#' @param ... Arguments used to update the flashlight before extracting the response.
#' @return A numeric vector of responses.
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
  object <- flashlight(object, ...)
  required <- c("y", "linkinv", "data")
  stopifnot(sapply(with(object, required), Negate(is.null)))
  with(object, linkinv(data[[y]]))
}
