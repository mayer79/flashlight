#' Check flashlight
#'
#' Checks if an object of class \code{flashlight} or \code{multiflashlight} is consistently defined.
#'
#' @param x An object of class \code{flashlight} or \code{multiflashlight}.
#' @return The input \code{x} or an error message.
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' fit_log <- lm(log(Sepal.Length) ~ ., data = iris)
#' fl <- flashlight(fit, data = iris, y = "Sepal.Length", label = "ols")
#' fl_log <- flashlight(fit_log,  y = "Sepal.Length", label = "ols", linkinv = exp)
#' light_check(fl)
#' light_check(fl_log)
light_check <- function(x) {
  UseMethod("light_check")
}

#' @describeIn light_check Default check method not implemented yet.
#' @export
light_check.default <- function(x) {
  stop("No default method available yet.")
}

#' @describeIn light_check Checks if a flashlight object is consistently defined.
#' @export
light_check.flashlight <- function(x) {
  if (is.null(x$label)) {
    stop("label should not be NULL.")
  }
  nms <- names(x)
  is_function <- function(nm) {
    if (nm %in% nms && !is.function(x[[nm]])) {
      stop(paste(nm, "needs to be a function."))
    }
  }
  is_char <- function(nm, max_len = 1L) {
    if (nm %in% nms && !is.null(x[[nm]]) &&
        !(is.character(x[[nm]]) && length(x[[nm]]) <= max_len)) {
      stop(paste(nm, "needs to be a character of length one."))
    }
  }
  in_colnames <- function(nm) {
    if (nm %in% nms && !is.null(x[[nm]]) && !(x[[nm]] %in% colnames(x$data))) {
      stop(paste(nm, "needs to be a column in data."))
    }
  }
  lapply(c("predict_function", "linkinv"), is_function)
  lapply(c("y", "w"), is_char)
  is_char("by", max_len = Inf)
  if ("metrics" %in% nms && !is.null(x[["metrics"]]) &&
      !(is.list(x[["metrics"]]))) {
    stop("metrics needs to be a named list.")
  }
  if ("data" %in% nms && !is.null(x[["data"]])) {
    if (!inherits(x$data, "data.frame")) {
      stop("data should be a data.frame.")
    }
    lapply(c("y", "w", "by"), in_colnames)
  }
  invisible(x)
}

#' @describeIn light_check Checks if a multiflashlight object is consistently defined.
#' @export
light_check.multiflashlight <- function(x) {
  # by
  if (!all_identical(x, `[[`, "by")) {
    warning("Inconsistent 'by' variables specified.
            Please pass 'by' in subsequent calls to 'light_*' functions.")
  }
  # metrics
  if (!all_identical(x, `[[`, "metrics")) {
    warning("metrics differ across flashlights. This might produce wrong result in variable importance.
            Please pass 'metric(s)' in subsequent calls to 'light_performance' or 'light_importance'.")
  }
  # colnames(data)
  if (!all_identical(x, function(z) colnames(z$data))) {
    warning("Column names differ across data in flashlights. This is rarely a good idea and can be
            avoided by specifying individual 'predict_function'.")
  }
  # dim(data)
  if (!all_identical(x, function(z) dim(z$data))){
    warning("Data dimensions differ across data in flashlights. This might lead to unfair comparisons.
            Please pass 'data' in subsequent calls to 'light_*' functions.")
  }
  invisible(x)
}
