#' Apply Retransformation to SHAP Object in flashlight
#'
#' Internal function used to update shap values of a flashlight.
#'
#' @param x An object of class \code{flashlight}.
#' @param linkinv A vectorized inverse transformation.
#' @return A flashlight with updated shap object.
shap_link <- function(x, linkinv = x$linkinv) {
  stopifnot(is.flashlight(x), is.shap(x$shap))
  if (x$shap$use_linkinv) {
    if (isFALSE(all.equal(linkinv, x$shap$linkinv))) {
      warning("Link function seems to have changed. Recalculate shap values either with \n
              this retranformation or with option 'use_linkinv = FALSE'.")
    }
    return(x)
  }
  data <- x$shap$data
  shap_vars <- c("baseline_", "before_", "after_")
  data[, shap_vars] <- lapply(data[, shap_vars], linkinv)
  data[["shap_"]] <- data[["after_"]] - data[["before_"]]
  x$shap$data <- data
  x$shap$use_linkinv <- TRUE
  x$shap$linkinv <- linkinv
  x
}
