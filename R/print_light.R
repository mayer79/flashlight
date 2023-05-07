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

