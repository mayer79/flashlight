#' Prints light Object
#'
#' Print method for an object of class \code{light}.
#'
#' @importFrom dplyr is.tbl
#' @param x A on object of class \code{light}.
#' @param ... Further arguments passed from other methods.
#' @return Invisibly, the input is returned.
#' @method print light
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' fl <- flashlight(model = fit, label = "lm", y = "Sepal.Length", data = iris)
#' light_performance(fl, v = "Species")
#' light_effects(fl, v = "Sepal.Length")
print.light <- function(x, ...) {
  cat("\nI am an object with class(es)", paste(class(x), collapse = ", "), "\n")
  x_cs <- x[vapply(x, FUN = is.tbl, FUN.VALUE = TRUE)]
  if (length(x_cs)) {
    cat("\nTibbles:\n")
    for (nm in names(x_cs)) {
      cat("\n", nm, "\n")
      print(x_cs[[nm]], n = 2)
    }
  }
  invisible(x)
}

