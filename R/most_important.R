#' Most Important Variables.
#'
#' Returns the most important variable names sorted descendingly.
#'
#' @importFrom utils head
#' @importFrom dplyr group_by_at summarize_at arrange_at desc
#' @param x An object of class \code{light_importance}.
#' @param top_m Maximum number of important variables to be returned. Defaults to \code{Inf}, i.e. return all variables in descending order of importance.
#' @return A character vector of variable names sorted in descending order by importance.
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' fl <- flashlight(model = fit, label = "ols", data = iris, y = "Sepal.Length")
#' (imp <- light_importance(fl, seed = 4))
#' most_important(imp)
#' most_important(imp, 2)
#' @seealso \code{\link{light_importance}}.
most_important <- function(x, top_m = Inf) {
  UseMethod("most_important")
}

#' @describeIn most_important Default method not implemented yet.
#' @export
most_important.default <- function(x, top_m = Inf) {
  stop("No default method available yet.")
}

#' @describeIn most_important Extracts most important variables from an object of class \code{light_importance}.
#' @export
most_important.light_importance <- function(x, top_m = Inf) {
  data <- group_by_at(x$data, x$variable_name)
  total_importance <- summarize_at(data, x$value_name, sum, na.rm = TRUE)
  total_importance <- arrange_at(total_importance, x$value_name, desc)
  head(total_importance[[x$variable_name]], top_m)
}
