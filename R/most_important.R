#' Most Important Variables.
#'
#' Returns the most important variable names sorted descendingly.
#'
#' @param x An object of class "light_importance".
#' @param top_m Maximum number of important variables to be returned.
#'   Defaults to `Inf`, i.e., return all variables in descending order of importance.
#' @returns
#'   A character vector of variable names sorted in descending order by importance.
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' fl <- flashlight(model = fit, label = "ols", data = iris, y = "Sepal.Length")
#' (imp <- light_importance(fl, seed = 4))
#' most_important(imp)
#' most_important(imp, 2)
#' @seealso [light_importance()]
most_important <- function(x, top_m = Inf) {
  UseMethod("most_important")
}

#' @describeIn most_important Default method not implemented yet.
#' @export
most_important.default <- function(x, top_m = Inf) {
  stop("No default method available yet.")
}

#' @describeIn most_important Extracts most important variables from an object of class
#' "light_importance".
#' @export
most_important.light_importance <- function(x, top_m = Inf) {
  value_name <- getOption("flashlight.value_name")
  variable_name <- getOption("flashlight.variable_name")

  data <- dplyr::group_by(x$data, dplyr::across(tidyselect::all_of(variable_name)))
  total_importance <- dplyr::summarize(
    data,
    dplyr::across(
      tidyselect::all_of(value_name), .fns = function(x) sum(x, na.rm = TRUE)
    ),
    .groups = "drop"
  )
  total_importance <- dplyr::arrange(
    total_importance, dplyr::across(tidyselect::all_of(value_name), dplyr::desc)
  )
  utils::head(total_importance[[variable_name]], top_m)
}
