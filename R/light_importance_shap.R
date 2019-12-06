#' Variable Importance based on SHAP
#'
#' The function calculates average squared SHAP values per covariable, serving as a measure of importance. The (multi-)flashlight has to contain an object of class "shap". Call \code{add_shap} once before calling this function.
#'
#' @param x An object of class \code{flashlight} or \code{multiflashlight}.
#' @param by An optional vector of column names used to additionally group the results.
#' @param value_name Column name in resulting \code{data} containing the average squared SHAP value. Defaults to "value".
#' @param error_name Currently unused.
#' @param label_name Column name in resulting \code{data} containing the label of the flashlight. Defaults to "label".
#' @param variable_name Column name in resulting \code{data} containing the variable names. Defaults to "variable".
#' @param ... Further arguments passed from and to other methods.
#' @return An object of class \code{light_importance}, \code{light} (and a list) with the following elements.
#' \itemize{
#'   \item \code{data} A tibble with results. Can be used to build fully customized visualizations. The columns "value_original" and "value_shuffled" provide the performance before and after shuffling.
#'   \item \code{by} Same as input \code{by}.
#'   \item \code{value_name} Same as input \code{value_name}.
#'   \item \code{error_name} Same as input \code{error_name}.
#'   \item \code{label_name} Same as input \code{label_name}.
#'   \item \code{variable_name} Same as input \code{variable_name}.
#' }
#' @export
#' @examples
#' \dontrun{
#' fit_a <- lm(Sepal.Length ~ . + Petal.Length:Species, data = iris)
#' fit_b <- lm(Sepal.Length ~ . + Petal.Length, data = iris)
#' fl_a <- flashlight(model = fit_a, label = "a")
#' fl_b <- flashlight(model = fit_a, label = "b")
#' fls <- multiflashlight(list(fl_a, fl_b), data = iris, y = "Sepal.Length")
#' fls <- add_shap(fls)
#' imp <- light_importance_shap(fls)
#' plot(imp, swap_dim = TRUE)
#' }
#'
#' @seealso \code{\link{most_important}}, \code{\link{plot.light_importance}}.
light_importance_shap <- function(x, ...) {
  UseMethod("light_importance_shap")
}

#' @describeIn light_importance_shap Default method not implemented yet.
#' @export
light_importance_shap.default <- function(x, ...) {
  stop("light_importance_shap method is only available for objects of class flashlight or multiflashlight.")
}

#' @describeIn light_importance_shap Variable importance for a flashlight.
#' @export
light_importance_shap.flashlight <- function(x, by = x$by,
                                       value_name = "value", error_name = "error",
                                       label_name = "label",
                                       variable_name = "variable", ...) {
  stopifnot(is.shap(x$shap),
            !anyDuplicated(c(by, value_name, label_name, variable_name, error_name)))

  if (!is.null(x$shap$by) && !(x$shap$by %in% by)) {
    warning("SHAP values have been computed using other 'by' groups. This is not recommended.")
  }

  # Remove irrelevant rows and add squared shap
  data <- x$shap$data
  data <- data[data[[x$shap$variable_name]] %in% x$shap$v, ]
  data[[value_name]] <- data[[x$shap$shap_name]]^2
  if (x$shap$variable_name != variable_name) {
    data[[variable_name]] <- data[[x$shap$variable_name]]
  }

  # Group results by variable
  imp <- grouped_stats(data, x = value_name, w = x$w, by = c(by, variable_name), counts = FALSE)

  # Organize output
  imp[[label_name]] <- x$label
  imp[[error_name]] <- NA
  var_order <- c(label_name, by, variable_name, value_name, error_name)

  out <- list(data = imp[, var_order], by = by,
              value_name = value_name, error_name = error_name,
              label_name = label_name, variable_name = variable_name)
  class(out) <- c("light_importance", "light", "list")
  out
}

#' @describeIn light_importance_shap Variable importance for a multiflashlight.
#' @export
light_importance_shap.multiflashlight <- function(x, ...) {
  light_combine(lapply(x, light_importance_shap, ...),
                new_class = "light_importance_multi")
}
