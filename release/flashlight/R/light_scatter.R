#' Scatter
#'
#' This function prepares values for drawing a scatter plot of predicted values, responses, residuals, or SHAP values against a selected variable.
#'
#' @importFrom dplyr left_join as_tibble
#' @param x An object of class \code{flashlight} or \code{multiflashlight}.
#' @param v The variable to be shown on the x-axis.
#' @param data An optional \code{data.frame}. Not relevant for \code{type = "shap"}.
#' @param by An optional vector of column names used to additionally group the results.
#' @param type Type of the profile: Either "predicted", "response", "residual", or "shap".
#' @param use_linkinv Should retransformation function be applied? Default is TRUE. Not used for \code{type = "shap"}.
#' @param n_max Maximum number of data rows to consider. Will be randomly picked from the relevant data.
#' @param seed An integer random seed used for subsampling.
#' @param value_name Column name in resulting \code{data} containing the values belonging to \code{type}. Defaults to "value".
#' @param label_name Column name in resulting \code{data} containing the label of the flashlight. Defaults to "label".
#' @param type_name Column name in the resulting \code{data} with the plot \code{type}.
#' @param ... Further arguments passed from or to other methods.
#' @return An object of class \code{light_scatter}, \code{light} (and a list) with the following elements.
#' \itemize{
#'   \item \code{data} A tibble with results. Can be used to build fully customized visualizations.
#'   \item \code{by} Same as input \code{by}.
#'   \item \code{v} The variable evaluated.
#'   \item \code{type} Same as input \code{type}. For information only.
#'   \item \code{value_name} Same as input \code{value_name}.
#'   \item \code{label_name} Same as input \code{label_name}.
#'   \item \code{type_name} Same as input \code{type_name}.
#' }
#' @export
#' @examples
#' fit_a <- lm(Sepal.Length ~ . + Petal.Length:Species, data = iris)
#' fit_b <- lm(Sepal.Length ~ . + Petal.Length, data = iris)
#' fl_a <- flashlight(model = fit_a, label = "a")
#' fl_b <- flashlight(model = fit_b, label = "b")
#' fls <- multiflashlight(list(fl_a, fl_b), data = iris, y = "Sepal.Length")
#' pr <- light_scatter(fls, "Petal.Length")
#' plot(pr, alpha = 0.2)
#' plot(light_scatter(fls, "Petal.Length", by = "Species"), alpha = 0.2)
#'
#' @seealso \code{\link{plot.light_scatter}}.
light_scatter <- function(x, ...) {
  UseMethod("light_scatter")
}

#' @describeIn light_scatter Default method not implemented yet.
#' @export
light_scatter.default <- function(x, ...) {
  stop("light_scatter method is only available for objects of class flashlight or multiflashlight.")
}

#' @describeIn light_scatter Variable profile for a flashlight.
#' @export
light_scatter.flashlight <- function(x, v, data = x$data, by = x$by,
                                     type = c("predicted", "response", "residual", "shap"),
                                     use_linkinv = TRUE, n_max = Inf, seed = NULL,
                                     value_name = "value", label_name = "label",
                                     type_name = "type", ...) {
  type <- match.arg(type)

  # Checks
  stopifnot(v %in% colnames(data),
            (n <- nrow(data)) >= 1,
            !anyDuplicated(c(by, v, value_name, label_name, type_name)))

  # Update flashlight
  x <- flashlight(x, data = data, by = by,
                  linkinv = if (use_linkinv) x$linkinv else function(z) z)

  # Additional checks if SHAP and shap data extraction
  if (type == "shap") {
    if (!is.shap(x$shap)) {
      stop("No shap values calculated. Run 'add_shap' for the flashlight first.")
    }
    data <- x$shap$data[x$shap$data[[x$shap$variable_name]] == v, ]
  }

  # Subsample rows if data too large
  if (n_max < n) {
    if (!is.null(seed)) {
      set.seed(seed)
    }
    data <- data[sample(n, n_max), , drop = FALSE]
    if (type != "shap") {
      x <- flashlight(x, data = data)
    }
  }

  # Calculate values
  data[[value_name]] <- switch(type,
    response = response(x),
    predicted = predict(x),
    residual = residuals(x),
    shap = data[["shap_"]]
  )

  # Organize output
  data[[label_name]] <- x$label
  data[[type_name]] <- type
  data <- as_tibble(data[, c(label_name, type_name, x$w, by, v, value_name)])

  out <- list(data = data, by = by, v = v, type = type, value_name = value_name,
              label_name = label_name, type_name = type_name)
  class(out) <- c("light_scatter", "light", "list")
  out
}

#' @describeIn light_scatter light_scatter for a multiflashlight.
#' @export
light_scatter.multiflashlight <- function(x, ...) {
  light_combine(lapply(x, light_scatter, ...),
                new_class = "light_scatter_multi")
}
