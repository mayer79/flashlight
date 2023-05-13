#' Scatter
#'
#' This function prepares values for drawing a scatter plot of predicted values,
#' responses, or residuals against a selected variable.
#'
#' @param x An object of class "flashlight" or "multiflashlight".
#' @param v The variable name to be shown on the x-axis.
#' @param data An optional `data.frame`.
#' @param by An optional vector of column names used to additionally group the results.
#' @param type Type of the profile: Either "predicted", "response", or "residual".
#' @param use_linkinv Should retransformation function be applied? Default is `TRUE`.
#' @param n_max Maximum number of data rows to select. Will be randomly picked from the
#'   relevant data.
#' @param seed An integer random seed used for subsampling.
#' @param ... Further arguments passed from or to other methods.
#' @returns
#'   An object of class "light_scatter" with the following elements:
#'   - `data`: A tibble with results. Can be used to build fully customized
#'     visualizations. Column names can be controlled by
#'     `options(flashlight.column_name)`.
#'   - `by`: Same as input `by`.
#'   - `v`: The variable evaluated.
#'   - `type`: Same as input `type`. For information only.
#' @export
#' @examples
#' fit_a <- stats::lm(Sepal.Length ~ . -Petal.Length, data = iris)
#' fit_b <- stats::lm(Sepal.Length ~ ., data = iris)
#' fl_a <- flashlight(model = fit_a, label = "without Petal.Length")
#' fl_b <- flashlight(model = fit_b, label = "all")
#' fls <- multiflashlight(list(fl_a, fl_b), data = iris, y = "Sepal.Length")
#' pr <- light_scatter(fls, v = "Petal.Length")
#' plot(
#'   light_scatter(fls, "Petal.Length", by = "Species", type = "residual"),
#'   alpha = 0.2
#' )
#' @seealso [plot.light_scatter()]
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
                                     type = c("predicted", "response",
                                              "residual", "shap"),
                                     use_linkinv = TRUE, n_max = 400,
                                     seed = NULL, ...) {
  type <- match.arg(type)

  if (type == "shap") {
    stop("type = 'shap' is deprecated.")
  }

  stopifnot(
    "No data!" = is.data.frame(data) && nrow(data) >= 1L,
    "'by' not in 'data'!" = by %in% colnames(data),
    "'v' not in 'data'!" = v %in% colnames(data),
    !any(c("value_", "label_") %in% by)
  )
  if (type %in% c("response", "residual") && is.null(x$y)) {
    stop("You need to specify 'y' in flashlight.")
  }
  n <- nrow(data)

  # Subsample rows if data too large
  if (n > n_max) {
    if (!is.null(seed)) {
      set.seed(seed)
    }
    data <- data[sample(n, n_max), , drop = FALSE]
  }

  # Update flashlight
  x <- flashlight(
    x, data = data, by = by, linkinv = if (use_linkinv) x$linkinv else function(z) z
  )

  # Calculate values
  data$value_ <- switch(
    type,
    response = response(x),
    predicted = stats::predict(x),
    residual = stats::residuals(x)
  )

  # Organize output
  data$label_ <- x$label
  add_classes(
    list(
      data = tibble::as_tibble(data[, c("label_", by, v, "value_")]),
      by = by,
      v = v,
      type = type
    ),
    c("light_scatter", "light")
  )
}

#' @describeIn light_scatter light_scatter for a multiflashlight.
#' @export
light_scatter.multiflashlight <- function(x, ...) {
  light_combine(lapply(x, light_scatter, ...), new_class = "light_scatter_multi")
}
