#' Scatter Plot Data
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
#' @param n_max Maximum number of data rows to select. Will be randomly picked.
#' @param seed An integer random seed used for subsampling.
#' @param ... Further arguments passed from or to other methods.
#' @returns
#'   An object of class "light_scatter" with the following elements:
#'   - `data`: A tibble with results.
#'   - `by`: Same as input `by`.
#'   - `v`: The variable name evaluated.
#'   - `type`: Same as input `type`. For information only.
#' @export
#' @examples
#' fit_a <- lm(Sepal.Length ~ . -Petal.Length, data = iris)
#' fit_b <- lm(Sepal.Length ~ ., data = iris)
#'
#' fl_a <- flashlight(model = fit_a, label = "no Petal.Length")
#' fl_b <- flashlight(model = fit_b, label = "all")
#' fls <- multiflashlight(list(fl_a, fl_b), data = iris, y = "Sepal.Length")
#'
#' plot(light_scatter(fls, v = "Petal.Width"), color = "darkred")
#'
#' sc <- light_scatter(fls, "Petal.Length", by = "Species", type = "residual")
#' plot(sc)
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

#' Scatter Plot
#'
#' Values are plotted against a variable. The object returned is of class "ggplot"
#' and can be further customized. To avoid overplotting, try `alpha = 0.2` or
#' `position = "jitter"`.
#'
#' @importFrom rlang .data
#'
#' @inheritParams plot.light_performance
#' @param x An object of class "light_scatter".
#' @param swap_dim If multiflashlight and one "by" variable, or single flashlight
#'   with two "by" variables, swap the role of color variable and facet variable.
#'   If multiflashlight or one "by" variable, use colors instead of facets.
#' @param ... Further arguments passed to [ggplot2::geom_point()]. Typical arguments
#'   would be `alpha = 0.2` or `position = "jitter"` to avoid overplotting.
#' @returns An object of class "ggplot".
#' @export
#' @seealso [light_scatter()]
plot.light_scatter <- function(x, swap_dim = FALSE, facet_scales = "free_x",
                               rotate_x = FALSE, ...) {
  data <- x$data
  nby <- length(x$by)
  multi <- is.light_scatter_multi(x)
  ndim <- nby + multi
  if (ndim > 2L) {
    stop("Plot method not defined for more than two by variables or
         multiflashlight with more than one by variable.")
  }
  # Distinguish some cases
  p <- ggplot2::ggplot(
    x$data, ggplot2::aes(x = .data[[x$v]], y = value_)
  )
  if (ndim == 0L) {
    p <- p + ggplot2::geom_point(...)
  } else if (ndim == 1L) {
    first_dim <- if (multi) "label_" else x$by[1L]
    if (swap_dim) {
      p <- p +
        ggplot2::geom_point(ggplot2::aes(color = .data[[first_dim]]), ...) +
        ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(alpha = 1)))
    } else {
      p <- p +
        ggplot2::geom_point(...) +
        ggplot2::facet_wrap(first_dim, scales = facet_scales)
    }
  } else {
    second_dim <- if (multi) "label_" else x$by[2L]
    wrap_var <- if (swap_dim) x$by[1L] else second_dim
    col_var <- if (swap_dim) second_dim else x$by[1L]
    p <- p +
      ggplot2::geom_point(ggplot2::aes(color = .data[[col_var]]), ...) +
      ggplot2::facet_wrap(wrap_var, scales = facet_scales) +
      override_alpha()
  }
  if (rotate_x) {
    p <- p + rotate_x()
  }
  p + ggplot2::ylab(x$type)
}
