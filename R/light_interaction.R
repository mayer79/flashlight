#' Interaction Strength
#'
#' Calculates overall interaction strength per covariable by calculating the average within-evaluation-point standard deviation across mean-centered ICE curves, see the vignette for details. Additive covariable effects get a value of 0, while covariables with interaction effects get positive values as their c-ICE curves differ. While relatively efficient to calculate, this method does not show between what specific variable pairs we have strongest interactions. Note that continuous variables are binned using quantile cuts to get more stable results.
#'
#' The minimum required elements in the (multi-)flashlight are "predict_function", "model", "linkinv" and "data", where the latest can be passed on the fly. Which rows in \code{data} are profiled? This is specified by \code{indices}. If not given and \code{n_max} is smaller than the number of rows in \code{data}, then row indices will be sampled randomly from \code{data}. If the same rows should be used for all flashlights in a multiflashlight, there are two options: Either pass a \code{seed} (with potentially undesired consequences for subsequent code) or a vector of indices used to select rows. In both cases, \code{data} should be the same for all flashlights considered.
#'
#' @importFrom stats setNames
#' @importFrom dplyr is.tbl as_tibble
#' @param x An object of class \code{flashlight} or \code{multiflashlight}.
#' @param data An optional \code{data.frame}.
#' @param by An optional vector of column names used to additionally group the results.
#' @param v Vector of variables to be assessed.
#' @param n_bins Maximum number of unique values to evaluate for numeric \code{v}.
#' @param indices A vector of row numbers to consider.
#' @param n_max If \code{indices} is not given, maximum number of rows to consider. Will be randomly picked from \code{data} if necessary.
#' @param seed An integer random seed.
#' @param use_linkinv Should retransformation function be applied? Default is FALSE.
#' @param value_name Column name in resulting \code{data} containing the interaction strenght. Defaults to "value".
#' @param label_name Column name in resulting \code{data} containing the label of the flashlight. Defaults to "label".
#' @param error_name Currently not used.
#' @param variable_name Column name in resulting \code{data} containing the variable names. Defaults to "variable".
#' @param ... Further arguments passed to or from other methods.
#' @return An object of class \code{light_importance}, \code{light} (and a list) with the following elements.
#' \itemize{
#'   \item \code{data} A tibble containing the results. Can be used to build fully customized visualizations. Its column names are specified by the items in this list (except for "method").
#'   \item \code{by} Same as input \code{by}.
#'   \item \code{value_name} Same as input \code{value_name}.
#'   \item \code{error_name} Same as input \code{error_name}.
#'   \item \code{label_name} Same as input \code{label_name}.
#'   \item \code{variable_name} Same as input \code{variable_name}.
#' }
#' @export
#' @examples
#' fit_additive <- lm(Sepal.Length ~ Petal.Length + Petal.Width, data = iris)
#' fit_nonadditive <- lm(Sepal.Length ~ Petal.Length * Petal.Width, data = iris)
#' fl_additive <- flashlight(model = fit_additive, label = "additive")
#' fl_nonadditive <- flashlight(model = fit_nonadditive, label = "nonadditive")
#' fls <- multiflashlight(list(fl_additive, fl_nonadditive), data = iris, y = "Sepal.Length")
#' plot(light_interaction(fls), rotate_x = TRUE)
#' plot(light_interaction(fls, by = "Species"), swap_dim = TRUE)
#' @seealso \code{\link{light_ice}}.
light_interaction <- function(x, ...) {
  UseMethod("light_interaction")
}

#' @describeIn light_interaction Default method not implemented yet.
#' @export
light_interaction.default <- function(x, ...) {
  stop("light_interaction method is only available for objects of class flashlight or multiflashlight.")
}

#' @describeIn light_interaction Univariate interaction strengths for a flashlight object.
#' @export
light_interaction.flashlight <- function(x, data = x$data, by = x$by,
                                         v = NULL, n_max = 50,
                                         center_at = "mean",
                                         seed = NULL, use_linkinv = FALSE,
                                         n_bins = 9,
                                         indices = NULL, value_name = "value",
                                         error_name = "error", label_name = "label",
                                         variable_name = "variable", ...) {
  center_at <- match.arg(center_at)
  stopifnot((n <- nrow(data)) >= 1L,
            !anyDuplicated(c(by, v, value_name, label_name, error_name, variable_name, "id_xxx")))

  if (is.null(v)) {
    v <- setdiff(colnames(data), c(x$y, by))
  }

  # Pick ids
  if (is.null(indices)) {
    if (n_max < n) {
      if (!is.null(seed)) {
        set.seed(seed)
      }
      indices <- sample(n, n_max)
    } else {
      indices <- seq_len(n)
    }
  }
  data <- data[indices, , drop = FALSE]

  # Update flashlight
  x <- flashlight(x, data = data, by = by,
                  linkinv = if (use_linkinv) x$linkinv else function(z) z)

  # Aggregate c-ICE for every v
  core_func <- function(z) {
    dat <- light_ice(x, v = z,
                     n_bins = n_bins, cut_type = "quantile", n_max = Inf,
                     center = TRUE, center_at = center_at, value_name = value_name,
                     label_name = label_name, id_name = "id_xxx")$dat

    # For each grid value of v, calculate variance of curves
    dat_agg <- grouped_stats(dat, x = value_name, w = x$w, stats = "variance",
                             by = c(by, z), counts = FALSE, na.rm = TRUE, method = "ML")

    # Average within by groups
    grouped_stats(dat_agg, x = value_name, by = by, counts = FALSE)
  }
  data <- setNames(lapply(v, core_func), v)
  data <- bind_rows(data, .id = variable_name)
  data[[value_name]] <- ifelse(data[[value_name]] < 0.000001, 0, sqrt(data[[value_name]]))
  data[[label_name]] <- x$label
  data[[error_name]] <- NA
  if (!is.tbl(data)) {
    data <- as_tibble(data)
  }
  # Collect results
  var_order <- c(label_name, by, variable_name, value_name, error_name)
  out <- list(data = data[, var_order], by = by,
              value_name = value_name, error_name = error_name,
              label_name = label_name, variable_name = variable_name)
  class(out) <- c("light_importance", "light", "list")
  out
}

#' @describeIn light_interaction for a multiflashlight object.
#' @export
light_interaction.multiflashlight <- function(x, ...) {
  light_combine(lapply(x, light_interaction, ...), new_class = "light_importance_multi")
}
