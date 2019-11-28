#' Interaction Strength
#'
#' Based on (zero) mean-centered ICE curves, different measures of interaction strength are calculated.
#' For the default (\code{type = "overall"}), we get a single value per input variable z.
#' It measures the variance in c-ICE curves explained by all interactions of z
#' (i.e. the variance of all effects of z unexplained by the main effect of z).
#' For \code{type = "pairwise"}, for each combination of variable pairs z1, z2, a value is provided.
#' It measures the variance in c-ICE curves explained by the pairwise interaction terms and is
#' assessed by calculating the squared difference between two-dimensional ICE curves spanned by
#' z1 and z2 and the two corresponding one-dimensional ICE curves. The values can be normalized by the total variance.
#' Unnormalized values help to compare interaction strength across variables resp. variable pairs,
#' while normalized values show interaction strength relative to the overall effects of z (resp. z1 and z2).
#' In order to simplify interpretation, unnormalized values are root-transformed to be on the scale of the response variable.
#' Normalized values can be interpreted as proportions of variance explained. In order to reproduce Friedman's H statistic [1],
#' choose \code{type = "pairwise"}, \code{normalize = TRUE} and \code{take_sqrt = TRUE}.
#'
#' Note that continuous variables are binned using quantile cuts to get more stable results. The minimum required elements in the (multi-)flashlight are "predict_function", "model", "linkinv" and "data", where the latest can be passed on the fly.
#'
#' @importFrom stats setNames
#' @importFrom dplyr is.tbl as_tibble bind_rows bind_cols
#' @importFrom utils combn
#' @param x An object of class \code{flashlight} or \code{multiflashlight}.
#' @param data An optional \code{data.frame}.
#' @param by An optional vector of column names used to additionally group the results.
#' @param v Vector of variables to be assessed.
#' @param type Type of the measure of interaction strength: Either "overall" or "pairwise".
#' @param normalize Should the variances explained be normalized? Default is \code{FALSE}.
#' @param take_sqrt By default, unnormalized values are root-transformed while normalized values are not.
#' @param grid_size Grid size used to form the outer product. Will be randomly picked from data.
#' @param n_max Maximum number of data rows to consider. Will be randomly picked from \code{data} if necessary.
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
#' @references [1] Friedman, J. H. and Popescu, B. E. (2008). “Predictive learning via rule ensembles.” The Annals of Applied Statistics. JSTOR, 916–54..
#' @examples
#' fit_additive <- lm(Sepal.Length ~ Petal.Length + Petal.Width, data = iris)
#' fit_nonadditive <- lm(Sepal.Length ~ Petal.Length * Petal.Width, data = iris)
#' fl_additive <- flashlight(model = fit_additive, label = "additive")
#' fl_nonadditive <- flashlight(model = fit_nonadditive, label = "nonadditive")
#' fls <- multiflashlight(list(fl_additive, fl_nonadditive), data = iris, y = "Sepal.Length")
#' x <- fls$nonadditive
#' plot(light_interaction(fls))
#' plot(light_interaction(fls, type = "pairwise", normalize = TRUE))
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

#' @describeIn light_interaction Interaction strengths for a flashlight object.
#' @export
light_interaction.flashlight <- function(x, data = x$data, by = x$by,
                                         v = NULL, type = c("overall", "pairwise"),
                                         normalize = FALSE,
                                         take_sqrt = !normalize,
                                         grid_size = 20,
                                         n_max = 200, seed = NULL,
                                         use_linkinv = FALSE,
                                         value_name = "value",
                                         error_name = "error", label_name = "label",
                                         variable_name = "variable", ...) {
  type <- match.arg(type)

  stopifnot((n <- nrow(data)) >= 1L,
            !anyDuplicated(c(by, v, value_name, label_name, error_name, variable_name,
                             "id_", "value_", "value_i", "value_j", "denominator_")))

  if (is.null(v)) {
    v <- setdiff(colnames(data), c(x$y, by))
  }

  # Reduce data size
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (n_max < n) {
    data <- data[sample(n, n_max), , drop = FALSE]
    n <- nrow(data)
  }

  # Select grid indices from the reduced data set
  if (grid_size < n) {
    grid_ind <- sample(n, grid_size)
  } else {
    grid_size <- n
    grid_ind <- seq_len(grid_size)
  }

  # Update flashlight
  x <- flashlight(x, data = data, by = by,
                  linkinv = if (use_linkinv) x$linkinv else function(z) z)

  # Function to safely replace bad values by 0
  replace_bad <- function(x) {
    .bad <- x < 0.000001 | !is.finite(x)
    if (any(.bad)) {
      x[.bad] <- 0
    }
    x
  }

  # ICE wrapper used in the core function
  call_ice <- function(grid, vn, drop_w = FALSE) {
    ice <- light_ice(x, grid = grid, n_max = Inf, value_name = vn,
                     id_name = "id_")$data
    ice[, setdiff(colnames(ice), c(label_name, if (drop_w) x$w))]
  }
  core_func <- function(z) {
    # Generate data and the values of the numerator
    if (type == "overall") {
      dat <- call_ice(data[grid_ind, z, drop = FALSE], "value_")
      dat[[value_name]] <- grouped_center(dat, x = "value_", w = x$w, by = c(by, z))^2
    } else {
      dat_ij <- call_ice(data[grid_ind, z], "value_")
      dat_i <- call_ice(data[grid_ind, z[1], drop = FALSE], "value_i", drop_w = TRUE)
      dat_j <- call_ice(data[grid_ind, z[2], drop = FALSE], "value_j", drop_w = TRUE)
      dat <- bind_cols(dat_ij, dat_i, dat_j)
      dat[, c("value_j", "value_i", "value_")] <- scale(dat[, c("value_j", "value_i", "value_")], scale = F)
      dat[[value_name]] <- (dat[["value_"]] - dat[["value_i"]] - dat[["value_j"]])^2
    }

    # Aggregate numerator by groups
    numerator <- grouped_stats(dat, x = value_name, w = x$w, by = by,
                               counts = FALSE, na.rm = TRUE)
    if (!normalize) {
      return(numerator)
    }

    dat[["denominator_"]] <- dat[["value_"]]^2
    denominator <- grouped_stats(dat, x = "denominator_", w = x$w, by = by,
                                 counts = FALSE, na.rm = TRUE)

    # Calculate ratio
    numerator[[value_name]] <- replace_bad(numerator[[value_name]]) / denominator[["denominator_"]]
    numerator
  }
  if (type == "overall") {
    data <- setNames(lapply(v, core_func), v)
  } else {
    v_pairs <- combn(v, 2, simplify = FALSE)
    data <- setNames(lapply(v_pairs, core_func), lapply(v_pairs, paste, collapse = ":"))
  }
  data <- bind_rows(data, .id = variable_name)
  data[[value_name]] <- replace_bad(data[[value_name]])

  # Take square-root
  if (take_sqrt) {
    data[[value_name]] <- sqrt(data[[value_name]])
  }

  # Prepare output
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
