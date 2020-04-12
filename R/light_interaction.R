#' Interaction Strength
#'
#' This function provides Friedman's H statistic for overall interaction strength per covariable as well as its version for pairwise interactions, see reference below. As a fast alterantive to assess overall interaction strength, with \code{type = "ice"}, the function offers a method based on centered ICE curves: The corresponding H* statistic measures how much of the variability of a c-ICE curve is unexplained by the main effect. As for Friedman's H statistic, it can be useful to consider unnormalized or squared values (see Details below).
#'
#' Friedman's H statistic relates the interaction strength of a variable (pair) to the total effect strength of that variable (pair) based on partial dependence curves. Due to this normalization step, even variables with low importance can have high values for H. The function \code{light_interaction} offers the option to skip this normalization step in order to have a more direct comparison of the interaction effects across variable (pairs). The values of such unnormalized H are on the scale of the response variable. Use \code{take_sqrt = FALSE} to return squared values of H. Note that in general, for each variable (pair) predictions are done on a data set with \code{grid_size * n_max}, so be cautious with increasing the defaults too much. Still, even with larger \code{grid_size} and \code{n_max}, there might be considerable variation across different runs, thus setting a seed might be required for reproducibility. The minimum required elements in the (multi-) flashlight are a "predict_function", "model", and "data".
#'
#' @importFrom stats setNames
#' @importFrom dplyr as_tibble bind_rows bind_cols group_by_at do ungroup
#' @importFrom tidyr expand_grid
#' @importFrom utils combn
#' @importFrom rlang .data
#' @param x An object of class \code{flashlight} or \code{multiflashlight}.
#' @param data An optional \code{data.frame}.
#' @param by An optional vector of column names used to additionally group the results.
#' @param v Vector of variables to be assessed.
#' @param pairwise Should overall interaction strength per variable be shown or pairwise interactions? Defaults to \code{FALSE}.
#' @param type Are measures based on Friedman's H statistic ("H") or on "ice" curves? Option "ice" is available only if \code{pairwise = FALSE}.
#' @param normalize Should the variances explained be normalized? Default is \code{TRUE} in order to reproduce Friedman's H statistic.
#' @param take_sqrt In order to reproduce Friedman's H statistic, resulting values are root transformed. Set to \code{FALSE} if squared values should be returned.
#' @param grid_size Grid size used to form the outer product. Will be randomly picked from data (after limiting to \code{n_max}).
#' @param n_max Maximum number of data rows to consider. Will be randomly picked from \code{data} if necessary.
#' @param seed An integer random seed used for subsampling.
#' @param use_linkinv Should retransformation function be applied? Default is FALSE.
#' @param value_name Column name in resulting \code{data} containing the interaction strenght. Defaults to "value".
#' @param label_name Column name in resulting \code{data} containing the label of the flashlight. Defaults to "label".
#' @param error_name Currently not used.
#' @param variable_name Column name in resulting \code{data} containing the variable names. Defaults to "variable".
#' @param type_name Column name in the resulting \code{data} with the plot \code{type}.
#' @param ... Further arguments passed to or from other methods.
#' @return An object of class \code{light_importance}, \code{light} (and a list) with the following elements.
#' \itemize{
#'   \item \code{data} A tibble containing the results. Can be used to build fully customized visualizations. Its column names are specified by the items in this list (except for "method").
#'   \item \code{by} Same as input \code{by}.
#'   \item \code{type} Same as input \code{type}. For information only.
#'   \item \code{value_name} Same as input \code{value_name}.
#'   \item \code{error_name} Same as input \code{error_name}.
#'   \item \code{label_name} Same as input \code{label_name}.
#'   \item \code{variable_name} Same as input \code{variable_name}.
#'   \item \code{type_name} Same as input \code{type_name}.
#' }
#' @export
#' @references Friedman, J. H. and Popescu, B. E. (2008). “Predictive learning via rule ensembles.” The Annals of Applied Statistics. JSTOR, 916–54.
#' @examples
#' fit_additive <- lm(Sepal.Length ~ Petal.Length + Petal.Width + Species, data = iris)
#' fit_nonadditive <- lm(Sepal.Length ~ Petal.Length * Petal.Width + Species, data = iris)
#' fl_additive <- flashlight(model = fit_additive, label = "additive")
#' fl_nonadditive <- flashlight(model = fit_nonadditive, label = "nonadditive")
#' fls <- multiflashlight(list(fl_additive, fl_nonadditive), data = iris, y = "Sepal.Length")
#' x <- fls$nonadditive
#' plot(st <- light_interaction(fls))
#' plot(light_interaction(fls, pairwise = TRUE))
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
                                         v = NULL, pairwise = FALSE,
                                         type = c("H", "ice"),
                                         normalize = TRUE, take_sqrt = TRUE,
                                         grid_size = 200, n_max = 1000,
                                         seed = NULL, use_linkinv = FALSE,
                                         value_name = "value",
                                         error_name = "error", label_name = "label",
                                         variable_name = "variable",
                                         type_name = "type", ...) {
  # Checks
  type <- match.arg(type)
  cols <- colnames(data)
  if (type == "ice" && pairwise) {
    stop("Pairwise interactions are implemented only for type = 'H'.")
  }
  stopifnot((n <- nrow(data)) >= 1L,
            !(c("id_", "id_curve", "w_") %in% cols),
            !anyDuplicated(c(by, v, value_name, label_name, error_name,
                             variable_name, "w_", "id_", "id_curve",
                             "value_", "value_i", "value_j", "denom_")))

  # Set seed
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Determine v
  if (is.null(v)) {
    v <- setdiff(cols, c(x$y, by, x$w))
  }
  stopifnot(length(v) >= 1L + pairwise)
  if (pairwise) {
    v <- combn(v, 2, simplify = FALSE)
  }

  # Sampling weights have to be dealt with since they can appear in both grid and sample
  has_w <- !is.null(x$w)
  w <- if (has_w) "w_"
  if (has_w) {
    data[[w]] <- data[[x$w]]
  }

  # Update flashlight (except for data)
  x <- flashlight(x, by = by, linkinv = if (use_linkinv) x$linkinv else function(z) z)

  # HELPER FUNCTIONS
  # Version of light_profile and light_ice
  call_pd <- function(X, z, vn = "value_", gid, only_values = FALSE, agg = TRUE) {
    # Weights of the grid ids
    if (has_w) {
      ww <- X[gid, w, drop = FALSE]
      ww[["id_"]] <- gid
    }
    grid <- X[gid, z, drop = FALSE]
    grid[["id_"]] <- gid
    X[, z] <- NULL
    X[["id_curve"]] <- seq_len(nrow(X))
    X <- expand_grid(X, grid)
    X[[vn]] <- predict(x, data = X[, cols, drop = FALSE])
    if (!agg) {
      X[[vn]] <- grouped_center(X, x = vn, by = "id_curve", na.rm = TRUE)
      return(X)
    }
    out <- grouped_weighted_mean(X, x = vn, w = w, by = "id_")
    out <- out[order(out[["id_"]]), ]
    if (has_w) {
      out[[w]] <- ww[[w]][match(out[["id_"]], ww[["id_"]])]
    }
    out[[vn]] <- grouped_center(out, x = vn, w = w)
    if (only_values) out[, vn, drop = FALSE] else out
  }
  # Get predictions on grid in the same order as through call_pd
  call_f <- function(X, vn = "value_", gid) {
    out <- X[gid, ]
    out[[vn]] <- predict(x, data = out[, cols, drop = FALSE])
    out[[vn]] <- grouped_center(out, x = vn, w = w)
    out[["id_"]] <- gid
    out[order(out[["id_"]]), c("id_", vn, w)]
  }
  # Functions that calculates the test statistic
  statistic <- function(z, dat, grid_id) {
    if (nrow(dat) <= 2) {
      setNames(data.frame(0), value_name)
    }
    if (type == "H") {
      z_i <- z[1]
      z_j <- if (pairwise) z[2] else setdiff(cols, z_i)
      pd_f <- if (pairwise) call_pd(dat, z = z, gid = grid_id) else call_f(dat, gid = grid_id)
      pd_i <- call_pd(dat, z = z_i, vn = "value_i", gid = grid_id, only_values = TRUE)
      pd_j <- call_pd(dat, z = z_j, vn = "value_j", gid = grid_id, only_values = TRUE)
      dat <- bind_cols(pd_f, pd_i, pd_j)
      dat[[value_name]] <- (dat[["value_"]] - dat[["value_i"]] - dat[["value_j"]])^2
    }
    else {
      dat <- call_pd(dat, z = z, gid = grid_id, agg = FALSE)
      dat[[value_name]] <- grouped_center(dat, x = "value_", w = w, by = "id_")^2
    }
    # Aggregate & normalize
    num <- weighted_mean(dat[[value_name]], w = if (has_w) dat[[w]], na.rm = TRUE)
    if (normalize) {
      num <- .zap_small(num) /
        weighted_mean(dat[["value_"]]^2, w = if (has_w) dat[[w]], na.rm = TRUE)
    }
    setNames(data.frame(.zap_small(if (take_sqrt) sqrt(num) else num)), value_name)
  }
  # Calculate statistic for each variable (pair) and combine results
  core_func <- function(X) {
    # Reduce data size and select grid values
    n <- nrow(X)
    if (n_max < n) {
      X <- X[sample(n, n_max), , drop = FALSE]
      n <- n_max
    }
    if (grid_size < n) {
      grid_id <- sample(n, grid_size)
    } else {
      grid_size <- n
      grid_id <- seq_len(grid_size)
    }

    # Calculate Friedman's H statistic for each variable (pair)
    out <- lapply(v, statistic, dat = X, grid_id = grid_id)
    names(out) <- if (pairwise) lapply(v, paste, collapse = ":") else v
    bind_rows(out, .id = variable_name)
  }

  # Call core function for each "by" group
  agg <- if (is.null(by)) as_tibble(core_func(data)) else
    ungroup(do(group_by_at(data, by), core_func(.data)))

  # Prepare output
  agg[[label_name]] <- x$label
  agg[[error_name]] <- NA
  agg[[type_name]] <- type

  # Collect results
  var_order <- c(label_name, by, variable_name, value_name, error_name)
  out <- list(data = agg[, var_order], by = by, type = type,
              value_name = value_name, error_name = error_name,
              label_name = label_name, variable_name = variable_name,
              type_name = type_name)
  class(out) <- c("light_importance", "light", "list")
  out
}


#' @describeIn light_interaction for a multiflashlight object.
#' @export
light_interaction.multiflashlight <- function(x, ...) {
  light_combine(lapply(x, light_interaction, ...), new_class = "light_importance_multi")
}

# Helper function used to clip small values.
.zap_small <- function(x, eps = 1e-12, val = 0) {
  .bad <- abs(x) < eps | !is.finite(x)
  if (any(.bad)) {
    x[.bad] <- val
  }
  x
}

