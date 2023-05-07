#' Interaction Strength
#'
#' This function provides Friedman's H statistic for overall interaction strength per
#' covariable as well as its version for pairwise interactions, see the reference below.
#'
#' As a fast alternative to assess overall interaction strength, with `type = "ice"`,
#' the function offers a method based on centered ICE curves:
#' The corresponding H* statistic measures how much of the variability of a c-ICE curve
#' is unexplained by the main effect. As for Friedman's H statistic, it can be useful
#' to consider unnormalized or squared values (see Details below).
#'
#' Friedman's H statistic relates the interaction strength of a variable (pair)
#' to the total effect strength of that variable (pair) based on partial dependence
#' curves. Due to this normalization step, even variables with low importance can
#' have high values for H. The function [light_interaction()] offers the option
#' to skip normalization in order to have a more direct comparison of the interaction
#' effects across variable (pairs). The values of such unnormalized H statistics are
#' on the scale of the response variable. Use `take_sqrt = FALSE` to return
#' squared values of H. Note that in general, for each variable (pair), predictions
#' are done on a data set with `grid_size * n_max`, so be cautious with
#' increasing the defaults too much. Still, even with larger `grid_size`
#' and `n_max`, there might be considerable variation across different runs,
#' thus, setting a seed is recommended.
#'
#' The minimum required elements in the (multi-) flashlight are a "predict_function",
#' "model", and "data".
#'
#' @param x An object of class "flashlight" or "multiflashlight".
#' @param data An optional `data.frame`.
#' @param by An optional vector of column names used to additionally group the results.
#' @param v Vector of variable names to be assessed.
#' @param pairwise Should overall interaction strength per variable be shown or
#'   pairwise interactions? Defaults to `FALSE`.
#' @param type Are measures based on Friedman's H statistic ("H") or on "ice" curves?
#'   Option "ice" is available only if `pairwise = FALSE`.
#' @param normalize Should the variances explained be normalized?
#'   Default is `TRUE` in order to reproduce Friedman's H statistic.
#' @param take_sqrt In order to reproduce Friedman's H statistic,
#'   resulting values are root transformed. Set to `FALSE` if squared values
#'   should be returned.
#' @param grid_size Grid size used to form the outer product. Will be randomly
#'   picked from data (after limiting to `n_max`).
#' @param n_max Maximum number of data rows to consider. Will be randomly picked
#'   from `data` if necessary.
#' @param seed An integer random seed used for subsampling.
#' @param use_linkinv Should retransformation function be applied? Default is `FALSE`.
#' @param ... Further arguments passed to or from other methods.
#' @returns
#'   An object of class "light_importance" with the following elements:
#'   - `data` A tibble containing the results. Can be used to build fully customized
#'     visualizations. Column names can be controlled by
#'     `options(flashlight.column_name)`.
#'   - `by` Same as input `by`.
#'   - `type` Same as input `type`. For information only.
#' @export
#' @references
#'   Friedman, J. H. and Popescu, B. E. (2008). "Predictive learning via rule
#'     ensembles." The Annals of Applied Statistics. JSTOR, 916–54.
#' @examples
#' v <- c("Petal.Length", "Petal.Width")
#' fit_add <- stats::lm(Sepal.Length ~ Petal.Length + Petal.Width, data = iris)
#' fit_nonadd <- stats::lm(Sepal.Length ~ Petal.Length * Petal.Width, data = iris)
#' fl_add <- flashlight(model = fit_add, label = "additive")
#' fl_nonadd <- flashlight(model = fit_nonadd, label = "nonadditive")
#' fls <- multiflashlight(list(fl_add, fl_nonadd), data = iris)
#' plot(st <- light_interaction(fls, v = v), fill = "darkgreen")
#' plot(light_interaction(fls, v = v, pairwise = TRUE), fill = "darkgreen")
#' plot(st <- light_interaction(fls, v = v, by = "Species"), fill = "darkgreen")
#' @seealso [light_ice()]
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
                                         grid_size = 200L, n_max = 1000L,
                                         seed = NULL,
                                         use_linkinv = FALSE, ...) {
  type <- match.arg(type)

  warning_on_names(c("value_name", "label_name", "variable_name", "error_name"), ...)

  value_name <- getOption("flashlight.value_name")
  label_name <- getOption("flashlight.label_name")
  variable_name <- getOption("flashlight.variable_name")
  error_name <- getOption("flashlight.error_name")

  if (length(by) >= 2L) {
    stop("light_interaction() does not support more than one by variable.")
  }
  stopifnot(
    "No data!" = is.data.frame(data) && nrow(data) >= 1L,
    "'by' not in 'data'!" = by %in% colnames(data),
    "Not all 'v' in 'data'" = v %in% colnames(data),
    !(c("id_", "id_curve", "w_") %in% colnames(data))
  )
  check_unique(
    c(by, v),
    opt_names = c(value_name, label_name, error_name, variable_name),
    temp_names = c("w_", "id_", "id_curve", "value_", "value_i", "value_j", "denom_")
  )
  if (type == "ice" && pairwise) {
    stop("Pairwise interactions are implemented only for type = 'H'.")
  }
  cols <- colnames(data)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Determine v if not yet available
  if (is.null(v)) {
    v <- setdiff(cols, c(x$y, by, x$w))
  }
  stopifnot(length(v) >= 1L + pairwise)
  if (pairwise) {
    v <- utils::combn(v, 2, simplify = FALSE)
  }

  # Sampling weights have to be dealt with since they can appear in both grid and sample
  has_w <- !is.null(x$w)
  w <- if (has_w) "w_"
  if (has_w) {
    data[[w]] <- data[[x$w]]
  }

  # Update flashlight (except for data)
  x <- flashlight(
    x, by = by, linkinv = if (use_linkinv) x$linkinv else function(z) z
  )

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
    X <- tidyr::expand_grid(X, grid)
    X[[vn]] <- stats::predict(x, data = X[, cols, drop = FALSE])
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
    out[[vn]] <- stats::predict(x, data = out[, cols, drop = FALSE])
    out[[vn]] <- grouped_center(out, x = vn, w = w)
    out[["id_"]] <- gid
    out[order(out[["id_"]]), c("id_", vn, w)]
  }
  # Functions that calculates the test statistic
  statistic <- function(z, dat, grid_id) {
    if (nrow(dat) <= 2) {
      return(stats::setNames(data.frame(0), value_name))
    }
    if (type == "H") {
      z_i <- z[1L]
      z_j <- if (pairwise) z[2L] else setdiff(cols, z_i)
      if (pairwise) {
        pd_f <- call_pd(dat, z = z, gid = grid_id)
      } else {
        pd_f <- call_f(dat, gid = grid_id)
      }
      pd_i <- call_pd(dat, z = z_i, vn = "value_i", gid = grid_id, only_values = TRUE)
      pd_j <- call_pd(dat, z = z_j, vn = "value_j", gid = grid_id, only_values = TRUE)
      dat <- dplyr::bind_cols(pd_f, pd_i, pd_j)
      dat[[value_name]] <- (dat[["value_"]] - dat[["value_i"]] - dat[["value_j"]])^2
    }
    else if (type == "ice") {
      dat <- call_pd(dat, z = z, gid = grid_id, agg = FALSE)
      dat[[value_name]] <- grouped_center(dat, x = "value_", w = w, by = "id_")^2
    } else {
      stop("Only type H or ice implemented.")
    }
    # Aggregate & normalize
    num <- MetricsWeighted::weighted_mean(
      dat[[value_name]], w = if (has_w) dat[[w]], na.rm = TRUE
    )
    if (normalize) {
      num <- .zap_small(num) /
        MetricsWeighted::weighted_mean(
          dat[["value_"]]^2, w = if (has_w) dat[[w]], na.rm = TRUE
        )
    }
    stats::setNames(
      data.frame(.zap_small(if (take_sqrt) sqrt(num) else num)), value_name
    )
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
    dplyr::bind_rows(out, .id = variable_name)
  }

  # Call core function for each "by" group (should rework code...)
  if (is.null(by)) {
    agg <- core_func(data)
  } else {
    agg_l <- lapply(split(data, f = data[[by]]), core_func)
    for (nm in names(agg_l)) {
      agg_l[[nm]][, by] <- nm
    }
    agg <- dplyr::bind_rows(agg_l)
  }
  agg <- tibble::as_tibble(agg)

  # Prepare output
  agg[[label_name]] <- x$label
  agg[[error_name]] <- NA
  var_order <- c(label_name, by, variable_name, value_name, error_name)
  add_classes(
    list(data = agg[, var_order], by = by, type = type), c("light_importance", "light")
  )
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

