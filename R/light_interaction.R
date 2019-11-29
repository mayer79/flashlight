#' Interaction Strength
#'
#' This function provides Friedman's H statistic [1] for overall interaction strength per covariable as well as its version for pairwise interactions. As a fast alterantive to assess overall interaction strength, with \code{type = "ice"} the function offers a method based on centered ICE curves: The corresponding H* statistic measures how much of variability of a c-ICE curve is unexplained by the main effect. As for Friedman's H statistic, it can be useful to consider unnormalized or squared values (see Details below).
#'
#' Friedman's H statistic relates the interaction strength of a variable (pair) to the total effect strength of that variable (pair). Due to this normalization step, even variables with low importance can have high values for H. The function \code{light_interaction} offers the option to skip that step in order to have a more direct comparison of the interaction effects across variable (pairs). The values of unnormalized H are on the scale of the response variable. Use \code{take_sqrt = FALSE} to return squared values of H.
#'
#' @importFrom stats setNames
#' @importFrom dplyr as_tibble bind_rows bind_cols arrange_at expand_grid group_by_at do ungroup
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
#' plot(light_interaction(fls, pairwise = TRUE, normalize = FALSE))
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
                                         v = NULL, pairwise = FALSE,
                                         type = c("H", "ice"),
                                         normalize = TRUE, take_sqrt = TRUE,
                                         grid_size = 30, n_max = 100,
                                         seed = NULL, use_linkinv = FALSE,
                                         value_name = "value",
                                         error_name = "error", label_name = "label",
                                         variable_name = "variable", ...) {
  # Checks
  type <- match.arg(type)
  if (type == "ice" & pairwise) {
    stop("Pairwise interactions are implemented only for type = 'pd'.")
  }
  stopifnot((n <- nrow(data)) >= 1L,
            !(by %in% v),
            !anyDuplicated(c(by, v, value_name, label_name, error_name, variable_name,
                             "id_", "id_curve", "value_", "value_i", "value_j", "denom_")))

  # Set seed
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Determine v
  if (is.null(v)) {
    v <- setdiff(colnames(data), c(x$y, by))
  }
  if (pairwise) {
    v <- combn(v, 2, simplify = FALSE)
  }

  # Update flashlight with info on linkinv
  # (we do not update data and by as they are dealt with separately)
  x <- flashlight(x, linkinv = if (use_linkinv) x$linkinv else function(z) z)

  # HELPER FUNCTIONS
  # Slim version of light_profile and light_ice
  call_pd <- function(X, z, vn = "value_", gid, only_values = FALSE, agg = TRUE) {
    cols <- colnames(X)
    if (!is.null(x$w)) {
      ww <- X[gid, x$w, drop = FALSE]
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
    out <- grouped_stats(X, x = vn, w = x$w, by = "id_", counts = FALSE, na.rm = TRUE)
    out <- arrange_at(out, "id_")
    if (!is.null(x$w)) {
      out[[x$w]] <- ww[[x$w]][match(out[["id_"]], ww[["id_"]])]
    }
    out[[vn]] <- grouped_center(out, x = vn, w = x$w)
    if (only_values) out[, vn, drop = FALSE] else out
  }
  # Get predictions on grid in the same order as through call_pd
  call_f <- function(X, vn = "value_", gid) {
    out <- X[gid, ]
    out[[vn]] <- predict(x, data = out)
    out[[vn]] <- grouped_center(out, x = vn, w = x$w)
    out[["id_"]] <- gid
    arrange_at(out, "id_")[, c("id_", vn, x$w)]
  }
  # Functions that calculates the test statistic
  H_statistic <- function(z, dat, grid_id) {
    if (nrow(dat) <= 2) {
      return(0)
    }
    if (type == "H") {
      z_j <- if (pairwise) z[2] else setdiff(colnames(dat), z)
      pd_f <- if (pairwise) call_pd(dat, z = z, gid = grid_id) else call_f(dat, gid = grid_id)
      pd_i <- call_pd(dat, z = z[1], vn = "value_i", gid = grid_id, only_values = TRUE)
      pd_j <- call_pd(dat, z = z_j, vn = "value_j", gid = grid_id, only_values = TRUE)
      dat <- bind_cols(pd_f, pd_i, pd_j)
      dat[[value_name]] <- (dat[["value_"]] - dat[["value_i"]] - dat[["value_j"]])^2
    }
    else {
      dat <- call_pd(dat, z = z, gid = grid_id, agg = FALSE)
      dat[[value_name]] <- grouped_center(dat, x = "value_", w = x$w, by = "id_")^2
    }
    # Aggregate & normalize
    ww <- if (!is.null(x$w)) dat[[x$w]]
    num <- weighted_mean(dat[[value_name]], ww, na.rm = TRUE)
    if (normalize) {
      num <- zap_small(num) / weighted_mean(dat[["value_"]]^2, ww, na.rm = TRUE)
    }
    zap_small(if (take_sqrt) sqrt(num) else num)
  }
  # Calculate statistic for each variable (pair) and combine results
  core_func <- function(X) {
    # Reduce data sice and select grid values
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
    out <- lapply(v, H_statistic, dat = X, grid_id = grid_id)
    out <- lapply(out, function(z) setNames(data.frame(z), value_name))
    names(out) <- if (pairwise) lapply(v, paste, collapse = ":") else v
    bind_rows(out, .id = variable_name)
  }

  # Call core function for each by group
  agg <- if (is.null(by)) as_tibble(core_func(data)) else
    ungroup(do(group_by_at(data, by), core_func(.data)))

  # Prepare output
  agg[[label_name]] <- x$label
  agg[[error_name]] <- NA

  # Collect results
  var_order <- c(label_name, by, variable_name, value_name, error_name)
  out <- list(data = agg[, var_order], by = by,
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
