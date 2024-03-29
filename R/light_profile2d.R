#' 2D Partial Dependence and other 2D Profiles
#'
#' Calculates different types of 2D-profiles across two variables.
#' By default, partial dependence profiles are calculated (see Friedman).
#' Other options are response, predicted values, and residuals.
#' The results are aggregated by (weighted) means.
#'
#' Different binning options are available, see arguments below.
#' For high resolution partial dependence plots, it might be necessary to specify
#' `breaks`, `pd_evaluate_at` or `pd_grid` in order to avoid empty parts
#' in the plot. A high value of `n_bins` might not have the desired effect as it
#' internally capped at the number of distinct values of a variable.
#'
#' For partial dependence and prediction profiles, "model", "predict_function",
#' "linkinv" and "data" are required. For response profiles it is "y", "linkinv"
#' and "data". "data" can also be passed on the fly.
#'
#' @param x An object of class "flashlight" or "multiflashlight".
#' @param v A vector of exactly two variable names to be profiled.
#' @param data An optional `data.frame`.
#' @param by An optional vector of column names used to additionally group the results.
#' @param type Type of the profile: Either "partial dependence", "predicted",
#'   "response", or "residual".
#' @param breaks Named list of cut breaks specifying how to bin one or more numeric
#'   variables. Used to overwrite automatic binning via `n_bins` and `cut_type`.
#'   Ignored for non-numeric `v`.
#' @param n_bins Approximate number of unique values to evaluate for numeric `v`.
#'   Can be an unnamed vector of length 2 to distinguish between v.
#' @param cut_type Should numeric `v` be cut into "equal" or "quantile" bins?
#'   Can be an unnamed vector of length 2 to distinguish between v.
#' @param use_linkinv Should retransformation function be applied? Default is `TRUE`.
#' @param counts Should observation counts be added?
#' @param counts_weighted If `counts` is TRUE: Should counts be weighted by the
#'   case weights? If `TRUE`, the sum of `w` is returned by group.
#' @param pd_evaluate_at An named list of evaluation points for one or more variables.
#'   Only relevant for type = "partial dependence".
#' @param pd_grid An evaluation `data.frame` with exactly two columns,
#'   e.g., generated by [expand.grid()]. Only used for type = "partial dependence".
#'   Offers maximal flexibility.
#' @param pd_indices A vector of row numbers to consider in calculating partial
#'   dependence profiles. Only used for type = "partial dependence".
#' @param pd_n_max Maximum number of ICE profiles to calculate
#'   (will be randomly picked from `data`). Only used for type = "partial dependence".
#' @param pd_seed Integer random seed used to select ICE profiles.
#'   Only used for type = "partial dependence".
#' @param ... Further arguments passed to [formatC()] in forming
#'   the cut breaks of the `v` variables. Not relevant for partial dependence profiles.
#' @returns
#'   An object of class "light_profile2d" with the following elements:
#'   - `data` A tibble containing results.
#'   - `by` Names of group by variables.
#'   - `v` The two variable names evaluated.
#'   - `type` Same as input `type`. For information only.
#' @export
#' @references
#'   Friedman J. H. (2001). Greedy function approximation: A gradient boosting machine.
#'     The Annals of Statistics, 29:1189–1232.
#' @examples
#' fit_part <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
#' fl_part <- flashlight(
#'   model = fit_part, label = "part", data = iris, y = "Sepal.Length"
#' )
#'
#' # No effect of Petal.Width
#' plot(light_profile2d(fl_part, v = c("Petal.Length", "Petal.Width")))
#'
#' # Second model includes Petal.Width
#' fit_full <- lm(Sepal.Length ~ ., data = iris)
#' fl_full <- flashlight(
#'   model = fit_full, label = "full", data = iris, y = "Sepal.Length"
#' )
#' fls <- multiflashlight(list(fl_part, fl_full))
#'
#' plot(light_profile2d(fls, v = c("Petal.Length", "Petal.Width")))
#' @seealso [light_profile()], [plot.light_profile2d()]
light_profile2d <- function(x, ...) {
  UseMethod("light_profile2d")
}

#' @describeIn light_profile2d Default method not implemented yet.
#' @export
light_profile2d.default <- function(x, ...) {
  stop("No default method available yet.")
}

#' @describeIn light_profile2d 2D profiles for flashlight.
#' @export
light_profile2d.flashlight <- function(x, v = NULL,
                                       data = NULL, by = x$by,
                                       type = c("partial dependence",
                                                "predicted", "response",
                                                "residual", "shap"),
                                       breaks = NULL, n_bins = 11L,
                                       cut_type = "equal",
                                       use_linkinv = TRUE, counts = TRUE,
                                       counts_weighted = FALSE,
                                       pd_evaluate_at = NULL, pd_grid = NULL,
                                       pd_indices = NULL, pd_n_max = 1000L,
                                       pd_seed = NULL, ...) {
  type <- match.arg(type)

  if (type == "shap") {
    stop("type = 'shap' is deprecated.")
  }

  # Check if exactly two variables are specified
  if (type == "partial dependence" && !is.null(pd_grid)) {
    stopifnot(
      "pd_grid must be a data.frame" = is.data.frame(pd_grid),
      "pd_grid must have exactly two columns" = ncol(pd_grid) == 2L
    )
    v <- colnames(pd_grid)
  } else {
    stopifnot("Need exactly two 'v'." = length(v) == 2L)
  }

  # Turn binning arguments into a list of lists
  strategy <- fix_strategy(
    v,
    n_bins = n_bins,
    cut_type = cut_type,
    breaks = breaks,
    pd_evaluate_at = pd_evaluate_at
  )

  if (is.null(data)) {
    data <- x$data
  }

  # Checks on data and column names
  stopifnot(
    "No data!" = is.data.frame(data) && nrow(data) >= 1L,
    "'by' not in 'data'!" = by %in% colnames(data),
    "'v' not in 'data'." = v %in% colnames(data),
    !any(c("value_", "label_", "type_") %in% c(by, v))
  )

  # Update flashlight
  x <- flashlight(
    x, data = data, by = by, linkinv = if (use_linkinv) x$linkinv else function(z) z
  )

  # Calculate profiles
  if (type == "partial dependence") {
    # Construct pd_grid from strategy
    if (is.null(pd_grid)) {
      for (vv in v) {
        st <- strategy[[vv]]
        if (is.null(st$pd_evaluate_at)) {
          if (!is.null(st$breaks) && is.numeric(st$breaks)) {
            strategy[[vv]]$pd_evaluate_at <- midpoints(st$breaks)
          } else {
              strategy[[vv]]$pd_evaluate_at <- auto_cut(
                data[[vv]], n_bins = st$n_bins, cut_type = st$cut_type, ...
              )$bin_means
          }
        }
      }
      pd_grid <- expand.grid(lapply(strategy, `[[`, "pd_evaluate_at"))
    }

    # Calculate 2D ICE profiles
    data <- light_ice(
      x = x,
      grid = pd_grid,
      indices = pd_indices,
      n_max = pd_n_max,
      seed = pd_seed
    )$data
  } else {
    if (type %in% c("response", "residual") && is.null(x$y)) {
      stop("You need to specify 'y' in flashlight.")
    }

    # Add predictions/response to data
    data$value_ <- switch(
      type,
      response = response(x),
      predicted = stats::predict(x),
      residual = stats::residuals(x)
    )

    # Replace v values by binned values
    for (vv in v) {
      st <- strategy[[vv]]
      data[[vv]] <- auto_cut(
        data[[vv]],
        n_bins = st$n_bins,
        cut_type = st$cut_type,
        breaks = st$breaks,
        ...
      )$data$level
    }
  }

  # Aggregate predicted values
  agg <- grouped_stats(
    data = data,
    x = "value_",
    w = x$w,
    by = c(by, v),
    na.rm = TRUE,
    counts = counts,
    counts_weighted = counts_weighted,
    counts_name = "counts_"
  )

  # Finalize results
  agg <- transform(agg, label_ = x$label, type_ = type)
  out <- list(data = agg, by = by, v = v, type = type)
  add_classes(out, c("light_profile2d", "light"))
}

#' @describeIn light_profile2d 2D profiles for multiflashlight.
#' @export
light_profile2d.multiflashlight <- function(x, v = NULL, data = NULL,
                                            type = c("partial dependence",
                                                     "predicted", "response",
                                                     "residual", "shap"),
                                            breaks = NULL, n_bins = 11L,
                                            cut_type = "equal",
                                            pd_evaluate_at = NULL,
                                            pd_grid = NULL, ...) {
  type <- match.arg(type)
  is_pd <- type == "partial dependence"

  if (is.null(pd_grid) || !is_pd) {
    stopifnot("Need exactly two 'v'." = length(v) == 2L)

    # Turn binning arguments into a list of lists
    strategy <- fix_strategy(
      v,
      n_bins = n_bins,
      cut_type = cut_type,
      breaks = breaks,
      pd_evaluate_at = pd_evaluate_at
    )

    # Calculate common breaks for both variables independently
    for (vv in v) {
      st <- strategy[[vv]]
      if (is.null(st$breaks) && (is.null(st$pd_evaluate_at) || !is_pd)) {
        strategy[[vv]]$breaks <- common_breaks(
          x, v = vv, data = data, n_bins = st$n_bins, cut_type = st$cut_type
        )
      }
    }
    breaks <- lapply(strategy, `[[`, "breaks")
  }

  # Call light_profile2d for all flashlights
  all_profiles <- lapply(
    x,
    light_profile2d,
    v = v,
    data = data,
    type = type,
    breaks = breaks,
    n_bins = n_bins,
    cut_type = cut_type,
    pd_evaluate_at = pd_evaluate_at,
    pd_grid = pd_grid,
    ...
  )
  light_combine(all_profiles, new_class = "light_profile2d_multi")
}

#' Visualize 2D-Profiles, e.g., of Partial Dependence
#'
#' Minimal visualization of an object of class "light_profile2d".
#' The object returned is of class "ggplot" and can be further customized.
#'
#' The main geometry is [ggplot2::geom_tile()]. Additional dimensions
#' ("by" variable(s) and/or multiflashlight) are represented by `facet_wrap/grid`.
#' For all types of profiles except "partial dependence", it is natural to see
#' empty parts in the plot. These are combinations of the `v` variables that
#' do not appear in the data. Even for type "partial dependence", such gaps can occur,
#' e.g. for `cut_type = "quantile"` or if `n_bins` are larger than the number
#' of distinct values of a `v` variable.
#' Such gaps can be suppressed by setting `numeric_as_factor = TRUE`
#' or by using the arguments `breaks`, `pd_evaluate_at` or `pd_grid` in
#' [light_profile2d()].
#'
#' @importFrom rlang .data
#' @param x An object of class "light_profile2d".
#' @param swap_dim Swap the [ggplot2::facet_grid()] dimensions.
#' @param rotate_x Should the x axis labels be rotated by 45 degrees? Default is `TRUE`.
#' @param numeric_as_factor Should numeric x and y values be converted to factors first?
#'   Default is `FALSE`. Useful if `cut_type` was not set to "equal".
#' @param ... Further arguments passed to [ggplot2::geom_tile()].
#' @returns An object of class "ggplot".
#' @export
#' @seealso [light_profile2d()]
plot.light_profile2d <- function(x, swap_dim = FALSE, rotate_x = TRUE,
                                 numeric_as_factor = FALSE, ...) {
  multi <- is.light_profile2d_multi(x)
  ndim <- length(x$by) + multi
  if (ndim > 2L) {
    stop("Plot method not defined for more than two by variables or
         multiflashlight with more than one by variable.")
  }
  data <- x$data
  if (isTRUE(numeric_as_factor)) {
    data[x$v] <- lapply(data[x$v], as.factor)
  }

  # Build plot
  p <- ggplot2::ggplot(
    data, ggplot2::aes(x = .data[[x$v[1L]]], y = .data[[x$v[2L]]], fill = value_)
  ) +
    ggplot2::geom_tile(...)
  if (ndim == 1L) {
    p <- p + ggplot2::facet_wrap(if (multi) "label_" else x$by[1L])
  } else if (ndim == 2L) {
    d1 <- if (multi) "label_" else x$by[1L]
    d2 <- if (multi) x$by[1L] else x$by[2L]
    form <- if (!swap_dim) stats::reformulate(d1, d2) else stats::reformulate(d2, d1)
    p <- p + ggplot2::facet_grid(form)
  }
  if (rotate_x) {
    p <- p + rotate_x()
  }
  p
}
