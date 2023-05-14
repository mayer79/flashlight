#' ALE profile
#'
#' Internal function used by [light_profile()] to calculate ALE profiles.
#'
#' @noRd
#' @param x An object of class "flashlight".
#' @param v The variable to be profiled.
#' @param breaks Cut breaks for a numeric `v`. Only used if no `evaluate_at` is specified.
#' @param n_bins Maxmium number of unique values to evaluate for numeric `v`.
#'   Only used if no `evaluate_at` is specified.
#' @param cut_type For the default "equal", bins of equal width are created for `v`
#'   by [pretty()]. Choose "quantile" to create quantile bins.
#' @param counts Should counts be added?
#' @param counts_weighted If `counts = TRUE`: Should counts be weighted by the
#'   case weights? If `TRUE`, the sum of `w` is returned by group.
#' @param pred Optional vector with predictions.
#' @param evaluate_at Vector with values of `v` used to evaluate the profile.
#'   Only relevant for type = "partial dependence".
#' @param indices A vector of row numbers to consider.
#' @param n_max Maximum number of ICE profiles to calculate within interval (not within data).
#' @param seed Integer random seed passed to [light_ice()].
#' @param two_sided Standard ALE profiles are calculated via left derivatives.
#'   Set to `TRUE` if two-sided derivatives should be calculated.
#'   Only works for continuous `v`. More specifically: Usually, local effects at
#'   value x are calculated using points in \eqn{[x-e, x]}. Set `ale_two_sided = TRUE`
#'   to use points in \eqn{[x-e/2, x+e/2]} instead.
#' @param calibrate Should values be calibrated based on average preditions?
#'   Default is `TRUE`.
#' @param ... Other arguments passed to this function (currently unused).
#' @returns A tibble containing results.
ale_profile <- function(x, v, breaks = NULL, n_bins = 11L,
                        cut_type = c("equal", "quantile"),
                        counts = TRUE, counts_weighted = FALSE,
                        pred = NULL, evaluate_at = NULL,
                        indices = NULL, n_max = 1000L, seed = NULL,
                        two_sided = FALSE, calibrate = TRUE, ...) {
  cut_type <- match.arg(cut_type)
  data <- x$data
  stopifnot(
    "No data!" = is.data.frame(data) && nrow(data) >= 1L,
    "'v' not specified." = !is.null(v),
    "'v' not in 'data'." = v %in% colnames(data),
    !any(c("value_", "counts_", "id_") %in% c(x$by, v))
  )
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (!is.null(indices)) {
    data <- data[indices, , drop = FALSE]
    if (!is.null(pred)) {
      pred <- pred[indices]
    }
  }
  is_num <- is.numeric(data[[v]])

  # Evaluation points (including shift for two-sided derivatives)
  if (is.null(evaluate_at)) {
    if (!is.null(breaks)) {
      evaluate_at <- midpoints(breaks)
    } else {
      cuts <- auto_cut(data[[v]], breaks = breaks, n_bins = n_bins, cut_type = cut_type)
      breaks <- cuts$breaks
      evaluate_at <- cuts$bin_means
    }
  }
  if (two_sided) {
    if (!is.null(breaks)) {
      evaluate_at_orig <- evaluate_at
      evaluate_at <- breaks[-1L]
    } else {
      two_sided <- FALSE
    }
  }

  # Helper function used to calculate differences for any pair of x values
  ale_core <- function(from_to) {
    from <- from_to[1L]
    to <- from_to[2L]
    if (is_num) {
      .s <- data[[v]] >= from & data[[v]] <= to
    } else {
      .s <- data[[v]] %in% from_to
    }
    dat_i <- data[.s, , drop = FALSE]
    if (nrow(dat_i) == 0L) {
      return(NULL)
    }
    ice <- light_ice(x, v = v, data = dat_i, evaluate_at = from_to, n_max = n_max)$data
    if (is_num) {
      ice <- transform(ice, value_ = if (identical(to, from)) 0 else value_/(to - from))
    }
    # Safe reshaping
    dat_to <- ice[ice[[v]] %in% to, ]
    dat_from <- ice[ice[[v]] %in% from, ]
    dat_to <- transform(
      dat_to, value_ = value_ - dat_from$value_[match(id_, dat_from$id_)]
    )

    # Aggregation and output
    out <- grouped_stats(
      dat_to,
      x = "value_",
      w = x$w,
      by = x$by,
      counts_weighted = counts_weighted,
      na.rm = TRUE
    )
    out[[v]] <- to
    out
  }

  # Call ale_core once per interval and combine results
  eval_pair <- data.frame(
    from = evaluate_at[c(1L, 1:(length(evaluate_at) - 1L))], to = evaluate_at
  )
  ale <- dplyr::bind_rows(apply(eval_pair, 1L, ale_core))

  # Remove missing values before accumulation
  bad <- is.na(ale$value_)
  if (any(bad)) {
    ale <- ale[!bad, , drop = FALSE]
  }

  # Accumulate effects. Integrate out gaps
  wcumsum <- function(X) {
    transform(
      X,
      value_ = as.numeric(cumsum(value_ * (if (is_num) c(0, diff(X[[v]])) else 1)))
    )
  }
  ale <- Reframe(ale, FUN = wcumsum, .by = x$by)

  if (is.factor(data[[v]])) {
    ale[[v]] <- factor(ale[[v]], levels = levels(data[[v]]))
  }

  # Calibrate effects
  if (calibrate) {
    preds <- if (is.null(pred)) stats::predict(x) else pred
    if (is.null(x$by)) {
      pred_mean <- MetricsWeighted::weighted_mean(
        preds, if (!is.null(x$w)) data[[x$w]], na.rm = TRUE
      )
      ale_mean <- MetricsWeighted::weighted_mean(
        ale$value_, w = ale$counts_, na.rm = TRUE
      )
      ale <- tibble::as_tibble(transform(ale, value_ = value_ - ale_mean + pred_mean))
    } else {
      stopifnot(!(c("cal_xx", "shift_xx") %in% colnames(data)))
      dat_pred <- grouped_stats(
        cbind(data, cal_xx = preds),
        x = "cal_xx",
        w = x$w,
        by = x$by,
        counts = FALSE,
        na.rm = TRUE
      )
      dat_ale <- grouped_stats(
        ale, x = "value_", w = "counts_", by = x$by, counts = FALSE, na.rm = TRUE
      )
      dat_shift <- dplyr::left_join(dat_ale, dat_pred, by = x$by)
      dat_shift <- transform(dat_shift, shift_xx = cal_xx - value_)
      ale <- dplyr::left_join(
        ale, dat_shift[, c(x$by, "shift_xx"), drop = FALSE], by = x$by
      )
      ale <- transform(ale, value_ = value_ + shift_xx, shift_xx = NULL)
    }
  }
  # Revert shift for two-sided derivatives
  if (two_sided) {
    ale[[v]] <- evaluate_at_orig[match(ale[[v]], breaks[-1L])]
  }

  ale[, c(x$by, v, if (counts && "counts_" %in% colnames(ale)) "counts_", "value_")]
}
