#' ALE profile
#'
#' Internal function used by \code{light_profile} to calculate ALE profiles.
#'
#' @importFrom dplyr as_tibble left_join group_by_at do ungroup bind_rows
#' @importFrom rlang .data
#' @importFrom stats setNames
#' @param x An object of class \code{flashlight}.
#' @param v The variable to be profiled.
#' @param breaks Cut breaks for a numeric \code{v}. Only used if no \code{evaluate_at} is specified.
#' @param n_bins Maxmium number of unique values to evaluate for numeric \code{v}. Only used if no \code{evaluate_at} is specified.
#' @param cut_type For the default "equal", bins of equal width are created for \code{v} by \code{pretty}. Choose "quantile" to create quantile bins.
#' @param value_name Column name containing the profile value. Defaults to "value".
#' @param counts_name Name of the column containing counts if \code{counts} is TRUE.
#' @param counts Should counts be added?
#' @param counts_weighted If \code{counts} is TRUE: Should counts be weighted by the case weights? If TRUE, the sum of \code{w} is returned by group.
#' @param pred Optional vector with predictions.
#' @param evaluate_at Vector with values of \code{v} used to evaluate the profile. Only relevant for type = "partial dependence".
#' @param indices A vector of row numbers to consider.
#' @param n_max Maximum number of ICE profiles to calculate within interval (not within data).
#' @param seed Integer random seed passed to \code{light_ice}.
#' @param two_sided Standard ALE profiles are calculated via left derivatives. Set to TRUE if two-sided derivatives should be calculated. Only works for continuous \code{v}. More specifically: Usually, local effects at value x are calculated using points between x-e and x. Set \code{ale_two_sided = TRUE} to use points between x-e/2 and x+e/2.
#' @param calibrate Should values be calibrated based on average preditions? Default is TRUE.
#' @return A tibble containing results.
ale_profile <- function(x, v, breaks = NULL, n_bins = 11,
                        cut_type = c("equal", "quantile"),
                        value_name = "value", counts_name = "counts", counts = TRUE,
                        counts_weighted = FALSE, pred = NULL, evaluate_at = NULL,
                        indices = NULL, n_max = 1000, seed = NULL,
                        two_sided = FALSE, calibrate = TRUE) {
  # Initial stuff
  cut_type <- match.arg(cut_type)
  data <- x$data
  stopifnot(!is.null(v),
            v %in% colnames(data),
            nrow(data) >= 1L,
            !anyDuplicated(c(x$by, v, value_name, counts_name, "id_xxx")))
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
      evaluate_at <- breaks[-1]
    } else {
      two_sided <- FALSE
    }
  }

  # Helper function used to calculate differences for any pair of x values
  ale_core <- function(from_to) {
    from <- from_to[1]
    to <- from_to[2]
    if (is_num) {
      .s <- data[[v]] >= from & data[[v]] <= to
    } else {
      .s <- data[[v]] %in% from_to
    }
    dat_i <- data[.s, , drop = FALSE]
    if (nrow(dat_i) == 0L) {
      return(NULL)
    }
    ice <- light_ice(x, v = v, data = dat_i, evaluate_at = from_to,
                     n_max = n_max, value_name = value_name,
                     id_name = "id_xxx")$data
    if (is_num) {
      ice[[value_name]] <- if (identical(to, from)) 0 else ice[[value_name]] / (to - from)
    }

    # Safe reshaping
    dat_to <- ice[ice[[v]] %in% to, ]
    dat_from <- ice[ice[[v]] %in% from, ]
    dat_to[[value_name]] <- dat_to[[value_name]] -
      dat_from[[value_name]][match(dat_to[["id_xxx"]], dat_from[["id_xxx"]])]

    # Aggregation and output
    out <- grouped_stats(dat_to, x = value_name, w = x$w, by = x$by,
                         counts_name = counts_name,
                         counts_weighted = counts_weighted, na.rm = TRUE)
    out[[v]] <- to
    out
  }

  # Call ale_core once per interval and combine results
  eval_pair <- data.frame(from = evaluate_at[c(1L, 1:(length(evaluate_at) - 1L))],
                          to = evaluate_at)
  ale <- bind_rows(apply(eval_pair, 1, ale_core))

  # Remove missing values before accumulation
  if (any(bad <- is.na(ale[[value_name]]))) {
    ale <- ale[!bad, , drop = FALSE]
  }

  # Accumulate effects. Integrate out gaps
  wcumsum <- function(X) {
    X[[value_name]] <- cumsum(X[[value_name]] * (if (is_num) c(0, diff(X[[v]])) else 1))
    X
  }
  ale <- if (is.null(x$by)) wcumsum(ale) else
      ungroup(do(group_by_at(ale, x$by), wcumsum(.data)))
  if (is.factor(data[[v]])) {
    ale[[v]] <- factor(ale[[v]], levels = levels(data[[v]]))
  }

  # Calibrate effects
  if (calibrate) {
    preds <- if (is.null(pred)) predict(x) else pred
    if (is.null(x$by)) {
      pred_mean <- weighted_mean(preds, if (!is.null(x$w)) data[[x$w]], na.rm = TRUE)
      ale_mean <- weighted_mean(ale[[value_name]], w = ale[[counts_name]], na.rm = TRUE)
      ale[[value_name]] <- ale[[value_name]] - ale_mean + pred_mean
      ale <- as_tibble(ale)
    } else {
      stopifnot(!(c("cal_xx", "shift_xx") %in% colnames(data)))
      dat_pred <- grouped_stats(cbind(data, cal_xx = preds), x = "cal_xx", w = x$w,
                                by = x$by, counts = FALSE, na.rm = TRUE)
      dat_ale <- grouped_stats(ale, x = value_name, w = counts_name,
                               by = x$by, counts = FALSE, na.rm = TRUE)
      dat_shift <- left_join(dat_ale, dat_pred, by = x$by)
      dat_shift[["shift_xx"]] <- dat_shift[["cal_xx"]] - dat_shift[[value_name]]
      ale <- left_join(ale, dat_shift[, c(x$by, "shift_xx"), drop = FALSE], by = x$by)
      ale[[value_name]] <- ale[[value_name]] + ale[["shift_xx"]]
      ale[["shift_xx"]] <- NULL
    }
  }
  # Revert shift for two-sided derivatives
  if (two_sided) {
    ale[[v]] <- evaluate_at_orig[match(ale[[v]], breaks[-1])]
  }
  ale[, c(x$by, v, if (counts && counts_name %in% colnames(ale)) counts_name, value_name)]
}
