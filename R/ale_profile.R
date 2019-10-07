#' ALE profile
#'
#' Internal function used by \code{light_profile} to calculate ALE profiles.
#'
#' @importFrom dplyr as_tibble left_join
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
#' @param n_max Maximum number of ICE profiles to calculate (will be randomly picked from \code{data}). Only used if type = "partial dependence".
#' @param seed Integer random seed passed to \code{light_ice}.
#' @param calibrate Should values be calibrated based on average preditions? Default is TRUE.
#' @return A tibble containing results.
ale_profile <- function(x, v, breaks = NULL, n_bins = 11, cut_type = c("equal", "quantile"),
                        value_name = "value", counts_name = "counts", counts = TRUE,
                        counts_weighted = FALSE, pred = NULL, evaluate_at = NULL,
                        n_max = 1000, seed = NULL, calibrate = TRUE) {
  cut_type <- match.arg(cut_type)
  data <- x$data
  stopifnot(!is.null(v), v %in% colnames(data))

  ale_core <- function(from_to) {
    from <- from_to[1]
    to <- from_to[2]
    if (is.numeric(to)) {
      .s <- data[[v]] >= from & data[[v]] <= to
    } else {
      .s <- data[[v]] %in% from_to
    }
    dat_i <- data[.s, , drop = FALSE]
    if (nrow(dat_i) == 0L) {
      return(NULL)
    }
    ice <- light_ice(x, v = v, data = dat_i, evaluate_at = from_to,
                     n_max = n_max, seed = seed, value_name = value_name,
                     id_name = "id_xxx")$data
    # Safe reshaping
    dat_to <- ice[ice[[v]] == to, ]
    dat_from <- ice[ice[[v]] == from, ]
    dat_to[[value_name]] <- dat_to[[value_name]] -
      dat_from[[value_name]][match(dat_to[["id_xxx"]], dat_from[["id_xxx"]])]

    # Aggregation
    out <- grouped_stats(dat_to, x = value_name, w = x$w, by = x$by,
                         counts_name = counts_name, counts_weighted = counts_weighted)

    # Output
    out[[v]] <- to
    out
  }

  if (is.null(evaluate_at)) {
    evaluate_at <- if (!is.null(breaks)) midpoints(breaks) else
      auto_cut(data[[v]], n_bins = n_bins, cut_type = cut_type)$bin_means
  }
  eval_pair <- data.frame(from = evaluate_at[c(1L, 1:(length(evaluate_at) - 1L))],
                          to = evaluate_at)
  ale <- do.call(rbind, apply(eval_pair, 1, ale_core))
  if (length(x$by)) {
    ale[[value_name]] <- ave(ale[[value_name]], ale[[x$by]], FUN = cumsum)
  } else {
    ale[[value_name]] <- cumsum(ale[[value_name]])
  }
  if (is.factor(data[[v]])) {
    ale[[v]] <- factor(ale[[v]], levels = levels(data[[v]]))
  }

  # Calibration
  if (calibrate) {
    preds <- if (is.null(pred)) predict(x) else pred
    if (is.null(x$by)) {
      pred_mean <- weighted_mean(preds, if (!is.null(x$w)) data[[x$w]], na.rm = TRUE)
      ale_mean <- weighted_mean(ale[[value_name]], w = ale[[counts_name]], na.rm = TRUE)
      ale[[value_name]] <- ale[[value_name]] - ale_mean + pred_mean
    } else {
      stopifnot(!(c("cal_xx", "shift_xx") %in% colnames(data)))
      dat_pred <- grouped_stats(cbind(data, cal = preds), x = "cal_xx", w = x$w,
                                by = x$by, counts = FALSE, na.rm = TRUE)
      dat_ale <- grouped_stats(ale, x = value_name, w = counts_name,
                               by = x$by, counts = FALSE, na.rm = TRUE)
      dat_shift <- left_join(dat_ale, dat_pred, by = x$by)
      dat_shift[["shift_xx"]] <- dat_shift[["cal_xx"]] - dat_shift[[value_name]]
      ale <- left_join(ale, dat_shift[, c(x$by, "shift_xx"), drop = FALSE], by = x$by)
      ale[[value_name]] <- ale[[value_name]] + ale[[value_name]]
      ale[["shift_xx"]] <- NULL
    }
  }
  if (!counts) {
    ale[[counts_name]] <- NULL
  }
  as_tibble(ale)
}

#' x <- light_effects(mod_full, v = "Petal.Width")
#' plot(x, use = "all")
#'
