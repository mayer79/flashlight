#' Combination of Response, Predicted, Partial Dependence, and ALE profiles.
#'
#' Calculates response- prediction-, partial dependence, and ALE profiles of a (multi-)flashlight with respect to a covariable \code{v}.
#'
#' Note that ALE profiles are being calibrated by (weighted) average predictions. The resulting level might be quite different from the one of the partial dependence profiles.
#'
#' @importFrom dplyr bind_rows
#' @param x An object of class \code{flashlight} or \code{multiflashlight}.
#' @param v The variable name to be profiled.
#' @param data An optional \code{data.frame}.
#' @param by An optional vector of column names used to additionally group the results.
#' @param stats Statistic to calculate for the response profile: "mean" or "quartiles".
#' @param breaks Cut breaks for a numeric \code{v}. Used to overwrite automatic binning via \code{n_bins} and \code{cut_type}. Ignored if \code{v} is not numeric.
#' @param n_bins Approximate number of unique values to evaluate for numeric \code{v}. Ignored if \code{v} is not numeric or if \code{breaks} is specified.
#' @param cut_type Should a numeric \code{v} be cut into "equal" or "quantile" bins? Ignored if \code{v} is not numeric or if \code{breaks} is specified.
#' @param use_linkinv Should retransformation function be applied? Default is TRUE.
#' @param counts_weighted Should counts be weighted by the case weights? If TRUE, the sum of \code{w} is returned by group.
#' @param v_labels If FALSE, return group centers of \code{v} instead of labels. Only relevant if \code{v} is numeric with many distinct values. In that case useful if e.g. different flashlights use different data sets.
#' @param pred Optional vector with predictions (after application of inverse link). Can be used to avoid recalculation of predictions over and over if the functions is to be repeatedly called for different \code{v} and predictions are computationally expensive to make. Not implemented for multiflashlight.
#' @param pd_indices A vector of row numbers to consider in calculating partial dependence and ALE profiles. Useful to force all flashlights to use the same basis for calculations of partial dependence and ALE.
#' @param pd_n_max Maximum number of ICE profiles to consider for partial depencence and ALE calculation (will be randomly picked from \code{data}).
#' @param pd_seed An integer random seed used to sample ICE profiles for partial dependence and ALE.
#' @param ale_two_sided If \code{TRUE}, \code{v} is continuous and \code{breaks} are passed or being calculated, then two-sided derivatives are calculated for ALE instead of left derivatives. This aligns the results better with the x labels. More specifically: Usually, local effects at value x are calculated using points between x-e and x. Set \code{ale_two_sided = TRUE} to use points between x-e/2 and x+e/2.
#' @param ... Further arguments passed to \code{cut3} resp. \code{formatC} in forming the cut breaks of the \code{v} variable.
#' @return An object of class \code{light_effects} with the following elements.
#' \itemize{
#'   \item \code{response} A tibble containing the response profiles. Column names can be controlled by \code{options(flashlight.column_name)}.
#'   \item \code{predicted} A tibble containing the prediction profiles.
#'   \item \code{pd} A tibble containing the partial dependence profiles.
#'   \item \code{ale} A tibble containing the ALE profiles.
#'   \item \code{by} Same as input \code{by}.
#'   \item \code{v} The variable(s) evaluated.
#'   \item \code{stats} Same as input \code{stats}.
#' }
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' fl <- flashlight(model = fit, label = "iris", data = iris, y = "Sepal.Length")
#' light_effects(fl, v = "Species")
#' @seealso \code{\link{light_profile}}, \code{\link{plot.light_effects}}.
light_effects <- function(x, ...) {
  UseMethod("light_effects")
}

#' @describeIn light_effects Default method.
#' @export
light_effects.default <- function(x, ...) {
  stop("Default method not implemented yet.")
}

#' @describeIn light_effects Profiles for a flashlight object.
#' @export
light_effects.flashlight <- function(x, v, data = NULL, by = x$by,
                                     stats = c("mean", "quartiles"),
                                     breaks = NULL, n_bins = 11,
                                     cut_type = c("equal", "quantile"),
                                     use_linkinv = TRUE,
                                     counts_weighted = FALSE,
                                     v_labels = TRUE, pred = NULL,
                                     pd_indices = NULL, pd_n_max = 1000,
                                     pd_seed = NULL,
                                     ale_two_sided = TRUE, ...) {
  stats <- match.arg(stats)
  cut_type <- match.arg(cut_type)

  warning_on_names(c("value_name", "label_name", "q1_name",
                     "q3_name", "type_name", "counts_name"), ...)

  if (is.null(data)) {
    data <- x$data
  }

  # Checks
  stopifnot(
    "No data!" = is.data.frame(data) && nrow(data) >= 1L,
    "'by' not in 'data'!" = by %in% colnames(data),
    "'v' not in 'data'." = v %in% colnames(data),
    "'v' not specified." = !is.null(v)
  )
  check_unique(c(by, v))

  # Update flashlight and calculate predictions
  x <- flashlight(x, data = data, by = by,
                  linkinv = if (use_linkinv) x$linkinv else function(z) z)

  # Pre-calculate predictions (to save time)
  if (is.null(pred)) {
    pred <- predict(x)
  } else if (length(pred) != nrow(data)) {
    stop("Wrong number of predicted values passed.")
  }

  # Calculate cut information on "data"
  cuts <- auto_cut(data[[v]], breaks = breaks, n_bins = n_bins,
                   cut_type = cut_type, x_name = v, ...)

  # Prepare argument lists for light_profile
  pd_list <- list(x = x, v = v, counts = FALSE,
                  pd_evaluate_at = cuts$bin_means,
                  pd_indices = pd_indices, pd_n_max = pd_n_max,
                  pd_seed = pd_seed)
  ale_list <- c(pd_list, list(type = "ale", pred = pred,
                              ale_two_sided = ale_two_sided))
  resp_list <- list(x = x, v = v, type = "response", stats = stats,
                    breaks = cuts$breaks, v_labels = FALSE,
                    counts = TRUE, counts_weighted = counts_weighted)
  pred_list <- list(x = x, v = v, type = "predicted", breaks = cuts$breaks,
                    v_labels = FALSE, counts = FALSE, pred = pred)
  arg_lists <- list(
    response = resp_list,
    predicted = pred_list,
    pd = pd_list,
    ale = ale_list
  )

  # Call light_profile for all types
  data_sets <- lapply(
    arg_lists,
    function(arg) do.call(light_profile, arg)$data
  )

  # Unify x scale
  if (v_labels) {
    for (nm in names(data_sets)) {
      data_sets[[nm]][[v]] <-
        cuts$bin_labels[match(data_sets[[nm]][[v]], cuts$bin_means)]
    }
  }

  # Collect results
  out <- c(data_sets, list(by = by, v = v, stats = stats))
  add_classes(out, c("light_effects", "light"))
}

#' @describeIn light_effects Effect profiles for a multiflashlight object.
#' @export
light_effects.multiflashlight <- function(x, v, data = NULL, breaks = NULL,
                                          n_bins = 11,
                                          cut_type = c("equal", "quantile"),
                                          ...) {
  cut_type <- match.arg(cut_type)
  if ("pred" %in% names(list(...))) {
    stop("'pred' not implemented for multiflashlight")
  }

  # align breaks for numeric v
  if (is.null(breaks)) {
    breaks <- common_breaks(x = x, v = v, data = data, n_bins = n_bins,
                            cut_type = cut_type)
  }
  all_effects <- lapply(x, light_effects, v = v, data = data, breaks = breaks,
                        n_bins = n_bins, cut_type = cut_type, ...)
  light_combine(all_effects, new_class = "light_effects_multi")
}
