#' Combination of Response, Predicted, Partial Dependence, and ALE Profiles
#'
#' Calculates response- prediction-, partial dependence, and ALE profiles of a (multi-)flashlight with respect to a covariable \code{v}.
#'
#' Note that ALE profiles are being calibrated by (weighted) average predictions. The resulting level might be quite different from the one of the partial dependence profiles.
#'
#' @importFrom dplyr bind_rows
#' @param x An object of class \code{flashlight} or \code{multiflashlight}.
#' @param v The variable to be profiled.
#' @param data An optional \code{data.frame}.
#' @param by An optional vector of column names used to additionally group the results.
#' @param stats Statistic to calculate for the response profile: "mean" or "quartiles".
#' @param breaks Cut breaks for a numeric \code{v}.
#' @param n_bins Maxmium number of unique values to evaluate for numeric \code{v}.
#' @param cut_type For the default "equal", bins of equal width are created for \code{v} by \code{pretty}. Choose "quantile" to create quantile bins (recommended if interested in ALE).
#' @param use_linkinv Should retransformation function be applied? Default is TRUE.
#' @param value_name Column name in resulting data objects containing the profile value. Defaults to "value".
#' @param q1_name Name of the resulting column with first quartile values. Only relevant for \code{stats} "quartiles".
#' @param q3_name Name of the resulting column with third quartile values. Only relevant for \code{stats} "quartiles".
#' @param label_name Column name in resulting \code{data} containing the label of the flashlight. Defaults to "label".
#' @param type_name Name of the column in \code{data} containing \code{type}.
#' @param counts_name Name of the column containing counts.
#' @param counts_weighted Should counts be weighted by the case weights? If TRUE, the sum of \code{w} is returned by group.
#' @param v_labels If FALSE, return group centers of \code{v} instead of labels. Only relevant if \code{v} is numeric with many distinct values. In that case useful if e.g. different flashlights use different data sets.
#' @param pred Optional vector with predictions (after application of inverse link). Can be used to avoid recalculation of predictions over and over if the functions is to be repeatedly called for different \code{v} and predictions are computationally expensive to make.
#' @param pd_indices A vector of row numbers to consider in calculating partial dependence and ALE profiles. Useful to force all flashlights to use the same basis for calculations of partial dependence and ALE.
#' @param pd_n_max Maximum number of ICE profiles to consider for partial depencence and ALE calculation (will be randomly picked from \code{data}).
#' @param pd_seed An integer random seed used to sample ICE profiles for partial dependence and ALE.
#' @param ale_two_sided If \code{TRUE}, \code{v} is continuous and \code{breaks} are passed or being calculated, then two-sided derivatives are calculated for ALE instead of left derivatives. This aligns the results better with the x labels. More specifically: Usually, local effects at value x are calculated using points between x-e and x. Set \code{ale_two_sided = TRUE} to use points between x-e/2 and x+e/2.
#' @param ... Further arguments passed to \code{cut3} resp. \code{formatC} in forming the cut breaks of the \code{v} variable.
#' @return An object of classes \code{light_effects}, \code{light} (and a list) with the following elements.
#' \itemize{
#'   \item \code{response} A tibble containing the response profiles.
#'   \item \code{predicted} A tibble containing the prediction profiles.
#'   \item \code{pd} A tibble containing the partial dependence profiles.
#'   \item \code{by} Same as input \code{by}.
#'   \item \code{v} The variable(s) evaluated.
#'   \item \code{stats} Same as input \code{stats}.
#'   \item \code{value_name} Same as input \code{value_name}.
#'   \item \code{q1_name} Same as input \code{q1_name}.
#'   \item \code{q3_name} Same as input \code{q3_name}.
#'   \item \code{label_name} Same as input \code{label_name}.
#'   \item \code{type_name} Same as input \code{type}.
#'   \item \code{counts_name} Same as input \code{counts_name}.
#' }
#' @export
#' @examples
#' fit_full <- lm(Sepal.Length ~ ., data = iris)
#' mod_full <- flashlight(model = fit_full, label = "full", data = iris, y = "Sepal.Length")
#'
#' light_effects(mod_full, v = "Species")
#' light_effects(mod_full, v = "Species", stats = "quartiles")
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
                                     use_linkinv = TRUE, value_name = "value",
                                     q1_name = "q1", q3_name = "q3", label_name = "label",
                                     type_name = "type", counts_name = "counts",
                                     counts_weighted = FALSE, v_labels = TRUE, pred = NULL,
                                     pd_indices = NULL, pd_n_max = 1000, pd_seed = NULL,
                                     ale_two_sided = TRUE, ...) {
  stats <- match.arg(stats)
  cut_type <- match.arg(cut_type)

  if (is.null(data)) {
    data <- x$data
  }

  # Checks
  stopifnot((n <- nrow(data)) >= 1L,
            !anyDuplicated(c(by, v, "level", counts_name, value_name, label_name,
                             if (stats == "quartiles") c(q1_name, q3_name))),
            v %in% colnames(data))

  # Update flashlight and calculate predictions
  x <- flashlight(x, data = data, by = by,
                  linkinv = if (use_linkinv) x$linkinv else function(z) z)
  if (is.null(pred)) {
    pred <- predict(x)
  }

  # Set levels of v
  cuts <- auto_cut(data[[v]], breaks = breaks, n_bins = n_bins,
                   cut_type = cut_type, x_name = v, ...)

  # Partial dependence
  pd <- light_profile(x, v = v, value_name = value_name,
                      label_name = label_name, type_name = type_name,
                      counts = FALSE, pd_evaluate_at = cuts$bin_means,
                      pd_indices = pd_indices,
                      pd_n_max = pd_n_max, pd_seed = pd_seed)$data

  ale <- light_profile(x, v = v, type = "ale", value_name = value_name,
                       label_name = label_name, type_name = type_name,
                       counts = FALSE, breaks = cuts$breaks,
                       pd_evaluate_at = cuts$bin_means,
                       pd_indices = pd_indices, pd_n_max = pd_n_max,
                       pd_seed = pd_seed, pred = pred,
                       ale_two_sided = ale_two_sided)$data

  # Overwrite v variable in data and update flashlight
  data[[v]] <- cuts$data[[v]]
  x <- flashlight(x, data = data)

  # Response profile
  response <- light_profile(x = x, v = v, type = "response", stats = stats,
                            breaks = cuts$breaks,
                            v_labels = FALSE, value_name = value_name,
                            q1_name = q1_name, q3_name = q3_name,
                            label_name = label_name, type_name = type_name,
                            counts = TRUE, counts_name = counts_name,
                            counts_weighted = counts_weighted)$data

  # Prediction profile based on precalculated predictions "pred"
  predicted <- light_profile(x = x, v = v, type = "predicted",
                             breaks = cuts$breaks,
                             v_labels = FALSE, value_name = value_name,
                             label_name = label_name, type_name = type_name,
                             counts = FALSE, pred = pred)$data

  data_sets <- list(response = response, predicted = predicted, pd = pd, ale = ale)

  # Unify x scale
  if (v_labels) {
    for (nm in names(data_sets)) {
      data_sets[[nm]][[v]] <- cuts$bin_labels[match(data_sets[[nm]][[v]], cuts$bin_means)]
    }
  }

  # Collect results
  out <- c(data_sets,
           list(by = by, v = v, stats = stats, value_name = value_name,
           q1_name = q1_name, q3_name = q3_name, label_name = label_name,
           type_name = type_name, counts_name = counts_name))
  class(out) <- c("light_effects", "light", "list")
  out
}

#' @describeIn light_effects Effect profiles for a multiflashlight object.
#' @export
light_effects.multiflashlight <- function(x, v, data = NULL, breaks = NULL, n_bins = 11,
                                          cut_type = c("equal", "quantile"), ...) {
  cut_type <- match.arg(cut_type)

  if (is.null(breaks)) {
    if (is.null(data)) {
      stopifnot(all(vapply(x, function(z) nrow(z$data) >= 1L, FUN.VALUE = TRUE)),
                all(vapply(x, function(z) v %in% colnames(z$data), FUN.VALUE = TRUE)))
      v_vec <- unlist(lapply(x, function(z) z$data[[v]]))
    } else {
      stopifnot(nrow(data) >= 1L, v %in% colnames(data))
      v_vec <- data[[v]]
    }
    breaks <- auto_cut(v_vec, breaks = breaks, n_bins = n_bins,
                       cut_type = cut_type)$breaks
  }
  all_effects <- lapply(x, light_effects, v = v, data = data, breaks = breaks,
                        n_bins = n_bins, cut_type = cut_type, ...)

  light_combine(all_effects, new_class = "light_effects_multi")
}
