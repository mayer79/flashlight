#' Combination of Response, Predicted, Partial Dependence, and ALE profiles.
#'
#' Calculates response- prediction-, partial dependence, and ALE profiles of a
#' (multi-)flashlight with respect to a covariable `v`.
#'
#' Note that ALE profiles are being calibrated by (weighted) average predictions.
#' The resulting level might be quite different from the one of the partial
#' dependence profiles.
#'
#' @inheritParams light_profile
#' @param counts_weighted Should counts be weighted by the case weights?
#'   If `TRUE`, the sum of `w` is returned by group.
#' @param v_labels If `FALSE`, return group centers of `v` instead of labels.
#'   Only relevant if `v` is numeric with many distinct values.
#'   In that case useful for instance when different flashlights use different data sets.
#' @returns
#'   An object of class "light_effects" with the following elements:
#'   - `response`: A tibble containing the response profiles.
#'     Column names can be controlled by `options(flashlight.column_name)`.
#'   - `predicted`: A tibble containing the prediction profiles.
#'   - `pd`: A tibble containing the partial dependence profiles.
#'   - `ale`: A tibble containing the ALE profiles.
#'   - `by`: Same as input `by`.
#'   - `v`: The variable(s) evaluated.
#' @export
#' @examples
#' fit_lin <- stats::lm(Sepal.Length ~ ., data = iris)
#' fl_lin <- flashlight(model = fit_lin, label = "lin", data = iris, y = "Sepal.Length")
#'
#' # PDP, average response, average predicted by Species
#' eff <- light_effects(fl_lin, v = "Petal.Length")
#' plot(eff)
#'
#' # PDP and ALE
#' plot(eff, use = c("pd", "ale"))
#'
#' # Second model with non-linear Petal.Length effect
#' fit_nonlin <- stats::lm(Sepal.Length ~ . + I(Petal.Length^2), data = iris)
#' fl_nonlin <- flashlight(
#'   model = fit_nonlin, label = "nonlin", data = iris, y = "Sepal.Length"
#' )
#' fls <- multiflashlight(list(fl_lin, fl_nonlin))
#'
#' # PDP and ALE
#' plot(light_effects(fls, v = "Petal.Length"), use = c("pd", "ale"))
#' @seealso [light_profile()], [plot.light_effects()]
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
                                     stats = "mean",
                                     breaks = NULL, n_bins = 11L,
                                     cut_type = c("equal", "quantile"),
                                     use_linkinv = TRUE,
                                     counts_weighted = FALSE,
                                     v_labels = TRUE, pred = NULL,
                                     pd_indices = NULL, pd_n_max = 1000L,
                                     pd_seed = NULL,
                                     ale_two_sided = TRUE, ...) {
  cut_type <- match.arg(cut_type)

  if (stats == "quartiles") {
    stop("stats = 'quartiles' is deprecated. The argument 'stats' will be removed in version 1.1.0.")
  }

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

  # Update flashlight and calculate predictions
  x <- flashlight(
    x, data = data, by = by, linkinv = if (use_linkinv) x$linkinv else function(z) z
  )

  # Pre-calculate predictions (to save time)
  if (is.null(pred)) {
    pred <- stats::predict(x)
  } else if (length(pred) != nrow(data)) {
    stop("Wrong number of predicted values passed.")
  }

  # Calculate cut information on "data"
  cuts <- auto_cut(
    data[[v]], breaks = breaks, n_bins = n_bins, cut_type = cut_type, x_name = v, ...
  )

  # Prepare argument lists for light_profile
  pd_list <- list(
    x = x,
    v = v,
    counts = FALSE,
    pd_evaluate_at = cuts$bin_means,
    pd_indices = pd_indices,
    pd_seed = pd_seed
  )
  ale_list <- c(
    pd_list, list(type = "ale", pred = pred, ale_two_sided = ale_two_sided)
  )
  resp_list <- list(
    x = x,
    v = v,
    type = "response",
    breaks = cuts$breaks,
    v_labels = FALSE,
    counts = TRUE,
    counts_weighted = counts_weighted
  )
  pred_list <- list(
    x = x,
    v = v,
    type = "predicted",
    breaks = cuts$breaks,
    v_labels = FALSE,
    counts = FALSE,
    pred = pred
  )
  arg_lists <- list(
    response = resp_list,
    predicted = pred_list,
    pd = pd_list,
    ale = ale_list
  )

  # Call light_profile for all types
  data_sets <- lapply(arg_lists, function(arg) do.call(light_profile, arg)$data)

  # Unify x scale
  if (v_labels) {
    for (nm in names(data_sets)) {
      data_sets[[nm]][[v]] <-
        cuts$bin_labels[match(data_sets[[nm]][[v]], cuts$bin_means)]
    }
  }

  # Collect results
  out <- c(data_sets, list(by = by, v = v))
  add_classes(out, c("light_effects", "light"))
}

#' @describeIn light_effects Effect profiles for a multiflashlight object.
#' @export
light_effects.multiflashlight <- function(x, v, data = NULL, breaks = NULL,
                                          n_bins = 11L,
                                          cut_type = c("equal", "quantile"), ...) {
  cut_type <- match.arg(cut_type)
  if ("pred" %in% names(list(...))) {
    stop("'pred' not implemented for multiflashlight")
  }

  # align breaks for numeric v
  if (is.null(breaks)) {
    breaks <- common_breaks(
      x = x, v = v, data = data, n_bins = n_bins, cut_type = cut_type
    )
  }
  all_effects <- lapply(
    x,
    light_effects,
    v = v,
    data = data,
    breaks = breaks,
    n_bins = n_bins,
    cut_type = cut_type,
    ...
  )
  light_combine(all_effects, new_class = "light_effects_multi")
}

#' Visualize Multiple Types of Profiles Together
#'
#' Visualizes response-, prediction-, partial dependence, and/or ALE profiles
#' of a (multi-)flashlight with respect to a covariable `v`.
#' Different flashlights or a single flashlight with one "by" variable are separated
#' by a facet wrap.
#'
#' @importFrom rlang .data
#'
#' @inheritParams plot.light_performance
#' @param x An object of class "light_effects".
#' @param use A vector of elements to show. Any subset of ("response", "predicted",
#'   "pd", "ale") or "all". Defaults to all except "ale"
#' @param zero_counts Logical flag if 0 count levels should be shown on the x axis.
#' @param size_factor Factor used to enlarge default `size/linewidth` in
#'   [ggplot2::geom_point()] and [ggplot2::geom_line()].
#' @param facet_nrow Number of rows in [ggplot2::facet_wrap()].
#' @param show_points Should points be added to the line (default is `TRUE`).
#' @param ... Further arguments passed to geoms.
#' @returns An object of class "ggplot".
#' @export
#' @seealso [light_effects()], [plot_counts()]
plot.light_effects <- function(x, use = c("response", "predicted", "pd"),
                               zero_counts = TRUE, size_factor = 1,
                               facet_scales = "free_x", facet_nrow = 1L,
                               rotate_x = TRUE, show_points = TRUE, ...) {
  # Checks
  stopifnot(length(use) >= 1L)
  if ("all" %in% use) {
    use <- c("response", "predicted", "pd", "ale")
  }

  nby <- length(x$by)
  multi <- is.light_effects_multi(x)
  if (nby + multi > 1L) {
    stop("Plot method unavailable for multiple 'by' variables or a multiflashlight and a 'by' variable.")
  }

  # Combine data for plotting points and lines
  data <- dplyr::bind_rows(x[use])

  # Remove 0 count entries in "data"
  n <- nrow(data)
  if (!zero_counts && n) {
    data <- dplyr::semi_join(data, x$response, by = c("label_", x$by, x$v))
  }

  # Put together the plot
  if (n) {
    p <- ggplot2::ggplot(data, ggplot2::aes(y = value_, x = .data[[x$v]])) +
      ggplot2::geom_line(
        ggplot2::aes(color = type_, group = type_), linewidth = size_factor/3, ...
      )
    if (show_points) {
      p <- p + ggplot2::geom_point(
        ggplot2::aes(color = type_, group = type_), size = size_factor, ...
      )
    }
  } else {
    p <- ggplot2::ggplot(x$response, ggplot2::aes(y = value_, x = .data[[x$v]]))
  }
  if (multi || nby) {
    p <- p + ggplot2::facet_wrap(
      if (multi) "label_" else x$by[1L], scales = facet_scales, nrow = facet_nrow
    )
  }
  p <- p +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom", legend.title = ggplot2::element_blank())
  if (rotate_x) {
    p <- p + rotate_x()
  }
  p + ggplot2::ylab("Value")
}
