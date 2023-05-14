#' Permutation Variable Importance
#'
#' Importance of variable `v` is measured as drop in performance
#' by permuting the values of `v`, see Fisher et al. 2018 (reference below).
#'
#' The minimum required elements in the (multi-)flashlight are "y", "predict_function",
#' "model", "data" and "metrics".
#'
#' @param x An object of class "flashlight" or "multiflashlight".
#' @param data An optional `data.frame`.
#' @param by An optional vector of column names used to additionally group the results.
#' @param type Type of importance: "permutation" (currently the only option).
#' @param v Vector of variable names to assess importance for.
#'   Defaults to all variables in `data` except "by" and "y".
#' @param n_max Maximum number of rows to consider.
#' @param seed An integer random seed used to select and shuffle rows.
#' @param m_repetitions Number of permutations. Defaults to 1.
#'   A value above 1 provides more stable estimates of variable importance and
#'   allows the calculation of standard errors measuring the uncertainty from permuting.
#' @param metric An optional named list of length one with a metric as element.
#'   Defaults to the first metric in the flashlight. The metric needs to be a function
#'   with at least four arguments: actual, predicted, case weights w and `...`.
#' @param lower_is_better Logical flag indicating if lower values in the metric
#'   are better or not. If set to `FALSE`, the increase in metric is multiplied by -1.
#' @param use_linkinv Should retransformation function be applied? Default is `FALSE`.
#' @param ... Further arguments passed to [light_performance()].
#' @returns
#'   An object of class "light_importance" with the following elements:
#'   - `data` A tibble with results. Can be used to build fully customized visualizations.
#'     Column names can be controlled by `options(flashlight.column_name)`.
#'   - `by` Same as input `by`.
#'   - `type` Same as input `type`. For information only.
#' @export
#' @references
#'   Fisher A., Rudin C., Dominici F. (2018). All Models are Wrong but many are Useful:
#'     Variable Importance for Black-Box, Proprietary, or Misspecified Prediction
#'     Models, using Model Class Reliance. Arxiv.
#' @examples
#' fit <- stats::lm(Sepal.Length ~ Petal.Length, data = iris)
#' fl <- flashlight(model = fit, label = "full", data = iris, y = "Sepal.Length")
#' plot(light_importance(fl), fill = "chartreuse4")
#' @seealso [most_important()], [plot.light_importance()]
light_importance <- function(x, ...) {
  UseMethod("light_importance")
}

#' @describeIn light_importance Default method not implemented yet.
#' @export
light_importance.default <- function(x, ...) {
  stop("light_importance method is only available for objects of class flashlight or multiflashlight.")
}

#' @describeIn light_importance Variable importance for a flashlight.
#' @export
light_importance.flashlight <- function(x, data = x$data, by = x$by,
                                        type = c("permutation", "shap"),
                                        v = NULL, n_max = Inf, seed = NULL,
                                        m_repetitions = 1L,
                                        metric = x$metrics[1L],
                                        lower_is_better = TRUE,
                                        use_linkinv = FALSE, ...) {
  type <- match.arg(type)

  if (type == "shap") {
    stop("type = 'shap' is deprecated.")
  }

  if (is.null(v)) {
    v <- setdiff(colnames(data), c(x$y, by))
  }

  # Checks
  key_vars <- c("label_", "metric_", by)
  stopifnot(
    "No data!" = is.data.frame(data) && nrow(data) >= 1L,
    "'by' not in 'data'!" = by %in% colnames(data),
    "Not all 'v' in 'data'" = v %in% colnames(data),
    !any(c("metric_", "value_", "label_", "variable_", "error_") %in% by),
    "Need a metric." = !is.null(metric),
    "Need exactly one metric." = length(metric) == 1L,
    "No 'y' defined in flashlight!" = !is.null(x$y)
  )
  n <- nrow(data)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Subsample to n_max
  if (n > n_max) {
    data <- data[sample(n, n_max), , drop = FALSE]
  }

  # Update flashlight with everything except data
  x <- flashlight(
    x,
    by = by,
    metrics = metric,
    linkinv = if (use_linkinv) x$linkinv else function(z) z
  )

  # Helper function
  perfm <- function(X, vn = "value_orig_") {
    rename_one(
      light_performance(x, data = X, use_linkinv = TRUE, ...)$data, "value_", vn
    )
  }

  # Performance before shuffling
  metric_full <- perfm(data)

  # Performance difference after shuffling
  core_func <- function(z, S) {
    S[[z]] <- if (length(by))
      stats::ave(S[[z]], S[, by, drop = FALSE], FUN = sample) else sample(S[[z]])
    perfm(S, vn = "value_shuffled_")
  }
  if (m_repetitions > 1L) {
    # Helper function that returns standard error and mean
    mean_error <- function(X) {
      x <- stats::na.omit(X$value_shuffled_)
      data.frame(value_shuffled_ = mean(x), error_ = stats::sd(x) / sqrt(length(x)))
    }
    imp <- replicate(
      m_repetitions,
      stats::setNames(lapply(v, core_func, S = data), v),
      simplify = FALSE
    )
    imp <- unlist(imp, recursive = FALSE)
    imp <- dplyr::bind_rows(imp, .id = "variable_")
    imp <- Reframe(imp, FUN = mean_error, .by = c(key_vars, "variable_"))
  } else {
    imp <- stats::setNames(lapply(v, core_func, S = data), v)
    imp <- dplyr::bind_rows(imp, .id = "variable_")
    imp$error_ <- NA
  }
  imp <- dplyr::left_join(imp, metric_full, by = key_vars)
  imp$value_ <- (imp$value_shuffled_ - imp$value_orig_)
  if (!lower_is_better) {
    imp$value_ <- -imp$value_
  }

  # Organize output
  var_order <- c(key_vars, "variable_", "value_", "error_")
  add_classes(
    list(data = imp[, var_order], by = by, type = type),
    c("light_importance", "light")
  )
}

#' @describeIn light_importance Variable importance for a multiflashlight.
#' @export
light_importance.multiflashlight <- function(x, ...) {
  light_combine(lapply(x, light_importance, ...), new_class = "light_importance_multi")
}

#' Most Important Variables.
#'
#' Returns the most important variable names sorted descendingly.
#'
#' @param x An object of class "light_importance".
#' @param top_m Maximum number of important variables to be returned.
#' @returns A character vector of variable names sorted in descending importance.
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' fl <- flashlight(model = fit, label = "ols", data = iris, y = "Sepal.Length")
#' (imp <- light_importance(fl, seed = 4))
#' most_important(imp)
#' most_important(imp, top_m = 2)
#' @seealso [light_importance()]
most_important <- function(x, top_m = Inf) {
  stopifnot(inherits(x, "light_importance"))
  out <- rowsum(x$data$value_, x$data$variable_, na.rm = TRUE)
  rownames(out)[utils::head(order(-out), top_m)]
}
