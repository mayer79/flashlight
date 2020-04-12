#' Variable Importance
#'
#' Two algorithms to calculate variable importance are available: (a) Permutation importance and (b) SHAP importance. Algorithm (a) measures importance of variable v as the drop in performance by permuting the values of v, see Fisher et al. 2018 (reference below). Algorithm (b) measures variable importance by averaging absolute SHAP values.
#'
#' For algorithm (a), the minimum required elements in the (multi-) flashlight are "y", "predict_function", "model", "data" and "metrics". For algorithm (b), the only required element is "shap". Call \code{add_shap} once to add such object.
#' Note: The values of the permutation algorithm (a) are on the scale of the selected metric. For shap algorithm (b), the values are on the scale of absolute values of the predictions.
#'
#' @importFrom dplyr left_join bind_rows group_by_at ungroup summarize_at
#' @importFrom stats ave setNames sd qnorm
#' @param x An object of class \code{flashlight} or \code{multiflashlight}.
#' @param data An optional \code{data.frame}. Not used for \code{type = "shap"}.
#' @param by An optional vector of column names used to additionally group the results.
#' @param type Type of importance: "permutation" (default) or "shap". "shap" is only available if a "shap" object is contained in \code{x}.
#' @param v Vector of variables to assess importance for. Defaults to all variables in \code{data} except "by" and "y".
#' @param n_max Maximum number of rows to consider. Not used for \code{type = "shap"}.
#' @param seed An integer random seed used to select and shuffle rows. Not used for \code{type = "shap"}.
#' @param m_repetitions Number of permutations. Defaults to 1. A value above 1 provides more stable estimates of variable importance and allows the calculation of standard errors measuring the uncertainty from permuting. Not used for \code{type = "shap"}.
#' @param metric An optional named list of length one with a metric as element. Defaults to the first metric in the flashlight. The metric needs to be a function with at least four arguments: actual, predicted, case weights w and \code{...}. Irrelevant for \code{type = "shap"}.
#' @param lower_is_better Logical flag indicating if lower values in the metric are better or not. If set to FALSE, the increase in metric is multiplied by -1. Not used for \code{type = "shap"}.
#' @param use_linkinv Should retransformation function be applied? Default is FALSE. Not uses for \code{type = "shap"}.
#' @param metric_name Name of the resulting column containing the name of the metric. Defaults to "metric". Irrelevant for \code{type = "shap"}.
#' @param value_name Column name in resulting \code{data} containing the variable importance. Defaults to "value".
#' @param error_name Column name in resulting \code{data} containing the standard error of permutation importance. Defaults to "error".
#' @param label_name Column name in resulting \code{data} containing the label of the flashlight. Defaults to "label".
#' @param variable_name Column name in resulting \code{data} containing the variable names. Defaults to "variable".
#' @param ... Further arguments passed to \code{light_performance}. Not used for \code{type = "shap"}.
#' @return An object of class \code{light_importance}, \code{light} (and a list) with the following elements.
#' \itemize{
#'   \item \code{data} A tibble with results. Can be used to build fully customized visualizations.
#'   \item \code{by} Same as input \code{by}.
#'   \item \code{type} Same as input \code{type}. For information only.
#'   \item \code{metric_name} Column name representing the name of the metric. For information only.
#'   \item \code{value_name} Same as input \code{value_name}.
#'   \item \code{error_name} Same as input \code{error_name}.
#'   \item \code{label_name} Same as input \code{label_name}.
#'   \item \code{variable_name} Same as input \code{variable_name}.
#' }
#' @export
#' @references Fisher A., Rudin C., Dominici F. (2018). All Models are Wrong but many are Useful: Variable Importance for Black-Box, Proprietary, or Misspecified Prediction Models, using Model Class Reliance. ArXiv <arxiv.org/abs/1801.01489>.
#' @examples
#' fit_part <- lm(Sepal.Length ~ Petal.Length, data = iris)
#' fit_full <- lm(Sepal.Length ~ ., data = iris)
#' mod_full <- flashlight(model = fit_full, label = "full", data = iris, y = "Sepal.Length")
#' mod_part <- flashlight(model = fit_part, label = "part", data = iris, y = "Sepal.Length")
#' mods <- multiflashlight(list(mod_full, mod_part))
#' light_importance(mods)
#' @seealso \code{\link{most_important}}, \code{\link{plot.light_importance}}.
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
                                        v = NULL, n_max = Inf,
                                        seed = NULL, m_repetitions = 1,
                                        metric = x$metrics[1],
                                        lower_is_better = TRUE, use_linkinv = FALSE,
                                        metric_name = "metric",
                                        value_name = "value", error_name = "error",
                                        label_name = "label",
                                        variable_name = "variable", ...) {
  type <- match.arg(type)

  # Select v; if SHAP, extract data
  if (type == "shap") {
    if (!is.shap(x$shap)) {
      stop("No shap values calculated. Run 'add_shap' for the flashlight first.")
    }
    if (is.null(v)) {
      v <- x$shap$v
    }
    data <- x$shap$data[x$shap$data[[x$shap$variable_name]] %in% v, ]
  } else if (is.null(v)) {
    v <- setdiff(colnames(data), c(x$y, by))
  }
  # Checks
  key_vars <- c(label_name, metric_name, by)
  stopifnot(v %in% colnames(data),
            (n <- nrow(data)) >= 1L,
            !anyDuplicated(c(key_vars, value_name, variable_name, error_name)))

  # Subsample to n_max
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (n > n_max) {
    data <- data[sample(n, n_max), , drop = FALSE]
  }

  # Calculations
  if (type == "shap") {
    # Calculate variable importance
    data[[value_name]] <- abs(data[["shap_"]])

    # Rename variable column
    if (x$shap$variable_name != variable_name) {
      data[[variable_name]] <- data[[x$shap$variable_name]]
    }

    # Group results by variable
    imp <- grouped_stats(data, x = value_name, w = x$w,
                         by = c(by, variable_name), counts = FALSE)

    # Add missing columns
    imp[[label_name]] <- x$label
    imp[ c(error_name, metric_name)] <- NA
  } else {
    # Update flashlight with everything except data
    stopifnot(!is.null(metric), length(metric) == 1L)
    x <- flashlight(x, by = by, metrics = metric,
                    linkinv = if (use_linkinv) x$linkinv else function(z) z)

    # Helper function
    perfm <- function(X, vn = "value_original") {
      light_performance(x, data = X, use_linkinv = TRUE, metric_name = metric_name,
                        value_name = vn, label_name = label_name, ...)$data
    }

    # Performance before shuffling
    metric_full <- perfm(data)

    # Performance difference after shuffling
    core_func <- function(z, S) {
      S[[z]] <- if (length(by)) ave(S[[z]], S[, by, drop = FALSE],
                                    FUN = sample) else sample(S[[z]])
      perfm(S, vn = "value_shuffled")
    }
    if (m_repetitions > 1) {
      imp <- replicate(m_repetitions, setNames(lapply(v, core_func, S = data), v),
                       simplify = FALSE)
      imp <- unlist(imp, recursive = FALSE)
      imp <- bind_rows(imp, .id = variable_name)
      imp <- group_by_at(imp, c(key_vars, variable_name))
      se <- function(z, ...) {
        sd(z, ...) / sqrt(sum(!is.na(z)))
      }
      imp <- summarize_at(imp, "value_shuffled",
                          setNames(list(se, mean), c(error_name, "value_shuffled")), na.rm = TRUE)
      imp <- ungroup(imp)
    } else {
      imp <- setNames(lapply(v, core_func, S = data), v)
      imp <- bind_rows(imp, .id = variable_name)
      imp[[error_name]] <- NA
    }
    imp <- left_join(imp, metric_full, by = key_vars)
    imp[[value_name]] <- (imp[["value_shuffled"]] - imp[["value_original"]]) *
      if (lower_is_better) 1 else -1
  }

  # Organize output
  var_order <- c(key_vars, variable_name, value_name, error_name)
  out <- list(data = imp[, var_order], by = by,
              type = type, metric_name = metric_name,
              value_name = value_name, error_name = error_name,
              label_name = label_name, variable_name = variable_name)
  class(out) <- c("light_importance", "light", "list")
  out
}

#' @describeIn light_importance Variable importance for a multiflashlight.
#' @export
light_importance.multiflashlight <- function(x, ...) {
  light_combine(lapply(x, light_importance, ...),
                new_class = "light_importance_multi")
}
