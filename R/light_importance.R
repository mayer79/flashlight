#' Variable Importance
#'
#' Two algorithms to calculate variable importance are available: (a) Permutation importance and (b) SHAP importance. Algorithm (a) measures importance of variable v as the drop in performance by permuting the values of v, see Fisher et al. 2018 (reference below). Algorithm (b) measures variable importance by averaging absolute SHAP values.
#'
#' For algorithm (a), the minimum required elements in the (multi-) flashlight are "y", "predict_function", "model", "data" and "metrics". For algorithm (b), the only required element is "shap". Call \code{add_shap} once to add such object.
#' Note: The values of the permutation algorithm (a) are on the scale of the selected metric. For shap algorithm (b), the values are on the scale of absolute values of the predictions.
#'
#' @importFrom dplyr left_join bind_rows group_by summarize across cur_data
#' @importFrom tidyselect all_of
#' @importFrom stats ave setNames sd qnorm
#' @param x An object of class \code{flashlight} or \code{multiflashlight}.
#' @param data An optional \code{data.frame}. Not used for \code{type = "shap"}.
#' @param by An optional vector of column names used to additionally group the results.
#' @param type Type of importance: "permutation" (default) or "shap". "shap" is only available if a "shap" object is contained in \code{x}.
#' @param v Vector of variable names to assess importance for. Defaults to all variables in \code{data} except "by" and "y".
#' @param n_max Maximum number of rows to consider. Not used for \code{type = "shap"}.
#' @param seed An integer random seed used to select and shuffle rows. Not used for \code{type = "shap"}.
#' @param m_repetitions Number of permutations. Defaults to 1. A value above 1 provides more stable estimates of variable importance and allows the calculation of standard errors measuring the uncertainty from permuting. Not used for \code{type = "shap"}.
#' @param metric An optional named list of length one with a metric as element. Defaults to the first metric in the flashlight. The metric needs to be a function with at least four arguments: actual, predicted, case weights w and \code{...}. Irrelevant for \code{type = "shap"}.
#' @param lower_is_better Logical flag indicating if lower values in the metric are better or not. If set to FALSE, the increase in metric is multiplied by -1. Not used for \code{type = "shap"}.
#' @param use_linkinv Should retransformation function be applied? Default is FALSE. Not uses for \code{type = "shap"}.
#' @param ... Further arguments passed to \code{light_performance}. Not used for \code{type = "shap"}.
#' @return An object of class \code{light_importance} with the following elements.
#' \itemize{
#'   \item \code{data} A tibble with results. Can be used to build fully customized visualizations. Column names can be controlled by \code{options(flashlight.column_name)}.
#'   \item \code{by} Same as input \code{by}.
#'   \item \code{type} Same as input \code{type}. For information only.
#' }
#' @export
#' @references Fisher A., Rudin C., Dominici F. (2018). All Models are Wrong but many are Useful: Variable Importance for Black-Box, Proprietary, or Misspecified Prediction Models, using Model Class Reliance. Arxiv.
#' @examples
#' fit <- lm(Sepal.Length ~ Petal.Length, data = iris)
#' fl <- flashlight(model = fit, label = "full", data = iris, y = "Sepal.Length")
#' light_importance(fl)
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
                                        v = NULL, n_max = Inf, seed = NULL,
                                        m_repetitions = 1,
                                        metric = x$metrics[1],
                                        lower_is_better = TRUE,
                                        use_linkinv = FALSE, ...) {
  type <- match.arg(type)

  warning_on_names(c("metric_name", "value_name", "label_name",
                     "variable_name", "error_name"), ...)

  metric_name <- getOption("flashlight.metric_name")
  value_name <- getOption("flashlight.value_name")
  label_name <- getOption("flashlight.label_name")
  variable_name <- getOption("flashlight.variable_name")
  error_name <- getOption("flashlight.error_name")

  # Select v; if SHAP, extract data
  if (type == "shap") {
    if (!is.shap(x$shap)) {
      stop("No shap values calculated. Run 'add_shap' for the flashlight first.")
    }
    if (is.null(v)) {
      v <- x$shap$v
    }
    data <- x$shap$data[x$shap$data[[variable_name]] %in% v, ]
  } else if (is.null(v)) {
    v <- setdiff(colnames(data), c(x$y, by))
  }

  # Checks compatible with both shap and permutation importance
  key_vars <- c(label_name, metric_name, by)
  stopifnot(
    "No data!" = is.data.frame(data) && nrow(data) >= 1L,
    "'by' not in 'data'!" = by %in% colnames(data),
    "Not all 'v' in 'data'" = v %in% colnames(data)
  )
  check_unique(by, c(label_name, metric_name, value_name,
                     variable_name, error_name))
  n <- nrow(data)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Subsample to n_max
  if (n > n_max) {
    data <- data[sample(n, n_max), , drop = FALSE]
  }

  # Calculations
  if (type == "shap") {
    # Calculate variable importance
    data[[value_name]] <- abs(data[["shap_"]])

    # Group results by variable
    imp <- grouped_stats(data, x = value_name, w = x$w,
                         by = c(by, variable_name), counts = FALSE)

    # Add missing columns
    imp[[label_name]] <- x$label
    imp[ c(error_name, metric_name)] <- NA
  } else {
    stopifnot(
      "Need a metric." = !is.null(metric),
      "Need exactly one metric." = length(metric) == 1L,
      "No 'y' defined in flashlight!" = !is.null(x$y)
    )

    # Update flashlight with everything except data
    x <- flashlight(x, by = by, metrics = metric,
                    linkinv = if (use_linkinv) x$linkinv else function(z) z)

    # Helper function
    perfm <- function(X, vn = "value_original") {
      rename_one(light_performance(x, data = X, use_linkinv = TRUE, ...)$data,
                 value_name, vn)
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
      # Helper function that returns standard error and mean
      mean_error <- function(X) {
        x <- X[["value_shuffled"]]
        x <- x[!is.na(x)]
        setNames(data.frame(sd(x) / sqrt(length(x)), mean(x)),
                 c(error_name, "value_shuffled"))
      }
      imp <- replicate(m_repetitions,
                       setNames(lapply(v, core_func, S = data), v),
                       simplify = FALSE)
      imp <- unlist(imp, recursive = FALSE)
      imp <- bind_rows(imp, .id = variable_name)
      imp <- group_by(imp, across(all_of(c(key_vars, variable_name))))
      imp <- summarize(imp, mean_error(cur_data()), .groups = "drop")
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
  add_classes(list(data = imp[, var_order], by = by, type = type),
              c("light_importance", "light"))
}

#' @describeIn light_importance Variable importance for a multiflashlight.
#' @export
light_importance.multiflashlight <- function(x, ...) {
  light_combine(lapply(x, light_importance, ...),
                new_class = "light_importance_multi")
}
