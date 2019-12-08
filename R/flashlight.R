#' Create or Update a flashlight
#'
#' Creates or updates a \code{flashlight} object. If a flashlight is to be created, all arguments are optional except \code{label}. If a flashlight is to be updated, all arguments are optional up to \code{x} (the flashlight to be updated).
#'
#' @importFrom stats predict
#' @importFrom MetricsWeighted rmse
#' @param x An object of class \code{flashlight}. If not provided, a new flashlight is created based on further input. Otherwise, \code{x} is updated based on further input.
#' @param model A fitted model of any type. Most models require a customized \code{predict_function}.
#' @param data A \code{data.frame} or \code{tibble} used as basis for calculations.
#' @param y Variable name of response.
#' @param predict_function A real valued function with two arguments: A model and a data of the same structure as \code{data}. Only the order of the two arguments matter, not their names.
#' @param linkinv An inverse transformation function applied after \code{predict_function}.
#' @param w A variable name of case weights.
#' @param by A character vector with names of grouping variables.
#' @param metrics A named list of metrics. Here, a metric is a function with exactly four arguments: actual, predicted, w (case weights) and \code{...} like those in package \code{MetricsWeighted}.
#' @param label Name of the flashlight. Required.
#' @param shap An optional shap object. Typically added by calling \code{add_shap}.
#' @param update_and_check_shap When updating the flashlight: Should any shap object be updated and checked as well?
#' @param ... Arguments passed from or to other functions.
#' @return An object of class \code{flashlight} (and \code{list}) containing each input (except \code{x}) as element.
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' (fl <- flashlight(model = fit, data = iris, y = "Sepal.Length", label = "ols"))
#' (fl_updated <- flashlight(fl, linkinv = exp))
#' @seealso \code{\link{multiflashlight}}.
flashlight <- function(x, ...) {
  UseMethod("flashlight")
}

#' @describeIn flashlight Used to create a flashlight object. No \code{x} has to be passed in this case.
#' @export
flashlight.default <- function(x, model = NULL, data = NULL, y = NULL,
                               predict_function = predict, linkinv = function(z) z,
                               w = NULL, by = NULL, metrics = list(rmse = rmse),
                               label = NULL, shap = NULL, ...) {
  x <- c(list(model = model, data = data, y = y,
              predict_function = predict_function, linkinv = linkinv, w = w,
              by = by, metrics = metrics, label = label, shap = shap), list(...))
  class(x) <- c("flashlight", "list")
  light_check(x)
}

#' @describeIn flashlight Used to update an existing flashlight object.
#' @export
flashlight.flashlight <- function(x, update_and_check_shap = FALSE, ...) {
  args <- list(...)
  x[names(args)] <- args

  if (update_and_check_shap) {
    if (!is.shap(x$shap)) {
      stop("No 'shap' object available. Run 'add_shap' first.")
    }
    # Deal with link functions
    same_link <- isTRUE(all.equal(x$shap$linkinv, x$linkinv))
    old_is_id <- isTRUE(all.equal(x$shap$linkinv, function(z) z))
    if (x$shap$use_linkinv && !same_link && !old_is_id) {
      stop("SHAP values have been computed using other 'linkinv'. It is recommended to
            calculate SHAP values without 'linkinv' in order to stay flexibel.")
    } else if (!x$shap$use_linkinv || (!same_link && old_is_id)) {
      data <- x$shap$data
      shap_vars <- c("baseline_", "before_", "after_")
      data[, shap_vars] <- lapply(data[, shap_vars], x$linkinv)
      data[["shap_"]] <- data[["after_"]] - data[["before_"]]
      x$shap$data <- data
      x$shap$use_linkinv <- TRUE
      x$shap$linkinv <- x$linkinv
    }
    # Rebase due to different "by"
    if (!isTRUE(all.equal(x$by, x$shap$by))) {
      cols <- colnames(x$data)
      data <- x$shap$data
      pred <- predict(x, data = data[, cols, drop = FALSE])
      if (is.null(x$by)) {
        data[["baseline_"]] <- weighted_mean(pred, w = x$w, na.rm = TRUE)
      } else {
        data[["baseline_"]] <- pred
        gmean <- grouped_stats(data, x = "baseline_", w = x$w, by = x$by, value_name = "gmean_")
        x$shap$data[["baseline_"]] <- left_join(data, gmean, by = x$by)[["gmean_"]]
      }
      x$shap$by <- x$by
    }
  }
  light_check(x)
}
