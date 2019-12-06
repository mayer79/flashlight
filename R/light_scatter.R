#' Scatter
#'
#' The function extracts relevant information on the effect of a variable from a (multi-)flashlight.
#'
#' @importFrom dplyr left_join
#' @param x An object of class \code{flashlight} or \code{multiflashlight}.
#' @param v The variable to be profiled.
#' @param by An optional vector of column names used to additionally group the results.
#' @param baseline Should baseline be added to SHAP values? If \code{TRUE} (the default), the vertical axis is on the same scale as the model response. Otherwise, it is the effect with respect to the baseline.
#' @param value_name Column name in resulting \code{data} containing the SHAP values. Defaults to "value".
#' @param label_name Column name in resulting \code{data} containing the label of the flashlight. Defaults to "label".
#' @param ... Further arguments passed from or to other methods.
#' @return An object of class \code{light_scatter}, \code{light} (and a list) with the following elements.
#' \itemize{
#'   \item \code{data} A tibble with results. Can be used to build fully customized visualizations.
#'   \item \code{by} Same as input \code{by}.
#'   \item \code{v} The variable evaluated.
#'   \item \code{value_name} Same as input \code{value_name}.
#'   \item \code{label_name} Same as input \code{label_name}.
#' }
#' @export
#' @examples
#' \dontrun{
#' fit <- lm(Sepal.Length ~ . + Petal.Length:Species, data = iris)
#' x <- flashlight(model = fit, label = "lm", data = iris, y = "Sepal.Length")
#' x <- add_shap(x)
#' plot(light_scatter(x, "Petal.Length"))
#' }
#'
#' @seealso \code{\link{plot.light_scatter}}.
light_scatter <- function(x, ...) {
  UseMethod("light_scatter")
}

#' @describeIn light_scatter Default method not implemented yet.
#' @export
light_scatter.default <- function(x, ...) {
  stop("light_scatter method is only available for objects of class flashlight or multiflashlight.")
}

#' @describeIn light_scatter Variable profile for a flashlight.
#' @export
light_scatter.flashlight <- function(x, v, by = x$by, baseline = TRUE,
                                    value_name = "value", label_name = "label", ...) {
  stopifnot(is.shap(x$shap),
            v %in% x$shap$v,
            !anyDuplicated(c(by, value_name, label_name)))

  if (!is.null(x$shap$by) && !(x$shap$by %in% by)) {
    warning("SHAP values have been computed using other 'by' groups. This is not recommended.")
  }
  data <- x$shap$data
  if (x$shap$shap_name != value_name) {
    data[[value_name]] <- data[[x$shap$shap_name]]
  }

  # Pick rows with SHAP values for variable v
  data[[label_name]] <- x$label
  vdata <- data[data[[x$shap$variable_name]] == v, c(label_name, by, v, value_name)]

  # Add optional baseline
  if (baseline) {
    bdata <- data[data[[x$shap$variable_name]] == "baseline", ]
    if (is.null(x$shap$by)) {
      bs <- bdata[[value_name]][1]
    } else {
      stopifnot(!("mean_" %in% colnames(vdata)))
      gmean <- grouped_stats(bdata, x = value_name, by = x$shap$by,
                             counts = FALSE, value_name = "mean_")
      bs <- left_join(vdata, gmean, by = x$shap$by)[["mean_"]]
    }
    vdata[[value_name]] <- vdata[[value_name]] + bs
  }

  # Organize output
  out <- list(data = vdata, by = by,
              v = v, value_name = value_name, label_name = label_name)
  class(out) <- c("light_scatter", "light", "list")
  out
}

#' @describeIn light_scatter light_scatter for a multiflashlight.
#' @export
light_scatter.multiflashlight <- function(x, ...) {
  light_combine(lapply(x, light_scatter, ...),
                new_class = "light_scatter_multi")
}
