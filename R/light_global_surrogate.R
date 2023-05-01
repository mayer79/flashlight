#' Global Surrogate Tree
#'
#' Model predictions are modelled by a single decision tree, serving as an easy
#' to interprete surrogate to the original model.
#' As suggested in Molnar (see reference below), the quality of the surrogate
#' tree can be measured by its R-squared. The size of the tree can be modified
#' by passing `...` arguments to [rpart::rpart()].
#'
#' @param x An object of class "flashlight" or "multiflashlight".
#' @param data An optional `data.frame`.
#' @param by An optional vector of column names used to additionally group the results.
#' For each group, a separate tree is grown.
#' @param v Vector of variables used in the surrogate model.
#' Defaults to all variables in `data` except "by", "w" and "y".
#' @param use_linkinv Should retransformation function be applied? Default is `TRUE`.
#' @param n_max Maximum number of data rows to consider to build the tree.
#' @param seed An integer random seed used to select data rows if `n_max` is lower than
#' the number of data rows.
#' @param keep_max_levels Number of levels of categorical and factor variables to keep.
#' Other levels are combined to a level "Other". This prevents [rpart::rpart()] to
#' take too long to split non-numeric variables with many levels.
#' @param ... Arguments passed to [rpart::rpart()], such as `maxdepth`.
#' @return An object of class "light_global_surrogate" with the following elements:
#'
#' - `data` A tibble with results. Can be used to build fully customized visualizations.
#'    Column names can be controlled by `options(flashlight.column_name)`.
#' - `by` Same as input `by`.
#'
#' @export
#' @references Molnar C. (2019). Interpretable Machine Learning.
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' x <- flashlight(model = fit, label = "lm", data = iris)
#' light_global_surrogate(x)
#'
#' @seealso [plot.light_global_surrogate()]
light_global_surrogate <- function(x, ...) {
  UseMethod("light_global_surrogate")
}

#' @describeIn light_global_surrogate Default method not implemented yet.
#' @export
light_global_surrogate.default <- function(x, ...) {
  stop("light_global_surrogate method is only available for objects of class flashlight or multiflashlight.")
}

#' @describeIn light_global_surrogate Surrogate model for a flashlight.
#' @export
light_global_surrogate.flashlight <- function(x, data = x$data, by = x$by,
                                              v = NULL, use_linkinv = TRUE,
                                              n_max = Inf, seed = NULL,
                                              keep_max_levels = 4L, ...) {
  warning_on_names(c("label_name", "tree_name"), ...)

  label_name <- getOption("flashlight.label_name")
  tree_name <- getOption("flashlight.tree_name")

  # Checks
  stopifnot(
    "No data!" = is.data.frame(data) && nrow(data) >= 1L,
    "'by' not in 'data'!" = by %in% colnames(data),
    "Not all 'v' in 'data'" = v %in% colnames(data)
  )
  check_unique(by, c(label_name, tree_name))

  # Set v and remove 'by' from it
  if (is.null(v)) {
    v <- setdiff(colnames(data), c(x$y, by, x$w))
  } else if (!is.null(by)) {
    v <- setdiff(v, by)
  }

  # Subsample if data is very large
  n <- nrow(data)
  if (n > n_max) {
    if (!is.null(seed)) {
      set.seed(seed)
    }
    data <- data[sample(n, n_max), , drop = FALSE]
  }

  x <- flashlight(
    x, data = data, by = by, linkinv = if (use_linkinv) x$linkinv else function(z) z
  )

  # Add response of tree model
  stopifnot(!("pred_" %in% colnames(data)))
  data[["pred_"]] <- stats::predict(x)

  # Lump factors with many levels for tree fit
  for (vv in v) {
    data[[vv]] <- .fct_lump(data[[vv]], keep_max_levels = keep_max_levels)
  }

  # Fit tree within by group
  core_func <- function(X) {
    fit <- rpart::rpart(
      stats::reformulate(v, "pred_"), data = X,
      weights = if (!is.null(x$w)) X[[x$w]],
      model = FALSE,
      y = FALSE,
      xval = 0,
      ...
    )
    r2 <- MetricsWeighted::r_squared(X[["pred_"]], stats::predict(fit, X))
    stats::setNames(data.frame(r2, I(list(fit))), c("r_squared", tree_name))
  }
  res <- if (is.null(by)) tibble::as_tibble(core_func(data)) else
    dplyr::summarize(dplyr::group_by(data, dplyr::across(tidyselect::all_of(by))),
              core_func(dplyr::cur_data()), .groups = "drop")

  # Organize output
  res[[label_name]] <- x$label
  out <- list(data = res[, c(label_name, by, "r_squared", tree_name)], by = by)
  add_classes(out, c("light_global_surrogate", "light"))
}

#' @describeIn light_global_surrogate Surrogate model for a multiflashlight.
#' @export
light_global_surrogate.multiflashlight <- function(x, ...) {
  light_combine(
    lapply(x, light_global_surrogate, ...),
    new_class = "light_global_surrogate_multi"
  )
}

# Helper function to lump too many levels of a factor to category "Other"
.fct_lump <- function(x, keep_max_levels = 4L, other_name = "Other") {
  if (!is.character(x) && !is.factor(x)) {
    return(x)
  }
  if (!is.factor(x)) {
    x <- factor(x)
  }
  m <- nlevels(x)
  if (m > keep_max_levels + 1L) {
    drop_levels <- names(sort(table(x), decreasing = TRUE))[(keep_max_levels + 1L):m]
    levels(x)[match(drop_levels, levels(x))] <- other_name
  }
  x
}
