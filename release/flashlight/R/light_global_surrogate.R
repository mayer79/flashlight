#' Global Surrogate Tree
#'
#' Model predictions are modelled by a single decision tree, serving as an easy to interprete surrogate to the original model. As suggested in Molnar [1], the quality of the surrogate tree can be measured by its R-squared.
#'
#' The size of the tree can be modified by passing \code{...} arguments to \code{rpart}.
#'
#' @importFrom rpart rpart
#' @importFrom stats setNames
#' @importFrom dplyr as_tibble group_by_at do ungroup
#' @importFrom rlang .data
#' @importFrom MetricsWeighted r_squared
#' @param x An object of class \code{flashlight} or \code{multiflashlight}.
#' @param data An optional \code{data.frame}.
#' @param by An optional vector of column names used to additionally group the results. For each group, a separate tree is grown.
#' @param v Vector of variables used in the surrogate model. Defaults to all variables in \code{data} except "by", "w" and "y".
#' @param use_linkinv Should retransformation function be applied? Default is \code{TRUE}.
#' @param n_max Maximum number of data rows to consider to build the tree.
#' @param seed An integer random seed used to select data rows if \code{n_max} is lower than the number of data rows.
#' @param keep_max_levels Number of levels of categorical and factor variables to keep. Other levels are combined to a level "Other". This prevents \code{rpart} to take too long to split non-numeric variables with many levels.
#' @param label_name Column name in resulting \code{data} containing the label of the flashlight. Defaults to "label".
#' @param tree_name Column name in resulting \code{data} containing the trees. Defaults to "tree".
#' @param ... Arguments passed to \code{rpart}, such as \code{maxdepth}.
#' @return An object of class \code{light_global_surrogate}, \code{light} (and a list) with the following elements.
#' \itemize{
#'   \item \code{data} A tibble with results. Can be used to build fully customized visualizations.
#'   \item \code{by} Same as input \code{by}.
#'   \item \code{label_name} Same as input \code{label_name}.
#'   \item \code{tree_name} Name of column with tree objects.
#' }
#' @export
#' @references [1] Molnar C. (2019). Interpretable Machine Learning.
#' @examples
#' fit1 <- lm(Sepal.Length ~ ., data = iris)
#' fit2 <- lm(Sepal.Length ~ Petal.Length, data = iris)
#' fl1 <- flashlight(model = fit1, label = "full")
#' fl2 <- flashlight(model = fit2, label = "partial")
#' fls <- multiflashlight(list(fl1, fl2), data = iris, y = "Sepal.Length")
#' light_global_surrogate(fls, maxdepth = 4)
#'
#' @seealso \code{\link{plot.light_global_surrogate}}.
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
                                              keep_max_levels = 4,
                                              label_name = "label",
                                              tree_name = "tree", ...) {
  if (is.null(v)) {
    v <- setdiff(colnames(data), c(x$y, by, x$w))
  }
  # Checks
  stopifnot(v %in% colnames(data),
            (n <- nrow(data)) >= 1L,
            !anyDuplicated(c(label_name, tree_name, by)))

  if (n > n_max) {
    if (!is.null(seed)) {
      set.seed(seed)
    }
    data <- data[sample(n, n_max), , drop = FALSE]
  }

  x <- flashlight(x, data = data, by = by,
                  linkinv = if (use_linkinv) x$linkinv else function(z) z)

  # Lump factors
  for (vv in v) {
    data[[vv]] <- .fct_lump(data[[vv]], keep_max_levels = keep_max_levels)
  }

  # Calculations
  stopifnot(!("pred_" %in% colnames(data)))
  data[["pred_"]] <- predict(x)

  # Core function
  core_func <- function(X) {
    fit <- rpart(reformulate(v, "pred_"), data = X,
                 weights = if (!is.null(x$w)) X[[x$w]], ...)
    r2 <- r_squared(X[["pred_"]], predict(fit, X))
    setNames(data.frame(r2, I(list(fit))), c("r_squared", tree_name))
  }

  # Call core function for each "by" group
  res <- if (is.null(by)) as_tibble(core_func(data)) else
    ungroup(do(group_by_at(data, by), core_func(.data)))

  # Prepare output
  res[[label_name]] <- x$label

  # Organize output
  out <- list(data = res[, c(label_name, by, "r_squared", tree_name)],
              by = by, label_name = label_name, tree_name = tree_name)
  class(out) <- c("light_global_surrogate", "light", "list")
  out
}

#' @describeIn light_global_surrogate Surrogate model for a multiflashlight.
#' @export
light_global_surrogate.multiflashlight <- function(x, ...) {
  light_combine(lapply(x, light_global_surrogate, ...),
                new_class = "light_global_surrogate_multi")
}

# Helper function to lump too many levels of a factor to category "Other"
.fct_lump <- function(x, keep_max_levels = 4, other_name = "Other") {
  if (!is.character(x) && !is.factor(x)) {
    return(x)
  }
  if (!is.factor(x)) {
    x <- factor(x)
  }
  m <- nlevels(x)
  if (m > keep_max_levels + 1) {
    drop_levels <- names(sort(table(x), decreasing = TRUE))[(keep_max_levels + 1):m]
    levels(x)[match(drop_levels, levels(x))] <- other_name
  }
  x
}
