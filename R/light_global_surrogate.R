#' Global Surrogate Model
#'
#' Predictions from the model are modelled by a single decision tree using "rpart". This tree serves as a simplified or "surrogate" version of the original model that can be easily interpreted.
#'
#' The size of the tree can be modified using the \code{...} sent to \code{rpart}.
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
#' @param use_linkinv Should retransformation function be applied? Default is TRUE.
#' @param n_max Maximum number of data rows to consider.
#' @param seed An integer random seed used to select rows.
#' @param keep_max_levels Number of factor levels to retain. Other levels are lumped to "Other".
#' @param label_name Column name in resulting \code{data} containing the label of the flashlight. Defaults to "label".
#' @param tree_name Column name in resulting \code{data} containing the trees. Defaults to "tree".
#' @param ... Arguments passed to \code{rpart}.
#' @return An object of class \code{light_surrogate}, \code{light} (and a list) with the following elements.
#' \itemize{
#'   \item \code{data} A tibble with results. Can be used to build fully customized visualizations.
#'   \item \code{by} Same as input \code{by}.
#'   \item \code{label_name} Same as input \code{label_name}.
#'   \item \code{tree_name} Name of column with tree objects.
#' }
#' @export
#' @references [1] Fisher A., Rudin C., Dominici F. (2018). All Models are Wrong but many are Useful: Variable Importance for Black-Box, Proprietary, or Misspecified Prediction Models, using Model Class Reliance. ArXiv <arxiv.org/abs/1801.01489>.
#' @examples
#' fit1 <- lm(Sepal.Length ~ ., data = iris)
#' fit2 <- lm(Sepal.Length ~ Petal.Length, data = iris)
#' fl1 <- flashlight(model = fit1, label = "full")
#' fl2 <- flashlight(model = fit2, label = "partial")
#' fls <- multiflashlight(list(fl1, fl2), data = iris, y = "Sepal.Length")
#'
#' gs <- light_surrogate(fls)
#' rpart.plot(gs$data$tree[[1]], roundint = FALSE, type = 5)

#' @seealso \code{\link{plot.light_surrogate}}.
light_surrogate <- function(x, ...) {
  UseMethod("light_surrogate")
}

#' @describeIn light_surrogate Default method not implemented yet.
#' @export
light_surrogate.default <- function(x, ...) {
  stop("light_surrogate method is only available for objects of class flashlight or multiflashlight.")
}

#' @describeIn light_surrogate Surrogate model for a flashlight.
#' @export
light_surrogate.flashlight <- function(x, data = x$data, by = x$by,
                                       v = NULL, use_linkinv = TRUE,
                                       n_max = Inf, seed = NULL,
                                       rpart_control = NULL,
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
    data[[vv]] <- .fct_lump(data[[vv]], keep_max = keep_max_levels)
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
  out <- list(data = res, by = by, label_name = label_name, tree_name = tree_name)
  class(out) <- c("light_surrogate", "light", "list")
  out
}

#' @describeIn light_surrogate Surrogate model for a multiflashlight.
#' @export
light_surrogate.multiflashlight <- function(x, ...) {
  light_combine(lapply(x, light_surrogate, ...),
                new_class = "light_surrogate_multi")
}

# Helper function
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
