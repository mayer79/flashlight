#' Add SHAP values to (multi-)flashlight
#'
#' The function calls \code{light_breakdown} for \code{n_shap} observations and adds the resulting (approximate) SHAP decompositions as element "shap" to the (multi)-flashlight for further analyses. We offer two approximations to SHAP: For \code{visit_strategy = "importance"}, the breakdown algorithm in [1] is used with importance based visit order. Use \code{visit_strategy = "permutation"} to run breakdown for multiple random permutations, averaging the results. This approximation will be closer to exact SHAP values, but very slow. Most available arguments can be chosen to reduce computation time.
#'
#' @importFrom dplyr bind_rows
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @param x An object of class \code{flashlight} or \code{multiflashlight}.
#' @param v Vector of variables to assess contribution for. Defaults to all except those specified by "y", "w" and "by".
#' @param visit_strategy In what sequence should variables be visited? By "importance" (fast), by \code{n_perm} "permutation" (slow) or as "v" (not recommended).
#' @param n_shap Number of SHAP decompositions to calculate.
#' @param n_max Maximum number of rows in \code{data} to consider in the reference data. Set to lower value if \code{data} is large.
#' @param n_perm Number of permutations of random visit sequences. Only used if \code{visit_strategy = "permutation"}.
#' @param seed An integer random seed.
#' @param use_linkinv Should retransformation function be applied? We suggest to keep the default (\code{FALSE}) as the values can be retransformed later.
#' @param verbose Should progress bar be shown? Default is \code{TRUE}.
#' @param variable_name Column name in \code{data} of element "shap" containing the variable names. Defaults to "variable".
#' @param ... Further arguments passed from or to other methods.
#' @return An object of class \code{flashlight} or \code{multiflashlight} with additional element "shap" of class "shap" (and "list").
#' @export
#' @references [1] A. Gosiewska and P. Biecek (2019). IBREAKDOWN: Uncertainty of model explanations for non-additive predictive models. ArXiv <arxiv.org/abs/1903.11420>.
#' @examples
#' \dontrun{
#' fit <- lm(Sepal.Length ~ . + Petal.Length:Species, data = iris)
#' x <- flashlight(model = fit, label = "lm", data = iris, y = "Sepal.Length")
#' x <- add_shap(x)
#' is.shap(x$shap)
#' }
add_shap <- function(x, ...) {
  UseMethod("add_shap")
}

#' @describeIn add_shap Default method not implemented yet.
#' @export
add_shap.default <- function(x, ...) {
  stop("add_shap method is only available for objects of class flashlight or multiflashlight.")
}

#' @describeIn add_shap Variable attribution to single observation for a flashlight.
#' @export
add_shap.flashlight <- function(x, v = NULL,
                                visit_strategy = c("importance", "permutation", "v"),
                                n_shap = 100, n_max = Inf, n_perm = 20,
                                seed = NULL, use_linkinv = FALSE, verbose = TRUE,
                                variable_name = "variable", ...) {
  visit_strategy <- match.arg(visit_strategy)
  data <- x$data
  stopifnot((n <- nrow(data)) >= 1L)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Subsample data to n_max rows
  if (n_max < n) {
    data <- data[sample(n, n_max), , drop = FALSE]
    n <- n_max
  }

  # Which rows to decompose?
  if (n_shap < n) {
    new_obs <- data[sample(n, n_shap), , drop = FALSE]
  } else {
    new_obs <- data
    n_shap <- n
  }

  # Update temporary flashlight
  xx <- flashlight(x, data = data)

  # Determine v
  if (is.null(v)) {
    v <- setdiff(colnames(data), c(x$by, x$w, x$y))
  }
  key_vars <- c(variable_name, "baseline_", "before_", "after_", "shap_")
  stopifnot(length(v) >= 1L,
            !(c("baseline", "prediction") %in% v),
            !anyDuplicated(c(x$by, x$w, v, key_vars)))

  core_func <- function(i) {
    shp <- light_breakdown(xx, new_obs = new_obs[i, ], v = v,
                           visit_strategy = visit_strategy,
                           n_max = Inf, n_perm = n_perm, use_linkinv = use_linkinv,
                           after_name = "after_", before_name = "before_",
                           label_name = "label_", variable_name = variable_name,
                           description = FALSE)$data
    # Move baseline to column
    bs <- shp[["before_"]][1]
    shp <- shp[shp[[variable_name]] %in% v, ]
    shp[["shap_"]] <- shp[["after_"]] - shp[["before_"]]
    shp[["baseline_"]] <- bs

    # Full
    expand_grid(shp[, key_vars], new_obs[i, ])
  }

  # Call light_breakdown for each row in new_obs
  if (verbose) {
    cat("\nCrunching SHAP values for flashlight '", x$label, "'\n", sep = "")
    pb <- txtProgressBar(1, n_shap, style = 3)
  }
  out <- vector(mode = "list", length = n_shap)
  for (i in seq_len(n_shap)) {
    out[[i]] <- core_func(i)
    if (verbose) {
      setTxtProgressBar(pb, i)
    }
  }

  # Organize output
  shap <- c(x[c("by", "w", "linkinv")],
            list(data = bind_rows(out), variable_name = variable_name,
                 v = v, use_linkinv = use_linkinv))
  class(shap) <- "shap"
  flashlight(x, shap = shap)
}

#' @describeIn add_shap Add SHAP to multiflashlight.
#' @export
add_shap.multiflashlight <- function(x, ...) {
  multiflashlight(lapply(x, add_shap, ...))
}
