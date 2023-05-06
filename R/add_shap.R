#' Add SHAP values to (multi-)flashlight
#'
#' The function calls [light_breakdown()] for `n_shap` observations and adds the
#' resulting (approximate) SHAP decompositions as static element "shap" to the
#' (multi)-flashlight for further analyses.
#'
#' We offer two approximations to SHAP: For `visit_strategy = "importance"`,
#' the breakdown algorithm (see reference) is used with importance based visit order.
#' Use the default `visit_strategy = "permutation"` to run breakdown for
#' multiple random permutations, averaging the results.
#' This approximation will be closer to exact SHAP values, but very slow.
#' Most available arguments can be chosen to reduce computation time.
#'
#' @param x An object of class "flashlight" or "multiflashlight".
#' @param v Vector of variables to assess contribution for.
#'   Defaults to all except those specified by "y", "w" and "by".
#' @param visit_strategy In what sequence should variables be visited? By `n_perm`
#'   "permutation" (slow), by "importance" (fast), or as "v" (not recommended).
#' @param n_shap Number of SHAP decompositions to calculate.
#' @param n_max Maximum number of rows in `data` to consider in the reference data.
#'   Set to lower value if `data` is large.
#' @param n_perm Number of permutations of random visit sequences.
#'   Only used if `visit_strategy = "permutation"`.
#' @param seed An integer random seed.
#' @param use_linkinv Should retransformation function be applied?
#'   We suggest to keep the default (`FALSE`) as the values can be retransformed later.
#' @param verbose Should progress bar be shown? Default is `TRUE`.
#' @param ... Further arguments passed from or to other methods.
#' @returns An object of class "flashlight" or "multiflashlight"
#'   with additional element "shap" of class "shap" (and "list").
#' @export
#' @references
#'   A. Gosiewska and P. Biecek (2019). IBREAKDOWN: Uncertainty of model explanations
#'     for non-additive predictive models. ArXiv <arxiv.org/abs/1903.11420>.
#' @examples
#' \dontrun{
#' fit <- lm(Sepal.Length ~ . + Petal.Length:Species, data = iris)
#' x <- flashlight(model = fit, label = "lm", data = iris, y = "Sepal.Length")
#' x <- add_shap(x)
#' is.shap(x$shap)
#' plot(light_importance(x, type = "shap"))
#' plot(light_scatter(x, type = "shap", v = "Petal.Length"))
#' plot(light_scatter(x, type = "shap", v = "Petal.Length", by = "Species"))
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
                                visit_strategy = c("permutation", "importance", "v"),
                                n_shap = 200, n_max = Inf, n_perm = 12,
                                seed = NULL, use_linkinv = FALSE,
                                verbose = TRUE, ...) {
  visit_strategy <- match.arg(visit_strategy)

  warning_on_names("variable_name", ...)

  variable_name <- getOption("flashlight.variable_name")
  before_name <- getOption("flashlight.before_name")
  after_name <- getOption("flashlight.after_name")

  key_vars <- c(variable_name, "baseline_", before_name, after_name, "shap_")

  # Determine data and n
  data <- x$data
  stopifnot(
    "No data!" = is.data.frame(data) && nrow(data) >= 1L,
    "n_shap should be positive" = n_shap >= 1L,
    "n_max should be positive" = n_max >= 1L,
    "n_shap should not be larger than n_max" = n_max >= n_shap
  )
  n <- nrow(data)
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (n > n_max) {
    data <- data[sample(n, n_max), , drop = FALSE]
    n <- n_max
  }

  # Which rows (new_obs) to decompose?
  if (n > n_shap) {
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
  stopifnot(
    "No 'v' specified." = length(v) >= 1L,
    "Not all 'v' in data." = v %in% colnames(data)
  )
  check_unique(
    c(x$by, x$w, v),
    c(variable_name, before_name, after_name),
    temp_names = c("baseline_", "shap_")
  )

  core_func <- function(i) {
    shp <- light_breakdown(
      xx,
      new_obs = new_obs[i, ],
      v = v,
      visit_strategy = visit_strategy,
      n_max = Inf,
      n_perm = n_perm,
      use_linkinv = use_linkinv,
      description = FALSE
    )$data

    # Move baseline to column
    bs <- shp[[before_name]][1L]
    shp <- shp[shp[[variable_name]] %in% v, ]
    shp[["shap_"]] <- shp[[after_name]] - shp[[before_name]]
    shp[["baseline_"]] <- bs

    # Full
    tidyr::expand_grid(shp[, key_vars], new_obs[i, ])
  }

  # Call light_breakdown for each row in new_obs
  if (verbose) {
    cat("\nCrunching SHAP values for flashlight '", x$label, "'\n", sep = "")
    pb <- utils::txtProgressBar(1, n_shap, style = 3)
  }
  out <- vector(mode = "list", length = n_shap)
  for (i in seq_len(n_shap)) {
    out[[i]] <- core_func(i)
    if (verbose) {
      utils::setTxtProgressBar(pb, i)
    }
  }

  # Organize output
  shap <- c(
    x[c("by", "w", "linkinv")],
    list(data = dplyr::bind_rows(out), v = v, use_linkinv = use_linkinv)
  )
  class(shap) <- "shap"
  flashlight(x, shap = shap)
}

#' @describeIn add_shap Add SHAP to multiflashlight.
#' @export
add_shap.multiflashlight <- function(x, ...) {
  multiflashlight(lapply(x, add_shap, ...))
}
