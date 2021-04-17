#' Plot Global Surrogate Trees
#'
#' Using \code{rpart.plot}, trees fitted by \code{light_global_surrogate} are visualized.
#'
#' @importFrom rpart.plot rpart.plot
#' @importFrom graphics par
#' @method plot light_global_surrogate
#' @param x An object of class \code{light_global_surrogate}.
#' @param type Plot type, see help of \code{rpart.plot}. Default is 5.
#' @param auto_main Automatic plot titles (only if multiple trees are shown in the same figure).
#' @param mfrow If multiple trees are shown in the same figure: what value of \code{mfrow} to use in \code{par}?
#' @param ... Further arguments passed to \code{rpart.plot}.
#' @return An object of class \code{ggplot2}.
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' x <- flashlight(model = fit, label = "lm", data = iris)
#' plot(light_global_surrogate(x))
#' @seealso \code{\link{light_global_surrogate}}.
plot.light_global_surrogate <- function(x, type = 5, auto_main = TRUE,
                                        mfrow = NULL, ...) {
  label_name <- getOption("flashlight.label_name")
  tree_name <- getOption("flashlight.tree_name")
  data <- x$data
  multi <- is.light_global_surrogate_multi(x)
  ndim <- length(x$by) + multi
  if (ndim == 0L) {
    rpart.plot(data[[tree_name]][[1]], roundint = FALSE, type = type, ...)
  } else if (ndim == 1) {
    dim_col <- data[[if (multi) label_name else x$by[1]]]
    m <- length(dim_col)
    if (is.null(mfrow)) {
      nr <- floor(sqrt(m))
      mfrow <- c(nr, ceiling(m / nr))
    }
    old_params <- par(mfrow = mfrow)
    on.exit(par(old_params))

    for (i in seq_len(m)) {
      rpart.plot(data$tree[[i]], roundint = FALSE, type = type,
                 main = if (auto_main) dim_col[i], ...)
    }
  } else {
    stop("Either one 'by' variable or a multiflashlight is supported.")
  }
}

