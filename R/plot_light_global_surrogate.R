#' Plot Global Surrogate Trees
#'
#' Use [rpart.plot::rpart.plot()] to visualize trees fitted by
#' [light_global_surrogate()].
#'
#' @param x An object of class "light_global_surrogate".
#' @param type Plot type, see help of [rpart.plot::rpart.plot()]. Default is 5.
#' @param auto_main Automatic plot titles (only if multiple trees are shown).
#' @param mfrow If multiple trees are shown in the same figure:
#'   what value of `mfrow` to use in [graphics::par()]?
#' @param ... Further arguments passed to [rpart.plot::rpart.plot()].
#' @returns An object of class "ggplot".
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' x <- flashlight(model = fit, label = "lm", data = iris)
#' plot(light_global_surrogate(x))
#' @seealso [light_global_surrogate()]
plot.light_global_surrogate <- function(x, type = 5, auto_main = TRUE,
                                        mfrow = NULL, ...) {
  data <- x$data
  multi <- is.light_global_surrogate_multi(x)
  ndim <- length(x$by) + multi
  if (ndim == 0L) {
    rpart.plot::rpart.plot(data$tree_[[1L]], roundint = FALSE, type = type, ...)
  } else if (ndim == 1L) {
    dim_col <- data[[if (multi) "label_" else x$by[1L]]]
    m <- length(dim_col)
    if (is.null(mfrow)) {
      nr <- floor(sqrt(m))
      mfrow <- c(nr, ceiling(m / nr))
    }
    old_params <- graphics::par(mfrow = mfrow)
    on.exit(graphics::par(old_params))

    for (i in seq_len(m)) {
      rpart.plot::rpart.plot(
        data$tree_[[i]],
        roundint = FALSE,
        type = type,
        main = if (auto_main) dim_col[i],
        ...
      )
    }
  } else {
    stop("Either one 'by' variable or a multiflashlight is supported.")
  }
}

