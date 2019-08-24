#' Visualize Model Importance
#'
#' Minimal visualization of an object of class \code{light_importance} as \code{geom_bar}. The object returned is of class \code{ggplot} and can be further customized.
#'
#' The plot is organized as a bar plot with variable names as x-aesthetic. Up to two additional dimensions (multiflashlight and one "by" variable or single flashlight with two "by" variables) can be visualized by facetting and dodge/fill. Set \code{swap_dim = FALSE} to revert the role of these two dimensions. One single additional dimension is visualized by a facet wrap, or - if \code{swap_dim = FALSE} - by dodge/fill.
#'
#' @import ggplot2
#' @importFrom stats reformulate
#' @method plot light_importance
#' @param x An object of class \code{light_importance}.
#' @param top_m Maximum number of important variables to be returned.
#' @param swap_dim If multiflashlight and one "by" variable or single flashlight with two "by" variables, swap the role of dodge/fill variable and facet variable. If multiflashlight or one "by" variable, use facets instead of colors.
#' @param facet_scales Scales argument passed to \code{facet_wrap}.
#' @param ... Further arguments passed to \code{geom_bar}.
#' @return An object of class \code{ggplot2}.
#' @export
#' @examples
#' fit_full <- lm(Sepal.Length ~ ., data = iris)
#' fit_part <- lm(Sepal.Length ~ Petal.Length, data = iris)
#' mod_full <- flashlight(model = fit_full, label = "full", data = iris, y = "Sepal.Length")
#' mod_part <- flashlight(model = fit_part, label = "part", data = iris, y = "Sepal.Length")
#' mods <- multiflashlight(list(mod_full, mod_part), by = "Species")
#'
#' plot(light_importance(mod_full), fill = "darkred")
#' plot(light_importance(mod_full, variable_name = "v", label_name = "model",
#'   metric_name = "m", value_name = "drop"))
#' plot(light_importance(mod_full), top_m = 2)
#' plot(light_importance(mods))
#' plot(light_importance(mods), swap_dim = TRUE)
#' plot(light_importance(mods, by = NULL), fill = "darkgreen")
#' plot(light_importance(mods, by = NULL), swap_dim = TRUE)
#' @seealso \code{\link{light_importance}}.
plot.light_importance <- function(x, top_m = Inf, swap_dim = FALSE,
                                  facet_scales = "fixed", ...) {
  data <- x$data
  nby <- length(x$by)
  multi <- is.light_importance_multi(x)
  ndim <- nby + multi
  if (ndim > 2L) {
    stop("Plot method not defined for more than two by variables or
         multiflashlight with more than one by variable.")
  }

  # Subset and revert for plotting
  most_important <- most_important(x, top_m = top_m)
  data <- data[data[[x$variable_name]] %in% most_important, , drop = FALSE]
  data[[x$variable_name]] <- factor(data[[x$variable_name]], levels = rev(most_important))

  # Differentiate some plot cases
  p <- ggplot(data, aes_string(y = x$value_name, x = x$variable_name))
  if (ndim == 0L) {
    p <- p + geom_bar(stat = "identity", ...)
  } else if (ndim == 1L) {
    first_dim <- if (multi) x$label_name else x$by[1]
    if (swap_dim) {
      p <- p + geom_bar(aes_string(fill = first_dim),
                        stat = "identity", position = "dodge", ...)
    } else {
      p <- p + geom_bar(stat = "identity", ...) +
        facet_wrap(reformulate(first_dim), scales = facet_scales)
    }
  } else {
    second_dim <- if (multi) x$label_name else x$by[2]
    wrap_var <- if (!swap_dim) x$by[1] else second_dim
    dodge_var <- if (!swap_dim) second_dim else x$by[1]
    p <- p + geom_bar(aes_string(fill = dodge_var),
                      stat = "identity", position = "dodge", ...) +
      facet_wrap(reformulate(wrap_var), scales = facet_scales)
  }
  p + coord_flip()
}

