#' Visualize Variable Importance
#'
#' Minimal visualization of an object of class \code{light_importance} as \code{geom_bar}. If multiple repetitions of the algorithm were done, standard errors are added as \code{geom_errorbar}. The object returned is of class \code{ggplot} and can be further customized.
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
#' @param rotate_x Should x axis labels be rotated by 45 degrees? Default is FALSE.
#' @param error_bars Should error bars be added? Defaults to TRUE. Only available if \code{light_importance} was run with multiple permutations, i.e. by setting \code{m_repetitions} > 1.
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
#' plot(light_importance(mod_full, m_repetitions = 4), fill = "darkred")
#' plot(light_importance(mods), swap_dim = TRUE)
#' plot(light_importance(mods, by = NULL), fill = "darkgreen")
#' @seealso \code{\link{light_importance}}.
plot.light_importance <- function(x, top_m = Inf, swap_dim = FALSE,
                                  facet_scales = "fixed", rotate_x = FALSE,
                                  error_bars = TRUE, ...) {
  data <- x$data
  nby <- length(x$by)
  multi <- is.light_importance_multi(x)
  ndim <- nby + multi
  if (error_bars) {
    if (all(is.na(data[[x$error_name]]))) {
      error_bars <- FALSE
    }
  }
  if (ndim > 2L) {
    stop("Plot method not defined for more than two by variables or
         multiflashlight with more than one by variable.")
  }

  # Subset and revert for plotting
  most_important <- most_important(x, top_m = top_m)
  data <- data[data[[x$variable_name]] %in% most_important, , drop = FALSE]
  data[[x$variable_name]] <- factor(data[[x$variable_name]], levels = rev(most_important))
  data[["low_"]] <- data[[x$value_name]] - data[[x$error_name]]
  data[["high_"]] <- data[[x$value_name]] + data[[x$error_name]]

  # Differentiate some plot cases
  p <- ggplot(data, aes_string(y = x$value_name, x = x$variable_name,
                               ymin = "low_", ymax = "high_"))
  if (ndim == 0L) {
    p <- p + geom_bar(stat = "identity", ...)
    if (error_bars) {
      p <- p + geom_errorbar(width = 0, color = "black")
    }
  } else if (ndim == 1L) {
    first_dim <- if (multi) x$label_name else x$by[1]
    if (swap_dim) {
      p <- p + geom_bar(aes_string(fill = first_dim),
                        stat = "identity", position = "dodge", ...)
      if (error_bars) {
        p <- p + geom_errorbar(aes_string(group = first_dim),
                               width = 0, color = "black", position = position_dodge(0.9))
      }
    } else {
      p <- p + geom_bar(stat = "identity", ...) +
        facet_wrap(reformulate(first_dim), scales = facet_scales)
      if (error_bars) {
        p <- p + geom_errorbar(width = 0, color = "black")
      }
    }
  } else {
    second_dim <- if (multi) x$label_name else x$by[2]
    wrap_var <- if (!swap_dim) x$by[1] else second_dim
    dodge_var <- if (!swap_dim) second_dim else x$by[1]
    p <- p + geom_bar(aes_string(fill = dodge_var),
                      stat = "identity", position = "dodge", ...) +
      facet_wrap(reformulate(wrap_var), scales = facet_scales)
    if (error_bars) {
      p <- p + geom_errorbar(aes_string(group = dodge_var),
                             width = 0, color = "black", position = position_dodge(0.9))
    }
  }
  if (rotate_x) {
    p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  }
  p
  p + coord_flip()
}

