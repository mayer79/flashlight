#' Visualize Variable Contribution Breakdown for Single Observation
#'
#' Minimal visualization of an object of class \code{light_breakdown} as waterfall plot. The object returned is of class \code{ggplot} and can be further customized.
#'
#' The waterfall plot is to be read from top to bottom. The first line describes the (weighted) average prediction in the query data used to start with. Then, each additional line shows how the prediction changes due to the impact of the corresponding variable. The last line finally shows the original prediction of the selected observation. Multiple flashlights are shown in different facets. Positive and negative impacts are visualized with different colors.
#'
#' @import ggplot2
#' @method plot light_breakdown
#' @param x An object of class \code{light_breakdown}.
#' @param facet_scales Scales argument passed to \code{facet_wrap}.
#' @param facet_ncol \code{ncol} argument passed to \code{facet_wrap}.
#' @param ... Further arguments passed to \code{geom_label}.
#' @return An object of class \code{ggplot2}.
#' @export
#' @examples
#' fit_full <- lm(Sepal.Length ~ ., data = iris)
#' fit_part <- lm(Sepal.Length ~ Petal.Length, data = iris)
#' mod_full <- flashlight(model = fit_full, label = "full", data = iris, y = "Sepal.Length")
#' mod_part <- flashlight(model = fit_part, label = "part", data = iris, y = "Sepal.Length")
#' mods <- multiflashlight(list(mod_full, mod_part))
#'
#' plot(x <- light_breakdown(mod_full, new_obs = iris[1, ]))
#' plot(light_breakdown(mods, new_obs = iris[1, ]), size = 2.5)
#' plot(light_breakdown(mods, new_obs = iris[1, ]), facet_ncol = 2)
#' @seealso \code{\link{light_importance}}.
plot.light_breakdown <- function(x, facet_scales = "free", facet_ncol = 1, ...) {
  data <- x$data
  stopifnot(!(c("fill_", "xmin_", "xmax_", "y_") %in% colnames(data)))
  data[["fill_"]] <- (data[[x$after_name]] - data[[x$before_name]]) > 0
  data[["xmin_"]] <- data[[x$step_name]] - 0.5
  data[["xmax_"]] <- data[[x$step_name]] + 0.5
  data[["y_"]] <- pmin(data[[x$before_name]], data[[x$after_name]])

  p <- ggplot(data, aes_string(x = x$step_name, y = "y_",
                               ymin = x$before_name, ymax = x$after_name,
                               xmin = "xmin_", xmax = "xmax_")) +
    geom_rect(aes_string(fill = "fill_"), color = "black", show.legend = FALSE) +
    labs(x = element_blank(), y = "prediction") +
    geom_label(aes_string(label = x$description_name), hjust = -0.05, ...) +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.4))) +
    scale_x_reverse() +
    coord_flip() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())

  if (is.light_breakdown_multi(x)) {
    p <- p +
      facet_wrap(reformulate(x$label_name), scales = facet_scales, ncol = facet_ncol)
  }
  p
}

