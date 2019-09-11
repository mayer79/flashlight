#' Visualize Variable Contribution Breakdown for Single Observation
#'
#' Minimal visualization of an object of class \code{light_breakdown} as waterfall plot. The object returned is of class \code{ggplot} and can be further customized.
#'
#' The waterfall plot is to be read from top to bottom. The first line describes the (weighted) average prediction in the query data used to start with. Then each additional line shows how the prediction changes due to the impact of the corresponding variable. The last line finally shows the original predition on the selected observation.
#'
#' @import ggplot2
#' @method plot light_breakdown
#' @param x An object of class \code{light_breakdown}.
#' @param facet_scales Scales argument passed to \code{facet_wrap}.
#' @param ... Further arguments passed to \code{geom_text}.
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
#' plot(light_breakdown(mods, new_obs = iris[1, ]))
#' @seealso \code{\link{light_importance}}.
plot.light_breakdown <- function(x, facet_scales = "free", ...) {
  data <- x$data
  stopifnot(!(c("step_", "ymin_", "ymax_") %in% colnames(data)))
  data[["step_"]] <- data[[x$step_name]]
  data[["ymin_"]] <- data[[x$before_name]]
  data[["ymax_"]] <- data[[x$after_name]]

  p <- ggplot(data, aes(x = step_, y = pmin(ymin_, ymax_), ymin = ymin_, ymax = ymax_,
                        xmin = step_ - 0.3, xmax = step_ + 0.3, fill = ymax_ - ymin_ > 0)) +
    geom_rect(aes(), color = "black", show.legend = FALSE) +
    labs(x = element_blank(), y = "prediction") +
    geom_text(aes_string(label = x$description_name), hjust = -0.05, ...) +
    scale_y_continuous(expand = expand_scale(mult = c(0.05, 0.3))) +
    scale_x_reverse() +
    coord_flip() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())

  if (is.light_breakdown_multi(x)) {
    p <- p +
      facet_wrap(reformulate(x$label_name), scales = facet_scales, ncol = 1)
  }
  p
}

