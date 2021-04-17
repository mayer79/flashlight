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
#' @param rotate_x Should x axis labels be rotated by 45 degrees? Default is FALSE.
#' @param ... Further arguments passed to \code{geom_label}.
#' @return An object of class \code{ggplot2}.
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ . + Petal.Length:Species, data = iris)
#' fl <- flashlight(model = fit, label = "lm", data = iris, y = "Sepal.Length")
#' plot(light_breakdown(fl, new_obs = iris[1, ]))
#' @seealso \code{\link{light_importance}}.
plot.light_breakdown <- function(x, facet_scales = "free",
                                 facet_ncol = 1, rotate_x = FALSE, ...) {

  after_name <- getOption("flashlight.after_name")
  before_name <- getOption("flashlight.before_name")
  description_name <- getOption("flashlight.description_name")
  label_name <- getOption("flashlight.label_name")
  step_name <- getOption("flashlight.step_name")

  data <- x$data
  stopifnot(!(c("fill_", "xmin_", "xmax_", "y_") %in% colnames(data)))
  data[["fill_"]] <- (data[[after_name]] - data[[before_name]]) > 0
  data[["xmin_"]] <- data[[step_name]] - 0.5
  data[["xmax_"]] <- data[[step_name]] + 0.5
  data[["y_"]] <- pmin(data[[before_name]], data[[after_name]])

  p <- ggplot(data, aes_string(x = step_name, y = "y_",
                               ymin = before_name, ymax = after_name,
                               xmin = "xmin_", xmax = "xmax_")) +
    geom_rect(aes_string(fill = "fill_"), color = "black",
              show.legend = FALSE) +
    labs(x = element_blank(), y = "prediction") +
    geom_label(aes_string(label = description_name), hjust = -0.05, ...) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.67))) +
    scale_x_reverse() +
    coord_flip() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())

  if (is.light_breakdown_multi(x)) {
    p <- p +
      facet_wrap(reformulate(label_name), scales = facet_scales,
                 ncol = facet_ncol)
  }
  if (rotate_x) {
    p <- p +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  }
  p
}

