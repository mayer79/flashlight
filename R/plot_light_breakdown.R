#' Visualize Variable Contribution Breakdown for Single Observation
#'
#' Minimal visualization of an object of class "light_breakdown" as waterfall plot.
#' The object returned is of class "ggplot" and can be further customized.
#'
#' The waterfall plot is to be read from top to bottom.
#' The first line describes the (weighted) average prediction in the query data
#' used to start with. Then, each additional line shows how the prediction changes
#' due to the impact of the corresponding variable.
#' The last line finally shows the original prediction of the selected observation.
#' Multiple flashlights are shown in different facets.
#' Positive and negative impacts are visualized with different colors.
#'
#' @importFrom rlang .data
#'
#' @inheritParams plot.light_performance
#' @param x An object of class "light_breakdown".
#' @param facet_ncol `ncol` argument passed to [ggplot2::facet_wrap()].
#' @param ... Further arguments passed to [ggplot2::geom_label()].
#' @returns An object of class "ggplot".
#' @export
#' @examples
#' fit <- stats::lm(Sepal.Length ~ ., data = iris)
#' fl <- flashlight(model = fit, label = "lm", data = iris, y = "Sepal.Length")
#' plot(light_breakdown(fl, new_obs = iris[1, ]))
#' @seealso [light_breakdown()]
plot.light_breakdown <- function(x, facet_scales = "free",
                                 facet_ncol = 1, rotate_x = FALSE, ...) {
  stopifnot(!(c("fill_", "xmin_", "xmax_", "y_") %in% colnames(x$data)))
  data <- transform(
    x$data,
    fill_ = (after_ - before_) > 0,
    ymin_ = step_ - 0.5,
    ymax_ = step_ + 0.5,
    x_ = pmin(before_, after_)
  )

  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = x_, y = step_, xmin = before_, xmax = after_, ymin = ymin_, ymax = ymax_
    )
  ) +
    ggplot2::geom_rect(
      ggplot2::aes(fill = fill_), color = "black", show.legend = FALSE
    ) +
    ggplot2::labs(x = "Prediction", y = ggplot2::element_blank()) +
    ggplot2::geom_label(ggplot2::aes(label = description_), hjust = -0.05, ...) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.67))) +
    ggplot2::scale_y_reverse() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )

  if (is.light_breakdown_multi(x)) {
    p <- p + ggplot2::facet_wrap(~label_, scales = facet_scales, ncol = facet_ncol)
  }
  if (rotate_x) {
    p <- p + rotate_x()
  }
  p
}

