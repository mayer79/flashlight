#' Visualize Variable Importance
#'
#' Visualization of an object of class "light_importance" via [ggplot2::geom_bar()].
#' If available, standard errors are added by [ggplot2::geom_errorbar()].
#' The object returned is of class "ggplot" and can be further customized.
#'
#' The plot is organized as a bar plot with variable names as x-aesthetic.
#' Up to two additional dimensions (multiflashlight and one "by" variable or single
#' flashlight with two "by" variables) can be visualized by facetting and dodge/fill.
#' Set `swap_dim = FALSE` to revert the role of these two dimensions.
#' One single additional dimension is visualized by a facet wrap,
#' or - if `swap_dim = FALSE` - by dodge/fill.
#'
#' @importFrom rlang .data
#'
#' @inheritParams plot.light_performance
#' @param x An object of class "light_importance".
#' @param top_m Maximum number of important variables to be returned.
#' @param swap_dim If multiflashlight and one "by" variable or single flashlight with
#'   two "by" variables, swap the role of dodge/fill variable and facet variable.
#'   If multiflashlight or one "by" variable, use facets instead of colors.
#' @param error_bars Should error bars be added? Defaults to `TRUE`.
#'   Only available if [light_importance()] was run with multiple permutations
#'   by setting `m_repetitions` > 1.
#' @param ... Further arguments passed to [ggplot2::geom_bar()].
#' @returns An object of class "ggplot".
#' @export
#' @examples
#' fit_full <- stats::lm(Sepal.Length ~ ., data = iris)
#' fit_part <- stats::lm(Sepal.Length ~ Petal.Length, data = iris)
#' mod_full <- flashlight(
#'   model = fit_full, label = "full", data = iris, y = "Sepal.Length"
#' )
#' mod_part <- flashlight(
#'   model = fit_part, label = "part", data = iris, y = "Sepal.Length"
#' )
#' mods <- multiflashlight(list(mod_full, mod_part), by = "Species")
#' plot(light_importance(mod_part, m_repetitions = 4), fill = "darkred")
#' plot(light_importance(mods), swap_dim = TRUE)
#' @seealso [light_importance()]
plot.light_importance <- function(x, top_m = Inf, swap_dim = FALSE,
                                  facet_scales = "fixed",
                                  rotate_x = FALSE, error_bars = TRUE, ...) {
  data <- x$data
  nby <- length(x$by)
  multi <- is.light_importance_multi(x)
  ndim <- nby + multi
  if (error_bars) {
    if (all(is.na(data$error_))) {
      error_bars <- FALSE
    }
  }
  if (ndim > 2L) {
    stop("Plot method not defined for more than two by variables or
         multiflashlight with more than one by variable.")
  }

  # Subset and revert for plotting
  most_imp <- most_important(x, top_m = top_m)
  data <- transform(
    subset(data, variable_ %in% most_imp),
    variable_ = factor(variable_, levels = rev(most_imp)),
    low_ = value_ - error_,
    high_ = value_ + error_
  )

  # Distinguish some plot cases
  p <- ggplot2::ggplot(
    data, ggplot2::aes(x = value_, y = variable_, xmin = low_, xmax = high_)
  )
  if (ndim == 0L) {
    p <- p + ggplot2::geom_bar(stat = "identity", ...)
    if (error_bars) {
      p <- p + ggplot2::geom_errorbar(width = 0, color = "black")
    }
  } else if (ndim == 1L) {
    first_dim <- if (multi) "label_" else x$by[1L]
    if (swap_dim) {
      p <- p + ggplot2::geom_bar(
        ggplot2::aes(fill = .data[[first_dim]]),
        stat = "identity",
        position = "dodge",
        ...
      )
      if (error_bars) {
        p <- p + ggplot2::geom_errorbar(
          ggplot2::aes(group = .data[[first_dim]]),
          width = 0,
          color = "black",
          position = ggplot2::position_dodge(0.9)
        )
      }
    } else {
      p <- p +
        ggplot2::geom_bar(stat = "identity", ...) +
        ggplot2::facet_wrap(first_dim, scales = facet_scales)
      if (error_bars) {
        p <- p + ggplot2::geom_errorbar(width = 0, color = "black")
      }
    }
  } else {
    second_dim <- if (multi) "label_" else x$by[2L]
    wrap_var <- if (!swap_dim) x$by[1L] else second_dim
    dodge_var <- if (!swap_dim) second_dim else x$by[1L]
    p <- p + ggplot2::geom_bar(
      ggplot2::aes(fill = .data[[dodge_var]]),
      stat = "identity",
      position = "dodge",
      ...
    ) +
      ggplot2::facet_wrap(wrap_var, scales = facet_scales)
    if (error_bars) {
      p <- p + ggplot2::geom_errorbar(
        ggplot2::aes(group = .data[[dodge_var]]),
        width = 0,
        color = "black",
        position = ggplot2::position_dodge(0.9)
      )
    }
  }
  if (rotate_x) {
    p <- p + rotate_x()
  }
  # label
  type <- switch(
    x$type,
    permutation = "Drop in performance",
    H = "Friedman's H",
    ice = "ICE based interaction strength"
  )
  p + ggplot2::labs(x = type, y = ggplot2::element_blank())
}

