#' Visualize Different Types of Profiles Together
#'
#' Visualizes response-, prediction-, and partial dependence profiles of a (multi-)flashlight with respect to a covariable \code{v}. Different flashlights or a single flashlight with one "by" variable are separated by a facet wrap.
#'
#' @import ggplot2
#' @importFrom stats reformulate
#' @importFrom dplyr semi_join bind_rows
#' @method plot light_effects
#' @param x An object of class \code{light_effects}.
#' @param use A vector with elements in "response", "predicted" and "pd" (partial dependence) with elements to show. Defaults to all.
#' @param zero_counts Logical flag if 0 count levels should be shown on the x axis.
#' @param size_factor Factor used to enlarge default \code{size} in \code{geom_point} and \code{geom_line}.
#' @param facet_scales Scales argument passed to \code{facet_wrap}.
#' @param rotate_x Should x axis labels be rotated by 45 degrees?
#' @param ... Further arguments passed to geoms.
#' @return An object of class \code{ggplot2}.
#' @export
#' @examples
#' \dontrun{
#' fit_full <- lm(Sepal.Length ~ ., data = iris)
#' fit_part <- glm(Sepal.Length ~ Petal.Length, data = iris)
#' mod_full <- flashlight(model = fit_full, label = "full", data = iris,
#'   y = "Sepal.Length", w = "Petal.Length")
#' mod_part <- flashlight(model = fit_part, label = "part", data = iris,
#'   y = "Sepal.Length", w = "Petal.Length")
#' mods <- multiflashlight(list(mod_full, mod_part))
#'
#' plot(light_effects(mod_full, v = "Species"))
#' x <- light_effects(mod_full, v = "Petal.Width")
#' plot(x)
#' plot(x, use = "response")
#' plot(x, use = "predicted")
#' plot(x, use = "pd")
#' plot_counts(plot(x), x, alpha = 0.2)
#' plot_counts(plot(x, use = "response"), x, alpha = 0.2)
#' plot_counts(plot(x, use = "pd"), x, alpha = 0.2)
#'
#' x <- light_effects(mod_full, v = "Petal.Width", stats = "quartiles")
#' plot(x)
#' plot(x, size_factor = 3)
#' plot_counts(plot(x), x, alpha = 0.2)
#' plot_counts(plot(x, use = "response"), x, alpha = 0.2)
#'
#' x <- light_effects(mod_full, v = "Petal.Width", by = "Species")
#' plot(x) +
#'   scale_color_viridis_d(begin = 0.2, end = 0.8)
#' p <- plot(x, zero_counts = TRUE)
#' plot_counts(p, x, zero_counts = TRUE, alpha = 0.2)
#'
#' x <- light_effects(mod_full, v = "Petal.Width", by = "Species", stats = "quartiles")
#' plot(x)
#' plot_counts(plot(x), x, alpha = 0.2)
#'
#' # Smuggle additional line into the pic
#' x_add <- light_profile(mod_full, v = "Petal.Width", type = "response", by = "Species")
#' x_add$data$type <- "observed"
#' x$predicted <- bind_rows(x$predicted, x_add$data)
#' (p <- plot(x))
#' plot_counts(p, x, alpha = 0.2)
#'
#' x <- light_effects(mods, v = "Petal.Width")
#' plot(x, zero_counts = TRUE)
#' plot_counts(plot(x, zero_counts = TRUE), x, alpha = 0.2, zero_counts = TRUE)
#' plot(light_effects(mods, v = "Petal.Width", stats = "quartiles"))
#'
#' # Different data: Use eigher fixed breaks or v_labels = FALSE
#' mod_full <- flashlight(model = fit_full, label = "full",
#'   data = iris[1:75, ], y = "Sepal.Length")
#' mod_part <- flashlight(model = fit_part, label = "part",
#'   data = iris[76:150, ], y = "Sepal.Length")
#' mods <- multiflashlight(list(mod_full, mod_part))
#'
#' plot(light_effects(mods, v = "Petal.Length", breaks = 0:8))
#' eff <- light_effects(mods, v = "Petal.Length")
#' plot_counts(plot(eff), eff, show_labels = FALSE)
#' plot_counts(plot(eff, zero_counts = TRUE), eff, zero_counts = TRUE)
#'
#' # Log-linear OLS vs. Gamma
#' ir <- iris
#' ir$log_sl <- log(ir$Sepal.Length)
#' fit_lm <- lm(log_sl ~ Petal.Length + Petal.Width, data = ir)
#' fit_glm <- glm(Sepal.Length ~ Petal.Length + Petal.Width,
#'   data = ir, family = Gamma(link = log))
#' fl_lm <- flashlight(model = fit_lm, label = "lm", y = "log_sl", linkinv = exp)
#' fl_glm <- flashlight(model = fit_glm, label = "glm", y = "Sepal.Length",
#'   predict_function = function(m, X) predict(m, X, type = "response"))
#' fls <- multiflashlight(list(fl_lm, fl_glm), data = ir)
#'
#' plot(light_effects(fls, v = "Petal.Length"))
#' plot(light_effects(fls, v = "Petal.Length", use_linkinv = FALSE))
#' }
#' @seealso \code{\link{light_effects}}, \code{\link{plot_counts}}.
plot.light_effects <- function(x, use = c("response", "predicted", "pd"),
                               zero_counts = FALSE, size_factor = 1,
                               facet_scales = "free_x", rotate_x = TRUE, ...) {
  # Checks
  stopifnot(length(use) >= 1L)
  nby <- length(x$by)
  multi <- is.light_effects_multi(x)
  if (nby + multi > 1L) {
    stop("Plot method unavailable for multiple 'by' variables or a multiflashlight and a 'by' variable.")
  }

  # Combine data for plotting points and lines
  data <- bind_rows(x[setdiff(use, if (x$stats == "quartiles") "response")])

  # Remove 0 count entries in "data"
  n <- nrow(data)
  if (nby + multi >= 1L && !zero_counts && n) {
    data <- semi_join(data, x$response, by = c(x$label_name, x$by, x$v))
  }

  # Prepare crossbar if required
  crossbar_required <- x$stats == "quartiles" && "response" %in% use
  if (crossbar_required) {
    crossbar <- geom_crossbar(data = x$response, aes_string(ymin = x$q1_name, ymax = x$q3_name),
                              width = 0.3, fill = "darkblue", colour = "black", alpha = 0.1, ...)
  }

  # Put together the plot
  if (n) {
    tp <- x$type_name
    p <- ggplot(data, aes_string(y = x$value_name, x = x$v)) +
      geom_point(aes_string(color = tp, group = tp), size = size_factor, ...) +
      geom_line(aes_string(color = tp, group = tp), size = size_factor / 3, ...)
    if (crossbar_required) {
      p <- p + crossbar
    }
  } else {
    p <- ggplot(x$response, aes_string(y = x$value_name, x = x$v)) +
      crossbar
  }
  if (multi || nby) {
    p <- p + facet_wrap(reformulate(if (multi) x$label_name else x$by[1]), scales = facet_scales)
  }
  p <- p +
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank())
  if (rotate_x) {
    p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  }
  p
}
