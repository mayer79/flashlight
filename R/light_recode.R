#' Recode Factor Columns
#'
#' Recodes factor levels of columns in data slots of an object of class \code{light}.
#'
#' @param x An object of class \code{light}.
#' @param what Column identifier to be recoded, e.g. "type". For backward compatibility, also the option identifier (e.g. "type_name") can be passed.
#' @param levels Current levels/values of \code{type_name} column (in desired order).
#' @param labels New levels of \code{type_name} column in same order as \code{levels}.
#' @param ... Further arguments passed to \code{factor}.
#' @return \code{x} with new factor levels of \code{type_name} column.
#' @export
#' @examples
#' fit_full <- lm(Sepal.Length ~ ., data = iris)
#' fit_part <- lm(Sepal.Length ~ Petal.Length, data = iris)
#' mod_full <- flashlight(model = fit_full, label = "full", data = iris, y = "Sepal.Length")
#' mod_part <- flashlight(model = fit_part, label = "part", data = iris, y = "Sepal.Length")
#' mods <- multiflashlight(list(mod_full, mod_part))
#' eff <- light_effects(mods, v = "Species")
#' eff <- light_recode(eff, what = "type_name",
#'                     levels = c("response", "predicted", "partial dependence", "ale"),
#'                     labels = c("Observed", "Fitted", "PD", "ALE"))
#' plot(eff, use = "all")
#' @seealso \code{\link{plot.light_effects}}.
light_recode <- function(x, ...) {
  UseMethod("light_recode")
}

#' @describeIn light_recode Default method not implemented yet.
#' @export
light_recode.default <- function(x, ...) {
  stop("No default method available yet.")
}

#' @describeIn light_recode Recoding factors in data slots of \code{light} object.
#' @export
light_recode.light <- function(x, what, levels, labels, ...) {
  if (!is.null(wt <- getOption(paste("flashlight", what, sep = ".")))) {
    what <- wt
  }
  data_slots <- names(x)[vapply(x, FUN = is.data.frame, FUN.VALUE = TRUE)]
  for (z in data_slots) {
    if (what %in% colnames(x[[z]])) {
      x[[z]][[what]] <- factor(x[[z]][[what]], levels = levels,
                               labels = labels, ...)
    }
  }
  x
}


