#' Combine Objects
#'
#' Combines a list of similar objects each of class \code{light} by row binding \code{data.frame} slots and retaining the other slots from the first list element.
#'
#' @importFrom dplyr bind_rows
#' @param x A list of objects of the same class.
#' @param new_class An optional vector with additional class names to be added to the output.
#' @param ... Further arguments passed from or to other methods.
#' @return If \code{x} is a list, an object like each element but with unioned rows in data slots.
#' @export
#' @examples
#' fit_lm <- lm(Sepal.Length ~ ., data = iris)
#' fit_glm <- glm(Sepal.Length ~ ., family = Gamma(link = "log"), data = iris)
#' mod_lm <- flashlight(model = fit_lm, label = "lm", data = iris, y = "Sepal.Length")
#' mod_glm <- flashlight(model = fit_glm, label = "glm", data = iris, y = "Sepal.Length",
#'                       predict_function = function(object, newdata)
#'                       predict(object, newdata, type = "response"))
#' mods <- multiflashlight(list(mod_lm, mod_glm))
#' perf_lm <- light_performance(mod_lm)
#' perf_glm <- light_performance(mod_glm)
#' manual_comb <- light_combine(list(perf_lm, perf_glm),
#'   new_class = "light_performance_multi")
#' auto_comb <- light_performance(mods)
#' all.equal(manual_comb, auto_comb)
light_combine <- function(x, ...) {
  UseMethod("light_combine")
}

#' @describeIn light_combine Default method not implemented yet.
#' @export
light_combine.default <- function(x, ...) {
  stop("No default method available yet.")
}

#' @describeIn light_combine Since there is nothing to combine, the input is returned except for additional classes.
#' @export
light_combine.light <- function(x, new_class = NULL, ...) {
  add_classes(x, new_class)
}

#' @describeIn light_combine Combine a list of similar light objects.
#' @export
light_combine.list <- function(x, new_class = NULL, ...) {
  stopifnot(all(sapply(x, inherits, "light")),
            all_identical(x, class),
            all_identical(x, length),
            all_identical(x, names))

  out <- x[[1]]
  lab <- getOption("flashlight.label_name")
  data_slots <- names(out)[vapply(out, FUN = is.data.frame, FUN.VALUE = TRUE)]
  other_slots <- setdiff(names(out), data_slots)

  # Compare non-data slots for identity
  if (length(other_slots)) {
    stopifnot(
      vapply(other_slots, FUN = function(s) all_identical(x, `[[`, s),
             FUN.VALUE = TRUE)
    )
  }

  # Row bind data elements
  if (length(data_slots)) {
    for (d in data_slots) {
      out[[d]] <- bind_rows(lapply(x, `[[`, d))
      out[[d]][[lab]] <- factor(out[[d]][[lab]],
                                levels = unique(out[[d]][[lab]]))
    }
  }
  class(out) <- union(new_class, class(x[[1]]))
  out
}

