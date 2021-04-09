# Helper functions

# Add vector of classes upfront existing ones
add_classes <- function(x, classes) {
  class(x) <- union(classes, class(x))
  x
}
