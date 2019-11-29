library(iml)
library(ggplot2)
library(gbm)

# Generate data
n <- 1000
set.seed(326)
data <- data.frame(
  v = runif(n),
  x = runif(n, -1, 1),
  z = sample(0:1, n, replace = TRUE, prob = c(0.5, 0.5)),
  w = sample(1:3, n, replace = TRUE))
data$y <- with(data, sin(v) + x * z + rnorm(n))

fit <- gbm(y ~ ., data = data, n.trees = 100, n.cores = 8,
           interaction.depth = 5, shrinkage = 0.05)
pf <-  function(model, newdata) {
 predict(model, newdata, n.trees = model$n.trees)
}
sDat <- data[1:50, ]

fl <- flashlight(model = fit, data = sDat, y = "y", label ="lm",
                 predict_function = pf) # 0.478
X <- sDat[, setdiff(colnames(data), "y")]
Y <- sDat[, "y"]
predictor = Predictor$new(fit, data = X, y = Y, predict.fun = pf)

# Pairwise
interact.gbm(fit, data = sDat, i.var = c("x", "v"))
interact.gbm(fit, data = sDat, i.var = c("x", "z"))
light_interaction(fl, pairwise = TRUE)$data
Interaction$new(predictor, feature = "x")

## Overall
light_interaction(fl)$data
Interaction$new(predictor)

# Grouped
light_interaction(fl, by = "z")$data
light_interaction(fl, by = "z", pairwise = TRUE)$data
