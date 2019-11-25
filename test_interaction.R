library(iml)
library(ggplot2)
library(gbm)

# SITUATION: EMPIRICAL WEIGHTING IS IMPORTANT
n <- 1000
set.seed(326)
data <- data.frame(
  v = runif(n),
  x = runif(-1, 1, n),
  z = sample(0:1, n, replace = TRUE, prob = c(0.04, 0.96)))
data$y <- with(data, sin(v) + x * z + rnorm(n))

fit <- gbm(y ~ ., data = data, n.trees = 100, n.cores = 8,
           interaction.depth = 5, shrinkage = 0.05)
pf <-  function(model, newdata) {
 predict(model, newdata, n.trees = model$n.trees)
}
sDat <- data#[1:50, ]
interact.gbm(fit, data = sDat, i.var = c("x", "v")) # 0.258
interact.gbm(fit, data = sDat, i.var = c("x", "z")) # 0.51


fl <- flashlight(model = fit, data = sDat, y = "y", label ="lm",
                 predict_function = pf) # 0.478
light_interaction(fl, type = "pairwise", normalize = TRUE)$data
# label variable  value error
# <chr> <chr>     <dbl> <lgl>
# 1 lm    v:x      0.122  NA
# 2 lm    v:z      0.0635 NA
# 3 lm    x:z      0.478  NA

X <- sDat[, setdiff(colnames(data), "y")]
Y <- sDat[, "y"]
predictor = Predictor$new(fit, data = X, y = Y, predict.fun = pf)
Interaction$new(predictor, feature = "x")
# .feature .interaction
# 1      v:x    0.2980999
# 2      z:x    0.5391665

FeatureImp$new(predictor, loss = mse)



