library(iml)
library(ggplot2)
library(gbm)

# SITUATION: EMPIRICAL WEIGHTING IS IMPORTANT
n <- 1000
set.seed(326)
data <- data.frame(
  v = runif(n),
  x = runif(n, -1, 1),
  z = sample(0:1, n, replace = TRUE, prob = c(0.5, 0.5)))
data$y <- with(data, sin(v) + x * z + rnorm(n))

fit <- gbm(y ~ ., data = data, n.trees = 100, n.cores = 8,
           interaction.depth = 5, shrinkage = 0.05)
pf <-  function(model, newdata) {
 predict(model, newdata, n.trees = model$n.trees)
}
sDat <- data[1:50, ]
interact.gbm(fit, data = sDat, i.var = c("x", "v")) # 0.1652052
interact.gbm(fit, data = sDat, i.var = c("x", "z")) # 0.02125058


fl <- flashlight(model = fit, data = sDat, y = "y", label ="lm",
                 predict_function = pf) # 0.478
light_interaction(fl, pairwise = TRUE, normalize = TRUE, take_sqrt = TRUE)$data
# 1 lm    v:x      0.292  NA
# 2 lm    v:z      0.0139 NA
# 3 lm    x:z      0.0400 NA

X <- sDat[, setdiff(colnames(data), "y")]
Y <- sDat[, "y"]
predictor = Predictor$new(fit, data = X, y = Y, predict.fun = pf)
Interaction$new(predictor, feature = "x")

# 1      v:x   0.19213283
# 2      z:x   0.03081365


## Overall
light_interaction(fl)$data
Interaction$new(predictor)

