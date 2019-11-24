library(iml)
library(ggplot2)
library(gbm)

fit <- gbm(price~., data = diamonds, n.trees = 500, n.cores = 8, interaction.depth = 3, shrinkage = 0.05)

pf <-  function(model, newdata) {
  predict(model, newdata, n.trees = model$n.trees)
}

interact.gbm(fit, data = diamonds[1:500,], i.var = c("clarity", "carat"))

fl <- flashlight(model=fit, data = diamonds[1:500, ],
                 y = "price", label ="gbm", predict_function = pf)

X <- diamonds[1:500, setdiff(colnames(diamonds), "price")]
Y <- diamonds[1:500, "price"]
predictor = Predictor$new(fit, data=X, y=Y, predict.fun = pf)
Interaction$new(predictor, feature = "carat", grid.size = 9)
Interaction$new(predictor, feature = "cut", grid.size = 9)

plot(light_interaction(fl, type = "pairwise")$data)

