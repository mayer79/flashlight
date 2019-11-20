library(iml)
fit_nonadditive <- lm(Sepal.Length ~ Petal.Length * Species + Petal.Width, data = iris)
fl <- flashlight(model=fit_nonadditive, data = iris[c(1:10, 51:60, 101:111), ], y = "Sepal.Length", label ="lm")

X <- iris[c(1:10, 51:60, 101:111), ]
Y <- iris[c(1:10, 51:60, 101:111), "Sepal.Width"]
predictor = Predictor$new(fit_nonadditive, data=X, y=Y)
ia1 = Interaction$new(predictor)
ia1
plot(ia1)

light_interaction(fl)$data
