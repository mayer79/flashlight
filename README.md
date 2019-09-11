# The flashlight package

The goal of this package is shed light on black box machine learning models.

The main props of `flashlight`:

1. It is simple, yet flexible.

2. It offers model agnostic tools like model performance, variable importance, ICE profiles, partial dependence, further effects plots, and variable contribution breakdown for single observations.

3. It allows to assess multiple models in parallel.

4. It supports "group by" operations.

5. It works with case weights.

Currently, models with numeric or binary response are supported.

## Installation

From CRAN:
```
install.packages("flashlight")
```

Latest version from github:
```
library(devtools)
install_github("mayer79/flashlight")
```

## Example Code

``` r
# Fit model
fit <- lm(Sepal.Length ~ ., data = iris)

# Make flashlight
fl <- flashlight(model = fit, data = iris, y = "Sepal.Length", label = "ols",
                 metrics = list(rmse = rmse, `R-squared` = r_squared))

# Performance: rmse and R-squared
plot(light_performance(fl), fill = "darkred")
plot(light_performance(fl, by = "Species"), fill = "darkred")

# Variable importance by drop in rmse
imp <- light_importance(fl)
plot(imp, fill = "darkred")
plot(light_importance(fl, by = "Species")) +
   scale_fill_viridis_d(begin = 0.2, end = 0.8)
most_important(imp, 2)

# ICE profiles for Petal.Width
plot(light_ice(fl, v = "Petal.Width"))
plot(light_ice(fl, v = "Petal.Width", by = "Species"))

# Partial dependence profiles for Petal.Width
plot(light_profile(fl, v = "Petal.Width"))
plot(light_profile(fl, v = "Petal.Width", by = "Species"))

# Prediction, response and residual profiles
plot(light_profile(fl, v = "Petal.Width", type = "response", stats = "quartiles"))
plot(light_profile(fl, v = "Petal.Width", type = "predicted"))
plot(light_profile(fl, v = "Petal.Width", type = "residual", stats = "quartiles"))

# Response profiles, prediction profiles and partial depencence in one
plot(light_effects(fl, v = "Petal.Width"))

# Variable contribution breakdown for single observation
plot(light_breakdown(fl, new_obs = iris[2, ]))

```
Check out the vignette to see the full capabilities of the package.
