# The flashlight package <a href='https://github.com/mayer79/flashlight'><img src='man/figures/logo.png' align="right" height="138.5" /></a>

[![CRAN version](http://www.r-pkg.org/badges/version/flashlight)](https://cran.r-project.org/package=flashlight) [![](https://cranlogs.r-pkg.org/badges/flashlight)](https://cran.r-project.org/package=flashlight) [![](https://cranlogs.r-pkg.org/badges/grand-total/flashlight?color=orange)](https://cran.r-project.org/package=flashlight)

The goal of this package is shed light on black box machine learning models.

The main props of `flashlight`:

1. It is simple, yet flexible.

2. It offers model agnostic tools like model performance, variable importance, global surrogate models, ICE profiles, partial dependence, ALE, and further effects plots, scatter plots, interaction strength, and variable contribution breakdown/SHAP for single observations.

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
install_github("mayer79/flashlight", subdir = "release/flashlight")
```

## Example Code

``` r
library(MetricsWeighted)
library(flashlight)

# Fit model
fit <- lm(Sepal.Length ~ ., data = iris)

# Make flashlight
fl <- flashlight(
  model = fit, 
  data = iris, 
  y = "Sepal.Length", 
  label = "ols",               
  metrics = list(rmse = rmse, `R-squared` = r_squared)
)

# Performance: rmse and R-squared
plot(light_performance(fl), fill = "darkred")
plot(light_performance(fl, by = "Species"), fill = "darkred")

# Variable importance by increase in rmse
imp <- light_importance(fl, m_repetitions = 4)
plot(imp, fill = "darkred")

# ICE profiles for Petal.Width
plot(light_ice(fl, v = "Petal.Width"))
plot(light_ice(fl, v = "Petal.Width", center = "first"))

# Partial dependence profiles for Petal.Width
plot(light_profile(fl, v = "Petal.Width"))
plot(light_profile(fl, v = "Petal.Width", by = "Species"))

# 2D partial dependence
plot(light_profile2d(fl, v = c("Petal.Width", "Petal.Length")))

# Accumulated local effects (ALE) profiles for Petal.Width
plot(light_profile(fl, v = "Petal.Width", type = "ale"))

# Prediction, response and residual profiles, e.g.
plot(light_profile(fl, v = "Petal.Width", type = "residual"))

# All in one...
plot(light_effects(fl, v = "Petal.Width"), use = "all")

# Scatter plots
plot(light_scatter(fl, v = "Petal.Width", type = "predicted"))

# Variable contribution breakdown for single observation
plot(light_breakdown(fl, new_obs = iris[2, ]))

# Global surrogate
plot(light_global_surrogate(fl))

```
Check out the vignette to see the full capabilities of the package.
