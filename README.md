# {flashlight} <a href='https://github.com/mayer79/flashlight'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![CRAN status](http://www.r-pkg.org/badges/version/flashlight)](https://cran.r-project.org/package=flashlight)
[![R-CMD-check](https://github.com/mayer79/flashlight/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mayer79/flashlight/actions)
[![Codecov test coverage](https://codecov.io/gh/mayer79/flashlight/branch/main/graph/badge.svg)](https://app.codecov.io/gh/mayer79/flashlight?branch=main)

[![](https://cranlogs.r-pkg.org/badges/flashlight)](https://cran.r-project.org/package=flashlight) 
[![](https://cranlogs.r-pkg.org/badges/grand-total/flashlight?color=orange)](https://cran.r-project.org/package=flashlight)

<!-- badges: end -->

## Overview

The goal of this package is shed light on black box machine learning models.

The main props of {flashlight}:

1. It is simple, yet flexible.
2. It offers model agnostic tools like model performance, variable importance, global surrogate models, ICE profiles, partial dependence, ALE, and further effects plots, scatter plots, interaction strength, and variable contribution breakdown/SHAP for single observations.
3. It allows to assess multiple models in parallel.
4. It supports "group by" operations.
5. It works with case weights.

Currently, models with numeric or binary response are supported.

## Installation

```r
# From CRAN
install.packages("flashlight")

# Development version
devtools::install_github("mayer79/flashlight")
```

## Usage

``` r
library(ggplot2)
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
```

### Performance (overall and grouped by `Species`)

``` r
plot(light_performance(fl), fill = "darkred") +
  ggtitle("Overall")
plot(light_performance(fl, by = "Species"), fill = "darkred") +
  ggtitle("Grouped by Species")
```
<p>
  <img src="tools/figs/perf.png" alt="Performance" width="40%" hspace="20"/>
  <img src="tools/figs/perf_grouped.png" alt="Grouped" width="40% hspace="20"/>
</p>

### Permutation importance

``` r
imp <- light_importance(fl, m_repetitions = 4)
plot(imp, fill = "darkred")
```
![](tools/figs/imp.png)

### ICE curves for `Petal.Width`

``` r
plot(light_ice(fl, v = "Petal.Width"))
```
![](tools/figs/ice.png)

### Partial dependence plot for Petal.Width

```r
plot(light_profile(fl, v = "Petal.Width")) +
  ggtitle("Overall")
plot(light_profile(fl, v = "Petal.Width", by = "Species")) +
  ggtitle("Grouped by Species")
```
<p>
  <img src="tools/figs/pd.png" alt="Partial Dependence" width="40%" hspace="20"/>
  <img src="tools/figs/pd_grouped.png" alt="Partial Dependence (grouped)" width="40%" hspace="20"/>
</p>

### 2D partial dependence

```r
plot(light_profile2d(fl, v = c("Petal.Width", "Petal.Length")))
```
![](tools/figs/pd2d.png)

### Accumulated local effects (ALE) profiles for Petal.Width

``` r
plot(light_profile(fl, v = "Petal.Width", type = "ale"))
```
![](tools/figs/ale.png)

### Prediction, response and residual profiles, e.g.

``` r
plot(light_profile(fl, v = "Petal.Width", type = "residual", 
                   stats = "quartile"))
```
![](tools/figs/residual.png)

### Different profile plots in one...

``` r
plot(light_effects(fl, v = "Petal.Width"), use = "all")
```
![](tools/figs/effects.png)

### Variable contribution breakdown for single observation

``` r
plot(light_breakdown(fl, new_obs = iris[2, ]))
```
![](tools/figs/breakdown.png)

### Global surrogate

``` r
plot(light_global_surrogate(fl))
```
![](tools/figs/surrogate.png)

Check out the vignette to see the full capabilities of the package.
