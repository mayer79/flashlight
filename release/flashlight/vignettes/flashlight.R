## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  fig.width = 7,
  fig.height = 6
)

## ----setup---------------------------------------------------------------
library(flashlight)      # model interpretation
library(MetricsWeighted) # Metrics
library(dplyr)           # data prep
library(moderndive)      # data
library(caret)           # data split
library(xgboost)         # gradient boosting
library(ranger)          # random forest

## ------------------------------------------------------------------------
# Fit model
fit <- lm(Sepal.Length ~ ., data = iris)

# Make flashlight
fl <- flashlight(model = fit, data = iris, y = "Sepal.Length", label = "ols",
                 metrics = list(rmse = rmse, `R-squared` = r_squared))

# Performance: rmse and R-squared
plot(light_performance(fl), fill = "darkred")
plot(light_performance(fl, by = "Species"), fill = "darkred")

# Variable importance by drop in rmse
plot(light_importance(fl), fill = "darkred")
plot(light_importance(fl, by = "Species"), fill = "darkblue", alpha = 0.7)

# ICE profiles for Petal.Width
plot(light_ice(fl, v = "Petal.Width"), alpha = 0.4)
plot(light_ice(fl, v = "Petal.Width", by = "Species"))

# Partial dependence profiles for Petal.Width
plot(light_profile(fl, v = "Petal.Width"))
plot(light_profile(fl, v = "Petal.Width", by = "Species"))

# Observed, predicted, and partial dependence profiles
plot(light_effects(fl, v = "Petal.Width"))
plot(light_effects(fl, v = "Petal.Width", stats = "quartiles"))
eff <- light_effects(fl, v = "Petal.Width", by = "Species")
plot(eff) %>% 
  plot_counts(eff, alpha = 0.2)

## ------------------------------------------------------------------------
head(house_prices)

## ------------------------------------------------------------------------
prep <- transform(house_prices, 
                  log_price = log(price),
                  grade = as.integer(as.character(grade)),
                  year = factor(lubridate::year(date)),
                  age = lubridate::year(date) - yr_built,
                  zipcode = as.numeric(as.character(zipcode)),
                  waterfront = factor(waterfront, levels = c(FALSE, TRUE), labels = c("no", "yes")))

x <- c("grade", "year", "age", "sqft_living", "sqft_lot", "zipcode", 
       "condition", "waterfront")

## ------------------------------------------------------------------------
# Data wrapper for the linear model
prep_lm <- function(data) {
  data %>% 
    mutate(sqrt_living = log(sqft_living),
           sqrt_lot = log(sqft_lot),
           zipcode = factor(zipcode %/% 10))
}

# Data wrapper for xgboost
prep_xgb <- function(data, x) {
  data %>% 
    select_at(x) %>% 
    mutate_if(Negate(is.numeric), as.integer) %>% 
    data.matrix()
}

