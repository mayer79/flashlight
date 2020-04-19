## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  fig.width = 7,
  fig.height = 6
)

## ----setup--------------------------------------------------------------------
library(ggplot2)
library(flashlight)      # model interpretation
library(MetricsWeighted) # metrics
library(dplyr)           # data prep
library(moderndive)      # data
library(xgboost)         # gradient boosting
library(ranger)          # random forest

## -----------------------------------------------------------------------------
# Fit model
fit <- lm(Sepal.Length ~ ., data = iris)

# Make flashlight
fl <- flashlight(model = fit, data = iris, y = "Sepal.Length", label = "ols",
                 metrics = list(rmse = rmse, `R-squared` = r_squared))

# Performance: rmse and R-squared
plot(light_performance(fl), fill = "darkred")
plot(light_performance(fl, by = "Species"), fill = "darkred")

# Variable importance by increase in rmse
imp <- light_importance(fl, m_repetitions = 4)
plot(imp, fill = "darkred")
plot(light_importance(fl, by = "Species"))
most_important(imp, 2)

# ICE profiles for Petal.Width
plot(light_ice(fl, v = "Petal.Width"))
plot(light_ice(fl, v = "Petal.Width", center = "first"))
plot(light_ice(fl, v = "Petal.Width", by = "Species"))

# Partial dependence profiles for Petal.Width
plot(light_profile(fl, v = "Petal.Width"))
plot(light_profile(fl, v = "Petal.Width", by = "Species"))

# Accumulated local effects (ALE) profiles for Petal.Width
plot(light_profile(fl, v = "Petal.Width", type = "ale"))
plot(light_profile(fl, v = "Petal.Width", by = "Species", type = "ale"))

# Prediction, response and residual profiles
plot(light_profile(fl, v = "Petal.Width", type = "response", stats = "quartiles"))
plot(light_profile(fl, v = "Petal.Width", type = "predicted"))
plot(light_profile(fl, v = "Petal.Width", type = "residual", stats = "quartiles"))

# Response profiles, prediction profiles, partial dependence, and ALE profiles in one
plot(light_effects(fl, v = "Petal.Width"), use = "all")

# Scatter plots
plot(light_scatter(fl, v = "Petal.Width", type = "predicted"))

# Variable contribution breakdown for single observation
plot(light_breakdown(fl, new_obs = iris[2, ]))

# Global surrogate
plot(light_global_surrogate(fl))


## -----------------------------------------------------------------------------
head(house_prices)

## -----------------------------------------------------------------------------
prep <- mutate(house_prices,
               log_price = log(price),
               grade = as.integer(as.character(grade)),
               year = as.numeric(format(date, '%Y')),
               age = year - yr_built,
               year = factor(year),
               zipcode = as.factor(as.character(zipcode)),
               waterfront = factor(waterfront, levels = c(FALSE, TRUE), labels = c("no", "yes")))

x <- c("grade", "year", "age", "sqft_living", "sqft_lot", "zipcode",
       "condition", "waterfront")

## -----------------------------------------------------------------------------
# Data wrapper for the linear model
prep_lm <- function(data) {
  data %>%
    mutate(sqrt_living = log(sqft_living),
           sqrt_lot = log(sqft_lot))
}

# Data wrapper for xgboost
prep_xgb <- function(data, x) {
  data %>%
    select_at(x) %>%
    mutate_if(Negate(is.numeric), as.integer) %>%
    data.matrix()
}

## -----------------------------------------------------------------------------
# Train / valid / test split (70% / 20% / 10%)
set.seed(56745)
ind <- sample(10, nrow(prep), replace = TRUE)

train <- prep[ind >= 4, ]
valid <- prep[ind %in% 2:3, ]
test <- prep[ind == 1, ]

(form <- reformulate(x, "log_price"))
fit_lm <- lm(update.formula(form, . ~ . + I(sqft_living^2)), data = prep_lm(train))

# Random forest
fit_rf <- ranger(form, data = train, respect.unordered.factors = TRUE, 
                 num.trees = 100, seed = 8373)
cat("R-squared OOB:", fit_rf$r.squared)

# Gradient boosting
dtrain <- xgb.DMatrix(prep_xgb(train, x), label = train[["log_price"]])
dvalid <- xgb.DMatrix(prep_xgb(valid, x), label = valid[["log_price"]])

params <- list(learning_rate = 0.2,
               max_depth = 5,
               alpha = 1,
               lambda = 1,
               colsample_bytree = 0.8)

fit_xgb <- xgb.train(params,
                     data = dtrain,
                     watchlist = list(train = dtrain, valid = dvalid),
                     nrounds = 50,
                     print_every_n = 10,
                     objective = "reg:linear")

## -----------------------------------------------------------------------------
fl_mean <- flashlight(model = mean(train$log_price), label = "mean",
                      predict_function = function(mod, X) rep(mod, nrow(X)))
fl_lm <- flashlight(model = fit_lm, label = "lm",
                    predict_function = function(mod, X) predict(mod, prep_lm(X)))
fl_rf <- flashlight(model = fit_rf, label = "rf",
                    predict_function = function(mod, X) predict(mod, X)$predictions)
fl_xgb <- flashlight(model = fit_xgb, label = "xgb",
                     predict_function = function(mod, X) predict(mod, prep_xgb(X, x)))
print(fl_xgb)

## -----------------------------------------------------------------------------
fls <- multiflashlight(list(fl_mean, fl_lm, fl_rf, fl_xgb), y = "log_price", linkinv = exp,
                       data = valid, metrics = list(rmse = rmse, `R-squared` = r_squared))

## -----------------------------------------------------------------------------
fl_lm <- fls$lm

## -----------------------------------------------------------------------------
perf <- light_performance(fls)
perf
plot(perf)

## -----------------------------------------------------------------------------
plot(perf, fill = "darkred") +
  xlab(element_blank())

## -----------------------------------------------------------------------------
head(perf$data)

perf$data %>%
  ggplot(aes(x = label, y = value, group = metric, color = metric)) +
  geom_point()

## -----------------------------------------------------------------------------
(imp <- light_importance(fls, v = x))
plot(imp, fill = "darkred")

## -----------------------------------------------------------------------------
cp <- light_ice(fls, v = "sqft_living", n_max = 30, seed = 35)
plot(cp, alpha = 0.2)

## -----------------------------------------------------------------------------
cp <- light_ice(fls, v = "sqft_living", n_max = 30, seed = 35, center = "first")
plot(cp, alpha = 0.2)

## -----------------------------------------------------------------------------
pd <- light_profile(fls, v = "sqft_living")
pd
plot(pd)

## -----------------------------------------------------------------------------
pd <- light_profile(fls, v = "sqft_living", pd_evaluate_at = seq(1000, 4000, by = 100))
plot(pd)

## -----------------------------------------------------------------------------
pd <- light_profile(fls, v = "condition")
plot(pd)

## -----------------------------------------------------------------------------
ale <- light_profile(fls, v = "sqft_living", type = "ale")
ale
plot(ale)

## -----------------------------------------------------------------------------
plot(light_profile(fls, v = "sqft_living", type = "ale", cut_type = "quantile"))

## -----------------------------------------------------------------------------
format_y <- function(x) format(x, big.mark = "'", scientific = FALSE)

pvp <- light_profile(fls, v = "sqft_living", type = "predicted", format = "fg", big.mark = "'")
plot(pvp) +
  scale_y_continuous(labels = format_y)

## -----------------------------------------------------------------------------
rvp <- light_profile(fl_lm, v = "sqft_living", type = "response", format = "fg")
plot(rvp) +
  scale_y_continuous(labels = format_y)

## -----------------------------------------------------------------------------
rvp <- light_profile(fl_lm, v = "sqft_living", type = "response",
                     stats = "quartiles", format = "fg")
plot(rvp) +
  scale_y_continuous(labels = format_y)

## -----------------------------------------------------------------------------
fls$mean <- NULL
rvp <- light_profile(fls, v = "sqft_living", type = "residual",
                     stats = "quartiles", format = "fg")
plot(rvp) +
  scale_y_continuous(labels = format_y)

## -----------------------------------------------------------------------------
plot(rvp, swap_dim = TRUE) +
  scale_y_continuous(labels = format_y)

## -----------------------------------------------------------------------------
rvp <- light_profile(fls, v = "sqft_living", type = "residual",
                     stats = "quartiles", format = "fg", n_bins = 5)
plot(rvp, swap_dim = TRUE) +
  scale_y_continuous(labels = format_y)

## -----------------------------------------------------------------------------
rvp <- light_profile(fls, v = "sqft_living", use_linkinv = FALSE,
                     stats = "quartiles", pd_center = "mean")
plot(rvp)

## -----------------------------------------------------------------------------
eff <- light_effects(fl_lm, v = "condition")
p <- plot(eff) +
  scale_y_continuous(labels = format_y)
p

## -----------------------------------------------------------------------------
plot_counts(p, eff, alpha = 0.2)

## -----------------------------------------------------------------------------
eff <- light_effects(fl_lm, v = "condition", linkinv = I)
p <- plot(eff, use = "all") +
  scale_y_continuous(labels = format_y) +
  ggtitle("Effects plot on modelled log scale")
p

## -----------------------------------------------------------------------------
eff <- light_effects(fl_lm, v = "condition", stats = "quartiles")
p <- plot(eff, rotate_x = FALSE) +
   scale_y_continuous(labels = format_y)
plot_counts(p, eff, fill = "blue", alpha = 0.2, width = 0.3)

## -----------------------------------------------------------------------------
eff <- light_effects(fls, v = "sqft_living", v_labels = FALSE, 
                     cut_type = "quantile", n_bins = 25)
plot(eff, use = c("pd", "ale"), show_points = FALSE) + 
  scale_y_continuous(labels = format_y) +
  coord_cartesian(xlim = c(1000, 4000), ylim = c(3e5, 9e5))

## -----------------------------------------------------------------------------
pr <- light_scatter(fls, v = "sqft_living", n_max = 300)
plot(pr, alpha = 0.2)

## -----------------------------------------------------------------------------
st <- light_interaction(fls, v = x, grid_size = 30, n_max = 50, seed = 42)
plot(st)

## -----------------------------------------------------------------------------
st_pair <- light_interaction(fls, v = most_important(st, 4), 
                             pairwise = TRUE, n_max = 50, seed = 42)
plot(st_pair)

## -----------------------------------------------------------------------------
bd <- light_breakdown(fl_lm, new_obs = valid[1, ], v = x, n_max = 1000, seed = 74)
plot(bd, size = 3)

## -----------------------------------------------------------------------------
surr <- light_global_surrogate(fls$xgb, v = x)
print(surr$data$r_squared)
plot(surr)

## -----------------------------------------------------------------------------
fls <- multiflashlight(fls, by = "year")

# Performance
plot(light_performance(fls))

# With swapped dimension
plot(light_performance(fls), swap_dim = TRUE)

# Importance
imp <- light_importance(fls, v = x)
plot(imp, top_m = 4)
plot(imp, swap_dim = TRUE)

# Effects: ICE
plot(light_ice(fls, v = "sqft_living", seed = 4345),
     alpha = 0.8, facet_scales = "free_y") +
  scale_y_continuous(labels = format_y)

# c-ICE
plot(light_ice(fls, v = "sqft_living", seed = 4345, center = "middle"),
     alpha = 0.8, facet_scales = "free_y") +
  scale_y_continuous(labels = format_y)

# Effects: Partial dependence
plot(light_profile(fls, v = "sqft_living"))
plot(light_profile(fls, v = "sqft_living"), swap_dim = TRUE)
plot(light_profile(fls, v = "sqft_living", stats = "quartiles", center = "mean"))

# Effects: ALE
plot(light_profile(fls, v = "sqft_living", type = "ale", cut_type = "quantile"))
plot(light_profile(fls, v = "sqft_living", type = "ale", cut_type = "quantile"), swap_dim = TRUE)

# Effects: Combined plot (only one flashlight)
# -> we need to manually pass "by" or update the single flashlight
z <- light_effects(fls, v = "sqft_living", format = "fg",
                   stats = "quartiles", n_bins = 5, by = NULL)
p <- plot(z) +
  scale_y_continuous(labels = format_y) +
  coord_cartesian(ylim = c(0, 3e6))
plot_counts(p, z, alpha = 0.2)

# Scatter
pr <- light_scatter(fls, v = "sqft_living", n_max = 300)
plot(pr, alpha = 0.8)

# Variable contribution breakdown for single observation (on log-scale)
# -> "by" selects the relevant rows in data/valid
plot(light_breakdown(fl_lm, new_obs = valid[1, ], v = x, top_m = 3))

# Global surrogate
plot(light_global_surrogate(fls$xgb, v = x, maxdepth = 3))

## -----------------------------------------------------------------------------
# Add weight info to the flashlight
fl_weighted <- flashlight(fl, w = "Petal.Length", label = "ols weighted")
fls <- multiflashlight(list(fl, fl_weighted))

# Performance: rmse and R-squared
plot(light_performance(fls))
plot(light_performance(fls, by = "Species"))

# Variable importance by drop in rmse
plot(light_importance(fls, by = "Species"))

# ICE profiles for Petal.Width
# (not affected by weights because nothing is being aggregated)
indices <- seq(10, 150, by = 10)
plot(light_ice(fls, v = "Petal.Width", indices = indices), alpha = 0.2)
plot(light_ice(fls, v = "Petal.Width", by = "Species", indices = indices))

# c-ICE -> lines overlap, no interactions at all
plot(light_ice(fls, v = "Petal.Width", indices = indices, center = "first"), alpha = 0.2)
plot(light_ice(fls, v = "Petal.Width", by = "Species", indices = indices, center = "first"))

# Partial dependence profiles for Petal.Width
plot(light_profile(fls, v = "Petal.Width"))
plot(light_profile(fls, v = "Petal.Width", by = "Species"))

# ALE profiles for Petal.Width
plot(light_profile(fls, v = "Petal.Width", type = "ale"))
plot(light_profile(fls, v = "Petal.Width", by = "Species", type = "ale"))

# Observed, predicted, and partial dependence profiles
plot(light_effects(fls, v = "Petal.Width"))
eff <- light_effects(fls, v = "Petal.Width", stats = "quartiles")
plot(eff) %>%
  plot_counts(eff, alpha = 0.2, fill = "blue")

# Variable contribution breakdown for single observation (on log-scale)
plot(light_breakdown(fls, new_obs = iris[2, ]), size = 2.5)

# Global surrogate
plot(light_global_surrogate(fls))

## -----------------------------------------------------------------------------
ir <- iris
ir$virginica <- ir$Species == "virginica"

fit <- glm(virginica ~ Sepal.Length + Petal.Width, data = ir, family = binomial)

# Make flashlight
fl <- flashlight(model = fit, data = ir, y = "virginica", label = "lr",
                 metrics = list(logLoss = logLoss, AUC = AUC),
                 predict_function = function(m, d) predict(m, d, type = "response"))

# Performance: rmse and R-squared
plot(light_performance(fl), fill = "darkred")

# Variable importance by drop in rmse
plot(light_importance(fl, v = c("Sepal.Length", "Petal.Width")), fill = "darkred")

# ICE profiles for Petal.Width
plot(light_ice(fl, v = "Petal.Width"), alpha = 0.4)

# c-ICE profiles for Petal.Width
plot(light_ice(fl, v = "Petal.Width", center = "first"), alpha = 0.4)

# Partial dependence profiles for Petal.Width
plot(light_profile(fl, v = "Petal.Width"))

# ALE profiles for Petal.Width
plot(light_profile(fl, v = "Petal.Width", type = "ale", cut_type = "quantile"))

# Observed, predicted, and partial dependence profiles
eff <- light_effects(fl, v = "Petal.Width")
plot_counts(plot(eff, use = "all"), eff, alpha = 0.2)

# Variable contribution breakdown for single observation
plot(light_breakdown(fl, new_obs = ir[2, ], v = c("Sepal.Length", "Petal.Width")))


