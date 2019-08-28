#=====================================================================================
# BUILD THE PACKAGE
#=====================================================================================

# library(devtools) # Get latest MetricsWeighted branch
# install_github("mayer79/MetricsWeighted", ref = "cran_0.1.1")
library(MetricsWeighted)
library(dplyr)
library(tidyr)
library(rlang)
library(ggplot2)
library(ggpubr)
lapply(list.files("R", full.names = TRUE), source)

#======================================
# Performance
#======================================

fit_full <- lm(Sepal.Length ~ ., data = iris)
fit_part <- lm(Sepal.Length ~ Petal.Length, data = iris)
mod_full <- flashlight(model = fit_full, label = "full", data = iris, y = "Sepal.Length")
mod_part <- flashlight(model = fit_part, label = "part", data = iris, y = "Sepal.Length")
mods <- multiflashlight(list(mod_full, mod_part),
  metrics = list(rmse = rmse, `R-squared` = r_squared))
plot(light_performance(mod_full), fill = "darkred")
plot(light_performance(mod_full, metrics = list(rmse = rmse, `R-squared` = r_squared)))
plot(light_performance(mod_full, by = "Species"), fill = "darkred")
plot(light_performance(mods))
plot(light_performance(mods, by = "Species"))
plot(light_performance(mods, by = "Species"), swap_dim = TRUE) +
  scale_fill_viridis_d()

mod_part <- flashlight(mod_part, metrics = list(mse = mse))
mod_full <- flashlight(mod_full, metrics = list(r_squared = r_squared))

#======================================
# Importance
#======================================


fit_part <- lm(Sepal.Length ~ Petal.Length, data = iris)
fit_full <- lm(Sepal.Length ~ ., data = iris)
mod_full <- flashlight(model = fit_full, label = "full", data = iris, y = "Sepal.Length")
mod_part <- flashlight(model = fit_part, label = "part", data = iris, y = "Sepal.Length")
mods <- multiflashlight(list(mod_full, mod_part), by = "Species")

light_importance(mod_full, seed = 4)
light_importance(mod_full, variable_name = "v", label_name = "model",
  metric_name = "m", value_name = "drop")
light_importance(mod_full, metric = list(mae = MetricsWeighted::mae), lower_is_better = FALSE)
light_importance(mod_full, v = c("Petal.Length", "Species"))
light_importance(mods)

ir <- iris
ir$log_sl <- log(ir$Sepal.Length)
fit_lm <- lm(log_sl ~ Petal.Length, data = ir)
fit_glm <- glm(Sepal.Length ~ Petal.Length, data = ir, family = Gamma(link = log))
fl_lm <- flashlight(model = fit_lm, label = "lm", y = "log_sl", linkinv = exp)
fl_glm <- flashlight(model = fit_glm, label = "glm", y = "Sepal.Length",
  predict_function = function(m, X) predict(m, X, type = "response"))
fls <- multiflashlight(list(fl_lm, fl_glm), data = ir,
  metrics = list(rmse = rmse, r_squared = r_squared))
light_importance(fls, v = "Petal.Length", seed = 45)
light_importance(fls, v = "Petal.Length", seed = 45, use_linkinv = TRUE)


#======================================
# Profile
#======================================

fit_full <- lm(Sepal.Length ~ ., data = iris)
fit_part <- lm(Sepal.Length ~ Petal.Length, data = iris)
mod_full <- flashlight(model = fit_full, label = "full", data = iris, y = "Sepal.Length")
mod_part <- flashlight(model = fit_part, label = "part", data = iris, y = "Sepal.Length")
mods <- multiflashlight(list(mod_full, mod_part))

plot(light_profile(mod_full, v = "Species"))
plot(light_profile(mod_full, v = "Species", type = "response"))
plot(light_profile(mod_full, v = "Species", stats = "quartiles"))

plot(light_profile(mod_full, v = "Petal.Width"))
plot(light_profile(mod_full, v = "Petal.Width", type = "residual"))
plot(light_profile(mod_full, v = "Petal.Width", stats = "quartiles"))
plot(light_profile(mod_full, v = "Petal.Width", n_bins = 3))
plot(light_profile(mod_full, v = "Petal.Width", pd_evaluate_at = 2:4))
plot(light_profile(mod_full, pd_grid = data.frame(Petal.Width = 2:4)))

plot(light_profile(mod_full, v = "Petal.Width", by = "Species"))
plot(light_profile(mod_full, v = "Petal.Width", by = "Species"), swap_dim = TRUE)

plot(light_profile(mods, v = "Species"))
plot(light_profile(mods, v = "Petal.Width"))
plot(light_profile(mods, v = "Petal.Width"), swap_dim = TRUE)
plot(light_profile(mods, v = "Petal.Width", by = "Species"))
plot(light_profile(mods, v = "Petal.Width", by = "Species"), swap_dim = TRUE)
plot(light_profile(mods, v = "Petal.Width", by = "Species", type = "predicted"))
plot(light_profile(mods, v = "Petal.Width", by = "Species",
  type = "predicted", n_bins = 5), swap_dim = TRUE)
plot(light_profile(mods, v = "Petal.Width", by = "Species",
  type = "predicted", stats = "quartiles"), rotate_x = TRUE)

plot(light_profile(mods, v = "Petal.Width", by = "Species", stats = "quartiles",
     value_name = "pd", q1_name = "p25", q3_name = "p75", label_name = "model",
     type_name = "visualization", counts_name = "n"))

ir <- iris
ir$log_sl <- log(ir$Sepal.Length)
fit_lm <- lm(log_sl ~ Petal.Length + Petal.Width, data = ir)
fit_glm <- glm(Sepal.Length ~ Petal.Length + Petal.Width,
  data = ir, family = Gamma(link = log))
fl_lm <- flashlight(model = fit_lm, label = "lm", y = "log_sl", linkinv = exp)
fl_glm <- flashlight(model = fit_glm, label = "glm", y = "Sepal.Length",
  predict_function = function(m, X) predict(m, X, type = "response"))
fls <- multiflashlight(list(fl_lm, fl_glm), data = ir)

plot(light_profile(fls, v = "Petal.Length"))
plot(light_profile(fls, v = "Petal.Length", use_linkinv = FALSE))


#======================================
# Effects
#======================================

fit_full <- lm(Sepal.Length ~ ., data = iris)
fit_part <- glm(Sepal.Length ~ Petal.Length, data = iris)
mod_full <- flashlight(model = fit_full, label = "full", data = iris,
  y = "Sepal.Length", w = "Petal.Length")
mod_part <- flashlight(model = fit_part, label = "part", data = iris,
  y = "Sepal.Length", w = "Petal.Length")
mods <- multiflashlight(list(mod_full, mod_part))

plot(light_effects(mod_full, v = "Species"))
x <- light_effects(mod_full, v = "Petal.Width")
plot(x)
plot(x, use = "response")
plot(x, use = "predicted")
plot(x, use = "pd")
plot_counts(plot(x), x, alpha = 0.2)
plot_counts(plot(x, use = "response"), x, alpha = 0.2)
plot_counts(plot(x, use = "pd"), x, alpha = 0.2)

x <- light_effects(mod_full, v = "Petal.Width", stats = "quartiles")
plot(x)
plot(x, size_factor = 3)
plot_counts(plot(x), x, alpha = 0.2)
plot_counts(plot(x, use = "response"), x, alpha = 0.2)

x <- light_effects(mod_full, v = "Petal.Width", by = "Species")
plot(x)
p <- plot(x, zero_counts = FALSE)
plot_counts(p, x, zero_counts = FALSE, alpha = 0.2)

x <- light_effects(mod_full, v = "Petal.Width", by = "Species", stats = "quartiles")
plot(x)
plot_counts(plot(x), x, alpha = 0.2)

x <- light_effects(mods, v = "Petal.Width")
plot(x, zero_counts = TRUE)
plot_counts(plot(x, zero_counts = TRUE), x, alpha = 0.2, zero_counts = TRUE)
plot(light_effects(mods, v = "Petal.Width", stats = "quartiles"))

ir <- iris
ir$log_sl <- log(ir$Sepal.Length)
fit_lm <- lm(log_sl ~ Petal.Length + Petal.Width, data = ir)
fit_glm <- glm(Sepal.Length ~ Petal.Length + Petal.Width,
  data = ir, family = Gamma(link = log))
fl_lm <- flashlight(model = fit_lm, label = "lm", y = "log_sl", linkinv = exp)
fl_glm <- flashlight(model = fit_glm, label = "glm", y = "Sepal.Length",
  predict_function = function(m, X) predict(m, X, type = "response"))
fls <- multiflashlight(list(fl_lm, fl_glm), data = ir)
plot(light_effects(fls, v = "Petal.Length"))
plot(light_effects(fls, v = "Petal.Length", use_linkinv = FALSE))

#======================================
# NAs
#======================================

fit_full <- lm(Sepal.Length ~ ., data = iris)
fit_part <- lm(Sepal.Length ~ Petal.Length, data = iris)
ir <- iris
ir$Species[1:5] <- NA
ir$Petal.Length[6:10] <- NA

imp_data <- function(z) {
  z$Species[is.na(z$Species)] <- "virginica"
  z$Petal.Length[is.na(z$Petal.Length)] <- 4
  z
}

mod_full <- flashlight(model = fit_full, label = "full",
                       data = ir, y = "Sepal.Length")
mod_part <- flashlight(model = fit_part, label = "part",
                       data = ir, y = "Sepal.Length")
mods <- multiflashlight(list(mod_full, mod_part),
                        predict_function = function(m, d)  predict(m, imp_data(d)))

plot(light_performance(mods))
plot(light_importance(mods))

plot(light_ice(mods, v = "Species"))
plot(light_ice(mods, v = "Petal.Length"))

plot(light_profile(mods, v = "Species"))
plot(light_profile(mods, v = "Species", type = "response"))
plot(light_profile(mods, v = "Petal.Length"))
plot(light_profile(mods, v = "Petal.Length", type = "response"))

plot(light_effects(mods, v = "Species"))
x <- light_effects(mods, v = "Petal.Length", cut_type = "quantile")
plot_counts(plot(x), x, alpha = 0.2)
x <- light_effects(mods, v = "Petal.Length")
plot_counts(plot(x), x, alpha = 0.2)
plot(light_effects(mods, v = "Petal.Length"), use = "pd")
plot(light_effects(mods, v = "Petal.Length", v_labels = FALSE)) # not shown

#======================================
# profile
#======================================

fit_full <- lm(Sepal.Length ~ ., data = iris)
fit_part <- lm(Sepal.Length ~ Petal.Length, data = iris)

# Different data
mod_full <- flashlight(model = fit_full, label = "full",
  data = iris[1:75, ], y = "Sepal.Length")
mod_part <- flashlight(model = fit_part, label = "part",
  data = iris[76:150, ], y = "Sepal.Length")
mods <- multiflashlight(list(mod_full, mod_part))

plot(light_profile(mods, v = "Petal.Length"))
plot(light_profile(mods, v = "Petal.Length", breaks = seq(1.5, 6.5, by = 1)))
plot(light_profile(mods, v = "Petal.Length", pd_evaluate_at = 2:6))
plot(light_profile(mods, pd_grid = data.frame(Petal.Length = 2:6)))
plot(light_profile(mods, v = "Petal.Length", type = "predicted"))
plot(light_profile(mods, v = "Petal.Length", type = "predicted", breaks = 1:8))
plot(light_profile(mods, v = "Petal.Length", type = "predicted", v_labels = FALSE))

#======================================
# effects
#======================================

fit_full <- lm(Sepal.Length ~ ., data = iris)
fit_part <- lm(Sepal.Length ~ Petal.Length, data = iris)
# Different data: Use eigher fixed breaks or v_labels = FALSE
mod_full <- flashlight(model = fit_full, label = "full",
  data = iris[1:75, ], y = "Sepal.Length")
mod_part <- flashlight(model = fit_part, label = "part",
  data = iris[76:150, ], y = "Sepal.Length")
mods <- multiflashlight(list(mod_full, mod_part))
# light_effects(mods, v = "Petal.Length") # bad
light_effects(mods, v = "Petal.Length", breaks = 0:8) # good
light_effects(mods, v = "Petal.Length", v_labels = FALSE) # good

# Different data: Use eigher fixed breaks or v_labels = FALSE
mod_full <- flashlight(model = fit_full, label = "full",
  data = iris[1:75, ], y = "Sepal.Length")
mod_part <- flashlight(model = fit_part, label = "part",
  data = iris[76:150, ], y = "Sepal.Length")
mods <- multiflashlight(list(mod_full, mod_part))
plot(light_effects(mods, v = "Petal.Length", breaks = 0:8))
eff <- light_effects(mods, v = "Petal.Length")
plot_counts(plot(eff), eff, show_labels = FALSE)
plot_counts(plot(eff, zero_counts = FALSE), eff, zero_counts = FALSE)

#======================================
# Empty factor levels
#======================================

iris$Species <- relevel(iris$Species, "virginica")
# iris$Species <- as.character(iris$Species)
fit <- lm(Sepal.Length ~ ., data = iris)
ir <- iris[90:150, ]
ir$sw <- ir$Sepal.Width > 3
table(ir$Species)
table(ir$Species, ir$sw)
fl <- flashlight(model = fit, label = "lm", data = ir, y = "Sepal.Length")

plot(light_ice(fl, v = "Species"))
plot(light_profile(fl, v = "Species"))
plot(light_profile(fl, v = "Species", type = "response"))

# No by variable: zero_counts does not work
eff <- light_effects(fl, v = "Species")

(p <- plot(eff, zero_counts = FALSE))
plot_counts(p, eff, alpha = 0.2, zero_counts = FALSE) # bad

(p <- plot(eff))
plot_counts(p, eff, alpha = 0.2) # bad

# With by variable: zero_counts does not work
eff <- light_effects(fl, v = "Species", by = "sw")

(p <- plot(eff, zero_counts = FALSE))
plot_counts(p, eff, alpha = 0.2, zero_counts = FALSE) # bad

(p <- plot(eff))
plot_counts(p, eff, alpha = 0.2) # bad
