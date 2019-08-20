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
