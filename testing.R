#=====================================================================================
# BUILD THE PACKAGE
#=====================================================================================

# library(devtools) # Get latest MetricsWeighted branch
# install_github("mayer79/MetricsWeighted", ref = "cran_0.1.1")
library(flashlight)
library(MetricsWeighted)
library(dplyr)
library(tidyr)
library(rlang)
library(ggplot2)
library(ggpubr)
library(rpart)
library(rpart.plot)
# lapply(list.files("R", full.names = TRUE), source)

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
plot(light_performance(mods, by = "Species"), geom = "point", size = 10)

#======================================
# Importance
#======================================

fit_part <- lm(Sepal.Length ~ Petal.Length, data = iris)
fit_full <- lm(Sepal.Length ~ ., data = iris)
mod_full <- flashlight(model = fit_full, label = "full", data = iris, y = "Sepal.Length")
mod_part <- flashlight(model = fit_part, label = "part", data = iris, y = "Sepal.Length")
mods <- multiflashlight(list(mod_full, mod_part), by = "Species")

m_rep <- 10
plot(light_importance(mod_full, seed = 4, m_repetitions = m_rep))
plot(light_importance(mod_full, variable_name = "v", label_name = "model",
  metric_name = "m", value_name = "drop", m_repetitions = m_rep))
plot(light_importance(mod_full, metric = list(mae = MetricsWeighted::mae),
                      lower_is_better = FALSE, m_repetitions = m_rep))
plot(light_importance(mod_full, v = c("Petal.Length", "Species"), m_repetitions = m_rep))
plot(light_importance(mods, m_repetitions = m_rep))
plot(light_importance(mods, m_repetitions = m_rep), swap_dim = TRUE)
plot(light_importance(mod_full, m_repetitions = m_rep))
plot(light_importance(mod_full, m_repetitions = m_rep), swap_dim = TRUE)
plot(light_importance(mods, m_repetitions = m_rep, by = "Species"))
plot(light_importance(mods, m_repetitions = m_rep, by = "Species"), swap_dim = TRUE)
plot(light_importance(mod_full, m_repetitions = m_rep, by = "Species"))
plot(light_importance(mod_full, m_repetitions = m_rep, by = "Species"), swap_dim = TRUE)

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
# Interaction
#======================================

fit_additive <- lm(Sepal.Length ~ Petal.Length + Petal.Width, data = iris)
fit_nonadditive <- lm(Sepal.Length ~ Petal.Length * Petal.Width, data = iris)
fl_additive <- flashlight(model = fit_additive, label = "additive")
fl_nonadditive <- flashlight(model = fit_nonadditive, label = "nonadditive")
fls_addnonadd <- multiflashlight(list(fl_additive, fl_nonadditive), data = iris, y = "Sepal.Length")

#
pw = T
# pw = F
plot(light_interaction(fls_addnonadd, pairwise = pw))
plot(light_interaction(fls_addnonadd, pairwise = pw), swap_dim = TRUE)
plot(light_interaction(fls_addnonadd, pairwise = pw, by = "Species"))
plot(light_interaction(fls_addnonadd, pairwise = pw, by = "Species"), swap_dim = TRUE)

plot(light_interaction(fls_addnonadd$additive, pairwise = pw))
plot(light_interaction(fls_addnonadd$additive, pairwise = pw), swap_dim = TRUE)
plot(light_interaction(fls_addnonadd$additive, pairwise = pw, by = "Species"))
plot(light_interaction(fls_addnonadd$additive, pairwise = pw, by = "Species"), swap_dim = TRUE)

# All three solutions should be identical
ir <- mutate(iris, w = (Species == "setosa"))
fls_w <- multiflashlight(fls_addnonadd, data = ir, w = "w")
light_interaction(fls_addnonadd$nonadditive, pairwise = pw, data = iris %>%
                    filter(Species == "setosa"), grid_size = 50)$data
light_interaction(fls_addnonadd$nonadditive, pairwise = pw, by = "Species", grid_size = 150, n_max = 150)$data %>%
  filter(Species %in% "setosa")
light_interaction(fls_w$nonadditive, pairwise = pw, grid_size = 150, n_max = 150)$data


#======================================
# ICE
#======================================

fit_full <- lm(Sepal.Length ~ ., data = iris)
fit_part <- lm(Sepal.Length ~ Petal.Length, data = iris)
mod_full <- flashlight(model = fit_full, label = "full", data = iris, y = "Sepal.Length")
mod_part <- flashlight(model = fit_part, label = "part", data = iris, y = "Sepal.Length")
mods <- multiflashlight(list(mod_full, mod_part))
grid <- expand.grid(Species = levels(iris$Species), Petal.Length = 2:4)

light_ice(mod_full, grid = grid)
plot(light_ice(mod_full, v = "Species"), alpha = 0.2)
indices <- (1:15) * 10
plot(light_ice(mod_full, v = "Species"), rotate_x = TRUE)
plot(light_ice(mods, v = "Species", indices = indices))
plot(light_ice(mods, v = "Species", indices = indices, center = "first"))
plot(light_ice(mods, v = "Species", indices = indices, center = "last"))
plot(light_ice(mods, v = "Petal.Width", n_bins = 5, indices = indices))
plot(light_ice(mods, v = "Petal.Width", by = "Species", n_bins = 5, indices = indices))
plot(light_ice(mods, v = "Petal.Width", by = "Species",
               n_bins = 5, indices = indices, center = "first"))

ir <- iris
ir$log_sl <- log(ir$Sepal.Length)
fit_lm <- lm(log_sl ~ Petal.Length + Petal.Width, data = ir)
fit_glm <- glm(Sepal.Length ~ Petal.Length + Petal.Width,
  data = ir, family = Gamma(link = log))
fl_lm <- flashlight(model = fit_lm, label = "lm", y = "log_sl", linkinv = exp)
fl_glm <- flashlight(model = fit_glm, label = "glm", y = "Sepal.Length",
  predict_function = function(m, X) predict(m, X, type = "response"))
fls <- multiflashlight(list(fl_lm, fl_glm), data = ir, w = "Sepal.Width")
plot(light_ice(fls, v = "Petal.Length", indices = indices))
plot(light_ice(fls, v = "Petal.Length", indices = indices, center = "first"))
plot(light_ice(fls, v = "Petal.Length", indices = indices, center = "middle"))
plot(light_ice(fls, v = "Petal.Length", indices = indices, center = "mean"))
plot(light_ice(fls, v = "Petal.Length", indices = indices, center = "0"))
plot(light_ice(fls, v = "Petal.Length", indices = indices, by = "Species"))
plot(light_ice(fls, v = "Petal.Length", indices = indices, center = "middle", by = "Species"))
plot(light_ice(fls, v = "Petal.Length", indices = indices, center = "mean", by = "Species"))
plot(light_ice(fls, v = "Petal.Length", indices = indices, center = "0", by = "Species"))
plot(light_ice(fls, v = "Petal.Length", indices = indices, use_linkinv = FALSE))
plot(light_ice(fls, v = "Petal.Length", indices = indices, use_linkinv = FALSE,
               center = "first"))
plot(light_ice(fls, v = "Petal.Length", indices = indices, use_linkinv = FALSE,
               center = "last"))

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
plot(light_profile(mods, v = "Species"), show_points = FALSE)
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

# ALE
plot(light_profile(mods, v = "Species", type = "ale"))
plot(light_profile(mods, v = "Petal.Width", type = "ale"))
plot(light_profile(mods, v = "Petal.Width", type = "ale"), swap_dim = TRUE)
plot(light_profile(mods, v = "Petal.Width", type = "ale", by = "Species"))
plot(light_profile(mods, v = "Petal.Width", type = "ale", by = "Species"), swap_dim = TRUE)

ir <- iris
ir$log_sl <- log(ir$Sepal.Length)
fit_lm <- lm(log_sl ~ Petal.Length + Petal.Width + Species, data = ir)
fit_glm <- glm(Sepal.Length ~ Petal.Length + Petal.Width + Species,
  data = ir, family = Gamma(link = log))
fl_lm <- flashlight(model = fit_lm, label = "lm", y = "log_sl", linkinv = exp)
fl_glm <- flashlight(model = fit_glm, label = "glm", y = "Sepal.Length",
  predict_function = function(m, X) predict(m, X, type = "response"))
fls <- multiflashlight(list(fl_lm, fl_glm), data = ir)

plot(light_profile(fls, v = "Petal.Length"))
plot(light_profile(fls, v = "Petal.Length", use_linkinv = FALSE))
plot(light_profile(fls, v = "Petal.Length", type = "ale",  use_linkinv = FALSE))


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
plot(x, use = c("ale"), zero_counts = T)
plot_counts(plot(x), x, alpha = 0.2)
plot_counts(plot(x, use = "response"), x, alpha = 0.2)
plot_counts(plot(x, zero_counts = FALSE), x, alpha = 0.2)
plot_counts(plot(x, use = "pd"), x, alpha = 0.2)

x <- light_effects(mod_full, v = "Petal.Width", stats = "quartiles")
plot(x)
plot(x, size_factor = 3)
plot_counts(plot(x), x, alpha = 0.2)
plot_counts(plot(x, use = "response"), x, alpha = 0.2)
plot_counts(plot(x, use = "response", zero_counts = F), x, alpha = 0.2) # same

x <- light_effects(mod_full, v = "Petal.Width", by = "Species")
plot(x)
p <- plot(x, zero_counts = FALSE, use = "all")
plot_counts(p, x, alpha = 0.2)

x <- light_effects(mod_full, v = "Petal.Width", by = "Species", stats = "quartiles")
plot(x)
plot_counts(plot(x), x, alpha = 0.2, digits = -1)
plot_counts(plot(x, use = "all", zero_counts = FALSE), x, alpha = 0.2)

x <- light_effects(mods, v = "Petal.Width")
plot(x)
plot_counts(plot(x), x, alpha = 0.2)
plot_counts(plot(x, zero_counts = FALSE), x, alpha = 0.2)
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
plot(light_effects(fls, v = "Petal.Length"), use = "all")
plot(light_effects(fls, v = "Petal.Length", use_linkinv = FALSE), use = "all")

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
plot(light_importance(mods, m_repetitions = 2))

plot(light_ice(mods, v = "Species"))
plot(light_ice(mods, v = "Petal.Length"))

plot(light_profile(mods, v = "Species"))
plot(light_profile(mods, v = "Species", type = "response"))
plot(light_profile(mods, v = "Petal.Length"))
plot(light_profile(mods, v = "Petal.Length", type = "ale"))
plot(light_profile(mods, v = "Petal.Length", type = "response"))

plot(light_effects(mods, v = "Species"))
x <- light_effects(mods, v = "Petal.Length", cut_type = "quantile")
plot_counts(plot(x, use = "all"), x, alpha = 0.2)
x <- light_effects(mods, v = "Petal.Length") # -> BAD
plot_counts(plot(x, use = "all"), x, alpha = 0.2)
plot(light_effects(mods, v = "Petal.Length"), use = "pd")
plot(light_effects(mods, v = "Petal.Length"), use = "ale")
plot(light_effects(mods, v = "Petal.Length", v_labels = FALSE))

plot(light_interaction(mods))

plot(light_global_surrogate(mods))

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
plot(light_profile(mods, v = "Petal.Length", type = "ale"))
plot(light_profile(mods, v = "Petal.Length", type = "ale", ale_two_sided = TRUE))
plot(light_profile(mods, v = "Petal.Length", breaks = seq(1.5, 6.5, by = 1)))
plot(light_profile(mods, v = "Petal.Length", type = "ale", breaks = seq(1.5, 6.5, by = 1)))
plot(light_profile(mods, v = "Petal.Length", pd_evaluate_at = 2:6))
plot(light_profile(mods, v = "Petal.Length", pd_evaluate_at = 2:6, type = "ale"))
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
plot(light_effects(flashlight(mods$full, w = "Sepal.Width"), v = "Species", pd_indices = c(1, 60, 70)))

# Different data: Use eigher fixed breaks or v_labels = FALSE
mod_full <- flashlight(model = fit_full, label = "full",
  data = iris[1:75, ], y = "Sepal.Length")
mod_part <- flashlight(model = fit_part, label = "part",
  data = iris[76:150, ], y = "Sepal.Length")
mods <- multiflashlight(list(mod_full, mod_part))
plot(light_effects(mods, v = "Petal.Length", breaks = 0:8), use = "all")
eff <- light_effects(mods, v = "Petal.Length")
plot_counts(plot(eff), eff, show_labels = FALSE)
plot_counts(plot(eff, zero_counts = FALSE, use = "all"), eff)

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
plot(light_ice(fl, v = "Species", center = "first"))
plot(light_profile(fl, v = "Species"))
plot(light_profile(fl, v = "Species", type = "response"))
plot(light_profile(fl, v = "Species", type = "ale"))

# No by variable: zero_counts did not work
eff <- light_effects(fl, v = "Species")
# eff <- light_effects(fl, v = "Species", stats = "quartiles")

(p <- plot(eff, zero_counts = FALSE, use = "all"))
plot_counts(p, eff, alpha = 0.2) # fixed

(p <- plot(eff, use = "all"))
plot_counts(p, eff, alpha = 0.2) # fixed

# With by variable: zero_counts did not work
eff <- light_effects(fl, v = "Species", by = "sw")
# eff <- light_effects(fl, v = "Species", by = "sw", stats = "quartiles")

(p <- plot(eff, zero_counts = FALSE, use = "all"))
plot_counts(p, eff, alpha = 0.2) # fixed -> check calibration of ALE

# With different facet scale
(p <- plot(eff, zero_counts = FALSE, facet_scales = "fixed", use = "all"))
plot_counts(p, eff, alpha = 0.2, facet_scales = "fixed")

(p <- plot(eff))
plot_counts(p, eff, alpha = 0.2) # fixed

# Use only pd
(p <- plot(eff, use = "pd"))
plot_counts(p, eff, alpha = 0.2) # fixed

# Use only ALE
(p <- plot(eff, use = "ale"))
plot_counts(p, eff, alpha = 0.2) # fixed

(p <- plot(eff, use = "pd", zero_counts = FALSE))
plot_counts(p, eff, alpha = 0.2) # fixed

# Use only response
(p <- plot(eff, use = "response"))
plot_counts(p, eff, alpha = 0.2) # fixed

(p <- plot(eff, use = "response", zero_counts = FALSE))
plot_counts(p, eff, alpha = 0.2) # fixed

# Use only predicted
(p <- plot(eff, use = "predicted"))
plot_counts(p, eff, alpha = 0.2) # fixed

(p <- plot(eff, use = "predicted", zero_counts = FALSE))
plot_counts(p, eff, alpha = 0.2) # fixed

# GLOBAL SURROGATE
fit1 <- lm(Sepal.Length ~ ., data = iris)
fit2 <- lm(Sepal.Length ~ Petal.Length, data = iris)
fl1 <- flashlight(model = fit1, label = "full")
fl2 <- flashlight(model = fit2, label = "partial")
fls <- multiflashlight(list(fl1, fl2), data = iris, y = "Sepal.Length")
surr <- light_global_surrogate(fls)
plot(surr, cex = 0.6)
plot(light_global_surrogate(fls$full))
plot(light_global_surrogate(fls$full, by = "Species"))

# INTERACTION SPECIAL CHECKS
library(iml)
library(ggplot2)
library(gbm)

# Generate data
n <- 1000
set.seed(326)
data <- data.frame(
  v = runif(n),
  x = runif(n, -1, 1),
  z = sample(0:1, n, replace = TRUE, prob = c(0.5, 0.5)),
  w = sample(1:3, n, replace = TRUE),
  wunif = 1)
data$y <- with(data, sin(v) + x * z + rnorm(n))

fit <- gbm(y ~ ., data = data, n.trees = 100, n.cores = 8,
           interaction.depth = 5, shrinkage = 0.05)
pf <-  function(model, newdata) {
  predict(model, newdata, n.trees = model$n.trees)
}
sDat <- data[1:100, ]

fl <- flashlight(model = fit, data = sDat, y = "y", label ="lm",
                 predict_function = pf) # 0.478
X <- sDat[, setdiff(colnames(data), "y")]
Y <- sDat[, "y"]
predictor = Predictor$new(fit, data = X, y = Y, predict.fun = pf)

# Pairwise
interact.gbm(fit, data = sDat, i.var = c("x", "v"))
interact.gbm(fit, data = sDat, i.var = c("x", "z"))
light_interaction(fl, pairwise = TRUE, seed = 4)$data
Interaction$new(predictor, feature = "x")

## Overall
light_interaction(fl, seed = 4)$data
Interaction$new(predictor)
light_interaction(fl, data = head(sDat, 30), normalize = F)$data
light_interaction(fl, data = head(sDat, 30), type = "ice", normalize = F)$data

# Weighted
light_interaction(flashlight(fl, w = "w"), seed = 4)$data
light_interaction(flashlight(fl, w = "w"), pairwise = TRUE, seed = 4)$data

# Grouped
light_interaction(fl, by = "z", seed = 4)$data
light_interaction(fl, by = "z", pairwise = TRUE, seed = 4)$data
light_interaction(flashlight(fl, w = "wunif"), seed = 4, by = "z")$data
light_interaction(flashlight(fl, w = "wunif"), pairwise = TRUE, seed = 4, by = "z")$data



# SHAP CHECKS
fit1 <- lm(Sepal.Length ~ ., data = iris)
fit2 <- lm(Sepal.Length ~ . + Species:Petal.Length, data = iris)
fl1 <- flashlight(model = fit1, label = "additive")
fl2 <- flashlight(model = fit2, label = "non-additive")
fls <- multiflashlight(list(fl1, fl2), data = iris, y = "Sepal.Length")
fls <- add_shap(fls)
plot(light_importance(fls, type = "shap"))
plot(light_scatter(fls, v = "Petal.Length", type = "shap"), alpha = 0.2)
plot(light_scatter(fls, v = "Species", type = "shap"), alpha = 0.2)
plot(light_scatter(fls, v = "Petal.Length", type = "shap", by = "Species"), alpha = 0.2)


#========================
# DIAMONDS
#========================

library(ggplot2)
library(ranger)
diamonds$log_price <- log(diamonds$price)
.ind <- c(caret::createDataPartition(diamonds$price, p = 0.2, list = FALSE))
test <- diamonds[.ind, ]
train <- diamonds[-.ind, ]
fit_glm <- glm(price ~ log(carat) + color + clarity + cut, data = train, family = Gamma(link = "log"))
fit_rf <- ranger(log_price ~ carat + color + clarity + cut, data = train)
x <- c("carat", "color", "clarity", "cut")

fl_glm <- flashlight(model = fit_glm, label = "glm")
fl_rf <- flashlight(model = fit_rf, label = "rf",
                    predict_function = function(m, X) predict(m, X)$predictions)
fls <- multiflashlight(list(fl_glm, fl_rf), data = test, linkinv = exp, y = "log_price",
                       metrics = list(rmse = rmse, r_squared = r_squared))
predict(fls, data = head(test))
plot(light_performance(fls), fill = "darkgreen")
plot(light_importance(fls, v = x), fill = "darkgreen")
plot(light_interaction(fls, v = x, pairwise = TRUE), fill = "darkgreen")

li <- light_effects(fls, v = "color")
plot_counts(plot(li, use = "all"), li, alpha = 0.2)

li <- light_effects(fls, v = "cut")
plot_counts(plot(li, use = "all"), li, alpha = 0.2)

li <- light_effects(fls, v = "clarity")
plot_counts(plot(li, use = "all"), li, alpha = 0.2)

li <- light_effects(fls, v = "carat", breaks = c(seq(0.1, 1, 0.1), 2, 6))
plot_counts(plot(li, use = "all") +
              coord_cartesian(ylim = c(-10000, 20000)), li, alpha = 0.2)

