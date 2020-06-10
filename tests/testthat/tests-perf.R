context("light_performance")

fit_full <- lm(Sepal.Length ~ ., data = iris)
fit_part <- lm(Sepal.Length ~ Petal.Length, data = iris)
mod_full <- flashlight(model = fit_full, label = "full", data = iris, y = "Sepal.Length")
mod_part <- flashlight(model = fit_part, label = "part", data = iris, y = "Sepal.Length")
mods <- multiflashlight(list(mod_full, mod_part), by = "Species")

test_that("light_performance works for one flashlight with one metric and no by", {
  fit <- lm(Sepal.Length ~ ., data = iris)
  perf <- light_performance(flashlight(model = fit, label = "lm",
                                       data = iris, y = "Sepal.Length"))
  expect_equal(perf$data$value, 0.301, tolerance = 0.001)
  expect_true(inherits(plot(perf), "ggplot"))
})

test_that("light_performance works for multi-flashlight with one metric and no by", {
  fit1 <- lm(Sepal.Length ~ ., data = iris)
  fit2 <- lm(Sepal.Length ~ Petal.Length, data = iris)
  fl1 <- flashlight(model = fit1, label = "full")
  fl2 <- flashlight(model = fit2, label = "single")
  fls <- multiflashlight(list(fl1, fl2), data = iris, y = "Sepal.Length")
  perf <- light_performance(fls)
  expect_equal(perf$data$value, c(0.3006270, 0.4043516), tolerance = 0.001)
  expect_equal(as.character(perf$data$label), c("full", "single"))
  expect_true(inherits(plot(perf), "ggplot"))
})
