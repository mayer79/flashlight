context("light_performance")

test_that("basic functionality works", {
  fit <- lm(Sepal.Length ~ ., data = iris)
  fl <- flashlight(model = fit, label = "lm", data = iris,
                   y = "Sepal.Length", metrics = list(r2 = r_squared))
  perf <- light_performance(fl)
  expect_equal(perf$data$value, summary(fit)$r.squared, tolerance = 0.001)
  expect_true(inherits(plot(perf), "ggplot"))
})

fit <- lm(Sepal.Length ~ ., data = iris)
fl <- flashlight(model = fit, label = "lm", data = iris, y = "Sepal.Length")

test_that("light_performance works for one flashlight with one metric and no by", {
  perf <- light_performance(fl)
  expect_equal(perf$data$value, 0.301, tolerance = 0.001)
  expect_true(inherits(plot(perf), "ggplot"))
})

test_that("light_performance works for one flashlight with multiple metrics and no by", {
  perf <- light_performance(fl, metrics = list(mae = MetricsWeighted::mae, rmse = rmse, r2 = r_squared))
  expect_equal(dim(perf$data), c(3, 3))
  expect_equal(perf$data$value, c(0.243, 0.301, 0.867), tolerance = 0.001)
  expect_true(inherits(plot(perf), "ggplot"))
})

test_that("light_performance works for one flashlight with one metric and a by variable", {
  perf <- light_performance(fl, by = "Species")
  expect_equal(dim(perf$data), c(3, 4))
  expect_equal(perf$data$value[1], 0.254, tolerance = 0.001)
  expect_true(inherits(plot(perf), "ggplot"))
})

test_that("light_performance works for one flashlight with multiple metrics and a by variable", {
  perf <- light_performance(fl, by = "Species",
                            metrics = list(mae = MetricsWeighted::mae, rmse = rmse, r2 = r_squared))
  expect_equal(dim(perf$data), c(9, 4))
  expect_equal(perf$data$value[2], 0.254, tolerance = 0.001)
  expect_true(inherits(plot(perf), "ggplot"))
})

test_that("light_performance works for weighted flashlight with one metric and a by variable", {
  fl_weighted <- flashlight(fl, w = "Petal.Length")
  perf_weighted <- light_performance(fl_weighted, by = "Species")
  expect_equal(dim(perf_weighted$data), c(3, 4))
  expect_equal(perf_weighted$data$value[2], 0.3358, tolerance = 0.001)
  expect_true(inherits(plot(perf_weighted), "ggplot"))
})

fit1 <- lm(Sepal.Length ~ ., data = iris)
fit2 <- lm(Sepal.Length ~ Petal.Length, data = iris)
fl1 <- flashlight(model = fit1, label = "full")
fl2 <- flashlight(model = fit2, label = "single")
fls <- multiflashlight(list(fl1, fl2), data = iris, y = "Sepal.Length")

test_that("light_performance works for multi-flashlight with one metric and no by", {
  perf <- light_performance(fls)
  expect_equal(perf$data$value, c(0.3006270, 0.4043516), tolerance = 0.001)
  expect_equal(as.character(perf$data$label), c("full", "single"))
  expect_true(inherits(plot(perf), "ggplot"))
})

test_that("light_performance works for multi-flashlight with multiple metrics and no by", {
  perf <- light_performance(fls, metrics = list(mae = MetricsWeighted::mae, rmse = rmse, r2 = r_squared))
  expect_equal(dim(perf$data), c(6, 3))
  expect_equal(perf$data$value[1:3], c(0.243, 0.301, 0.867), tolerance = 0.001)
  expect_true(inherits(plot(perf), "ggplot"))
})

test_that("light_performance works for multi-flashlight with one metric and a by variable", {
  perf <- light_performance(fls, by = "Species")
  expect_equal(dim(perf$data), c(6, 4))
  expect_equal(perf$data$value[1], 0.254, tolerance = 0.001)
  expect_true(inherits(plot(perf), "ggplot"))
})

test_that("light_performance works for multi-flashlight with multiple metrics and a by variable", {
  perf <- light_performance(fls, by = "Species",
                            metrics = list(mae = MetricsWeighted::mae, rmse = rmse, r2 = r_squared))
  expect_equal(dim(perf$data), c(18, 4))
  expect_equal(perf$data$value[2], 0.254, tolerance = 0.001)
  expect_true(inherits(plot(perf), "ggplot"))
})

test_that("light_performance works for weighted multi-flashlight with one metric and a by variable", {
  fls_weighted <- multiflashlight(fls, w = "Petal.Length")
  perf_weighted <- light_performance(fls_weighted, by = "Species")
  expect_equal(dim(perf_weighted$data), c(6, 4))
  expect_equal(perf_weighted$data$value[2], 0.3358, tolerance = 0.001)
  expect_true(inherits(plot(perf_weighted), "ggplot"))
})

test_that("R-squared for weighted flashlight is the same as the one from summary.lm", {
  fit <- lm(Sepal.Length ~ ., data = iris, weights = iris$Sepal.Width)
  fl <- flashlight(model = fit, label = "lm", data = iris, w = "Sepal.Width",
                   y = "Sepal.Length", metrics = list(r2 = r_squared))
  perf <- light_performance(fl)
  expect_equal(perf$data$value, summary(fit)$r.squared, tolerance = 0.001)
  expect_true(inherits(plot(perf), "ggplot"))
})

