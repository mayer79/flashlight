fit <- stats::lm(Sepal.Length ~ ., data = iris)
fl <- flashlight(
  model = fit,
  label = "full",
  data = iris,
  y = "Sepal.Length",
  metrics = list(rmse = MetricsWeighted::rmse, r2 = MetricsWeighted::r_squared)
)
perf <- light_performance(fl)
perf_by <- light_performance(fl, by = "Species")
RMSE <- function(r) sqrt(mean(r^2))
get_rmse_r2 <- function(fit) c(RMSE(stats::resid(fit)), summary(fit)$r.squared)

test_that("basic functionality works for two metrics", {
  expect_equal(dim(perf$data), c(2L, 3L))
  expect_equal(perf$data$value, get_rmse_r2(fit))
  expect_s3_class(plot(perf), "ggplot")
})

test_that("light_performance works with by variable", {
  expect_equal(dim(perf_by$data), c(3 * nrow(perf$data), ncol(perf$data) + 1L))
  expect_equal(
    perf_by$data$value[c(1L, 3L, 5L)],
    as.numeric(tapply(stats::resid(fit), iris$Species, FUN = RMSE))
  )
  expect_s3_class(plot(perf_by), "ggplot")
})

test_that("light_performance works for weighted flashlight", {
  iris_w <- transform(iris, w1 = 1, w2 = 2, w3 = 1:nrow(iris))
  perf_w1 <- light_performance(flashlight(fl, w = "w1", data = iris_w))
  perf_w2 <- light_performance(flashlight(fl, w = "w2", data = iris_w), by = "Species")
  perf_w3 <- light_performance(flashlight(fl, w = "w3", data = iris_w))

  expect_equal(perf, perf_w1)
  expect_equal(perf_by, perf_w2)
  expect_false(identical(perf, perf_w3))

  expect_s3_class(plot(perf_w1), "ggplot")
  expect_s3_class(plot(perf_w2), "ggplot")
  expect_s3_class(plot(perf_w3), "ggplot")
})

test_that("R-squared for weighted flashlight is the same as the one from summary.lm", {
  fit_w <- stats::lm(Sepal.Length ~ ., data = iris, weights = iris$Sepal.Width)
  fl_w <- flashlight(
    model = fit,
    label = "wlm",
    data = iris,
    w = "Sepal.Width",
    y = "Sepal.Length",
    metrics = list(r2 = MetricsWeighted::r_squared)
  )
  perf_w <- light_performance(fl_w)
  expect_equal(perf_w$data$value, summary(fit_w)$r.squared, tolerance = 0.01)
  expect_s3_class(plot(perf_w), "ggplot")
})

test_that("Options work", {
  new_options = list(
    flashlight.label_name = "ell",
    flashlight.metric_name = "mm",
    flashlight.value_name = "vv"
  )
  withr::with_options(new_options, {
    perf <- light_performance(fl)
    expect_true(all(c("ell", "mm", "vv") %in% colnames(perf$data)))
  })
  expect_s3_class(plot(perf), "ggplot")
})

