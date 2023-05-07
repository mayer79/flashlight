fit_additive <- lm(Sepal.Length ~ Petal.Length + Petal.Width + Species, data = iris)
fit_nonadditive <- lm(Sepal.Length ~ Petal.Length * Petal.Width + Species, data = iris)
fl_additive <- flashlight(model = fit_additive, label = "additive")
fl_nonadditive <- flashlight(model = fit_nonadditive, label = "nonadditive")
fls <- multiflashlight(list(fl_additive, fl_nonadditive), data = iris, y = "Sepal.Length")

test_that("basic functionality works for light_interaction", {
  inter <- light_interaction(fls$additive)
  expect_equal(inter$data$value, rep(0, 4))
  expect_true(inherits(plot(inter), "ggplot"))

  inter <- light_interaction(fls$nonadditive)
  expect_equal(inter$data$value,
               c(0, 0.0421815, 0.0421815, 0), tolerance = 0.001)
  expect_true(inherits(plot(inter), "ggplot"))
})

test_that("light_interaction reacts on weights", {
  inter <- light_interaction(flashlight(fls$nonadditive, w = "Sepal.Width"))
  expect_equal(inter$data$value,
               c(0.03917691, 0.03917691, 0), tolerance = 0.001)
  expect_true(inherits(plot(inter), "ggplot"))
})

test_that("basic functionality works for light_interaction (ICE approach)", {
  inter <- light_interaction(fls$additive, type = "ice")
  expect_equal(inter$data$value, rep(0, 4))

  inter <- light_interaction(fls$nonadditive, type = "ice")
  expect_equal(inter$data$value,
               c(0, 0.0274, 0.843, 0), tolerance = 0.001)
  expect_true(inherits(plot(inter), "ggplot"))
})

test_that("basic functionality works for light_interaction with pairwise interactions", {
  inter <- light_interaction(fls$additive, pairwise = TRUE)
  expect_equal(inter$data$value, rep(0, 6))
  expect_true(inherits(plot(inter), "ggplot"))

  inter <- light_interaction(fls$nonadditive, pairwise = TRUE)
  expect_equal(inter$data$value[4], 0.0207711, tolerance = 0.001)
  expect_equal(inter$data$value[-4], rep(0, 5))
  expect_true(inherits(plot(inter), "ggplot"))
  expect_error(light_interaction(fls$additive, pairwise = TRUE, type = "ice"))
})

test_that("light_interaction reacts on 'normalize'", {
  inter <- light_interaction(fls$nonadditive, normalize = FALSE)
  expect_equal(inter$data$value,
               c(0, 0.031848, 0.031848, 0), tolerance = 0.001)

  inter <- light_interaction(fls$nonadditive, normalize = FALSE, pairwise = TRUE)
  expect_equal(inter$data$value[4], 0.03184801, tolerance = 0.001)

  inter <- light_interaction(fls$nonadditive, normalize = FALSE, type = "ice")
  expect_equal(inter$data$value,
               c(0, 0.043085, 0.043085, 0), tolerance = 0.001)
})

test_that("light_interaction reacts on 'take_sqrt'", {
  inter <- light_interaction(fls$nonadditive, take_sqrt = FALSE)
  expect_equal(inter$data$value,
               c(0, 0.00178, 0.00178, 0), tolerance = 0.001)

  inter <- light_interaction(fls$nonadditive, take_sqrt = FALSE, pairwise = TRUE)
  expect_equal(inter$data$value[4], 0.00043143, tolerance = 0.001)

  inter <- light_interaction(fls$nonadditive, take_sqrt = FALSE, type = "ice")
  expect_equal(inter$data$value,
               c(0, 0.00075, 0.71004, 0), tolerance = 0.001)
})

test_that("light_interaction reacts on multiflashlight", {
  inter <- light_interaction(fls, type = "ice")
  expect_equal(inter$data$value, c(rep(0, 5), 0.0274, 0.843, 0), tolerance = 0.001)
  expect_true(inherits(plot(inter), "ggplot"))

  inter <- light_interaction(fls, pairwise = TRUE)
  expect_equal(inter$data$value[inter$data$label == "additive"], rep(0, 6))
  expect_equal(inter$data$value[inter$data$label == "nonadditive"][4],
               0.0207711, tolerance = 0.001)
})

test_that("light_interaction reacts on 'by'", {
  inter <- light_interaction(fls$nonadditive, by = "Species")
  dat <- inter$data
  expect_equal(dat$value[dat$variable == "Sepal.Width"], rep(0, 3))
  expect_equal(dat$value[dat$variable %in% c("Petal.Width", "Petal.Length")],
               rep(c(0.00404, 0.007588311, 0.0077), each = 2L), tolerance = 0.001)
  expect_true(inherits(plot(inter), "ggplot"))
})

test_that("Options work", {
  fit <- lm(Sepal.Length ~ ., data = iris)
  fl <- flashlight(
    model = fit,
    label = "lm",
    data = iris,
    y = "Sepal.Length",
    metrics = list(r2 = MetricsWeighted::r_squared)
  )
  new_options = list(
    flashlight.label_name = "ell",
    flashlight.variable_name = "var",
    flashlight.value_name = "val",
    flashlight.error_name = "err"
  )
  withr::with_options(new_options, {
    inter <- light_interaction(fl)
    expect_true(all(c("ell", "var", "val", "err") %in% colnames(inter$data)))
    expect_true(inherits(plot(inter), "ggplot"))
  })
})
