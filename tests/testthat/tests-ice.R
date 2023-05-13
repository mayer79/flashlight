test_that("basic functionality and n_max work", {
  fit <- stats::lm(Sepal.Length ~ Species + 0, data = iris)
  fl <- flashlight(model = fit, label = "lm", data = iris, y = "Sepal.Length")
  ice <- light_ice(fl, v = "Species", n_max = 1)
  expect_equal(as.numeric(ice$data$value_), as.numeric(coef(fit)))
  expect_s3_class(plot(ice), "ggplot")
})

test_that("by functionality, indices and evaluate_at work", {
  fit <- stats::lm(Sepal.Length ~ Species * Petal.Width, data = iris)
  pred <- predict(
    fit, expand.grid(Petal.Width = c(0.1, 0.3, 0.5), Species = levels(iris$Species))
  )
  fl <- flashlight(model = fit, label = "lm", data = iris, y = "Sepal.Length")
  ice <- light_ice(
    fl,
    v = "Petal.Width",
    indices = c(1, 51, 101),
    by = "Species",
    evaluate_at = c(0.1, 0.3, 0.5)
  )
  expect_equal(as.numeric(ice$data$value_), as.numeric(pred))
  expect_s3_class(plot(ice), "ggplot")
})

fit <- stats::lm(Sepal.Length ~ Species, data = iris)
fl <- flashlight(model = fit, label = "lm", data = iris, y = "Sepal.Length")

test_that("center first work", {
  ice <- light_ice(fl, v = "Species", n_max = 1L, center = "first")
  expect_equal(as.numeric(ice$data$value_)[-1L], as.numeric(coef(fit))[-1L])
  expect_s3_class(plot(ice), "ggplot")
})

test_that("center 0 and n_max work", {
  ice <- light_ice(fl, v = "Species", n_max = 10L, center = "0")
  expect_equal(dim(ice$data), c(30L, 4L))
  expect_equal(mean(ice$data$value_), 0)
  expect_s3_class(plot(ice), "ggplot")
})

test_that("basic functionality works for multiflashlight", {
  fit1 <- stats::lm(Sepal.Length ~ Species + 0, data = iris)
  fl1 <- flashlight(model = fit1, label = "Species", data = iris, y = "Sepal.Length")
  fit2 <- stats::lm(Sepal.Length ~ 1, data = iris)
  fl2 <- flashlight(model = fit2, label = "Empty", data = iris, y = "Sepal.Length")
  fls <- multiflashlight(list(fl1, fl2))

  ice <- light_ice(fls, v = "Species", n_max = 1)
  expect_equal(as.numeric(ice$data$value_)[1:3], as.numeric(coef(fit1)))
  expect_equal(as.numeric(ice$data$value_)[4:6], rep(mean(iris$Sepal.Length), 3))
  expect_s3_class(plot(ice), "ggplot")

  ice <- light_ice(fls, v = "Petal.Length", indices = 1L, n_bins = 2L)
  expect_equal(ice$data$id_[1L], 1)
  expect_equal(as.numeric(ice$data$value_[1:2]), rep(mean(iris$Sepal.Length[1:50]), 2))
  expect_equal(as.numeric(ice$data$value_[3:4]), rep(mean(iris$Sepal.Length), 2))
  expect_s3_class(plot(ice), "ggplot")
})

test_that("weights have no impact on results", {
  fit <- stats::lm(Sepal.Length ~ Species + 0, data = iris)
  fl <- flashlight(model = fit, label = "lm", data = iris, y = "Sepal.Length")
  ice <- light_ice(fl, v = "Species", indices = 1:3)

  fl_weighted <- flashlight(
    model = fit,
    label = "weighted by Petal.Length",
    data = iris,
    y = "Sepal.Length",
    w = "Petal.Length"
  )
  ice_weighted <- light_ice(fl_weighted, v = "Species", indices = 1:3)

  expect_equal(ice$data$value_, ice_weighted$data$value_)
})
