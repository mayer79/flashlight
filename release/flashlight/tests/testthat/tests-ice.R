context("light_ice")

test_that("basic functionality and n_max work", {
  fit <- lm(Sepal.Length ~ Species + 0, data = iris)
  fl <- flashlight(model = fit, label = "lm", data = iris, y = "Sepal.Length")
  ice <- light_ice(fl, v = "Species", n_max = 1)
  expect_equal(as.numeric(ice$data$value), as.numeric(coef(fit)))
  expect_true(inherits(plot(ice), "ggplot"))
})

test_that("by functionality, indices and evaluate_at work", {
  fit <- lm(Sepal.Length ~ Species * Petal.Width, data = iris)
  pred <- predict(fit, expand.grid(Petal.Width = c(0.1, 0.3, 0.5),
                                   Species = levels(iris$Species)))
  fl <- flashlight(model = fit, label = "lm", data = iris, y = "Sepal.Length")
  ice <- light_ice(fl, v = "Petal.Width", indices = c(1, 51, 101), by = "Species",
                   evaluate_at = c(0.1, 0.3, 0.5))
  expect_equal(as.numeric(ice$data$value), as.numeric(pred))
  expect_true(inherits(plot(ice), "ggplot"))
})

fit <- lm(Sepal.Length ~ Species, data = iris)
fl <- flashlight(model = fit, label = "lm", data = iris, y = "Sepal.Length")

test_that("center first work", {
  ice <- light_ice(fl, v = "Species", n_max = 1, center = "first")
  expect_equal(as.numeric(ice$data$value)[-1], as.numeric(coef(fit))[-1])
  expect_true(inherits(plot(ice), "ggplot"))
})

test_that("center 0 and n_max work", {
  ice <- light_ice(fl, v = "Species", n_max = 10, center = "0")
  expect_equal(dim(ice$data), c(30L, 4L))
  expect_equal(mean(ice$data$value), 0)
  expect_true(inherits(plot(ice), "ggplot"))
})

test_that("basic functionality works for multiflashlight", {
  fit1 <- lm(Sepal.Length ~ Species + 0, data = iris)
  fl1 <- flashlight(model = fit1, label = "Species", data = iris, y = "Sepal.Length")
  fit2 <- lm(Sepal.Length ~ 1, data = iris)
  fl2 <- flashlight(model = fit2, label = "Empty", data = iris, y = "Sepal.Length")
  fls <- multiflashlight(list(fl1, fl2))

  ice <- light_ice(fls, v = "Species", n_max = 1)
  expect_equal(as.numeric(ice$data$value)[1:3], as.numeric(coef(fit1)))
  expect_equal(as.numeric(ice$data$value)[4:6], rep(mean(iris$Sepal.Length), 3))
  expect_true(inherits(plot(ice), "ggplot"))

  ice <- light_ice(fls, v = "Petal.Length", indices = 1, n_bins = 2)
  expect_equal(ice$data$id[1], 1)
  expect_equal(as.numeric(ice$data$value[1:2]), rep(mean(iris$Sepal.Length[1:50]), 2))
  expect_equal(as.numeric(ice$data$value[3:4]), rep(mean(iris$Sepal.Length), 2))
  expect_true(inherits(plot(ice), "ggplot"))
})

test_that("weights have no impact on results", {
  fit <- lm(Sepal.Length ~ Species + 0, data = iris)
  fl <- flashlight(model = fit, label = "lm", data = iris, y = "Sepal.Length")
  ice <- light_ice(fl, v = "Species", indices = 1:3)

  fl_weighted <- flashlight(model = fit, label = "weighted by Petal.Length",
                            data = iris, y = "Sepal.Length", w = "Petal.Length")
  ice_weighted <- light_ice(fl_weighted, v = "Species", indices = 1:3)

  expect_equal(ice$value, ice_weighted$value)
})

test_that("Options work", {
  fit <- lm(Sepal.Length ~ Species + 0, data = iris)
  fl <- flashlight(model = fit, label = "lm", data = iris, y = "Sepal.Length")

  new_options = list(
    flashlight.label_name = "ell",
    flashlight.value_name = "val",
    flashlight.id_name = "iii"
  )
  withr::with_options(new_options, {
    ice <- light_ice(fl, v = "Species", indices = 1:3)
    expect_true(all(c("ell", "val", "iii") %in% colnames(ice$data)))
    expect_true(inherits(plot(ice), "ggplot"))
  })
})
