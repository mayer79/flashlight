context("light_profile, light_effects and light_scatter")

fit <- lm(Sepal.Length ~ Species + 0, data = iris)
fl <- flashlight(model = fit, label = "lm", data = iris, y = "Sepal.Length")

test_that("light_profile works correctly for type response", {
  pr <- light_profile(fl, v = "Species", type = "response")
  expect_equal(pr$data$value,
               aggregate(Sepal.Length ~ Species, data = iris, FUN = mean)$Sepal.Length)
  expect_true(inherits(plot(pr), "ggplot"))
})

test_that("breaks work", {
  pr <- light_profile(fl, v = "Petal.Length", type = "response", breaks = c(1, 4, 7))
  expect_equal(pr$data$value,
               aggregate(Sepal.Length ~ Petal.Length > 4, data = iris, FUN = mean)$Sepal.Length)
  expect_true(inherits(plot(pr), "ggplot"))
})

test_that("n_bins work", {
  pr <- light_profile(fl, v = "Petal.Length", type = "response", n_bins = 2)
  expect_equal(dim(pr$data), c(2L, 5L))
})

test_that("v_labels work", {
  pr <- light_profile(fl, v = "Petal.Length", type = "response",
                      n_bins = 2, v_labels = FALSE)
  expect_true(is.numeric(pr$data$Petal.Length))
})

test_that("light_profile works correctly for type predicted", {
  pr <- light_profile(fl, v = "Species", type = "predicted")
  expect_equal(pr$data$value,
               aggregate(Sepal.Length ~ Species, data = iris, FUN = mean)$Sepal.Length)
  expect_true(inherits(plot(pr), "ggplot"))
})

test_that("light_profile uses pred", {
  pr <- light_profile(fl, v = "Species", type = "predicted", pred = rep(1:3, each = 50))
  expect_equal(pr$data$value, 1:3)
  fls <- multiflashlight(list(fl, fl))
  expect_error(light_profile(fls, v = "Species", type = "predicted",
                             pred = rep(1:3, each = 50)))
})

test_that("light_profile works correctly for type residual", {
  pr <- light_profile(fl, v = "Species", type = "residual")
  expect_equal(pr$data$value, c(0, 0, 0))
  expect_true(inherits(plot(pr), "ggplot"))
})

test_that("partial dependence is the same as ice", {
  pr <- light_profile(fl, v = "Species", indices = 1)
  ice <- light_ice(fl, v = "Species", indices = 1)
  expect_equal(pr$data$value, unname(ice$data$value))
})

test_that("partial dependence gives correct output for model with one covariable", {
  pr <- light_profile(fl, v = "Species")
  expect_equal(as.numeric(pr$data$value), as.numeric(coef(fit)))
})

test_that("partial dependence is constant if covariable not in model", {
  pr <- light_profile(fl, v = "Petal.Length")
  expect_true(var(pr$data$value) == 0)
})

test_that("ale gives correct output for model with one covariable", {
  pr <- light_profile(fl, v = "Species", type = "ale")
  expect_equal(pr$data$value, as.numeric(coef(fit)))
})

test_that("ale is constant if covariable not in model", {
  pr <- light_profile(fl, v = "Petal.Length", type = "ale")
  expect_true(var(pr$data$value) == 0)
})

test_that("light_profile works correctly for type 'response' with by variable", {
  fit <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
  fl <- flashlight(model = fit, label = "lm", data = iris, y = "Sepal.Length")
  pr <- light_profile(fl, v = "Petal.Length", type = "response", by = "Species", breaks = c(1, 4, 7))
  expect_equal(pr$data$value,
               aggregate(Sepal.Length ~ Species + (Petal.Length > 4), data = iris, FUN = mean)$Sepal.Length)
  expect_true(inherits(plot(pr), "ggplot"))

  setosa1 <- light_profile(fl, v = "Petal.Length", breaks = c(1, 4, 7), data = iris[1:50, ])
  setosa2 <- light_profile(fl, v = "Petal.Length", breaks = c(1, 4, 7), by = "Species")
  expect_equal(setosa1$data$value, setosa2$data$value[1:2])
})

test_that("light_profile works correctly for type 'ale' with by variable", {
  fit <- lm(Sepal.Length ~ Species + Petal.Width, data = iris)
  fl <- flashlight(model = fit, label = "lm", data = iris, y = "Sepal.Length")
  ale <- light_profile(fl, v = "Petal.Width", type = "ale", by = "Species")
  expect_true(inherits(plot(ale), "ggplot"))
  expect_true(is.light(ale))
})

test_that("basic functionality works for multiflashlight", {
  fit1 <- lm(Sepal.Length ~ Species + 0, data = iris)
  fl1 <- flashlight(model = fit1, label = "Species", data = iris, y = "Sepal.Length")
  fit2 <- lm(Sepal.Length ~ 1, data = iris)
  fl2 <- flashlight(model = fit2, label = "Empty", data = iris, y = "Sepal.Length")
  fls <- multiflashlight(list(fl1, fl2))

  pr <- light_profile(fls, v = "Species")
  expect_true(all(dim(pr$data) == c(6L, 5L)))
  expect_true(all(pr$data$value[4:6] != pr$data$value[1:3]))
  expect_equal(pr$data$value[4:6], rep(mean(iris$Sepal.Length), 3))
  expect_true(inherits(plot(pr), "ggplot"))

  pr <- light_profile(fls, v = "Species", type = "predicted")
  expect_true(all(dim(pr$data) == c(6L, 5L)))
  expect_true(all(pr$data$value[4:6] != pr$data$value[1:3]))
  expect_equal(pr$data$value[4:6], rep(mean(iris$Sepal.Length), 3))
  expect_true(inherits(plot(pr), "ggplot"))

  pr <- light_profile(fls, v = "Species", type = "ale")
  expect_true(all(dim(pr$data) == c(6L, 5L)))
  expect_true(all(pr$data$value[4:6] != pr$data$value[1:3]))
  expect_equal(pr$data$value[4:6], rep(mean(iris$Sepal.Length), 3))
  expect_true(inherits(plot(pr), "ggplot"))
})

test_that("light_profile reacts on weights", {
  fit <- lm(Sepal.Length ~ 1, data = iris)
  fl <- flashlight(model = fit, label = "empty", data = iris, y = "Sepal.Length", w = "Sepal.Width")
  pr <- light_profile(fl, v = "Species", type = "response")
  expect_equal(pr$data$value[1], with(iris[1:50, ], weighted.mean(Sepal.Length, Sepal.Width)))
})

fit1 <- lm(Sepal.Length ~ ., data = iris)
fl1 <- flashlight(model = fit1, label = "full", data = iris, y = "Sepal.Length")
fit2 <- lm(Sepal.Length ~ 1, data = iris)
fl2 <- flashlight(model = fit2, label = "Empty", data = iris, y = "Sepal.Length")
fls <- multiflashlight(list(fl1, fl2))

test_that("light_effects works and produces same as individual profiles", {
  eff <- light_effects(fls, v = "Petal.Length")

  pd <- light_profile(fls, v = "Petal.Length", counts = FALSE)
  ale <- light_profile(fls, v = "Petal.Length", type = "ale", counts = FALSE)
  resp <- light_profile(fls, v = "Petal.Length", type = "response")
  predicted <- light_profile(fls, v = "Petal.Length",
                             type = "predicted", counts = FALSE)

  expect_equal(eff$pd[-1], pd$data[-1])
  expect_equal(eff$ale[-1], ale$data[-1])
  expect_equal(eff$response, resp$data)
  expect_equal(eff$predicted, predicted$data)

  expect_true(inherits(plot(eff), "ggplot"))
  expect_true(inherits(plot_counts(plot(eff), eff), "ggplot"))
})

test_that("light_effects works with 'by'", {
  fls2 <- flashlight(fls$full, by = "Species")
  eff <- light_effects(fls2, v = "Petal.Length")

  pd <- light_profile(fls2, v = "Petal.Length", counts = FALSE)
  ale <- light_profile(fls2, v = "Petal.Length", type = "ale", counts = FALSE)
  resp <- light_profile(fls2, v = "Petal.Length", type = "response")
  predicted <- light_profile(fls2, v = "Petal.Length",
                             type = "predicted", counts = FALSE)

  expect_equal(eff$pd[-2], pd$data[-2])
  expect_equal(eff$ale[-2], ale$data[-2])
  expect_equal(eff$response, resp$data)
  expect_equal(eff$predicted, predicted$data)

  expect_true(inherits(plot(eff, use = "all"), "ggplot"))
  expect_true(inherits(plot_counts(plot(eff), eff), "ggplot"))
})

test_that("light_scatter works for type response", {
  sc <- light_scatter(fls, v = "Petal.Length",
                      type = "response", data = iris[1:5, ])
  expect_equal(sc$data$value, rep(iris$Sepal.Length[1:5], 2))
  expect_true(inherits(plot(sc), "ggplot"))
})

test_that("light_scatter works for type predicted", {
  sc <- light_scatter(fl2, v = "Petal.Length", type = "predicted")
  expect_equal(sc$data$value, rep(mean(iris$Sepal.Length), 150))
  expect_true(inherits(plot(sc), "ggplot"))
})

test_that("light_scatter works for type residual", {
  sc <- light_scatter(fl1, v = "Species", type = "residual", data = iris[1:50, ])
  expect_equal(mean(sc$data$value), 0)
  expect_true(inherits(plot(sc), "ggplot"))
})

test_that("Options work for light_scatter", {
  new_options = list(
    flashlight.label_name = "ell",
    flashlight.value_name = "val"
  )
  withr::with_options(new_options, {
    sc <- light_scatter(fl1, v = "Petal.Length", type = "predicted")
    expect_true(all(c("ell", "val") %in% colnames(sc$data)))
    expect_true(inherits(plot(sc), "ggplot"))
  })
})
