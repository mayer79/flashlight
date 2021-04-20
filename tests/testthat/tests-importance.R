context("importance")

fit1 <- lm(Sepal.Length ~ Petal.Width + Species + Sepal.Width, data = iris)
fit2 <- lm(Sepal.Length ~ Petal.Width, data = iris)
fl1 <- flashlight(model = fit1, label = "1", data = iris, y = "Sepal.Length")
fl2 <- flashlight(model = fit2, label = "2", data = iris, y = "Sepal.Length")
fls <- multiflashlight(list(fl1, fl2))

test_that("most_important works ", {
  imp <- light_importance(fl1, seed = 1)
  expect_equal(most_important(imp, 1), "Species")
  expect_equal(most_important(imp),
               c("Species", "Sepal.Width", "Petal.Width", "Petal.Length"))

  imp <- light_importance(fls, seed = 1)
  expect_equal(most_important(imp, 1), "Petal.Width")
  expect_equal(most_important(imp),
               c("Petal.Width", "Species", "Sepal.Width", "Petal.Length"))
})

test_that("light_importance works", {
  imp <- light_importance(fl2, seed = 1)
  expect_equal(imp$data$value[imp$data$variable != "Petal.Width"], rep(0, 3))
  expect_true(imp$data$value[imp$data$variable == "Petal.Width"] > 0)
  expect_equal(imp$data$value[imp$data$variable == "Petal.Width"], 0.623468, tolerance = 0.001)
  expect_true(inherits(plot(imp), "ggplot"))
})

test_that("light_importance reacts on metric", {
  imp <- light_importance(fl2, seed = 1, metric = list(r_squared = r_squared))
  expect_equal(imp$data$value[imp$data$variable != "Petal.Width"], rep(0, 3))
  expect_true(imp$data$value[imp$data$variable == "Petal.Width"] < 0)
  expect_true(inherits(plot(imp), "ggplot"))
})

test_that("'lower_is_better' works", {
  imp <- light_importance(fl2, seed = 1, metric = list(r_squared = r_squared),
                          lower_is_better = FALSE)
  expect_true(imp$data$value[imp$data$variable == "Petal.Width"] > 0)
})

test_that("'by' and 'v' works", {
  fit3 <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
  fl3 <- flashlight(model = fit3, label = "3", data = iris, y = "Sepal.Length")
  imp <- light_importance(fl3, seed = 1, by = "Species", v = c("Petal.Width", "Species"))

  expect_equal(imp$data$value[imp$data$variable == "Species"], rep(0, 3))
  expect_true(all(imp$data$value[imp$data$variable == "Petal.Width"] > 0))
  expect_equal(imp$data$value[imp$data$variable == "Petal.Width"],
               c(0.02224045, 0.11677754, 0.11914359), tolerance = 0.001)
  expect_true(inherits(plot(imp), "ggplot"))
})

test_that("'w' reacts", {
  fit3 <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
  fl3 <- flashlight(model = fit3, label = "3", data = iris, y = "Sepal.Length", w = "Petal.Length")
  imp <- light_importance(fl3, seed = 1, by = "Species", v = c("Petal.Width", "Species"))

  expect_equal(imp$data$value[imp$data$variable == "Petal.Width"],
               c(0.02220016, 0.10929391, 0.11598350), tolerance = 0.001)
  expect_true(inherits(plot(imp), "ggplot"))
})

test_that("m_repetitions react", {
  imp <- light_importance(fl1, seed = 1, v = c("Petal.Width", "Species"), m_repetitions = 4)
  expect_equal(imp$data$error, c(0.005633942, 0.017129828), tolerance = 0.001)
  expect_true(all(!is.na(imp$data$error)))
  expect_true(inherits(plot(imp), "ggplot"))
})

test_that("m_repetitions react with 'by'", {
  imp <- light_importance(fl1, seed = 1, v = c("Petal.Width", "Species"),
                          m_repetitions = 4, by = "Species")
  expect_true(is.light(imp))
  expect_true(all(!is.na(imp$data$error)))
  expect_true(inherits(plot(imp), "ggplot"))
})

test_that("multiflashlight works", {
  imp <- light_importance(fls, seed = 1, v = c("Petal.Width", "Species"))
  expect_equal(dim(imp$data), c(4L, 5L))
  expect_equal(imp$data$value,
               c(0.1425708, 0.4369238, 0.5537231, 0.0000000), tolerance = 0.001)
  expect_true(inherits(plot(imp), "ggplot"))
})

test_that("Options work", {
  new_options = list(
    flashlight.metric_name = "mm",
    flashlight.value_name = "val",
    flashlight.label_name = "ell",
    flashlight.variable_name = "var",
    flashlight.error_name = "err"
  )
  withr::with_options(new_options, {
    imp <- light_importance(fls, seed = 1, v = c("Petal.Width", "Species"))
    expect_true(all(c("mm", "val", "ell", "var", "err") %in% colnames(imp$data)))
    expect_true(inherits(plot(imp), "ggplot"))
  })
})

