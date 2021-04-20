context("methods utils")

test_that("all_identical works", {
  x <- list(a = 1, b = 2)
  y <- list(a = 1, b = 3)
  expect_true(all_identical(list(x, y), `[[`, "a"))
  expect_false(all_identical(list(x, y), `[[`, "b"))
})

test_that("light_check, flashlight and multiflashlight work", {
  fit <- lm(Sepal.Length ~ Species + 0, data = iris)
  fl <- flashlight(model = fit, label = "lm", data = iris, y = "Sepal.Length")
  expect_true(is.flashlight(light_check(fl)))
  expect_error(light_check(1))
  expect_error(flashlight(fl, metrics = "no metric"))
  expect_error(flashlight(fl, linkinv = "no metric"))
  expect_error(flashlight(fl, data = "no metric"))
  expect_error(flashlight(fl, data = "no metric"))
  expect_error(multiflashlight(list(fl, 1)))
  expect_error(flashlight(1))
  expect_error(flashlight(fl, data = "bad data"))
})

test_that("light_combine works", {
  fit <- lm(Sepal.Length ~ Species + 0, data = iris)
  fl <- flashlight(model = fit, label = "lm", data = iris, y = "Sepal.Length")
  ell1 <- light_performance(fl)
  ell2 <- light_performance(fl)
  expect_equal(nrow(light_combine(list(ell1, ell2))$data), 2 * nrow(ell1$data))
  expect_equal(light_combine(ell1), ell1)
})

test_that("light_recode works", {
  fit <- lm(Sepal.Length ~ Species + 0, data = iris)
  fl <- flashlight(model = fit, label = "lm", data = iris, y = "Sepal.Length")
  eff <- light_effects(fl, v = "Species")
  eff <- light_recode(eff, what = "type",
                      levels = c("response", "predicted",
                                 "partial dependence", "ale"),
                      labels = c("Observed", "Fitted", "PD", "ALE"))
  expect_equal(as.character(eff$pd$type[1]), "PD")
})

test_that("selected 'is' functions work", {
  fit <- lm(Sepal.Length ~ Species + 0, data = iris)
  fl <- flashlight(model = fit, label = "lm", data = iris, y = "Sepal.Length")
  fls <- multiflashlight(list(fl, flashlight(fl, label = "lm2")))

  expect_true(is.flashlight(fl))
  expect_false(is.flashlight(1))

  expect_true(is.multiflashlight(fls))
  expect_false(is.flashlight(fls))
  expect_false(is.multiflashlight(1))
  expect_false(is.light(1))

  expect_true(is.light(light_performance(fl)))
  expect_true(is.light_performance(light_performance(fl)))
  expect_true(is.light_performance_multi(light_performance(fls)))
  expect_false(is.light_performance_multi(light_performance(fl)))

  expect_true(is.light(light_importance(fl)))
  expect_true(is.light_importance(light_importance(fl)))
  expect_true(is.light_importance_multi(light_importance(fls)))
  expect_false(is.light_importance_multi(light_importance(fl)))

  expect_true(is.light(light_importance(fl, v = "Species")))
  expect_true(is.light_importance(light_importance(fl, v = "Species")))
  expect_true(is.light_importance_multi(light_importance(fls, v = "Species")))
  expect_false(is.light_importance_multi(light_importance(fl, v = "Species")))

  expect_true(is.light(light_scatter(fl, v = "Species")))
  expect_true(is.light_scatter(light_scatter(fl, v = "Species")))
  expect_true(is.light_scatter_multi(light_scatter(fls, v = "Species")))
  expect_false(is.light_scatter_multi(light_scatter(fl, v = "Species")))

  expect_true(is.light(light_profile(fl, v = "Species")))
  expect_true(is.light_profile(light_profile(fl, v = "Species")))
  expect_true(is.light_profile_multi(light_profile(fls, v = "Species")))
  expect_false(is.light_profile_multi(light_profile(fl, v = "Species")))

  expect_true(is.light(light_effects(fl, v = "Species")))
  expect_true(is.light_effects(light_effects(fl, v = "Species")))
  expect_true(is.light_effects_multi(light_effects(fls, v = "Species")))
  expect_false(is.light_effects_multi(light_effects(fl, v = "Species")))

  expect_true(is.light(light_global_surrogate(fl)))
  expect_true(is.light_global_surrogate(light_global_surrogate(fl)))
  expect_true(is.light_global_surrogate_multi(light_global_surrogate(fls)))
  expect_false(is.light_global_surrogate_multi(light_global_surrogate(fl)))

  expect_true(is.light(light_breakdown(fl, new_obs = iris[1, ])))
  expect_true(is.light_breakdown(light_breakdown(fl, new_obs = iris[1, ])))
  expect_true(
    is.light_breakdown_multi(light_breakdown(fls, new_obs = iris[1, ]))
  )
  expect_false(
    is.light_breakdown_multi(light_breakdown(fl, new_obs = iris[1, ]))
  )
})

fit <- lm(Sepal.Length ~ Species + 0, data = iris)
fl <- flashlight(model = fit, label = "lm", data = iris, y = "Sepal.Length")
fls <- multiflashlight(list(fl, flashlight(fl, label = "lm2")))

test_that("response method works for (multi-)flashlights", {
  expect_equal(response(fl), iris$Sepal.Length)
  expect_equal(response(fls)[[2]], iris$Sepal.Length)
  expect_equal(response(flashlight(fl, linkinv = log)), log(iris$Sepal.Length))
})

test_that("predict method works for (multi-)flashlights", {
  expect_equal(predict(fl, data = head(iris)), predict(fit, head(iris)))
  expect_equal(predict(fls)[[1]], predict(fls)[[2]])
  expect_equal(predict(flashlight(fl, linkinv = log)), log(predict(fl)))
})

test_that("residuals method works for (multi-)flashlights", {
  expect_equal(resid(fl, data = head(iris)), head(resid(fit)))
  expect_equal(resid(fls)[[1]], resid(fls)[[2]])
})

