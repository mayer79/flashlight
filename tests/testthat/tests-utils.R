context("utils")

ir <- iris
ir$pred_ <- 1
fit <- lm(Sepal.Length ~ Petal.Length, data = ir)
fl <- flashlight(model = fit, label = "lm", data = ir,
                 y = "Sepal.Length")

test_that("check_unique works", {
  expect_true(is.light(light_performance(fl)))
  expect_error(
    light_performance(flashlight(fl, by = "pred_"))
  )
  new_options = list(
    flashlight.value_name = "pred_"
  )
  withr::with_options(new_options, {
  expect_error(
      light_performance(fl)
    )
  })

  new_options = list(
    flashlight.value_name = "xx",
    flashlight.label_name = "xx"
  )
  withr::with_options(new_options, {
    expect_error(
     light_performance(fl)
    )
  })

  expect_true(is.light(light_ice(fl, v = "Petal.Length")))

  expect_error(
    light_ice(fl, v = "Petal.Length", by = "Petal.Length")
  )

  new_options = list(
    flashlight.id_name = "Petal.Length"
  )
  withr::with_options(new_options, {
    expect_error(
      light_ice(fl, v = "Petal.Length")
    )
  })

  expect_error(
    light_scatter(fl, v = "Petal.Length", by = "Petal.Length")
  )

  expect_true(
    is.list(light_effects(fl, v = "Petal.Length"))
  )
  new_options = list(
    flashlight.label_name = "Petal.Length"
  )
  withr::with_options(new_options, {
    expect_error(
      light_profile(fl, v = "Petal.Length", type = "ale")
    )
  })

  expect_error(
    light_scatter(fl, v = "Petal.Length", by = c("Species", "Species"))
  )
})
