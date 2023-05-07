fit <- lm(Sepal.Length ~ Petal.Width, data = iris)
fl <- flashlight(model = fit, label = "lm", data = iris, y = "Sepal.Length")
fl <- add_shap(fl, n_shap = 10, seed = 1, verbose = FALSE)

test_that("add_shap works", {
  expect_true(is.shap(fl$shap))
})

test_that("shap mode for light_importance works", {
  imp_perm <- light_importance(fl)
  imp_shap <- light_importance(fl, type = "shap")
  expect_false(all(imp_perm$data$value == imp_shap$data$value))
  expect_equal(most_important(imp_perm, 1), most_important(imp_shap, 1))
  expect_true(inherits(plot(imp_shap), "ggplot"))
})

test_that("shap mode for light_scatter works", {
  pred <- light_scatter(fl, v = "Petal.Width", type = "predicted")
  shap <- light_scatter(fl, v = "Petal.Width", type = "shap")
  expect_false(all(dim(pred$data) == dim(shap$data)))
  expect_true(inherits(plot(shap), "ggplot"))

  shap <- light_scatter(fl, v = "Sepal.Width", type = "shap")
  expect_true(all(shap$data$value == 0))
})

test_that("shap mode for light_profile works", {
  shap <- light_profile(fl, v = "Petal.Width", type = "shap")
  # expect_true(all(diff(shap$data$value) >= 0))
  expect_true(inherits(plot(shap), "ggplot"))
})

test_that("shap mode for light_profile2d works", {
  shap <- light_profile2d(fl, v = c("Petal.Width", "Species"),
                          type = "shap", n_bins = 4)
  expect_true(inherits(plot(shap), "ggplot"))
})


