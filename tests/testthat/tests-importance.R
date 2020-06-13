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

# TODO: Check if vignette / house-prices remain identical!
