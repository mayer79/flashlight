fit <- lm(Sepal.Length ~ ., data = iris)
x <- flashlight(model = fit, label = "lm", data = iris, y = "Sepal.Length")

test_that("basic functionality works", {
  surr <- light_global_surrogate(x)
  expect_equal(surr$data$r_squared, 0.923, tolerance = 0.001)
  expect_true(inherits(plot(surr), "list"))
})

test_that("by variable work", {
  surr <- light_global_surrogate(x, by = "Species", v = c("Species", "Petal.Length"))
  expect_equal(dim(surr$data), c(3L, 4L))
})

test_that("argument 'v' works", {
  surr <- light_global_surrogate(x, v = "Petal.Length")
  expect_equal(dim(surr$data), c(1L, 3L))
})

test_that("multiflashlights work", {
  fit1 <- lm(Sepal.Length ~ Species, data = iris)
  fl1 <- flashlight(model = fit1, label = "Species", data = iris, y = "Sepal.Length")
  fit2 <- lm(Sepal.Length ~ Petal.Length, data = iris)
  fl2 <- flashlight(model = fit2, label = "Petal.Length", data = iris, y = "Sepal.Length")
  fls <- multiflashlight(list(fl1, fl2))
  surr <- light_global_surrogate(fls)
  expect_equal(dim(surr$data), c(2L, 3L))
  expect_equal(surr$data$r_squared, c(1, 0.978), tolerance = 0.001)
})


test_that("Options work", {
  fit <- lm(Sepal.Length ~ ., data = iris)
  fl <- flashlight(model = fit, label = "lm", data = iris)
  new_options = list(
    flashlight.label_name = "ell",
    flashlight.tree_name = "tt"
  )
  withr::with_options(new_options, {
    surr <- light_global_surrogate(fl)
    expect_true(all(c("ell", "tt") %in% colnames(surr$data)))
    expect_true(inherits(plot(surr), "list"))
  })
})

