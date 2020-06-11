context("cut utils")

test_that("midpoints are working", {
  expect_equal(midpoints(1:2), 1.5)
  expect_error(midpoints(1))
  expect_error(midpoints(c(1, NA)))
})

test_that("cut3 is working", {
  expect_equal(levels(cut3(1:3, breaks = c(0, 1.5, 2.5, 4))),
               c("(0, 1.5]", "(1.5, 2.5]", "(2.5, 4]"))
})

test_that("auto_cut is working", {
  x <- 1:10
  expect_equal(auto_cut(x, n_bins = 3)$breaks, c(0, 5, 10))
  ac <- auto_cut(c(NA, x), n_bins = 3)
  expect_equal(dim(ac$data), c(11L, 2L))
  expect_equal(ac$breaks, c(0, 5, 10))
  expect_equal(ac$bin_means, c(2.5, 7.5, NA))
  expect_equal(length(ac$bin_labels), 3L)
  expect_equal(auto_cut(x, cut_type = "quantile", n_bins = 3)$bin_means,
               c(2.5, 5.5, 8.5))
})

test_that("common_breaks give same results for split data as for combined data", {
  fit1 <- lm(Sepal.Length ~ ., data = iris)
  fit2 <- lm(Sepal.Length ~ Petal.Length, data = iris)
  fl1 <- flashlight(model = fit1, label = "full")
  fl2 <- flashlight(model = fit2, label = "single")
  fls <- multiflashlight(list(fl1, fl2), data = iris, y = "Sepal.Length")
  expect_equal(common_breaks(fls, v = "Petal.Length", data = NULL,
                             breaks = NULL, cut_type = "quantile", n_bins = 2),
               c(1.0, 4.3, 6.9))

  # Same result for multiflashlight with data distributed across flashlights
  fl1 <- flashlight(fls$full, data = iris[1:75, ])
  fl2 <- flashlight(fls$single, data = iris[76:150, ])
  fls2 <- multiflashlight(list(fl1, fl2))
  expect_equal(common_breaks(fls, v = "Petal.Length", data = NULL,
                             breaks = NULL, cut_type = "quantile", n_bins = 2),
               c(1.0, 4.3, 6.9))
})


