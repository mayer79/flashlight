context("grouped operations")

test_that("grouped_counts work", {
  expect_equal(grouped_counts(iris), data.frame(n = 150))
  expect_equal(grouped_counts(iris, by = "Species")$n, c(50, 50, 50))
  expect_equal(grouped_counts(iris, w = "Petal.Length"), data.frame(n = 563.7))
  expect_equal(grouped_counts(iris, by = "Species", w = "Petal.Length")$n,
               c(73.1, 213, 278), tolerance = 1)
})

test_that("grouped_weighted_mean work", {
  n <- 100
  data <- data.frame(x = rnorm(n), w = runif(n), group = factor(sample(1:3, n, TRUE)))
  expect_equal(grouped_weighted_mean(data, x = "x")$x,
               grouped_stats(data, x = "x")$x)
  expect_equal(grouped_weighted_mean(data, x = "x", w = "w")$x,
               grouped_stats(data, x = "x", w = "w")$x)
  expect_equal(grouped_weighted_mean(data, x = "x", by = "group")$x,
               grouped_stats(data, x = "x", by = "group")$x)
  expect_equal(grouped_weighted_mean(data, x = "x", w = "w", by = "group")$x,
               grouped_stats(data, x = "x", w = "w", by = "group")$x)
})

