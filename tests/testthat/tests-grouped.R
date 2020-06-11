context("grouped operations")

test_that("grouped_counts work", {
  expect_equal(grouped_counts(iris), data.frame(n = 150))
  expect_equal(grouped_counts(iris, by = "Species")$n, c(50, 50, 50))
  expect_equal(grouped_counts(iris, w = "Petal.Length"), data.frame(n = 563.7))
  expect_equal(grouped_counts(iris, by = "Species", w = "Petal.Length")$n,
               c(73.1, 213, 278), tolerance = 1)
})
