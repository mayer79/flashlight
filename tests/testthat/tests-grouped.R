context("grouped operations")

test_that("grouped_counts works", {
  expect_equal(grouped_counts(iris), data.frame(n = 150))
  expect_equal(grouped_counts(iris, by = "Species")$n, c(50, 50, 50))
  expect_equal(grouped_counts(iris, w = "Petal.Length"), data.frame(n = 563.7))
  expect_equal(grouped_counts(iris, by = "Species", w = "Petal.Length")$n,
               c(73.1, 213, 278), tolerance = 1)
})

test_that("grouped_weighted_mean works", {
  n <- 100
  set.seed(1)
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

test_that("grouped_center works", {
  data <- data.frame(x = c(1, 1, 2), w = c(2, 2, 1))
  res <- c(-1, -1, 2) / 3
  resw <- c(-1, -1, 4) / 5
  expect_equal(grouped_center(data, x = "x"), res)
  expect_equal(grouped_center(data, x = "x", w = "w"), resw)

  data2 <- data * 2
  data$g <- "A"
  data2$g <- "B"
  data3 <- rbind(data, data2)
  expect_equal(grouped_center(data3, x = "x", by = "g"), c(res, 2 * res))
  expect_equal(grouped_center(data3, x = "x", w = "w", by = "g"),
               c(resw, 2 * resw))
})
