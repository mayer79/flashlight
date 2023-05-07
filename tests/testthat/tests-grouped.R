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
  data <- data[order(data$group), ]  # .by does not sort
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

test_that("grouped_stats works", {
  data <- data.frame(x = 1:10, w = 1:10, g = rep(1:2, each = 5))

  expect_equal(grouped_stats(data, "x"), data.frame(counts = 10, x = 5.5))
  expect_equal(grouped_stats(data, "x", stats = "variance"),
               data.frame(counts = 10, x = var(1:10)))
  expect_equal(grouped_stats(data, "x", stats = "quartiles")[c("q1", "q3")],
               data.frame(q1 = 3.25, q3 = 7.75))

  expect_equal(grouped_stats(data, "x", by = "g")$x, c(3, 8))
  expect_equal(grouped_stats(data, "x", by = "g", stats = "variance")$x,
               c(2.5, 2.5))
  expect_equal(grouped_stats(data, "x", by = "g", stats = "quartiles")[["q1"]],
               c(2, 7))

  expect_equal(grouped_stats(data, "x", w = "w")$x, weighted.mean(data$x, data$w))
  expect_equal(grouped_stats(data, "x", w = "w", stats = "variance")$x,
               6.875)
  expect_equal(grouped_stats(data, "x", w = "w", stats = "quartiles")[c("q1", "q3")],
               data.frame(q1 = 5, q3 = 9))

  expect_equal(grouped_stats(data, "x", w = "w", by = "g")$x,
               c(weighted.mean(data$x[1:5], data$w[1:5]),
                 weighted.mean(data$x[6:10], data$w[6:10])))

  expect_equal(colnames(grouped_stats(data, "x", by = "g", counts_name = "n",
                                      value_name = "median", stats = "quartiles",
                                      q1_name = "p25", q3_name = "p75", )),
               c("g", "n", "p25", "median", "p75"))
})

