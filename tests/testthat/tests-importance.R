context("importance")

test_that("all_identical works", {
  x <- list(a = 1, b = 2)
  y <- list(a = 1, b = 3)
  expect_true(all_identical(list(x, y), `[[`, "a"))
  expect_false(all_identical(list(x, y), `[[`, "b"))
})

