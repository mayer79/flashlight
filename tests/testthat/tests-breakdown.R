fit1 <- stats::lm(Sepal.Length ~ Petal.Width, data = iris)
fit2 <- stats::lm(Sepal.Length ~ Petal.Width + Species + Sepal.Width, data = iris)
fl1 <- flashlight(model = fit1, label = "small", data = iris, y = "Sepal.Length")
fl2 <- flashlight(model = fit2, label = "large", data = iris, y = "Sepal.Length")
fls <- multiflashlight(list(fl1, fl2))

test_that("basic functionality works", {
  br <- light_breakdown(fl1, iris[1L, ])
  dat <- br$data
  expect_equal(dat$before_[-2L], dat$after_[-2L])
  expect_equal(dat$after_[2L] - dat$before_[2L], -0.8879879, tolerance = 0.001)
  expect_s3_class(plot(br), "ggplot")
})

test_that("light_breakdown reacts on v", {
  dat <- light_breakdown(fl1, iris[1L, ], v = "Petal.Width")$data
  expect_equal(nrow(dat), 3L)
})

test_that("light_breakdown reacts on visit_strategy v", {
  br <- light_breakdown(fl1, iris[1L, ], visit_strategy = "v")
  dat <- br$data
  expect_equal(dat$before_[-4L], dat$after_[-4L])
  expect_equal(dat$after_[4L] - dat$before_[4L], -0.8879879, tolerance = 0.001)
  expect_s3_class(plot(br), "ggplot")
})

test_that("light_breakdown reacts on visit_strategy shap", {
  br <- light_breakdown(fl1, iris[1L, ], visit_strategy = "permutation", seed = 1L)
  dat <- br$data
  expect_equal(dat$before_[-4L], dat$after_[-4L])
  expect_equal(dat$after_[4L] - dat$before_[4L], -0.8879879, tolerance = 0.001)
  expect_s3_class(plot(br), "ggplot")
})

test_that("light_breakdown reacts on weights", {
  br <- light_breakdown(flashlight(fl1, w = "Petal.Length"), iris[1L, ])
  dat <- br$data
  expect_equal(dat$before_[-2L], dat$after_[-2L])
  expect_equal(dat$after_[2L] - dat$before_[2L], -1.192293, tolerance = 0.001)
  expect_s3_class(plot(br), "ggplot")
})

test_that("light_breakdown reacts on by", {
  br <- light_breakdown(flashlight(fl1, by = "Species"), iris[1L, ])
  dat <- br$data
  expect_equal(dat$before_[-2L], dat$after_[-2L])
  expect_equal(dat$after_[2L] - dat$before_[2L], -0.04087469, tolerance = 0.001)
  expect_s3_class(plot(br), "ggplot")
})

test_that("light_breakdown reacts on multiflashlight", {
  br <- light_breakdown(fls, iris[1L, ])
  dat <- br$data
  expect_equal(nrow(dat), 6L * 2L)
  expect_false(all(dat[1:6, ] == dat[7:12, ]))
  expect_s3_class(plot(br), "ggplot")
})
