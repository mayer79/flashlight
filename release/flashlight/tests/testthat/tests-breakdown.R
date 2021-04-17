context("breakdown")

fit1 <- lm(Sepal.Length ~ Petal.Width, data = iris)
fit2 <- lm(Sepal.Length ~ Petal.Width + Species + Sepal.Width, data = iris)
fl1 <- flashlight(model = fit1, label = "small", data = iris, y = "Sepal.Length")
fl2 <- flashlight(model = fit2, label = "large", data = iris, y = "Sepal.Length")
fls <- multiflashlight(list(fl1, fl2))

test_that("basic functionality works", {
  br <- light_breakdown(fl1, iris[1, ])
  dat <- br$data
  expect_equal(dat$before[-2], dat$after[-2])
  expect_equal(dat$after[2] - dat$before[2], -0.8879879, tolerance = 0.001)
  expect_true(inherits(plot(br), "ggplot"))
})

test_that("light_breakdown reacts on v", {
  dat <- light_breakdown(fl1, iris[1, ], v = "Petal.Width")$data
  expect_equal(nrow(dat), 3L)
})

test_that("light_breakdown reacts on visit_strategy v", {
  br <- light_breakdown(fl1, iris[1, ], visit_strategy = "v")
  dat <- br$data
  expect_equal(dat$before[-4], dat$after[-4])
  expect_equal(dat$after[4] - dat$before[4], -0.8879879, tolerance = 0.001)
  expect_true(inherits(plot(br), "ggplot"))
})

test_that("light_breakdown reacts on visit_strategy shap", {
  br <- light_breakdown(fl1, iris[1, ], visit_strategy = "permutation", seed = 1)
  dat <- br$data
  expect_equal(dat$before[-4], dat$after[-4])
  expect_equal(dat$after[4] - dat$before[4], -0.8879879, tolerance = 0.001)
  expect_true(inherits(plot(br), "ggplot"))
})

test_that("light_breakdown reacts on weights", {
  br <- light_breakdown(flashlight(fl1, w = "Petal.Length"), iris[1, ])
  dat <- br$data
  expect_equal(dat$before[-2], dat$after[-2])
  expect_equal(dat$after[2] - dat$before[2], -1.192293, tolerance = 0.001)
  expect_true(inherits(plot(br), "ggplot"))
})

test_that("light_breakdown reacts on by", {
  br <- light_breakdown(flashlight(fl1, by = "Species"), iris[1, ])
  dat <- br$data
  expect_equal(dat$before[-2], dat$after[-2])
  expect_equal(dat$after[2] - dat$before[2], -0.04087469, tolerance = 0.001)
  expect_true(inherits(plot(br), "ggplot"))
})

test_that("light_breakdown reacts on multiflashlight", {
  br <- light_breakdown(fls, iris[1, ])
  dat <- br$data
  expect_equal(nrow(dat), 6L * 2L)
  expect_false(all(dat[1:6, ] == dat[7:12, ]))
  expect_true(inherits(plot(br), "ggplot"))
})

test_that("Options work", {
  new_options = list(
    flashlight.after_name = "a",
    flashlight.before_name = "b",
    flashlight.description_name = "d",
    flashlight.label_name = "ell",
    flashlight.step_name = "s",
    flashlight.variable_name = "var"
  )
  withr::with_options(new_options, {
    br <- light_breakdown(fl1, iris[1, ])
    expect_true(all(c("a", "b", "d", "ell", "s", "var") %in% colnames(br$data)))
    expect_true(inherits(plot(br), "ggplot"))
  })
})
