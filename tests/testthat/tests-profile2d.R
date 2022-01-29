fit <- lm(Sepal.Length ~ Species, data = iris)
fl <- flashlight(model = fit, label = "lm", data = iris, y = "Sepal.Length")

test_that("light_profile2d works correctly for type predicted", {
  pr <- light_profile2d(fl, v = c("Species", "Petal.Width"), type = "predicted")
  expect_equal(
    pr$data[!duplicated(pr$data$Species), ][["value"]],
    aggregate(Sepal.Length ~ Species, data = iris, FUN = mean)$Sepal.Length
  )
  expect_true(inherits(plot(pr), "ggplot"))
})

test_that("predicted + residuals = response", {
  v <- c("Species", "Petal.Length")
  pr_res <- light_profile2d(fl, v = v, type = "residual", n_bins = 3)
  pr_pred <- light_profile2d(fl, v = v, type = "predicted", n_bins = 3)
  pr_y <- light_profile2d(fl, v = v, type = "response", n_bins = 3)
  expect_equal(pr_res$data$value + pr_pred$data$value, pr_y$data$value)
  expect_true(inherits(plot(pr_res), "ggplot"))
})

test_that("partial dependence is constant if covariable not in model", {
  pr <- light_profile2d(fl, v = c("Petal.Length", "Petal.Width"))
  expect_true(var(pr$data$value) == 0)
})

fit <- lm(Sepal.Length ~ ., data = iris)
fl <- flashlight(model = fit, label = "lm",
                 data = iris, y = "Sepal.Length")

test_that("partial dependence is the same as ice", {
  v <- c("Petal.Width", "Petal.Length")
  a_grid <- expand.grid(list(Petal.Width = 1:2, Petal.Length = 2:3))
  pr <- light_profile2d(fl, pd_indices = 1, pd_grid = a_grid)$data
  pr <- pr[order(pr$Petal.Width, pr$Petal.Length), ]
  ice <- light_ice(fl, indices = 1, grid = a_grid)$data
  ice <- ice[order(ice$Petal.Width, ice$Petal.Length), ]
  expect_equal(pr$value, unname(ice$value))
})

test_that("n_bins work approximately (one value)", {
  pr <- light_profile2d(fl, v = c("Petal.Width", "Petal.Length"), n_bins = 3)
  expect_lt(nrow(pr$data), 20L)
})

test_that("n_bins work exactly with quantile cuts", {
  pr <- light_profile2d(fl, v = c("Petal.Width", "Petal.Length"),
                        n_bins = 3, cut_type = "quantile")
  expect_equal(nrow(pr$data), 9L)
})

test_that("n_bins work (two values)", {
  v <- c("Petal.Width", "Petal.Length")
  pr1 <- light_profile2d(fl, v = v, n_bins = 3:2, cut_type = "quantile")
  pr2 <- light_profile2d(fl, v = v, n_bins = 3, cut_type = "quantile")
  pr3 <- light_profile2d(fl, v = v, n_bins = c(3, 3), cut_type = "quantile")
  expect_equal(dim(pr2$data), dim(pr3$data))
  expect_equal(nrow(pr1$data) + 3L, nrow(pr2$data))
})

test_that("cut_type reacts", {
  v <- c("Petal.Width", "Petal.Length")
  pr1 <- light_profile2d(fl, v = v, n_bins = 3, cut_type = "equal")
  pr2 <- light_profile2d(fl, v = v, n_bins = 3, cut_type = "quantile")
  pr3 <- light_profile2d(fl, v = v, n_bins = 3,
                         cut_type = c("quantile", "equal"))
  expect_false(pr1$data[1L, 1L] == pr2$data[1L, 1L])
  expect_true(pr2$data[1L, 1L] == pr3$data[1L, 1L])
})

test_that("argument 'breaks' works for one variable", {
  v <- c("Petal.Width", "Species")
  pr <- light_profile2d(fl, v = v, breaks = list(Petal.Width = 0:3))
  expect_true(length(unique(pr$data$Petal.Width)) == 3L)
  expect_true(inherits(plot(pr), "ggplot"))
})

test_that("argument 'breaks' works for two variables", {
  v <- c("Petal.Width", "Petal.Length")
  pr <- light_profile2d(fl, v = v, breaks = list(
      Petal.Width = 0:3, Petal.Length = c(1, 4, 7)
    )
  )
  expect_true(nrow(pr$data) == 6L)
  expect_true(inherits(plot(pr), "ggplot"))
})

test_that("argument 'pd_evaluate_at' works for one variable", {
  v <- c("Petal.Width", "Species")
  pr <- light_profile2d(fl, v = v, pd_evaluate_at = list(Petal.Width = 1:2))
  expect_true(length(unique(pr$data$Petal.Width)) == 2L)
  expect_true(inherits(plot(pr), "ggplot"))
})

test_that("argument 'pd_evaluate_at' works for two variables", {
  v <- c("Petal.Width", "Petal.Length")
  pr <- light_profile2d(fl, v = v, pd_evaluate_at =
                          list(Petal.Width = 1:2, Petal.Length = 2:3))
  expect_true(nrow(pr$data) == 4L)
})

test_that("argument 'pd_grid' works for two variables", {
  v <- c("Petal.Width", "Petal.Length")
  a_list <- list(Petal.Width = 1:2, Petal.Length = 2:3)
  pr1 <- light_profile2d(fl, v = v, pd_evaluate_at = a_list)
  pr2 <- light_profile2d(fl, v = v, pd_grid = expand.grid(a_list))
  expect_equal(dim(pr1$data), dim(pr2$data))
})

test_that("label construction can be controlled by '...'", {
  v <- c("Petal.Width", "Petal.Length")
  pr <- light_profile2d(fl, v = v, n_bins = 3, cut_type = "equal",
                        type = "response", sep = ";")
  expect_true(grepl(";", pr$data$Petal.Width[1]))
})

test_that("light_profile2d reacts on 'by' variable", {
  v <- c("Petal.Width", "Petal.Length")
  at <- list(Petal.Width = 1:2, Petal.Length = 2:3)
  fl1 <- flashlight(model = fit, label = "lm", data = iris, y = "Sepal.Length")
  fl2 <- flashlight(fl1, data = iris[iris$Species == "setosa", ])
  pd1 <- light_profile2d(fl1, v = v, by = "Species", pd_evaluate_at = at)
  pd2 <- light_profile2d(fl2, v = v, pd_evaluate_at = at)
  expect_true(inherits(plot(pd1), "ggplot"))
  pd1$data <- pd1$data[pd1$data$Species == "setosa", ]
  expect_equal(pd1$data$value, pd2$data$value)
})

test_that("argument 'counts' works", {
  v <- c("Petal.Width", "Species")
  pr <- light_profile2d(fl, v = v, type = "response",
                        breaks = list(Petal.Width = 0:3))
  pr_nocounts <- light_profile2d(fl, v = v, type = "response",
                                 breaks = list(Petal.Width = 0:3),
                                 counts = FALSE)
  cnt <- pr$data[pr$data$Petal.Width == "[0, 1]" &
                   pr$data$Species == "versicolor", ][["counts"]]
  ref <- nrow(iris[iris$Petal.Width <= 1 & iris$Species == "versicolor", ])
  expect_equal(cnt, ref)
  expect_true("counts" %in% colnames(pr$data))
  expect_false("counts" %in% colnames(pr_nocounts$data))
})

test_that("argument 'linkinv' works", {
  v <- c("Petal.Width", "Species")
  pr <- light_profile2d(fl, v = v, type = "response",
                        breaks = list(Petal.Width = 0:3))
  pr_log <- light_profile2d(flashlight(fl, linkinv = log),
                            v = v, type = "response",
                            breaks = list(Petal.Width = 0:3))
  pr_log2 <- light_profile2d(flashlight(fl, linkinv = log),
                             v = v, type = "response",
                             use_linkinv = FALSE,
                             breaks = list(Petal.Width = 0:3))

  expect_true(all(pr_log$data$value < pr$data$value))
  expect_equal(pr_log2$data, pr$data)
})

fit1 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
fl1 <- flashlight(model = fit1, label = "lm",
                  data = iris, y = "Sepal.Length")
fit2 <- lm(Sepal.Length ~ 1, data = iris)
fl2 <- flashlight(model = fit2, label = "Empty",
                  data = iris, y = "Sepal.Length")
fls <- multiflashlight(list(fl1, fl2))
v <- c("Species", "Petal.Length")

test_that("multiflashlight works for partial dependence", {
  multi <- light_profile2d(fls, v = v, n_bins = 3)
  single <- light_profile2d(fls$lm, v = v, n_bins = 3)
  expect_true(all(multi$data[multi$data$label == "lm", ][["value"]] ==
                single$data$value))
  expect_true(inherits(plot(multi), "ggplot"))
})

test_that("multiflashlight works for type 'response'", {
  multi <- light_profile2d(fls, v = v, n_bins = 3, type = "response")
  single <- light_profile2d(fls$lm, v = v, n_bins = 3, type = "response")
  expect_true(all(multi$data[multi$data$label == "lm", ][["value"]] ==
                    single$data$value))
  expect_true(inherits(plot(multi), "ggplot"))
})

test_that("multiflashlight works for type 'predicted'", {
  multi <- light_profile2d(fls, v = v, n_bins = 3, type = "predicted")
  single <- light_profile2d(fls$lm, v = v, n_bins = 3, type = "predicted")
  expect_true(all(multi$data[multi$data$label == "lm", ][["value"]] ==
                    single$data$value))
  expect_true(inherits(plot(multi), "ggplot"))
})

v <- c("Petal.Width", "Petal.Length")

test_that("multiflashlight works with 'breaks', 'n_bins', 'cut_type'", {
  multi <- light_profile2d(fls, v = v, n_bins = c(8, 2), cut_type = "quantile",
                           breaks = list(Petal.Length = c(0, 4, 8)))
  expect_true(all(multi$data$Petal.Length %in% c(2, 6)))
  length(unique(multi$data$Petal.Width)) == 8
})

test_that("multiflashlight works with 'evaluate_at'", {
  at <- list(Petal.Length = c(0, 4, 8), Petal.Width = 1:2)
  multi <- light_profile2d(fls, v = v, n_bins = c(8, 2), cut_type = "quantile",
                           pd_evaluate_at = at)
  expect_true(all(multi$data$Petal.Length %in% c(0, 4, 8)))
  length(unique(multi$data$Petal.Width)) == 2
})

test_that("multiflashlight works with 'by' variable", {
  multi_by <- light_profile2d(fls, v = v, n_bins = 3, by = "Species")
  expect_true(inherits(plot(multi_by), "ggplot"))
})

test_that("light_profile2d uses weights and counts_weighted correctly", {
  ir <- iris
  ir$pw <- ir$Petal.Width > 1
  fit <- lm(Sepal.Length ~ 1, data = ir)
  fl <- flashlight(model = fit, label = "empty",
                   data = ir, y = "Sepal.Length", w = "Sepal.Width")
  pr <- light_profile2d(fl, v = c("Species", "pw"), type = "response")
  prc <- light_profile2d(fl, v = c("Species", "pw"), type = "response",
                         counts_weighted = TRUE)
  out <- pr$data
  reference <- with(ir[ir$Species == out$Species[1] & ir$pw == out$pw[1], ],
       weighted.mean(Sepal.Length, Sepal.Width))
  expect_equal(out$value[1], reference)
  expect_false(all(out$counts == prc$data$counts))
})

test_that("Options work for light_profile2d", {
  fit <- lm(Sepal.Length ~ Petal.Width, data = iris)
  fl <- flashlight(model = fit, label = "lm", data = iris)

  new_options = list(
    flashlight.label_name = "ell",
    flashlight.value_name = "val",
    flashlight.type_name = "tt",
    flashlight.counts_name = "n"
  )
  withr::with_options(new_options, {
    pd <- light_profile2d(fl, v = c("Petal.Width", "Species"))
    expect_true(all(c("ell", "val", "tt", "n") %in%
                      colnames(pd$data)))
    expect_true(inherits(plot(pd), "ggplot"))
  })
})

