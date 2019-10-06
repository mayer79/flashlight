
fit_full <- lm(Sepal.Length ~ ., data = iris)
x <- flashlight(model = fit_full, label = "full", data = iris, y = "Sepal.Length")
data <- x$data

breaks <- NULL
v <- "Petal.Length"
evaluate_at <- if (!is.null(breaks)) midpoints(breaks) else
  auto_cut(data[[v]], n_bins = 5)$bin_means
m <- length(evaluate_at)



ale_core <- function(from, to) {
   if (is.numeric(to)) {
    .s <- data[[v]] >= from & data[[v]] <= to
  } else {
    .s <- data[[v]] %in% c(from, to)
  }
  dat_i <- data[.s, , drop = FALSE]
  if (nrow(dat_i) == 0L) {
    return(NULL)
  }
  ice <- light_ice(x, v = v, data = dat_i, by = x$by,
                     evaluate_at = c(from, to), n_max = 100)$data
  dat_to <- ice[ice[[v]] == to, ]
  dat_from <- ice[ice[[v]] == from, ]
  dat_to[["value"]] <- dat_to[["value"]] -
    dat_from[["value"]][match(dat_to[["id"]], dat_from[["id"]])]
  out <- grouped_stats(dat_to, x = "value", w = x$w, by = x$by)
  out[[v]] <- to
  out
}

ale <- Map(ale_core, from = evaluate_at[c(1L, 1:(m - 1L))], to = evaluate_at)
ale <- do.call(rbind, ale)
ale[["value"]] <- if (length(x$by)) ave(ale[["value"]], ale[[x$by]], FUN = cumsum) else
  cumsum(ale[["value"]])

# Calibration without by (easier to follow, so we separate)
preds <- predict(x)
if (is.null(x$by)) {
  pred_mean <- weighted_mean(preds, if (!is.null(x$w)) data[[x$w]], na.rm = TRUE)
  ale_mean <- weighted_mean(ale[["value"]], w = ale[["counts"]], na.rm = TRUE)
  ale[["value"]] <- ale[["value"]] - ale_mean + pred_mean
  ale
} else { # Calibration with by
  dat_pred <- cbind(data, cal = preds)
  dat_pred <- grouped_stats(dat_pred, x = "cal", w = x$w, by = x$by, counts = FALSE, na.rm = TRUE)
  dat_ale <- grouped_stats(ale, x = "value", w = "counts", by = x$by, counts = FALSE, na.rm = TRUE)
  dat_shift <- merge(dat_ale, dat_pred, all.x = TRUE, by = x$by)
  dat_shift[["shift"]] <- dat_shift[["cal"]] - dat_shift[["value"]]
  ale <- merge(ale, dat_shift[, c(x$by, "shift"), drop = FALSE], all.x = TRUE, by = x$by)
  ale[["value"]] <- ale[["value"]] + ale[["shift"]]
  ale[["shift"]] <- NULL
}
ale

