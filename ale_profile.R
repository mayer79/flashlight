
fit_full <- lm(Sepal.Length ~ ., data = iris)
x <- flashlight(model = fit_full, label = "full", data = iris, y = "Sepal.Length")
data <- x$data

breaks <- NULL
v <- "Petal.Length"
evaluate_at <- if (!is.null(breaks)) midpoints(breaks) else
  auto_cut(data[[v]], n_bins = 5)$bin_means


ale_core <- function(from, to) {
  lookup <- setNames(data.frame(c("from", "to"), c(from, to)), c("key", v))

  if (is.logical(to) || is.numeric(to)) {
    .s <- data[[v]] >= from & data[[v]] <= to
  } else {
    .s <- data[[v]] %in% c(from, to)
  }
  dat_i <- data[.s, , drop = FALSE]
  if (nrow(dat_i) == 0L) {
    return(data.frame(counts = 0, Petal.Length = NA))
  }
  dat_i <- light_ice(x, v = v, data = dat_i , by = x$by,
                     evaluate_at = c(from, to), n_max = 100)$data
  dat_i <- left_join(dat_i, lookup, by = v)
  dat_i[[v]] <- NULL
  dat_i <- spread(dat_i, key = "key", value = "value")
  dat_i[["value"]] <- dat_i[["to"]] - dat_i[["from"]]
  grouped_stats(dat_i, x = "value", w = x$w, by = x$by)
}

m <- length(evaluate_at)
ale <- Map(ale_core, from = evaluate_at[c(1, 1:(m - 1L))], to = evaluate_at)
ale <- dplyr::bind_rows(ale)
ale[[v]] <- evaluate_at
ale[is.na(ale[["value"]]), "value"] <- 0
ale[["value"]] <- if (length(x$by)) ave(ale[["value"]], x$by, FUN = cumsum) else cumsum(ale[["value"]])


