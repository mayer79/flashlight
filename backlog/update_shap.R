if (update_and_check_shap) {
  if (!is.shap(x$shap)) {
    stop("No 'shap' object available. Run 'add_shap' first.")
  }
  # Deal with link functions
  same_link <- isTRUE(all.equal(x$shap$linkinv, x$linkinv))
  old_is_id <- isTRUE(all.equal(x$shap$linkinv, function(z) z))
  if (x$shap$use_linkinv && !same_link && !old_is_id) {
    stop("SHAP values have been computed using other 'linkinv'. It is recommended to
            calculate SHAP values without 'linkinv' in order to stay flexibel.")
  } else if (!x$shap$use_linkinv || (!same_link && old_is_id)) {
    data <- x$shap$data
    shap_vars <- c("baseline_", "before_", "after_")
    data[, shap_vars] <- lapply(data[, shap_vars], x$linkinv)
    data[["shap_"]] <- data[["after_"]] - data[["before_"]]
    x$shap$data <- data
    x$shap$use_linkinv <- TRUE
    x$shap$linkinv <- x$linkinv
  }
  # Rebase due to different "by"
  if (!isTRUE(all.equal(x$by, x$shap$by))) {
    cols <- colnames(x$data)
    data <- x$shap$data
    pred <- predict(x, data = data[, cols, drop = FALSE])
    if (is.null(x$by)) {
      data[["baseline_"]] <- weighted_mean(pred, w = x$w, na.rm = TRUE)
    } else {
      data[["baseline_"]] <- pred
      gmean <- grouped_stats(data, x = "baseline_", w = x$w, by = x$by, value_name = "gmean_")
      x$shap$data[["baseline_"]] <- left_join(data, gmean, by = x$by)[["gmean_"]]
    }
    x$shap$by <- x$by
  }
}
