generate_virtual_advisories <- function(df,
                                        group_var = "region",
                                        contaminant = "MERCURY",
                                        length_range_model = NULL,
                                        length_range_bins  = NULL,
                                        by = 5,
                                        # NEW: separate controls for plotting vs advisory binning
                                        plot_n = 300,              # how dense the smooth curve should be
                                        plot_when_fallback = FALSE # if FALSE, don't return a curve for groups using fallback
) {
  require(dplyr)
  require(tidyr)
  require(purrr)
  require(rlang)
  
  group_sym <- sym(group_var)
  
  # Filter to valid data
  df <- df %>% filter(Value > 0, Length > 0)
  
  # Determine dynamic length ranges if not provided
  observed_min <- floor(min(df$Length, na.rm = TRUE))
  observed_max <- ceiling(max(df$Length, na.rm = TRUE))
  
  min_rounded <- floor(observed_min / by) * by
  max_rounded <- ceiling(observed_max / by) * by
  
  if (is.null(length_range_model)) length_range_model <- c(min_rounded, max_rounded)
  if (is.null(length_range_bins))  length_range_bins  <- c(min_rounded, max_rounded)
  
  # Build bin labels (for advisory binning only)
  bin_breaks <- c(seq(length_range_bins[1], length_range_bins[2], by), Inf)
  bin_labels <- c(
    paste(seq(length_range_bins[1], length_range_bins[2] - by, by),
          seq(length_range_bins[1] + by, length_range_bins[2], by), sep = "-"),
    paste0(length_range_bins[2], "+")
  )
  
  # Fit models per group (power regression on log-log scale)
  model_info <- df %>%
    dplyr::group_by(!!group_sym) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      lm = purrr::map(data, ~ tryCatch(
        stats::lm(log(Value) ~ log(Length), data = .x),
        error = function(e) NULL
      )),
      pval = purrr::map_dbl(lm, ~{
        if (is.null(.x)) return(NA_real_)
        out <- tryCatch(summary(.x)$coefficients[2, 4], error = function(e) NA_real_)
        as.numeric(out)
      }),
      correlated = dplyr::if_else(!is.na(pval) & pval <= 0.05, TRUE, FALSE),
      r2 = purrr::map_dbl(lm, ~{
        if (is.null(.x)) return(NA_real_)
        out <- tryCatch(summary(.x)$r.squared, error = function(e) NA_real_)
        as.numeric(out)
      })
    )
  
  
  # ---- 1) SMOOTH CURVE DATA (for plotting the relationship) ----
  # Continuous-ish grid
  length_grid_plot <- tibble(
    Length = seq(length_range_model[1], length_range_model[2], length.out = plot_n)
  )
  
  # Predict concentration only (do not discretize to meals here)
  curve <- model_info %>%
    mutate(length_grid_plot = list(length_grid_plot)) %>%
    unnest(length_grid_plot) %>%
    mutate(
      predicted = map2_dbl(
        lm, Length,
        ~ exp(predict(.x, newdata = tibble(Length = .y)))
      )
    ) %>%
    rename(region = !!group_sym) %>%
    select(region, Length, predicted, correlated, r2, pval)
  
  # Optionally drop curves for groups that failed correlation (fallback groups)
  if (!plot_when_fallback) {
    curve <- curve %>% filter(correlated)
  }
  
  # ---- 2) ADVISORY GRID (for assigning meals + bin summaries) ----
  # Keep a 1-cm grid (or change to by=0.5 if you want finer) for advisory assignment
  length_grid_adv <- tibble(
    Length = seq(length_range_model[1], length_range_model[2], by = 1)
  )
  
  predictions <- model_info %>%
    mutate(length_grid_adv = list(length_grid_adv)) %>%
    unnest(length_grid_adv) %>%
    mutate(
      predicted = pmap_dbl(
        list(correlated, lm, Length, data),
        function(cor, model, L, dat) {
          cor <- isTRUE(cor)   # converts NA -> FALSE, TRUE stays TRUE
          if (cor && !is.null(model)) {
            exp(predict(model, newdata = tibble::tibble(Length = L)))
          } else {
            # fallback: bin-level mean concentration (piecewise constant by design)
            bin_L  <- cut(L, breaks = bin_breaks, right = FALSE, labels = bin_labels)
            bin_obs <- cut(dat$Length, breaks = bin_breaks, right = FALSE, labels = bin_labels)
            
            vals <- dat$Value[bin_obs == bin_L]
            
            if (length(vals) == 0 || all(is.na(vals))) {
              mean(dat$Value, na.rm = TRUE)  # last-resort fallback
            } else {
              mean(vals, na.rm = TRUE)
            }
            
          }
        }
      )
    ) %>%
    rename(region = !!group_sym) %>%
    mutate(
      meals_general   = assign_advisory(predicted, population = "General",   contaminant = contaminant),
      meals_sensitive = assign_advisory(predicted, population = "Sensitive", contaminant = contaminant)
    )
  
  # Bin and summarise (advisory outputs)
  predictions_binned <- predictions %>%
    mutate(length_bin = cut(Length, breaks = bin_breaks, right = FALSE, labels = bin_labels)) %>%
    filter(!is.na(length_bin)) %>%
    group_by(region, length_bin) %>%
    summarise(
      meals_general   = round(mean(meals_general,   na.rm = TRUE)),
      meals_sensitive = round(mean(meals_sensitive, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    arrange(region, length_bin)
  
  list(
    curve = curve,                         # <- use this for plotting smooth concentration-vs-length curves
    predictions = predictions,             # <- advisory grid (may be stepped for fallback groups)
    binned = predictions_binned,           # <- bin summary for tables
    model_info = model_info
  )
}
