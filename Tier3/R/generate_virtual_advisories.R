generate_virtual_advisories <- function(df,
                                        group_var = "region",
                                        contaminant = "MERCURY",
                                        length_range_model = NULL,
                                        length_range_bins = NULL,
                                        by = 5) {
  
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
  
  # Slight buffer (optional) or rounding to nearest 5
  min_rounded <- floor(observed_min / by) * by
  max_rounded <- ceiling(observed_max / by) * by
  
  if (is.null(length_range_model)) {
    length_range_model <- c(min_rounded, max_rounded)
  }
  
  if (is.null(length_range_bins)) {
    # Only bin within available range
    length_range_bins <- c(min_rounded, max_rounded)
  }
  
  # Build bin labels
  bin_breaks <- c(seq(length_range_bins[1], length_range_bins[2], by), Inf)
  bin_labels <- c(paste(seq(length_range_bins[1], length_range_bins[2] - by, by),
                        seq(length_range_bins[1] + by, length_range_bins[2], by), sep = "-"), paste0(length_range_bins[2], "+"))
  
  # Fit models per group
  model_info <- df %>%
    group_by(!!group_sym) %>%
    nest() %>%
    mutate(
      lm = map(data, ~ lm(log(Value) ~ log(Length), data = .x)),
      pval = map_dbl(lm, ~ summary(.x)$coefficients[2, 4]),
      correlated = pval <= 0.05,
      r2 = map_dbl(lm, ~summary(.x)$r.squared)
    )
  
  # Build prediction grid
  length_grid <- tibble(Length = seq(length_range_model[1], length_range_model[2], by = 1))
  
  # Generate predictions or fall back to bin-level means
  predictions <- model_info %>%
    mutate(length_grid = list(length_grid)) %>%
    unnest(length_grid) %>%
    mutate(
      predicted = pmap_dbl(list(correlated, lm, Length, data),
                           function(cor, model, L, dat) {
                             if (cor) {
                               exp(predict(model, newdata = tibble(Length = L)))
                             } else {
                               mean(dat$Value[cut(dat$Length, breaks = bin_breaks, right = FALSE, labels = bin_labels) ==
                                                cut(L, breaks = bin_breaks, right = FALSE, labels = bin_labels)],
                                    na.rm = TRUE)
                             }
                           }
      )
    ) %>%
    rename(region = !!group_sym) %>%
    mutate(
      meals_general = assign_advisory(predicted, population = "General", contaminant = contaminant),
      meals_sensitive = assign_advisory(predicted, population = "Sensitive", contaminant = contaminant)
    )
  
  # Bin and summarise
  predictions_binned <- predictions %>%
    mutate(
      length_bin = cut(Length, breaks = bin_breaks, right = FALSE, labels = bin_labels)
    ) %>%
    filter(!is.na(length_bin)) %>%
    group_by(region, length_bin) %>%
    summarise(
      meals_general = round(mean(meals_general, na.rm = TRUE)),
      meals_sensitive = round(mean(meals_sensitive, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    arrange(region, length_bin)
  
  list(predictions = predictions, binned = predictions_binned, model_info = model_info)
}
