plot_temporal_trends <- function(include_reference = TRUE, include_aoc = TRUE) {
  temporal_df <- full_data %>%
    filter(!is.na(Sample.Year), !is.na(Length), !is.na(Value)) %>%
    mutate(
      year = Sample.Year
    )
  
  # Test interaction with continuous Length
  interaction_model <- lm(Value ~ year * Length, data = temporal_df)
  anova_result <- anova(interaction_model)
  interaction_p <- anova_result["year:Length", "Pr(>F)"]
  interaction_sig <- !is.na(interaction_p) && interaction_p < 0.05
  interaction_fmt <- ifelse(is.na(interaction_p), "NA", ifelse(interaction_p < 0.001, "< 0.001", signif(interaction_p, 3)))
  
  # Define length bin range based on observed data
  len_min <- floor(min(temporal_df$Length, na.rm = TRUE) / 5) * 5
  len_max <- ceiling(max(temporal_df$Length, na.rm = TRUE) / 5) * 5
  len_breaks <- c(seq(len_min, len_max, by = 5), Inf)
  len_labels <- c(paste(seq(len_min, len_max - 5, by = 5), seq(len_min + 4, len_max - 1, by = 5), sep = "-"), paste0(len_max, "+"))
  
  temporal_df <- temporal_df %>%
    mutate(
      length_bin = cut(Length, breaks = len_breaks, labels = len_labels, right = FALSE)
    )
  
  regions_to_plot <- character(0)
  if (include_aoc) regions_to_plot <- c(regions_to_plot, "AOC")
  if (include_reference) regions_to_plot <- c(regions_to_plot, ref_1)
  if (include_reference && !params$combine_ref && ref_2 != "none") {
    regions_to_plot <- c(regions_to_plot, ref_2)
  }
  
  plot_list <- list()
  model_stats <- tibble(region = character(), r2 = numeric())
  trend_bins_all <- tibble(region = character(), length_bin = character(), trend = character())
  top_bins <- NULL
  
  if (interaction_sig) {
    
    
    top_bins <- temporal_df %>%
      count(length_bin, sort = TRUE) %>%
      slice_head(n = 3) %>%
      pull(length_bin)
    
    temporal_summary <- temporal_df %>%
      filter(length_bin %in% top_bins) %>%
      group_by(region, year, length_bin) %>%
      summarise(mean_value = mean(Value, na.rm = TRUE), .groups = "drop")
    
    for (reg in regions_to_plot) {
      region_data <- filter(temporal_summary, region == reg) %>% droplevels()
      if (n_distinct(region_data$length_bin) < 2) {
        message(glue::glue("Skipping region {reg} due to only one length_bin present."))
        next
      }
      
      mod <- lm(mean_value ~ year * length_bin, data = region_data)
      r2 <- summary(mod)$r.squared
      
      # Per-bin slope direction
      slopes <- coef(mod)
      bin_levels <- levels(region_data$length_bin)
      slope_df <- tibble(
        length_bin = bin_levels,
        slope = map_dbl(bin_levels, function(bin) {
          base <- slopes["year"]
          interaction_term <- slopes[paste0("year:length_bin", bin)]
          base + interaction_term
        }),
        trend = case_when(
          slope > 0 ~ "Increasing",
          slope < 0 ~ "Declining",
          TRUE ~ "Stable"
        )
      )
      
      trend_bins_all <- bind_rows(trend_bins_all, slope_df %>% mutate(region = reg) %>% select(region, length_bin, trend))
      
  
      
      
      p <- ggplot(region_data, aes(x = year, y = mean_value, color = length_bin, shape = length_bin)) +
        geom_point(size = 2) +
        geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
        labs(
          title = glue::glue("{reg}"),
          x = "Year",
          y = paste(contaminant_label, unit_label),
          color = "Length Bin", shape = "Length Bin"
        ) +
        theme_classic(base_size = 13)
      
      plot_list[[reg]] <- p
      model_stats <- bind_rows(model_stats, tibble(region = reg, r2 = r2, slope = NA_real_))
    }
    
  } else {
    temporal_summary <- temporal_df %>%
      group_by(region, year) %>%
      summarise(mean_value = mean(Value, na.rm = TRUE), .groups = "drop")
    
    for (reg in regions_to_plot) {
      region_data <- filter(temporal_summary, region == reg)
      
      mod <- lm(mean_value ~ year, data = region_data)
      r2 <- summary(mod)$r.squared
      slope <- coef(mod)[["year"]]
      
      p <- ggplot(region_data, aes(x = year, y = mean_value)) +
        geom_point(size = 2) +
        geom_smooth(method = "lm", se = FALSE, linewidth = 1, color = "black") +
        labs(
          title = glue::glue("{reg}"),
          x = "Year",
          y = paste(contaminant_label, unit_label)
        ) +
        theme_classic(base_size = 13)
      
      plot_list[[reg]] <- p
      model_stats <- bind_rows(model_stats, tibble(region = reg, r2 = r2, slope = slope))
    }
  }
  
  return(list(
    plot = ggarrange(plotlist = plot_list,
                     ncol = 1, nrow = length(plot_list),
                     align = "h", common.legend = TRUE, legend = "bottom"),
    interaction_p = interaction_p,
    interaction_sig = interaction_sig,
    top_bins = top_bins,
    model_stats = model_stats,
    include_reference = include_reference,
    trend_bins = trend_bins_all
  ))
}
