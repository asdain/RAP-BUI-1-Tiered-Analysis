## Make heatmap N table function

make_heatmap_table <- function(cons_data, length_levels, aoc_site_name, sites) {
  # Get AOC species-size combinations
  aoc_combinations <- cons_data %>%
    filter(guide_locname_eng == aoc_site_name,
           population_type_desc == "Sensitive") %>%
    distinct(Species = specname, Size = length_category_label) %>%
    mutate(Population = "Sensitive",
           Size = factor(Size, levels = length_levels, ordered = TRUE))
  
  reference_grid <- expand.grid(
    Species = unique(aoc_combinations$Species),
    Size = length_levels,
    Site = sites
  ) %>%
    mutate(Population = "Sensitive",
           Size = factor(Size, levels = length_levels, ordered = TRUE))
  
  reference_counts <- cons_data %>%
    filter(guide_locname_eng %in% sites,
           population_type_desc == "Sensitive",
           specname %in% unique(aoc_combinations$Species)) %>%
    group_by(Species = specname,
             Size = length_category_label,
             Site = guide_locname_eng) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(Size = factor(Size, levels = length_levels, ordered = TRUE))
  
  reference_full <- reference_grid %>%
    left_join(reference_counts, by = c("Species", "Size", "Site")) %>%
    mutate(n = replace_na(n, 0))
  
  reference_summary <- reference_full %>%
    group_by(Species, Population, Size) %>%
    summarise(n = sum(n), .groups = "drop")
  
  summary_data <- aoc_combinations %>%
    left_join(reference_summary,
              by = c("Species", "Population", "Size")) %>%
    arrange(Species, Size) %>%
    pivot_wider(names_from = Size, values_from = n) %>%
    select(-Population)
  
  size_cols <- setdiff(names(summary_data), "Species")
  
  columns_list <- list(Species = colDef(minWidth = 150))
  
  for (col in size_cols) {
    columns_list[[col]] <- colDef(
      name = col,
      align = "center",
      style = function(value) {
        val <- as.numeric(value)
        if (is.na(val)) {
          return(list(background = "#eeeeee", color = "#000000"))
        }
        if (val < 4) {
          color <- scales::col_numeric(c("#d80032", "#edf2f4"), domain = c(0, 5))(val)
        } else {
          max_val <- max(summary_data[size_cols], na.rm = TRUE)
          color <- scales::col_numeric(c("#8d99ae", "#2b2d42"), domain = c(4, max_val))(val)
        }
        list(background = color, color = "#ffffff")
      }
    )
  }
  
  reactable(
    summary_data,
    columns = columns_list,
    bordered = TRUE,
    striped = TRUE,
    highlight = TRUE,
    pagination = FALSE,
    style = list(fontFamily = "sans-serif", fontSize = "13px")
  )
}
