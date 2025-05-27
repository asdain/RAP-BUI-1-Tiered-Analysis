# Load required libraries
library(dplyr)
library(tidyr)
library(reactable)

# Define reference site lists
t2_full_sites <- c(
  "St. Lawrence River (Lake Ontario to Cornwall)",
  "Black Bay area (Lake Superior)", "Lake Superior (Eastern Basin)",
  "Lake Superior (Western Basin)", "North Channel (Lake Huron)",
  "Georgian Bay (Lake Huron)", "Lake Huron (Main Basin)",
  "Lake Ontario (Western Basin)", "Whitby Harbour (Lake Ontario)",
  "Frenchman Bay (Lake Ontario)", "Lake Erie", "Rondeau Bay (Lake Erie)",
  "Lake St. Clair", "West Lake", "East Lake", "Consecon Lake",
  "McLaughlin Bay (Lake Ontario)", "Oshawa Harbour (Lake Ontario)",
  "Jordan Harbour (Lake Ontario)", "Wheatley Harbour (Lake Erie)",
  "Collingwood Harbour (Georgian Bay - Lake Huron)",
  "Severn Sound (Georgian Bay - Lake Huron)",
  "Nipigon Bay (Lake Superior)")

t2_ontario_sites <- c(
  "St. Lawrence River (Lake Ontario to Cornwall)",
  "Lake Ontario (Western Basin)", "Whitby Harbour (Lake Ontario)",
  "Frenchman Bay (Lake Ontario)", "West Lake", "East Lake",
  "Consecon Lake", "McLaughlin Bay (Lake Ontario)",
  "Oshawa Harbour (Lake Ontario)", "Jordan Harbour (Lake Ontario)")

length_levels <- c("15-20cm", "20-25cm", "25-30cm", "30-35cm", "35-40cm", "40-45cm",
                   "45-50cm", "50-55cm", "55-60cm", "60-65cm", "65-70cm", "70-75cm", ">75cm")

# Function to create heatmap table
make_heatmap_table <- function(cons_data, sites, aoc_sites, title) {
  
  # Get AOC species-size combinations
  aoc_combinations <- cons_data %>%
    filter(guide_locname_eng %in% aoc_sites,
           population_type_desc == "Sensitive") %>%
    distinct(Species = specname, Size = length_category_label) %>%
    mutate(Population = "Sensitive",
           Size = factor(Size, levels = length_levels, ordered = TRUE))
  
  # Build full reference grid
  reference_grid <- expand.grid(
    Species = unique(aoc_combinations$Species),
    Size = length_levels,
    Site = sites
  ) %>%
    mutate(Population = "Sensitive",
           Size = factor(Size, levels = length_levels, ordered = TRUE))
  
  # Reference data counts
  reference_counts <- cons_data %>%
    filter(guide_locname_eng %in% sites,
           population_type_desc == "Sensitive",
           specname %in% unique(aoc_combinations$Species)) %>%
    group_by(Species = specname,
             Size = length_category_label,
             Site = guide_locname_eng) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(Size = factor(Size, levels = length_levels, ordered = TRUE))
  
  # Join counts
  reference_full <- reference_grid %>%
    left_join(reference_counts, by = c("Species", "Size", "Site")) %>%
    mutate(n = replace_na(n, 0))
  
  # Summarise across sites
  reference_summary <- reference_full %>%
    group_by(Species, Population, Size) %>%
    summarise(n = sum(n), .groups = "drop")
  
  # Join with AOC combinations
  summary_data <- aoc_combinations %>%
    left_join(reference_summary, by = c("Species", "Population", "Size")) %>%
    mutate(Size = factor(Size, levels = length_levels, ordered = TRUE)) %>%
    arrange(Species, Size) %>%
    pivot_wider(names_from = Size, values_from = n)
  
  # Patch: manually reorder columns after pivot_wider
  summary_data <- summary_data %>%
    select(Species, all_of(length_levels[length_levels %in% names(summary_data)]))
  
  size_cols <- setdiff(names(summary_data), c("Species"))
  
  columns_list <- list(Species = colDef(minWidth = 150))
  
  for (col in size_cols) {
    columns_list[[col]] <- colDef(
      name = col,
      align = "center",
      style = function(value) {
        val <- as.numeric(value)
        if (is.na(val)) return(list(background = "#eeeeee", color = "#000000"))
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

# Example calls
lake_st_francis_table_ontario <- make_heatmap_table(
  cons_data, t2_ontario_sites,
  aoc_sites = "St. Lawrence River (Lake St. Francis)",
  title = "Lake St. Francis"
)

lake_st_francis_table_greatlakes = make_heatmap_table(
  cons_data, t2_full_sites,
  aoc_sites = "St. Lawrence River (Lake St. Francis)",
  title = "Lake St. Francis"
)

bay_of_quinte_table_greatlakes <- make_heatmap_table(
  cons_data, t2_full_sites,
  aoc_sites = c("Belleville Nearshore", "Trenton Nearshore", "Lake Ontario (Eastern Basin)"),
  title = "Bay of Quinte"
)

bay_of_quinte_table_ontario <- make_heatmap_table(
  cons_data, t2_ontario_sites,
  aoc_sites = c("Belleville Nearshore", "Trenton Nearshore", "Lake Ontario (Eastern Basin)"),
  title = "Bay of Quinte"
)

lake_st_francis_table_ontario
lake_st_francis_table_greatlakes

bay_of_quinte_table_greatlakes
bay_of_quinte_table_ontario

