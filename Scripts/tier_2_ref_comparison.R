# Load required libraries
library(dplyr)
library(tidyr)
library(reactable)

# Reference site groups
t2_full_sites <- c("St. Lawrence River (Lake St. Francis)",
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

t2_ontario_sites <- c("St. Lawrence River (Lake St. Francis)",
                      "St. Lawrence River (Lake Ontario to Cornwall)",
                      "Lake Ontario (Western Basin)", "Whitby Harbour (Lake Ontario)",
                      "Frenchman Bay (Lake Ontario)", "West Lake", "East Lake",
                      "Consecon Lake", "McLaughlin Bay (Lake Ontario)",
                      "Oshawa Harbour (Lake Ontario)", "Jordan Harbour (Lake Ontario)")

# Define length category levels
length_levels <- c("15-20cm", "20-25cm", "25-30cm", "30-35cm", "35-40cm", "40-45cm",
                   "45-50cm", "50-55cm", "55-60cm", "60-65cm", "65-70cm", "70-75cm", ">75cm")

# Prepare tidy metadata
metadata_tidy <- cons_data %>%
  select(Species = specname,
         Population = population_type_desc,
         Site = guide_locname_eng,
         Size = length_category_label,
         Advisory = adv_level) %>%
  mutate(Size = factor(Size, levels = length_levels, ordered = TRUE),
         Location = case_when(
           Site == "St. Lawrence River (Lake St. Francis)" ~ "AOC",
           Site %in% t2_ontario_sites ~ "Lake Ontario",
           Site %in% t2_full_sites ~ "Great Lakes",
           TRUE ~ NA_character_
         )) %>%
  filter(!is.na(Location))

# Restrict to AOC species
metadata_tidy <- metadata_tidy %>%
  semi_join(
    metadata_tidy %>% filter(Location == "AOC") %>% distinct(Species),
    by = "Species"
  )

# AOC data rows
aoc_data <- metadata_tidy %>% filter(Location == "AOC")
aoc_rows <- aoc_data %>%
  group_by(Species, Population, Size) %>%
  summarize(Value = paste(unique(Advisory), collapse = ", "), .groups = "drop") %>%
  pivot_wider(names_from = Size, values_from = Value, values_fill = "") %>%
  mutate(Location = "AOC")

# Reference data, summarized only once per Species and Location
ref_data <- metadata_tidy %>% filter(Location != "AOC")
ref_rows <- ref_data %>%
  group_by(Species, Location, Size) %>%
  summarize(Value = as.character(n()), .groups = "drop") %>%
  pivot_wider(names_from = Size, values_from = Value, values_fill = "") %>%
  mutate(Population = "n")

# Combine AOC + Reference rows
table_data <- bind_rows(aoc_rows, ref_rows) %>%
  select(Species, Location, Population, all_of(length_levels)) %>%
  arrange(Species, factor(Location, levels = c("AOC", "Lake Ontario", "Great Lakes")), Population)

# Column definitions
columns_list <- list(
  Species = colDef(minWidth = 150),
  Location = colDef(minWidth = 120, style = function(value, index) {
    row <- table_data[index, ]
    if (row$Population == "n") list(fontSize = "12px", color = "#555") else list()
  }),
  Population = colDef(minWidth = 120, style = function(value) {
    if (value == "n") list(fontWeight = "bold", fontSize = "12px", color = "#555", borderTop = "2px solid #444") else list()
  })
)

for (col in length_levels) {
  columns_list[[col]] <- colDef(
    name = col,
    align = "center",
    style = function(value, index) {
      row <- table_data[index, ]
      if (row$Population == "n") list(fontSize = "12px", color = "#555") else list(fontSize = "13px")
    }
  )
}

# Render simplified table without dropdowns
reactable(
  table_data,
  columns = columns_list,
  defaultColDef = colDef(minWidth = 80, sortable = TRUE),
  bordered = TRUE,
  striped = TRUE,
  highlight = TRUE,
  pagination = TRUE,
  defaultPageSize = 10,
  showPageSizeOptions = TRUE,
  style = list(fontFamily = "sans-serif", fontSize = "13px", overflowX = "auto", whiteSpace = "nowrap")
)




# Heatmap Approach ------------------
# Load required libraries
library(dplyr)
library(tidyr)
library(reactable)
library(scales)

# Filter only reference sites
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

metadata_tidy <- cons_data %>%
  select(Species = specname,
         Population = population_type_desc,
         Site = guide_locname_eng,
         Size = length_category_label) %>%
  mutate(Size = factor(Size, levels = length_levels, ordered = TRUE),
         Location = case_when(
           Site %in% t2_ontario_sites ~ "Lake Ontario",
           Site %in% t2_full_sites & !(Site %in% t2_ontario_sites) ~ "Great Lakes",
           Site == "St. Lawrence River (Lake St. Francis)" ~ "AOC",
           TRUE ~ NA_character_
         )) %>%
  filter(!is.na(Location))

# Filter to only species present at AOC
aoc_species <- cons_data %>%
  filter(guide_locname_eng == "St. Lawrence River (Lake St. Francis)") %>%
  distinct(specname) %>%
  pull(specname)

metadata_tidy <- metadata_tidy %>%
  filter(Species %in% aoc_species, Population == "Sensitive")

# Function to create heatmap table
make_heatmap_table <- function(sites, title) {
  
  reference_data <- cons_data %>%
    filter(!(guide_locname_eng == "St. Lawrence River (Lake St. Lawrence)"),
           guide_locname_eng %in% sites,
           population_type_desc == "Sensitive",
           specname %in% aoc_species) %>%
    select(Species = specname, Population = population_type_desc, Size = length_category_label) %>%
    mutate(Size = factor(Size, levels = length_levels, ordered = TRUE))
  
  aoc_species_list <- cons_data %>%
    filter(guide_locname_eng == "St. Lawrence River (Lake St. Francis)",
           population_type_desc == "Sensitive") %>%
    distinct(Species = specname) %>%
    pull(Species)
  
  aoc_combinations <- expand.grid(
    Species = aoc_species_list,
    Population = "Sensitive",
    Size = length_levels
  ) %>%
    mutate(Size = factor(Size, levels = length_levels, ordered = TRUE)) 
  
  summary_data <- aoc_combinations %>%
    left_join(
      reference_data %>%
        count(Species, Population, Size),
      by = c("Species", "Population", "Size")
    ) %>%
    mutate(Size = factor(Size, levels = length_levels, ordered = TRUE)) %>%
    arrange(Species, Size) %>%
    pivot_wider(names_from = Size, values_from = n, values_fill = list(n = 0))
  
  size_cols <- setdiff(names(summary_data), c("Species", "Population"))
  
  columns_list <- list(
    Species = colDef(minWidth = 150),
    Population = colDef(minWidth = 120)
  )
  
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

# Create tables for both reference sets
lake_ontario_table <- make_heatmap_table(t2_ontario_sites, "Lake Ontario")
great_lakes_table <- make_heatmap_table(t2_full_sites, "Great Lakes")

# Display tables
lake_ontario_table
great_lakes_table



cons_ontario = cons_data %>%
  filter(guide_locname_eng %in% t2_ontario_sites)


# Round 2 -----------
# Filter only reference sites
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

metadata_tidy <- cons_data %>%
  select(Species = specname,
         Population = population_type_desc,
         Site = guide_locname_eng,
         Size = length_category_label) %>%
  mutate(Size = factor(Size, levels = length_levels, ordered = TRUE),
         Location = case_when(
           Site %in% t2_ontario_sites ~ "Lake Ontario",
           Site %in% t2_full_sites & !(Site %in% t2_ontario_sites) ~ "Great Lakes",
           Site == "St. Lawrence River (Lake St. Francis)" ~ "AOC",
           TRUE ~ NA_character_
         )) %>%
  filter(!is.na(Location))

# Filter to only species present at AOC
aoc_species <- cons_data %>%
  filter(guide_locname_eng == "St. Lawrence River (Lake St. Francis)") %>%
  distinct(specname) %>%
  pull(specname)

metadata_tidy <- metadata_tidy %>%
  filter(Species %in% aoc_species, Population == "Sensitive")

# Function to create heatmap table
make_heatmap_table <- function(sites, title) {
  
  reference_data <- cons_data %>%
    filter(!(guide_locname_eng == "St. Lawrence River (Lake St. Lawrence)"),
           guide_locname_eng %in% sites,
           population_type_desc == "Sensitive",
           specname %in% aoc_species) %>%
    select(Species = specname, Population = population_type_desc, Size = length_category_label) %>%
    mutate(Size = factor(Size, levels = length_levels, ordered = TRUE))
  
  # ✅ NEW: Get only species-size combinations from AOC
  aoc_combinations <- cons_data %>%
    filter(guide_locname_eng == "St. Lawrence River (Lake St. Francis)",
           population_type_desc == "Sensitive") %>%
    distinct(Species = specname, Size = length_category_label) %>%
    mutate(Population = "Sensitive",
           Size = factor(Size, levels = length_levels, ordered = TRUE))
  
  summary_data <- aoc_combinations %>%
    left_join(
      reference_data %>%
        count(Species, Population, Size),
      by = c("Species", "Population", "Size")
    ) %>%
    mutate(Size = factor(Size, levels = length_levels, ordered = TRUE)) %>%
    arrange(Species, Size) %>%
    pivot_wider(names_from = Size, values_from = n)
  
  size_cols <- setdiff(names(summary_data), c("Species", "Population"))
  
  columns_list <- list(
    Species = colDef(minWidth = 150),
    Population = colDef(minWidth = 120)
  )
  
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


# Create tables for both reference sets
lake_ontario_table <- make_heatmap_table(t2_ontario_sites, "Lake Ontario")
great_lakes_table <- make_heatmap_table(t2_full_sites, "Great Lakes")

# Display tables
lake_ontario_table
great_lakes_table



cons_ontario = cons_data %>%
  filter(guide_locname_eng %in% t2_ontario_sites)


### ROUND 3 (holy shit it worked) -------------
make_heatmap_table <- function(sites, title) {
  
  # Get AOC species-size combinations
  aoc_combinations <- cons_data %>%
    filter(guide_locname_eng == "St. Lawrence River (Lake St. Francis)",
           population_type_desc == "Sensitive") %>%
    distinct(Species = specname, Size = length_category_label) %>%
    mutate(Population = "Sensitive",
           Size = factor(Size, levels = length_levels, ordered = TRUE))
  
  # Build full reference grid: AOC species-size x reference sites
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
  
  # Join counts onto full grid → missing = 0
  reference_full <- reference_grid %>%
    left_join(reference_counts, by = c("Species", "Size", "Site")) %>%
    mutate(n = replace_na(n, 0))
  
  # Sum across sites to get total reference count per species-size
  reference_summary <- reference_full %>%
    group_by(Species, Population, Size) %>%
    summarise(n = sum(n), .groups = "drop")
  
  # Only keep species-size combinations present at AOC
  summary_data <- aoc_combinations %>%
    left_join(reference_summary,
              by = c("Species", "Population", "Size")) %>%
    arrange(Species, Size) %>%
    pivot_wider(names_from = Size, values_from = n)
  
  size_cols <- setdiff(names(summary_data), c("Species", "Population"))
  
  columns_list <- list(
    Species = colDef(minWidth = 150),
    Population = colDef(minWidth = 120)
  )
  
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


#### Final Metadata Comparison -----------
# Filter only reference sites
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

metadata_tidy <- cons_data %>%
  select(Species = specname,
         Population = population_type_desc,
         Site = guide_locname_eng,
         Size = length_category_label) %>%
  mutate(Size = factor(Size, levels = length_levels, ordered = TRUE),
         Location = case_when(
           Site %in% t2_ontario_sites ~ "Lake Ontario",
           Site %in% t2_full_sites & !(Site %in% t2_ontario_sites) ~ "Great Lakes",
           Site == "St. Lawrence River (Lake St. Francis)" ~ "AOC",
           TRUE ~ NA_character_
         )) %>%
  filter(!is.na(Location))

# Filter to only species present at AOC
aoc_species <- cons_data %>%
  filter(guide_locname_eng == "St. Lawrence River (Lake St. Francis)") %>%
  distinct(specname) %>%
  pull(specname)

metadata_tidy <- metadata_tidy %>%
  filter(Species %in% aoc_species, Population == "Sensitive")

# Function to create heatmap table
make_heatmap_table <- function(sites, title) {
  
  # Get AOC species-size combinations
  aoc_combinations <- cons_data %>%
    filter(guide_locname_eng == "St. Lawrence River (Lake St. Francis)",
           population_type_desc == "Sensitive") %>%
    distinct(Species = specname, Size = length_category_label) %>%
    mutate(Population = "Sensitive",
           Size = factor(Size, levels = length_levels, ordered = TRUE))
  
  # Build full reference grid: AOC species-size x reference sites
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
  
  # Join counts onto full grid → missing = 0
  reference_full <- reference_grid %>%
    left_join(reference_counts, by = c("Species", "Size", "Site")) %>%
    mutate(n = replace_na(n, 0))
  
  # Sum across sites to get total reference count per species-size
  reference_summary <- reference_full %>%
    group_by(Species, Population, Size) %>%
    summarise(n = sum(n), .groups = "drop")
  
  # Only keep species-size combinations present at AOC
  summary_data <- aoc_combinations %>%
    left_join(reference_summary,
              by = c("Species", "Population", "Size")) %>%
    arrange(Species, Size) %>%
    pivot_wider(names_from = Size, values_from = n)
  
  size_cols <- setdiff(names(summary_data), c("Species", "Population"))
  
  summary_data <- summary_data %>% select(-Population)
  
  columns_list <- list(
    Species = colDef(minWidth = 150)
  )
  
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


# Create tables for both reference sets
lake_ontario_table <- make_heatmap_table(t2_ontario_sites, "Lake Ontario")
great_lakes_table <- make_heatmap_table(t2_full_sites, "Great Lakes")

# Display tables
lake_ontario_table
great_lakes_table



cons_ontario = cons_data %>%
  filter(guide_locname_eng %in% t2_ontario_sites)


print(cons_data %>%
        distinct(adv_cause_multiple_name) %>%
        pull(adv_cause_multiple_name))
