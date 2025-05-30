# Shared utilities for Tier 1 and Tier 2 fish advisory reactables

#' Prepare species × size × population combinations for the AOC
prep_aoc_combinations <- function(cons_data, aoc_id, length_levels) {
  cons_data %>%
    filter(waterbody_group %in% aoc_id,
           population_type_desc %in% c("General", "Sensitive")) %>%
    distinct(Species = specname, Size = length_category_label, Population = population_type_desc) %>%
    mutate(Size = factor(Size, levels = length_levels, ordered = TRUE))
}

#' Filter and tag AOC/Reference data
filter_advisory_data <- function(cons_data, site_ids, aoc_id, length_levels) {
  cons_data %>%
    filter(waterbody_group %in% site_ids,
           population_type_desc %in% c("General", "Sensitive")) %>%
    mutate(
      Species = specname,
      Size = factor(length_category_label, levels = length_levels, ordered = TRUE),
      Population = population_type_desc,
      site_type = if_else(waterbody_group %in% aoc_id, "AOC", "Reference")
    )
}

#' Optionally filter to species of interest
filter_interest_species <- function(df, interest_species = NULL) {
  if (!is.null(interest_species)) {
    before_n <- nrow(df)
    df <- df %>% filter(Species %in% interest_species)
    after_n <- nrow(df)
    if (after_n == 0) warning("No matching species found for interest_species filter.")
  }
  df
}

#' Summarize maximum advisory by species/site/pop/size
summarise_max_advisory <- function(df) {
  df %>%
    group_by(Species, Site = guide_locname_eng, site_type, Population, Size) %>%
    summarise(advisory = max(adv_level, na.rm = TRUE), .groups = "drop")
}

#' Add display rows and order fields to clean repeated labels
add_row_order_labels <- function(df, length_levels) {
  existing_size_cols <- intersect(length_levels, names(df))
  
  df %>%
    arrange(Species, Population, site_order) %>%
    mutate(
      id = row_number(),
      Species_display = ifelse(duplicated(Species), "", Species),
      Population_display = ifelse(duplicated(paste(Species, Population)), "", Population)
    ) %>%
    select(Species_display, Population_display, Site, site_type, id,
           Species, Population, site_order, all_of(existing_size_cols))
}

#' Generate default column styling for shared sticky/species/pop columns
make_common_column_defs <- function() {
  list(
    Species_display = colDef(
      name = "Species", minWidth = 150, sortable = FALSE, sticky = "left",
      style = JS("function(rowInfo) { return { fontWeight: 'bold', fontSize: '15px' }; }")
    ),
    Population_display = colDef(
      name = "Population", minWidth = 100, sortable = FALSE, sticky = "left",
      style = JS("function(rowInfo) { return { fontWeight: 'bold', fontSize: '15px' }; }")
    ),
    site_type = colDef(show = FALSE),
    id = colDef(show = FALSE),
    Species = colDef(show = FALSE),
    Population = colDef(show = FALSE),
    site_order = colDef(show = FALSE)
  )
}


