# Shared utilities for Tier 1 and Tier 2 fish advisory reactables

#' Prepare species × size × population combinations for the AOC
prep_aoc_combinations <- function(df, aoc_id, length_levels = NULL) {
  
  if (is.null(length_levels)) {
    length_levels <- tryCatch(get("length_levels", envir = .GlobalEnv),
                              error = function(...) intersect(names(df), names(df)))
  }
  
  cons_data = df %>%
    filter(waterbody_group %in% aoc_id,
           population_type_desc %in% c("General", "Sensitive")) %>%
    distinct(Species = specname, Size = length_category_label, Population = population_type_desc) %>%
    mutate(Size = factor(Size, levels = length_levels, ordered = TRUE))
}




#' Filter and tag AOC/Reference data
filter_advisory_data <- function(df, site_ids, aoc_id, length_levels = NULL) {
  
  if (is.null(length_levels)) {
    length_levels <- tryCatch(get("length_levels", envir = .GlobalEnv),
                              error = function(...) intersect(names(df), names(df)))
  }
  
  cons_data = df %>%
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
add_row_order_labels <- function(df, length_levels = NULL) {
  
  if (is.null(length_levels)) {
    length_levels <- tryCatch(get("length_levels", envir = .GlobalEnv),
                              error = function(...) intersect(names(df), names(df)))
  }
  
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
      name = "Species", minWidth = 110, sortable = FALSE, sticky = "left",
      style = JS("function(rowInfo) { return { fontWeight: 'bold', fontSize: '12px' }; }")
    ),
    Population_display = colDef(
      name = "Population", minWidth = 80, sortable = FALSE, sticky = "left",
      style = JS("function(rowInfo) { return {  fontSize: '12px' }; }")
    ),
    site_type = colDef(show = FALSE),
    id = colDef(show = FALSE),
    Species = colDef(show = FALSE),
    Population = colDef(show = FALSE),
    site_order = colDef(show = FALSE)
  )
}


# Table utility to figure out which columns are the size bins
t1_size_cols <- function(df, length_levels = NULL) {
  if (!is.null(length_levels)) return(intersect(length_levels, names(df)))
  sc <- attr(df, "size_cols")
  if (!is.null(sc)) return(sc)
  setdiff(names(df), c("Species","Species_display","Row_Label","Unrestrictive_Threshold"))
}


# To return table advisory causes as indices (numbered) instead of icons (helpful for Word doc version)
# returns a list: list(df = mutated_df, legend = tibble(idx, contaminant))
encode_adv_causes_as_indices <- function(df, size_cols) {
  # gather causes only from Adv cause rows
  cause_vals <- df[df$Row_Label == "Adv cause", size_cols, drop = FALSE] |>
    unlist(use.names = FALSE) |>
    as.character()
  cause_vals <- cause_vals[!is.na(cause_vals) & nzchar(cause_vals)]
  
  if (length(cause_vals) == 0) {
    return(list(df = df, legend = tibble::tibble(idx = integer(), contaminant = character())))
  }
  
  contaminants <- unique(trimws(unlist(strsplit(cause_vals, ","))))
  contaminants <- contaminants[nzchar(contaminants)]
  # stable index (alphabetical so it's deterministic)
  contaminants <- sort(contaminants)
  map <- stats::setNames(seq_along(contaminants), contaminants)
  
  # replace each Adv cause cell with comma-separated indices "1,2,3"
  df2 <- df
  for (i in which(df2$Row_Label == "Adv cause")) {
    for (col in size_cols) {
      x <- df2[[col]][i]
      if (is.na(x) || !nzchar(x)) next
      items <- trimws(unlist(strsplit(as.character(x), ",")))
      nums <- map[items]
      df2[[col]][i] <- paste(nums, collapse = ", ")
    }
  }
  
  legend <- tibble::tibble(idx = seq_along(contaminants), contaminant = contaminants)
  list(df = df2, legend = legend)
}

