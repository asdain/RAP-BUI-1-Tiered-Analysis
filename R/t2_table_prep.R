
# Replaces median with custom function to take lower of two middle values in even-numbered lists
median_floor <- function(x) {
  x <- sort(x[!is.na(x)])
  n <- length(x)
  if (n == 0) return(NA)
  if (n %% 2 == 1) {
    x[(n + 1) / 2]
  } else {
    x[n / 2]  # lower of the two middles
  }
}




# Prep function: returns everything the renderers need
t2_prep_display <- function(df,
                            aoc_id,
                            reference_sites,
                            length_levels = NULL,
                            interest_species = NULL,
                            exclude_t1_passed = TRUE,
                            flags_df = NULL,
                            threshold_df = NULL) {
  if (is.null(length_levels)) {
    length_levels <- tryCatch(get("length_levels", envir = .GlobalEnv),
                              error = function(...) intersect(names(df), names(df)))
  }
  
  aoc_combinations <- prep_aoc_combinations(df, aoc_id, length_levels)
  
  filtered_data <- df %>%
    filter_advisory_data(site_ids = c(reference_sites, aoc_id), aoc_id, length_levels) %>%
    semi_join(aoc_combinations, by = c("Species", "Size", "Population")) %>%
    filter_interest_species(interest_species)
  
  if (nrow(filtered_data) == 0) {
    return(list(display_data = NULL))  # caller will handle
  }
  
  base_data <- summarise_max_advisory(filtered_data)
  
  if (isTRUE(exclude_t1_passed)) {
    # use provided flags_df if supplied (so you don't recompute)
    if (is.null(flags_df)) {
      flags_df <- get_species_pass_flags(df, aoc_id, reference_sites, length_levels, interest_species)
    }
    passed_species <- flags_df %>% filter(t1_pass) %>% pull(Species)
    filtered_data <- filtered_data %>% filter(!Species %in% passed_species)
    base_data <- base_data %>% filter(!Species %in% passed_species)
  }
  
  # T1 threshold mapping
  if (!is.null(threshold_df)) {
    thr_df <- threshold_df
    
    # Normalise column names
    if (!"Species" %in% names(thr_df)) {
      if ("spec" %in% names(thr_df))     thr_df <- dplyr::rename(thr_df, Species = spec)
      if ("specname" %in% names(thr_df)) thr_df <- dplyr::rename(thr_df, Species = specname)
    }
    if (!"Unrestrictive_Threshold" %in% names(thr_df)) {
      if ("threshold" %in% names(thr_df)) {
        thr_df <- dplyr::rename(thr_df, Unrestrictive_Threshold = threshold)
      }
    }
    
    thr_df <- thr_df %>%
      dplyr::select(Species, Unrestrictive_Threshold) %>%
      dplyr::distinct() %>%
      dplyr::filter(Species %in% base_data$Species)
    
    threshold_map <- as.list(thr_df$Unrestrictive_Threshold)
    names(threshold_map) <- thr_df$Species
  } else {
    threshold_map <- list()
  }
  
  # AOC row (wide)
  aoc_data <- base_data %>%
    filter(site_type == "AOC") %>%
    tidyr::pivot_wider(names_from = Size, values_from = advisory) %>%
    mutate(Site = "AOC", site_order = 1)
  
  # Reference long
  ref_long <- base_data %>% filter(site_type == "Reference")
  
  # Ref medians (wide)
  ref_medians_raw <- ref_long %>%
    group_by(Species, Population, Size) %>%
    summarise(Median = median_floor(advisory), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = Size, values_from = Median)
  
  size_cols <- intersect(length_levels, names(ref_medians_raw))
  
  ref_medians <- ref_medians_raw %>%
    mutate(Site = "Reference Median", site_type = "Reference", site_order = 2) %>%
    select(Species, Population, all_of(size_cols), Site, site_type, site_order)
  
  # Ref n (wide)
  ref_n_raw <- ref_long %>%
    group_by(Species, Population, Size) %>%
    summarise(n = n(), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = Size, values_from = n)
  
  ref_n <- ref_n_raw %>%
    mutate(Site = "n", site_type = "Reference", site_order = 3) %>%
    select(Species, Population, all_of(size_cols), Site, site_type, site_order)
  
  # Ref detail table for “details” (reactable) or appendix (flextable)
  ref_data <- ref_long %>%
    tidyr::pivot_wider(names_from = Size, values_from = advisory)
  
  # Combine display rows and order/label
  aoc_median_combo <- bind_rows(aoc_data, ref_medians, ref_n)
  display_data <- add_row_order_labels(aoc_median_combo, length_levels)  # your existing helper
  
  # Lookups for coloring logic
  comparison_lookup <- ref_medians %>%
    select(Species, Population, all_of(size_cols)) %>%
    tidyr::pivot_longer(cols = all_of(size_cols), names_to = "Size", values_to = "Median") %>%
    mutate(id = paste(Species, Population, Size, sep = "||"))
  
  n_lookup <- ref_n %>%
    select(Species, Population, all_of(size_cols)) %>%
    tidyr::pivot_longer(cols = all_of(size_cols), names_to = "Size", values_to = "n") %>%
    mutate(id = paste(Species, Population, Size, sep = "||"))
  
  aoc_lookup <- base_data %>%
    dplyr::filter(site_type == "AOC") %>%
    dplyr::select(Species, Population, Size, advisory) %>%
    dplyr::mutate(id = paste(Species, Population, Size, sep = "||"))
  
  list(
    display_data   = display_data,
    ref_data       = ref_data,
    size_cols      = size_cols,
    medians_map    = setNames(as.list(comparison_lookup$Median), comparison_lookup$id),
    n_map          = setNames(as.list(n_lookup$n), n_lookup$id),
    threshold_map  = threshold_map,
    aoc_map        = setNames(as.list(aoc_lookup$advisory), aoc_lookup$id)
  )
}


