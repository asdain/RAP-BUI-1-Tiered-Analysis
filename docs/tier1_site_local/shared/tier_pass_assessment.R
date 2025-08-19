#' Compute Tier 1 and Tier 2 pass/fail flags per species
#' Optionally excludes Tier 1 passing species from Tier 2 analysis
#' Returns a table with Species, t1_pass, t2_pass
get_species_pass_flags <- function(df, aoc_id, reference_sites, length_levels = NULL, interest_species = NULL) {

  
  if (is.null(length_levels)) {
    length_levels <- tryCatch(get("length_levels", envir = .GlobalEnv),
                              error = function(...) intersect(names(df), names(df)))
  }
  
  # Apply species filter
  if (!is.null(interest_species)) {
    cons_data <- cons_data %>%
      filter(specname %in% interest_species)
    
    if (nrow(cons_data) == 0) {
      warning("No matching records found for interest_species filter.")
      return(tibble(Species = interest_species, t1_pass = NA, t2_pass = NA))
    }
  }

    # Tier 1
  t1_flags <- df %>%
    filter(waterbody_group == aoc_id,
           population_type_desc %in% c("General", "Sensitive")) %>%
    group_by(specname) %>%
    summarise(t1_pass = all(adv_level >= params$restrict_threshold), .groups = "drop") %>%
    rename(Species = specname)
  
  # Tier 2
  base_data <- cons_data %>%
    filter_advisory_data(site_ids = c(aoc_id, reference_sites), aoc_id, length_levels) %>%
    summarise_max_advisory() 
  
  aoc_data <- base_data %>% filter(site_type == "AOC")
  ref_medians <- base_data %>%
    filter(site_type == "Reference") %>%
    group_by(Species, Population, Size) %>%
    summarise(Median = median(advisory, na.rm = TRUE), .groups = "drop")
  

  ref_counts <- base_data %>%
    filter(site_type == "Reference") %>%
    group_by(Species, Population, Size) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(Species) %>%
    summarise(
      low_ref_n = mean(n < 3, na.rm = TRUE) >= 0.5,
      .groups = "drop"
    )
  
  
  t2_compare <- aoc_data %>%
    left_join(ref_medians, by = c("Species", "Population", "Size")) %>%
    mutate(pass = is.na(Median) | advisory >= Median)
  
  t2_flags <- t2_compare %>%
    group_by(Species) %>%
    summarise(
      t2_pass = if (all(is.na(Median))) NA else all(pass),
      .groups = "drop"
    )
  
  
  # Join and merge low_n flag
  full_flags <- t1_flags %>%
    left_join(t2_flags, by = "Species") %>%
    left_join(ref_counts, by = "Species") %>%
    mutate(
      t2_pass = case_when(
        t1_pass ~ TRUE,
        !t1_pass & is.na(t2_pass) & is.na(low_ref_n) ~ NA,
        TRUE ~ t2_pass
      )
    )
  

  

  
  
  return(full_flags)
}




#' Generate a markdown list or table of species by pass/fail group
#' Optionally filters Tier 2 report to exclude Tier 1 passers
report_pass_fail_species <- function(flag_df = flags,
                                     tier = c("both", "t1", "t2"),
                                     output = c("list", "table"),
                                     filter_t1_pass = FALSE) {
  tier <- match.arg(tier)
  output <- match.arg(output)
  
  format_type <- if (knitr::is_html_output()) "html" else "markdown"
  
  if (output == "table") {
    summary_df <- flag_df %>%
      mutate(
        `Tier 1` = ifelse(t1_pass, "✔️", "❌"),
        `Tier 2` = case_when(
          !is.na(t2_pass) & t2_pass & low_ref_n ~ "✔️ ⚠️",
          !is.na(t2_pass) & !t2_pass & low_ref_n ~ "❌ ⚠️",
          !is.na(t2_pass) & t2_pass ~ "✔️",
          !is.na(t2_pass) & !t2_pass ~ "❌",
          is.na(t2_pass) & is.na(low_ref_n) ~ "— ⚠️",  # No reference data at all
          TRUE ~ "—"
        )
      ) %>%
      select(Species, `Tier 1`, `Tier 2`)
    
    
    
    if (tier == "t1") {
      print(knitr::kable(summary_df %>% select(Species, `Tier 1`), format = format_type))
    } else if (tier == "t2") {
      filtered <- if (filter_t1_pass) flag_df %>% filter(!t1_pass) else flag_df
      summary_df <- summary_df %>% filter(Species %in% filtered$Species)
      print(knitr::kable(summary_df %>% select(Species, `Tier 2`), format = format_type))
    } else {
      print(knitr::kable(summary_df, format = format_type))
    }
    
  } else if (tier == "t1") {
    passed <- flag_df %>% filter(t1_pass) %>% pull(Species)
    failed <- flag_df %>% filter(!t1_pass) %>% pull(Species)
    
    out <- c(
      "**Pass Tier 1:**",
      if (length(passed) > 0) paste0("- ", passed) else "_None_",
      "",
      "**Fail Tier 1:**",
      if (length(failed) > 0) paste0("- ", failed) else "_None_"
    )
    
    if (isTRUE(getOption("inline_output"))) {
      return(knitr::asis_output(paste(out, collapse = "\n")))
    } else {
      cat(paste(out, collapse = "\n"), "\n")
    }
    
  } else if (tier == "t2") {
    filtered <- if (filter_t1_pass) flag_df %>% filter(!t1_pass) else flag_df
    passed <- filtered %>% filter(t2_pass) %>% pull(Species)
    failed <- filtered %>% filter(!t2_pass) %>% pull(Species)
    
    out <- c(
      "**Pass Tier 2:**",
      if (length(passed) > 0) paste0("- ", passed) else "_None_",
      "",
      "**Fail Tier 2:**",
      if (length(failed) > 0) paste0("- ", failed) else "_None_"
    )
    
    if (isTRUE(getOption("inline_output"))) {
      return(knitr::asis_output(paste(out, collapse = "\n")))
    } else {
      cat(paste(out, collapse = "\n"), "\n")
    }
    
  } else {
    

    summary_df <- flag_df %>%
      mutate(
        `Tier 1` = ifelse(t1_pass, "✔️", "❌"),
        `Tier 2` = case_when(
          !is.na(t2_pass) & t2_pass & low_ref_n ~ "✔️ ⚠️",
          !is.na(t2_pass) & !t2_pass & low_ref_n ~ "❌ ⚠️",
          !is.na(t2_pass) & t2_pass ~ "✔️",
          !is.na(t2_pass) & !t2_pass ~ "❌",
          is.na(t2_pass) & is.na(low_ref_n) ~ "— ⚠️",  # No reference data at all
          TRUE ~ "—"
        )
      ) %>%
      select(Species, `Tier 1`, `Tier 2`)
    
    
    
    print(knitr::kable(summary_df, format = format_type))
  }
}




  
