#' Compute Tier 1 and Tier 2 pass/fail flags per species
#' Returns a table with Species, t1_pass, t2_pass
get_species_pass_flags <- function(cons_data, aoc_id, reference_sites, length_levels) {
  # T1 pass: all AOC advisories >= restrict_threshold
  t1_flags <- cons_data %>%
    filter(waterbody_group == aoc_id,
           population_type_desc %in% c("General", "Sensitive")) %>%
    group_by(specname) %>%
    summarise(t1_pass = all(adv_level >= params$restrict_threshold), .groups = "drop") %>%
    rename(Species = specname)
  
  # T2 pass: all AOC advisories >= reference median
  base_data <- cons_data %>%
    filter_advisory_data(site_ids = c(aoc_id, reference_sites), aoc_id, length_levels) %>%
    summarise_max_advisory()
  
  aoc_data <- base_data %>% filter(site_type == "AOC")
  ref_medians <- base_data %>%
    filter(site_type == "Reference") %>%
    group_by(Species, Population, Size) %>%
    summarise(Median = median(advisory, na.rm = TRUE), .groups = "drop")
  
  t2_compare <- aoc_data %>%
    left_join(ref_medians, by = c("Species", "Population", "Size")) %>%
    mutate(pass = is.na(Median) | advisory >= Median)
  
  t2_flags <- t2_compare %>%
    group_by(Species) %>%
    summarise(
      has_refs = any(!is.na(Median)),
      t2_pass = ifelse(!has_refs, FALSE, all(pass)),
      .groups = "drop"
    ) %>%
    select(Species, t2_pass)
  
  # Join both
  full_flags <- t1_flags %>%
    full_join(t2_flags, by = "Species") %>%
    mutate(
      t1_pass = ifelse(is.na(t1_pass), FALSE, t1_pass),
      t2_pass = ifelse(t1_pass, TRUE, ifelse(is.na(t2_pass), FALSE, t2_pass))
    )
  
  return(full_flags)
}

#' Generate a markdown list or table of species by pass/fail group
report_pass_fail_species <- function(flag_df, tier = c("both", "t1", "t2"), output = c("list", "table")) {
  tier <- match.arg(tier)
  output <- match.arg(output)
  
  format_type <- if (knitr::is_html_output()) "html" else "markdown"
  if (output == "table") {
    summary_df <- flag_df %>%
      mutate(
        `Tier 1` = ifelse(t1_pass, "✔️", "❌"),
        `Tier 2` = ifelse(t2_pass, "✔️", "❌")
      ) %>%
      select(Species, `Tier 1`, `Tier 2`)
    
    if (tier == "t1") {
      print(knitr::kable(summary_df %>% select(Species, `Tier 1`), format = format_type))
    } else if (tier == "t2") {
      print(knitr::kable(summary_df %>% select(Species, `Tier 2`), format = "markdown"))
    } else {
      print(knitr::kable(summary_df, format = "markdown"))
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
      return(knitr::asis_output(paste(out, collapse = "

")))
    } else {
      cat(paste(out, collapse = "
"), "
")
    }
  } else if (tier == "t2") {
    passed <- flag_df %>% filter(t2_pass) %>% pull(Species)
    failed <- flag_df %>% filter(!t2_pass) %>% pull(Species)
    
    out <- c(
      "**Pass Tier 2:**",
      if (length(passed) > 0) paste0("- ", passed) else "_None_",
      "",
      "**Fail Tier 2:**",
      if (length(failed) > 0) paste0("- ", failed) else "_None_"
    )
    if (isTRUE(getOption("inline_output"))) {
      return(knitr::asis_output(paste(out, collapse = "
")))
    } else {
      cat(paste(out, collapse = "
"), "
")
    }
  } else {
    summary_df <- flag_df %>%
      mutate(
        `Tier 1` = ifelse(t1_pass, "✔️", "❌"),
        `Tier 2` = ifelse(t2_pass, "✔️", "❌")
      ) %>%
      select(Species, `Tier 1`, `Tier 2`)
    
    print(knitr::kable(summary_df, format = "markdown"))
  }
}



  
