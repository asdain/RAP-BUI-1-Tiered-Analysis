#' Compute Tier 1 and Tier 2 pass/fail flags per species
#' Uses species-specific thresholds for Tier 1 (default 8 if unmapped)
#' Returns a tibble: Species, t1_pass, t2_pass, low_ref_n
get_species_pass_flags <- function(df,
                                   aoc_id,
                                   reference_sites,
                                   length_levels = NULL,
                                   interest_species = NULL,
                                   restrict_threshold = 8) {
  
  # Helper: resolve thresholds per species
  resolve_species_thresholds <- function(species_vec, map, default = 8) {
    if (is.null(map)) return(rep(default, length(species_vec)))
    if (is.numeric(map) && length(map) == 1L) return(rep(map, length(species_vec)))
    if (is.function(map)) return(vapply(species_vec, map, numeric(1)))
    if (is.numeric(map) && !is.null(names(map))) {
      out <- unname(map[species_vec]); out[is.na(out)] <- default; return(as.numeric(out))
    }
    if (is.data.frame(map)) {
      m <- map
      # normalize column names
      if (!"Species" %in% names(m)) {
        if ("spec" %in% names(m))      m <- dplyr::rename(m, Species = spec)
        else if ("specname" %in% names(m)) m <- dplyr::rename(m, Species = specname)
      }
      stopifnot("threshold" %in% names(m), "Species" %in% names(m))
      key <- setNames(m$threshold, m$Species)
      out <- unname(key[species_vec]); out[is.na(out)] <- default; return(as.numeric(out))
    }
    warning("Unrecognized restrict_threshold; defaulting to 8.")
    rep(default, length(species_vec))
  }
  
  # length_levels fallback (used downstream helpers)
  if (is.null(length_levels)) {
    length_levels <- tryCatch(get("length_levels", envir = .GlobalEnv),
                              error = function(...) intersect(names(df), names(df)))
  }
  
  # 1) Filter once, up-front
  df_filt <- df
  if (!is.null(interest_species)) {
    df_filt <- df_filt %>% dplyr::filter(specname %in% interest_species)
    if (nrow(df_filt) == 0) {
      warning("No matching records found for interest_species filter.")
      return(tibble::tibble(Species = interest_species, t1_pass = NA, t2_pass = NA, low_ref_n = NA))
    }
  }
  
  # 2) Attach species-specific thresholds to the filtered data (per-row)
  df_filt <- df_filt %>%
    dplyr::mutate(
      adv_level = suppressWarnings(as.numeric(adv_level)),
      thr = resolve_species_thresholds(specname, restrict_threshold, default = 8)
    )
  
  # 3) Tier 1 (pass if ALL AOC General/Sensitive bins meet/exceed the species threshold)
  t1_flags <- df_filt %>%
    dplyr::filter(waterbody_group == aoc_id,
                  population_type_desc %in% c("General", "Sensitive")) %>%
    dplyr::group_by(specname) %>%
    dplyr::summarise(
      t1_pass = all(adv_level >= thr, na.rm = TRUE),    # legacy: pass if advisory >= threshold
      .groups = "drop"
    ) %>%
    dplyr::rename(Species = specname)
  
  # 4) Build cons_data AFTER filtering (AOC + references only)
  cons_data <- df_filt %>%
    dplyr::filter(waterbody_group %in% c(aoc_id, reference_sites))
  
  # 5) Tier 2 pipeline (unchanged, now scoped to filtered species set)
  base_data <- cons_data %>%
    filter_advisory_data(site_ids = c(aoc_id, reference_sites), aoc_id, length_levels) %>%
    summarise_max_advisory()
  
  aoc_data <- base_data %>% dplyr::filter(site_type == "AOC")
  
  ref_medians <- base_data %>%
    dplyr::filter(site_type == "Reference") %>%
    dplyr::group_by(Species, Population, Size) %>%
    dplyr::summarise(Median = stats::median(advisory, na.rm = TRUE), .groups = "drop")
  
  ref_counts <- base_data %>%
    dplyr::filter(site_type == "Reference") %>%
    dplyr::group_by(Species, Population, Size) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::group_by(Species) %>%
    dplyr::summarise(
      low_ref_n = mean(n < 3, na.rm = TRUE) >= 0.5,
      .groups = "drop"
    )
  
  t2_compare <- aoc_data %>%
    dplyr::left_join(ref_medians, by = c("Species", "Population", "Size")) %>%
    dplyr::mutate(pass = is.na(Median) | advisory >= Median)
  
  t2_flags <- t2_compare %>%
    dplyr::group_by(Species) %>%
    dplyr::summarise(
      t2_pass = if (all(is.na(Median))) NA else all(pass),
      .groups = "drop"
    )
  
  # 6) Join and finalize
  full_flags <- t1_flags %>%
    dplyr::left_join(t2_flags, by = "Species") %>%
    dplyr::left_join(ref_counts, by = "Species") %>%
    dplyr::mutate(
      t2_pass = dplyr::case_when(
        t1_pass ~ TRUE,                                   # Tier 1 pass → Tier 2 auto-pass
        !t1_pass & is.na(t2_pass) & is.na(low_ref_n) ~ NA, # no ref data → NA
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
