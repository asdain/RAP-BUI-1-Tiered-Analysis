make_restrict_table <- function(df,
                                aoc_id,
                                length_levels = NULL,
                                restrict_threshold = 8,
                                interest_species = NULL) {
  
  if (is.null(length_levels)) {
    length_levels <- tryCatch(get("length_levels", envir = .GlobalEnv),
                              error = function(...) intersect(names(df), names(df)))
  }
  
  cons_data <- df %>%
    dplyr::filter(waterbody_group == aoc_id) %>%
    dplyr::mutate(length_category_label = factor(length_category_label,
                                                 levels = length_levels,
                                                 ordered = TRUE))
  
  dat_aoc <- cons_data %>%
    dplyr::select(
      spec = specname,
      pop_id = population_type_id,
      pop_name = population_type_desc,
      length_id = length_category_id,
      length_name = length_category_label,
      adv_level = adv_level,
      adv_cause = adv_cause_multiple_name
    )
  
  # >>> NEW: filter to interest species HERE (once)
  if (!is.null(interest_species)) {
    dat_aoc <- dat_aoc %>% dplyr::filter(spec %in% interest_species)
  }
  
  # thresholds (unchanged logic)
  if (is.numeric(restrict_threshold) && length(restrict_threshold) == 1L) {
    dat_aoc$thr <- restrict_threshold
  } else if (is.numeric(restrict_threshold) && !is.null(names(restrict_threshold))) {
    thr_vec <- restrict_threshold
    dat_aoc$thr <- thr_vec[dat_aoc$spec]
    dat_aoc$thr[is.na(dat_aoc$thr)] <- 8
  } else if (is.data.frame(restrict_threshold)) {
    thr_df <- restrict_threshold
    if (!"spec" %in% names(thr_df)) {
      if ("specname" %in% names(thr_df)) thr_df <- dplyr::rename(thr_df, spec = specname)
      if ("Species"  %in% names(thr_df)) thr_df <- dplyr::rename(thr_df, spec = Species)
    }
    if (!"threshold" %in% names(thr_df)) {
      stop("When passing a data.frame for restrict_threshold, include column 'threshold'.")
    }
    dat_aoc <- dat_aoc %>%
      dplyr::left_join(dplyr::select(thr_df, spec, threshold), by = "spec")
    dat_aoc$thr <- dat_aoc$threshold
    dat_aoc$thr[is.na(dat_aoc$thr)] <- 8
    dat_aoc$threshold <- NULL
  } else {
    stop("restrict_threshold must be a single number, a named numeric vector, or a data.frame with 'spec/specname/Species' and 'threshold'.")
  }
  
  # ensure numeric and (per your earlier code) use <= for restrictive
  dat_aoc <- dat_aoc %>%
    dplyr::mutate(
      adv_level = suppressWarnings(as.numeric(adv_level)),
      thr       = suppressWarnings(as.numeric(thr))
    )
  
  restrict_aoc <- dat_aoc %>%
    dplyr::mutate(restrictive = adv_level <= thr)   # matches your earlier CSV logic
  
  restrict_aoc_long <- restrict_aoc %>%
    dplyr::mutate(adv_level = as.character(adv_level)) %>%
    tidyr::pivot_longer(
      cols = c(adv_level, adv_cause),
      names_to = "Variable", values_to = "Value"
    ) %>%
    dplyr::mutate(VarPop = paste(pop_name, Variable, sep = "_"))
  
  restrict_aoc_wide <- restrict_aoc_long %>%
    dplyr::select(spec, length_name, VarPop, Value) %>%
    tidyr::pivot_wider(names_from = length_name, values_from = Value)
  
  t1_df <- restrict_aoc_wide %>%
    dplyr::arrange(spec, factor(VarPop, levels = c(
      "General_adv_level", "General_adv_cause",
      "Sensitive_adv_level", "Sensitive_adv_cause"
    ))) %>%
    dplyr::mutate(Row_Label = dplyr::case_when(
      VarPop == "General_adv_level" ~ "General",
      VarPop == "General_adv_cause" ~ "Adv cause",
      VarPop == "Sensitive_adv_level" ~ "Sensitive",
      VarPop == "Sensitive_adv_cause" ~ "Adv cause"
    )) %>%
    dplyr::select(spec, Row_Label, dplyr::everything(), -VarPop) %>%
    dplyr::rename(Species = spec)
  
  # carry per-species threshold
  thr_map <- dat_aoc %>% dplyr::distinct(Species = spec, Unrestrictive_Threshold = thr)
  t1_df <- t1_df %>%
    dplyr::left_join(thr_map, by = "Species") %>%
    dplyr::mutate(Species_display = ifelse(duplicated(Species), "", Species))
  
  size_cols <- length_levels[length_levels %in% names(t1_df)]
  col_order <- c("Species", "Species_display", "Unrestrictive_Threshold", "Row_Label", size_cols)
  t1_df <- t1_df[, col_order]
  
  return(t1_df)
}
