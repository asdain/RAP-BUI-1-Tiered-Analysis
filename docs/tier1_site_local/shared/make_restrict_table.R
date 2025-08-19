#==============================
# Function to convert input table into wide format for display
#==============================
make_restrict_table <- function(df,
                                aoc_id,
                                length_levels = NULL,
                                restrict_threshold = 8) {
  if (is.null(length_levels)) {
    length_levels <- tryCatch(get("length_levels", envir = .GlobalEnv),
                              error = function(...) intersect(names(df), names(df)))
  }
  
  # Filter to just the AOC
  cons_data <- df %>%
    filter(waterbody_group == aoc_id)
  
  # Ensure factor ordering for length categories
  cons_data <- cons_data %>%
    mutate(length_category_label = factor(length_category_label,
                                          levels = length_levels,
                                          ordered = TRUE))
  
  dat_aoc <- cons_data %>%
    select(
      spec = specname,
      pop_id = population_type_id,
      pop_name = population_type_desc,
      length_id = length_category_id,
      length_name = length_category_label,
      adv_level = adv_level,
      adv_cause = adv_cause_multiple_name
    )
  
  # Attach a per-row threshold column "thr"
  #    - single numeric: use as-is
  #    - named numeric vector: match by species name (names(restrict_threshold))
  #    - data.frame: join on species column
  if (is.numeric(restrict_threshold) && length(restrict_threshold) == 1L) {
    dat_aoc$thr <- restrict_threshold
    
  } else if (is.numeric(restrict_threshold) && !is.null(names(restrict_threshold))) {
    # named vector
    thr_vec <- restrict_threshold
    dat_aoc$thr <- thr_vec[dat_aoc$spec]
    # default to 8 if a species is missing in the vector
    dat_aoc$thr[is.na(dat_aoc$thr)] <- 8
    
  } else if (is.data.frame(restrict_threshold)) {
    thr_df <- restrict_threshold
    # normalize column names
    if (!"spec" %in% names(thr_df)) {
      if ("specname" %in% names(thr_df)) thr_df <- dplyr::rename(thr_df, spec = specname)
      if ("Species"  %in% names(thr_df)) thr_df <- dplyr::rename(thr_df, spec = Species)
    }
    if (!"threshold" %in% names(thr_df)) {
      stop("When passing a data.frame for restrict_threshold, it must have a 'threshold' column.")
    }
    dat_aoc <- dat_aoc %>%
      dplyr::left_join(dplyr::select(thr_df, spec, threshold), by = "spec")
    dat_aoc$thr <- dat_aoc$threshold
    dat_aoc$thr[is.na(dat_aoc$thr)] <- 8
    dat_aoc$threshold <- NULL
    
  } else {
    stop("restrict_threshold must be a single number, a named numeric vector, or a data.frame with columns 'spec/specname/Species' and 'threshold'.")
  }
  
  
  restrict_aoc <- dat_aoc %>%
    mutate(restrictive = adv_level <= thr)
  
  restrict_aoc_long <- restrict_aoc %>%
    mutate(adv_level = as.character(adv_level)) %>%
    pivot_longer(
      cols = c(adv_level, adv_cause),
      names_to = "Variable", values_to = "Value"
    ) %>%
    mutate(VarPop = paste(pop_name, Variable, sep = "_"))
  
  restrict_aoc_wide <- restrict_aoc_long %>%
    select(spec, length_name, VarPop, Value) %>%
    pivot_wider(names_from = length_name, values_from = Value)
  
  t1_df <- restrict_aoc_wide %>%
    arrange(spec, factor(VarPop, levels = c(
      "General_adv_level", "General_adv_cause",
      "Sensitive_adv_level", "Sensitive_adv_cause"
    ))) %>%
    mutate(Row_Label = case_when(
      VarPop == "General_adv_level" ~ "General",
      VarPop == "General_adv_cause" ~ "Adv cause",
      VarPop == "Sensitive_adv_level" ~ "Sensitive",
      VarPop == "Sensitive_adv_cause" ~ "Adv cause"
    )) %>%
    select(spec, Row_Label, everything(), -VarPop) %>%
    rename(Species = spec)
  
  
  t1_df <- t1_df %>%
    mutate(
      Species_display = ifelse(duplicated(Species), "", Species)
    )
  
  size_cols <- length_levels[length_levels %in% names(t1_df)]
  col_order = c("Species", "Species_display", "Row_Label", size_cols)
  t1_df = t1_df[,col_order]
  
  return(t1_df)
}







