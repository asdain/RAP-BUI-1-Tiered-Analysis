#==============================
# Function to convert input table into wide format for display
#==============================
make_restrict_table <- function(cons_data,
                                aoc_id,
                                length_levels) {
  # Filter to just the AOC
  cons_data <- cons_data %>%
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
  
  restrict_aoc <- dat_aoc %>%
    mutate(restrictive = adv_level <= params$restrict_threshold)
  
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







