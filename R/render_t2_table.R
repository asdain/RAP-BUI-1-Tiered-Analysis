
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



render_t2_table <- function(cons_data = NULL, 
                            aoc_id = params$AOC, 
                            reference_sites = params$reference_sites, 
                            length_levels = NULL, 
                            interest_species = params$interest_species, 
                            exclude_t1_passed = TRUE) {
  
  if (is.null(cons_data)) {
    cons_data <- get("cons_data", envir = .GlobalEnv)
  }
  
  if (is.null(length_levels)) {
    length_levels <- get("length_levels", envir = .GlobalEnv)
  }
  

  
  aoc_combinations <- prep_aoc_combinations(cons_data, aoc_id, length_levels)
  
  filtered_data <- cons_data %>%
    filter_advisory_data(site_ids = c(reference_sites, aoc_id), aoc_id, length_levels) %>%
    semi_join(aoc_combinations, by = c("Species", "Size", "Population")) %>%
    filter_interest_species(interest_species)
  
  if (nrow(filtered_data) == 0) {
    warning("Filtered data is empty. Check AOC and reference site IDs or species filter.")
    return(reactable(data.frame(Message = "No advisory data available for selected filters.")))
  }
  
  base_data <- summarise_max_advisory(filtered_data)
  
  if (exclude_t1_passed) {
    flags <- get_species_pass_flags(cons_data, aoc_id, reference_sites, length_levels)
    passed_species <- flags %>% filter(t1_pass) %>% pull(Species)
    filtered_data <- filtered_data %>% filter(!Species %in% passed_species)
    base_data <- base_data %>% filter(!Species %in% passed_species)
  }
  
  
  
  aoc_data <- base_data %>%
    filter(site_type == "AOC") %>%
    pivot_wider(names_from = Size, values_from = advisory) %>%
    mutate(site_order = 1)
  
  ref_long <- base_data %>% filter(site_type == "Reference")
  
  ref_medians_raw <- ref_long %>%
    group_by(Species, Population, Size) %>%
    summarise(Median = median_floor(advisory), .groups = "drop") %>%
    pivot_wider(names_from = Size, values_from = Median)
  size_cols_medians <- intersect(length_levels, names(ref_medians_raw))
  ref_medians <- ref_medians_raw %>%
    mutate(Site = "Reference Median", site_type = "Reference", site_order = 2) %>%
    select(Species, Population, all_of(size_cols_medians), Site, site_type, site_order)
  
  ref_n_raw <- ref_long %>%
    group_by(Species, Population, Size) %>%
    summarise(n = n(), .groups = "drop") %>%
    pivot_wider(names_from = Size, values_from = n)
  size_cols_n <- intersect(length_levels, names(ref_n_raw))
  ref_n <- ref_n_raw %>%
    mutate(Site = "n", site_type = "Reference", site_order = 3) %>%
    select(Species, Population, all_of(size_cols_n), Site, site_type, site_order)
  
  ref_data <- ref_long %>%
    pivot_wider(names_from = Size, values_from = advisory)
  
  aoc_median_combo <- bind_rows(aoc_data, ref_medians, ref_n)
  display_data <- add_row_order_labels(aoc_median_combo, length_levels)
  size_cols_display <- intersect(length_levels, names(display_data))
  
  if (length(size_cols_display) == 0 || nrow(ref_medians) == 0) {
    warning("No reference medians or size columns found. Check site IDs, species, or data coverage.")
    return(reactable(data.frame(Message = "No reference data available for selected filters.")))
  }
  
  comparison_lookup <- ref_medians %>%
    select(Species, Population, all_of(size_cols_medians)) %>%
    pivot_longer(cols = all_of(size_cols_medians), names_to = "Size", values_to = "Median") %>%
    mutate(id = paste(Species, Population, Size, sep = "||"))
  
  n_lookup <- ref_n %>%
    select(Species, Population, all_of(size_cols_n)) %>%
    pivot_longer(cols = all_of(size_cols_n), names_to = "Size", values_to = "n") %>%
    mutate(id = paste(Species, Population, Size, sep = "||"))
  
  comparison_medians <- setNames(as.list(comparison_lookup$Median), comparison_lookup$id)
  sample_ns <- setNames(as.list(n_lookup$n), n_lookup$id)
  
  columns_list <- make_common_column_defs()
  rowStyle_fn <- function(index) {
    row <- display_data[index, ]
    prev_row <- if (index > 1) display_data[index - 1, ] else NULL
    next_row <- if (index < nrow(display_data)) display_data[index + 1, ] else NULL
    
    style <- list()
    if (row$Site == "Reference Median") style$fontWeight <- "bold"
    if (is.null(prev_row) || prev_row$Species != row$Species) style$borderTop <- "2px solid #666"
    if (is.null(next_row) || next_row$Species != row$Species) style$borderBottom <- "2px solid #666"
    return(style)
  }
  
  for (col in size_cols_display) {
    columns_list[[col]] <- colDef(
      name = col,
      align = "center",
      sortable = FALSE,
      style = JS(sprintf(
        "function(rowInfo, colInfo, state) {
          const row = rowInfo.row;
          const val = row[colInfo.id];
          const id = row.Species + '||' + row.Population + '||' + '%s';
          const ref = state.meta.medians[id];
          const n = state.meta.ns[id];

          if (row.Site === 'n') { return { fontSize: '11px', color: '#666', fontStyle: 'italic', fontFamily: 'system-ui, sans-serif' }; }
          if (row.site_type === 'AOC') {
            if (val === null) { return { background: '#eeeeee', color: '#000000', fontWeight: 'bold', fontSize: '15px', fontFamily: 'system-ui, sans-serif' }; }
            if (n === undefined || n <= 3 || ref === undefined || ref === null) {
              return { background: '#999999', color: '#ffffff', fontWeight: 'bold', fontSize: '15px', fontFamily: 'system-ui, sans-serif' }; }
            if (val < ref) {
              return { background: '#d80032', color: '#ffffff', fontWeight: 'bold', fontSize: '15px', fontFamily: 'system-ui, sans-serif' }; }
            return { background: '#4CAF50', color: '#ffffff', fontWeight: 'bold', fontSize: '15px', fontFamily: 'system-ui, sans-serif' }; }
          return { fontWeight: 'normal', fontSize: '13px', fontFamily: 'system-ui, sans-serif' }; }",
        col))
    )
  }
  
  return(
    reactable(
      display_data,
      columns = columns_list,
      defaultExpanded = FALSE,
      bordered = FALSE,
      striped = FALSE,
      highlight = TRUE,
      pagination = FALSE,
      rowStyle = rowStyle_fn,
      style = list(
        fontFamily = "system-ui, sans-serif",
        fontSize = "13px",
        borderCollapse = "collapse",
        borderSpacing = "0",
        margin = "0 auto",
        width = "auto"
      ),
      details = function(index) {
        row <- display_data[index, ]
        if (row$Site == "Reference Median") {
          ref_rows <- ref_data %>%
            filter(Species == row$Species, Population == row$Population)
          ref_cols_present <- intersect(size_cols_display, names(ref_rows))
          ref_rows <- ref_rows %>%
            select(Site, all_of(ref_cols_present))
          reactable(ref_rows, compact = TRUE, bordered = TRUE, pagination = FALSE)
        } else {
          NULL
        }
      },
      meta = list(medians = comparison_medians, ns = sample_ns)
    )
  )
}
