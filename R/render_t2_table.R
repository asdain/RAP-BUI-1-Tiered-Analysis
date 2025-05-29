#' Generate Tier 2 Advisory Comparison Table
#'
#' This function generates a reactable comparing MECP fish consumption advisories
#' between an Area of Concern (AOC) and a set of reference sites.
#'
#' @param cons_data The full advisory dataset.
#' @param aoc_id A character vector of waterbody_group IDs for the AOC.
#' @param reference_sites A character vector of waterbody_group IDs for the reference sites.
#' @param length_levels An ordered vector of length category labels.
#' @param interest_species Optional character vector of species to include (default = NULL for all).
#' @return A reactable object rendering the comparison table.
render_t2_table <- function(cons_data, aoc_id, reference_sites, length_levels, interest_species = NULL) {
  aoc_combinations <- cons_data %>%
    filter(waterbody_group %in% aoc_id,
           population_type_desc %in% c("General", "Sensitive")) %>%
    distinct(Species = specname, Size = length_category_label, Population = population_type_desc) %>%
    mutate(Size = factor(Size, levels = length_levels, ordered = TRUE))
  
  filtered_data <- cons_data %>%
    filter(waterbody_group %in% c(reference_sites, aoc_id),
           population_type_desc %in% c("General", "Sensitive")) %>%
    mutate(Species = specname,
           Size = factor(length_category_label, levels = length_levels, ordered = TRUE),
           Population = population_type_desc,
           site_type = if_else(waterbody_group %in% aoc_id, "AOC", "Reference")) %>%
    semi_join(aoc_combinations, by = c("Species", "Size", "Population"))
  
  if (nrow(filtered_data) == 0) {
    warning("Filtered data is empty. Check AOC and reference site IDs or species filter.")
    return(reactable(data.frame(Message = "No advisory data available for selected filters.")))
  }
  
  if (!is.null(interest_species)) {
    before_n <- nrow(filtered_data)
    filtered_data <- filtered_data %>%
      filter(Species %in% interest_species)
    after_n <- nrow(filtered_data)
    if (after_n == 0) warning("No matching species found for interest_species filter.")
  }
  
  base_data <- filtered_data %>%
    group_by(Species, Site = guide_locname_eng, site_type, Population, Size) %>%
    summarise(advisory = max(adv_level, na.rm = TRUE), .groups = "drop")
  
  aoc_data <- base_data %>%
    filter(site_type == "AOC") %>%
    pivot_wider(names_from = Size, values_from = advisory) %>%
    mutate(site_order = 1)
  
  ref_long <- base_data %>% filter(site_type == "Reference")
  
  ref_medians <- ref_long %>%
    group_by(Species, Population, Size) %>%
    summarise(Median = median(advisory, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = Size, values_from = Median) %>%
    mutate(Site = "Reference Median", site_type = "Reference", site_order = 2)
  
  ref_n <- ref_long %>%
    group_by(Species, Population, Size) %>%
    summarise(n = n(), .groups = "drop") %>%
    pivot_wider(names_from = Size, values_from = n) %>%
    mutate(Site = "n", site_type = "Reference", site_order = 3)
  
  ref_data <- ref_long %>%
    pivot_wider(names_from = Size, values_from = advisory)
  
  aoc_median_combo <- bind_rows(aoc_data, ref_medians, ref_n)
  
  display_data <- aoc_median_combo %>%
    arrange(Species, Population, site_order) %>%
    mutate(
      id = row_number(),
      Species_display = ifelse(duplicated(Species), "", Species),
      Population_display = ifelse(duplicated(paste(Species, Population)), "", Population)
    ) %>%
    select(Species_display, Population_display, Site, site_type, id,
           Species, Population, site_order, all_of(length_levels))
  
  size_cols <- length_levels[length_levels %in% names(display_data)]
  
  if (length(size_cols) == 0 || nrow(ref_medians) == 0) {
    warning("No reference medians or size columns found. Check site IDs, species, or data coverage.")
    return(reactable(data.frame(Message = "No reference data available for selected filters.")))
  }
  
  comparison_lookup <- ref_medians %>%
    select(Species, Population, all_of(size_cols)) %>%
    pivot_longer(cols = all_of(size_cols), names_to = "Size", values_to = "Median") %>%
    mutate(id = paste(Species, Population, Size, sep = "||"))
  
  n_lookup <- ref_n %>%
    select(Species, Population, all_of(size_cols)) %>%
    pivot_longer(cols = all_of(size_cols), names_to = "Size", values_to = "n") %>%
    mutate(id = paste(Species, Population, Size, sep = "||"))
  
  comparison_medians <- setNames(as.list(comparison_lookup$Median), comparison_lookup$id)
  sample_ns <- setNames(as.list(n_lookup$n), n_lookup$id)
  
  columns_list <- list(
    Species_display = colDef(
      name = "Species", minWidth = 150, sortable = FALSE, sticky = "left",
      style = JS("function(rowInfo) { return { fontWeight: 'bold', fontSize: '15px' }; }")
    ),
    Population_display = colDef(
      name = "Population", minWidth = 100, sortable = FALSE, sticky = "left",
      style = JS("function(rowInfo) { return { fontWeight: 'bold', fontSize: '15px' }; }")
    ),
    Site = colDef(
      minWidth = 150, sortable = FALSE, sticky = "left", align = "center",
      style = JS(
        "function(rowInfo) {
          const site = rowInfo.row.Site;
          if (site === 'n') { return { fontSize: '13px', fontStyle: 'italic' }; }
          if (site === 'Reference Median') { return { fontSize: '13px' }; }
          if (rowInfo.row.site_type === 'AOC') { return { fontWeight: 'bold', fontSize: '15px' }; }
          return {}; }"
      )
    ),
    site_type = colDef(show = FALSE),
    id = colDef(show = FALSE),
    Species = colDef(show = FALSE),
    Population = colDef(show = FALSE),
    site_order = colDef(show = FALSE)
  )
  
  for (col in size_cols) {
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

          if (row.Site === 'n') { return { fontSize: '11px', color: '#666', fontStyle: 'italic' }; }
          if (row.site_type === 'AOC') {
            if (val === null) { return { background: '#eeeeee', color: '#000000', fontWeight: 'bold', fontSize: '15px' }; }
            if (n === undefined || n <= 3 || ref === undefined || ref === null) {
              return { background: '#999999', color: '#ffffff', fontWeight: 'bold', fontSize: '15px' }; }
            if (val < ref) {
              return { background: '#d80032', color: '#ffffff', fontWeight: 'bold', fontSize: '15px' }; }
            return { background: '#4CAF50', color: '#ffffff', fontWeight: 'bold', fontSize: '15px' }; }
          return { fontWeight: 'normal', fontSize: '13px' }; }",
        col))
    )
  }
  
  return(
    reactable(
      display_data,
      columns = columns_list,
      defaultExpanded = FALSE,
      bordered = TRUE,
      striped = TRUE,
      highlight = TRUE,
      pagination = FALSE,
      style = list(
        fontFamily = "system-ui, sans-serif",
        height = "1500px",
        width = "100%",
        overflowY = "auto",
        overflowX = "auto",
        margin = "0 auto"
      ),
      details = function(index) {
        row <- display_data[index, ]
        if (row$Site == "Reference Median") {
          ref_rows <- ref_data %>%
            filter(Species == row$Species, Population == row$Population) %>%
            select(Site, all_of(size_cols))
          reactable(ref_rows, compact = TRUE, bordered = TRUE, pagination = FALSE)
        } else {
          NULL
        }
      },
      meta = list(medians = comparison_medians, ns = sample_ns)
    )
  )
}
