make_heatmap_table <- function(df, length_levels = NULL, aoc_site_name, sites) {
  
  if (is.null(length_levels)) {
    # default: use the size labels present in the data, in their existing order
    length_levels <- df %>%
      dplyr::distinct(length_category_label) %>%
      dplyr::pull(length_category_label)
  }
  length_levels_chr <- as.character(length_levels)
  
  # Get AOC species-size combinations (Sensitive only)
  aoc_combinations <- df %>%
    dplyr::filter(
      guide_locname_eng == aoc_site_name,
      population_type_desc == "Sensitive"
    ) %>%
    dplyr::distinct(Species = specname, Size = length_category_label) %>%
    dplyr::mutate(
      Population = "Sensitive",
      Size = factor(Size, levels = length_levels_chr, ordered = TRUE)
    )
  
  reference_grid <- expand.grid(
    Species = unique(aoc_combinations$Species),
    Size    = length_levels_chr,
    Site    = sites,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(
      Population = "Sensitive",
      Size = factor(Size, levels = length_levels_chr, ordered = TRUE)
    )
  
  reference_counts <- df %>%
    dplyr::filter(
      guide_locname_eng %in% sites,
      population_type_desc == "Sensitive",
      specname %in% unique(aoc_combinations$Species)
    ) %>%
    dplyr::group_by(
      Species = specname,
      Size    = length_category_label,
      Site    = guide_locname_eng
    ) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(Size = factor(Size, levels = length_levels_chr, ordered = TRUE))
  
  reference_full <- reference_grid %>%
    dplyr::left_join(reference_counts, by = c("Species", "Size", "Site")) %>%
    dplyr::mutate(n = tidyr::replace_na(n, 0))
  
  reference_summary <- reference_full %>%
    dplyr::group_by(Species, Population, Size) %>%
    dplyr::summarise(n = sum(n), .groups = "drop")
  
  summary_data <- aoc_combinations %>%
    dplyr::left_join(reference_summary, by = c("Species", "Population", "Size")) %>%
    dplyr::arrange(Species, Size) %>%
    tidyr::pivot_wider(
      names_from  = Size,
      values_from = n,
      names_sort  = FALSE
    ) %>%
    dplyr::select(-Population)
  
  # ---- FORCE COLUMN ORDER HERE ----
  desired_size_cols <- length_levels_chr[length_levels_chr %in% names(summary_data)]
  summary_data <- summary_data %>%
    dplyr::select(dplyr::all_of(c("Species", desired_size_cols)))
  
  size_cols <- desired_size_cols
  
  columns_list <- list(Species = reactable::colDef(minWidth = 150))
  
  for (col in size_cols) {
    columns_list[[col]] <- reactable::colDef(
      name = col,
      align = "center",
      style = function(value) {
        val <- as.numeric(value)
        if (is.na(val)) return(list(background = "#eeeeee", color = "#000000"))
        
        if (val < 3) {
          color <- scales::col_numeric(c("#d80032", "#edf2f4"), domain = c(0, 3))(val)
        } else {
          max_val <- max(as.matrix(summary_data[, size_cols, drop = FALSE]), na.rm = TRUE)
          color <- scales::col_numeric(c("#8d99ae", "#2b2d42"), domain = c(3, max_val))(val)
        }
        list(background = color, color = "#ffffff")
      }
    )
  }
  
  reactable::reactable(
    summary_data,
    columns = columns_list,
    bordered = TRUE,
    striped = TRUE,
    highlight = TRUE,
    pagination = FALSE,
    style = list(fontFamily = "sans-serif", fontSize = "13px")
  )
}
