median_floor <- function(x) {
  x <- sort(x[!is.na(x)])
  n <- length(x)
  if (n == 0) return(NA_integer_)
  if (n %% 2 == 1) x[(n + 1) %/% 2] else x[n / 2]
}

render_t2_table <- function(df,
                            aoc_id,
                            reference_sites,
                            length_levels = NULL,
                            interest_species = NULL,
                            exclude_t1_passed = TRUE,
                            table_height = "1200px",
                            # optional helpers (pass in from shared/ if you have them)
                            prep_aoc_combinations_fn = prep_aoc_combinations,
                            filter_advisory_data_fn  = filter_advisory_data,
                            summarise_max_adv_fn     = summarise_max_advisory,
                            get_pass_flags_fn        = get_species_pass_flags,
                            add_row_order_labels_fn  = add_row_order_labels,
                            make_common_cols_fn      = NULL  # allow NULL
) {
  # ---- Validate inputs
  req_cols <- c("Species","Population","Size","Site","site_type","advisory")
  missing_cols <- setdiff(req_cols, names(df))
  if (length(missing_cols)) {
    return(reactable::reactable(
      data.frame(Message = paste("Tier 2: missing columns:", paste(missing_cols, collapse = ", "))),
      pagination = FALSE
    ))
  }
  if (missing(aoc_id) || is.null(aoc_id)) {
    return(reactable::reactable(data.frame(Message = "Tier 2: aoc_id not provided"), pagination = FALSE))
  }
  if (missing(reference_sites) || length(reference_sites) == 0) {
    return(reactable::reactable(data.frame(Message = "Tier 2: reference_sites not provided"), pagination = FALSE))
  }
  
  # ---- Length levels
  if (is.null(length_levels)) {
    length_levels <- tryCatch(get("length_levels", envir = .GlobalEnv),
                              error = function(...) intersect(names(df), names(df)))
  }
  
  # ---- Combinations present in AOC (Species x Size x Pop that exist at AOC)
  aoc_combos <- prep_aoc_combinations_fn(df, aoc_id, length_levels)
  
  # ---- Filter to AOC + References, keep only combos that exist at AOC
  filtered_data <- df |>
    filter_advisory_data_fn(site_ids = c(reference_sites, aoc_id), aoc_id = aoc_id, length_levels = length_levels) |>
    dplyr::semi_join(aoc_combos, by = c("Species","Size","Population"))
  
  # species filter (safe if helper missing)
  if (!is.null(interest_species)) {
    if (exists("filter_interest_species", mode = "function")) {
      filtered_data <- filter_interest_species(filtered_data, interest_species)
    } else {
      filtered_data <- dplyr::filter(filtered_data, .data$Species %in% interest_species)
    }
  }
  
  if (nrow(filtered_data) == 0) {
    return(reactable::reactable(data.frame(Message = "No advisory data available for selected filters."), pagination = FALSE))
  }
  
  # ---- Collapse to max advisory per Species/Pop/Size/Site
  base_data <- summarise_max_adv_fn(filtered_data)
  
  # ---- Optionally exclude Tier 1 passed species
  if (isTRUE(exclude_t1_passed) && exists("get_species_pass_flags", mode = "function")) {
    flags <- get_pass_flags_fn(df, aoc_id, reference_sites, length_levels)
    if (!is.null(flags) && nrow(flags)) {
      passed_species <- flags |>
        dplyr::filter(.data$t1_pass) |>
        dplyr::pull(.data$Species)
      if (length(passed_species)) {
        filtered_data <- dplyr::filter(filtered_data, !.data$Species %in% passed_species)
        base_data     <- dplyr::filter(base_data,     !.data$Species %in% passed_species)
      }
    }
  }
  
  # ---- AOC row (wide)
  aoc_data <- base_data |>
    dplyr::filter(.data$site_type == "AOC") |>
    tidyr::pivot_wider(names_from = .data$Size, values_from = .data$advisory) |>
    dplyr::mutate(site_order = 1L)
  
  # ---- Reference medians and counts (wide)
  ref_long <- dplyr::filter(base_data, .data$site_type == "Reference")
  
  ref_medians_raw <- ref_long |>
    dplyr::group_by(.data$Species, .data$Population, .data$Size) |>
    dplyr::summarise(Median = median_floor(.data$advisory), .groups = "drop") |>
    tidyr::pivot_wider(names_from = .data$Size, values_from = .data$Median)
  size_cols_medians <- intersect(length_levels, names(ref_medians_raw))
  ref_medians <- ref_medians_raw |>
    dplyr::mutate(Site = "Reference Median", site_type = "Reference", site_order = 2L) |>
    dplyr::select(.data$Species, .data$Population, dplyr::all_of(size_cols_medians), .data$Site, .data$site_type, .data$site_order)
  
  ref_n_raw <- ref_long |>
    dplyr::group_by(.data$Species, .data$Population, .data$Size) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    tidyr::pivot_wider(names_from = .data$Size, values_from = .data$n)
  size_cols_n <- intersect(length_levels, names(ref_n_raw))
  ref_n <- ref_n_raw |>
    dplyr::mutate(Site = "n", site_type = "Reference", site_order = 3L) |>
    dplyr::select(.data$Species, .data$Population, dplyr::all_of(size_cols_n), .data$Site, .data$site_type, .data$site_order)
  
  # ---- Raw reference rows (for details panel)
  ref_data <- tidyr::pivot_wider(ref_long, names_from = .data$Size, values_from = .data$advisory)
  
  # ---- Assemble display table
  aoc_median_combo <- dplyr::bind_rows(aoc_data, ref_medians, ref_n)
  if (exists("add_row_order_labels", mode = "function")) {
    display_data <- add_row_order_labels_fn(aoc_median_combo, length_levels)
  } else {
    display_data <- aoc_median_combo
  }
  
  size_cols_display <- intersect(length_levels, names(display_data))
  if (length(size_cols_display) == 0 || nrow(ref_medians) == 0) {
    return(reactable::reactable(data.frame(Message = "No reference data available for selected filters."), pagination = FALSE))
  }
  
  # ---- Lookups injected via meta (for JS styling)
  comparison_lookup <- ref_medians |>
    dplyr::select(.data$Species, .data$Population, dplyr::all_of(size_cols_medians)) |>
    tidyr::pivot_longer(cols = dplyr::all_of(size_cols_medians), names_to = "Size", values_to = "Median") |>
    dplyr::mutate(id = paste(.data$Species, .data$Population, .data$Size, sep = "||"))
  n_lookup <- ref_n |>
    dplyr::select(.data$Species, .data$Population, dplyr::all_of(size_cols_n)) |>
    tidyr::pivot_longer(cols = dplyr::all_of(size_cols_n), names_to = "Size", values_to = "n") |>
    dplyr::mutate(id = paste(.data$Species, .data$Population, .data$Size, sep = "||"))
  
  comparison_medians <- stats::setNames(as.list(comparison_lookup$Median), comparison_lookup$id)
  sample_ns          <- stats::setNames(as.list(n_lookup$n),           n_lookup$id)
  
  # ---- Column defs
  if (is.null(make_common_cols_fn)) {
    columns_list <- list(
      Species = reactable::colDef(sticky = "left", minWidth = 140, name = "Species",
                                  style = list(fontWeight = "bold", fontSize = "15px", fontFamily = "system-ui, sans-serif")),
      Population = reactable::colDef(sticky = "left", minWidth = 120, name = "Population",
                                     style = list(fontWeight = "bold", fontSize = "13px", fontFamily = "system-ui, sans-serif")),
      Site = reactable::colDef(sticky = "left", minWidth = 150)
    )
  } else {
    columns_list <- make_common_cols_fn()
  }
  
  # per-size columns with JS styling (reactable::JS)
  for (col in size_cols_display) {
    columns_list[[col]] <- reactable::colDef(
      name = col,
      align = "center",
      sortable = FALSE,
      style = reactable::JS(sprintf("
        function(rowInfo, colInfo, state) {
          const row = rowInfo.row;
          const val = row[colInfo.id];
          const id  = row.Species + '||' + row.Population + '||' + '%s';
          const ref = state.meta.medians[id];
          const n   = state.meta.ns[id];

          const base = { fontFamily: 'system-ui, sans-serif', fontWeight: 'bold', fontSize: '15px' };

          if (row.Site === 'n') { return { ...base, fontSize: '11px', color: '#666', fontStyle: 'italic' }; }
          if (row.site_type === 'AOC') {
            if (val === null) return { ...base, background: '#eeeeee', color: '#000' };
            if (n === undefined || n < 3 || ref === undefined || ref === null)
              return { ...base, background: '#999999', color: '#fff' };
            if (val < ref) return { ...base, background: '#d80032', color: '#fff' };
            return { ...base, background: '#4CAF50', color: '#fff' };
          }
          return { fontFamily: 'system-ui, sans-serif', fontSize: '13px' };
        }", col))
    )
  }
  
  rowStyle_fn <- function(index) {
    row <- display_data[index, ]
    prev_row <- if (index > 1) display_data[index - 1, ] else NULL
    next_row <- if (index < nrow(display_data)) display_data[index + 1, ] else NULL
    style <- list()
    if (identical(row$Site, "Reference Median")) style$fontWeight <- "bold"
    if (is.null(prev_row) || prev_row$Species != row$Species) style$borderTop <- "2px solid #666"
    if (is.null(next_row) || next_row$Species != row$Species) style$borderBottom <- "2px solid #666"
    style
  }
  
  # ---- Render
  reactable::reactable(
    display_data,
    columns    = columns_list,
    bordered   = FALSE,
    striped    = FALSE,
    highlight  = TRUE,
    pagination = FALSE,
    sortable   = FALSE,
    height     = table_height,
    rowStyle   = rowStyle_fn,
    style      = list(
      fontFamily = "system-ui, sans-serif",
      fontSize   = "13px",
      borderCollapse = "collapse",
      borderSpacing  = "0",
      margin     = "0 auto",
      width      = "auto"
    ),
    details = function(index) {
      row <- display_data[index, ]
      if (identical(row$Site, "Reference Median")) {
        ref_rows <- dplyr::filter(ref_data, .data$Species == row$Species, .data$Population == row$Population)
        ref_cols_present <- intersect(size_cols_display, names(ref_rows))
        ref_rows <- dplyr::select(ref_rows, .data$Site, dplyr::all_of(ref_cols_present))
        reactable::reactable(ref_rows, compact = TRUE, bordered = TRUE, pagination = FALSE)
      } else NULL
    },
    # Inject lookups for JS styling
    meta = list(medians = comparison_medians, ns = sample_ns)
  )
}
