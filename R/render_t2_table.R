render_t2_table <- function(prep, table_height = "1500px") {
  display_data <- prep$display_data
  ref_data     <- prep$ref_data
  size_cols    <- prep$size_cols
  
  columns_list <- make_common_column_defs()  # your existing helper
  
  rowStyle_fn <- function(index) {
    row <- display_data[index, ]
    prev_row <- if (index > 1) display_data[index - 1, ] else NULL
    next_row <- if (index < nrow(display_data)) display_data[index + 1, ] else NULL
    style <- list()
    if (row$Site == "Reference Median") style$fontWeight <- "bold"
    if (is.null(prev_row) || prev_row$Species != row$Species) style$borderTop <- "2px solid #666"
    if (is.null(next_row) || next_row$Species != row$Species) style$borderBottom <- "2px solid #666"
    style
  }
  
  # per-size column style using your JS, fed by meta maps
  for (col in size_cols) {
    columns_list[[col]] <- reactable::colDef(
      name = col, align = "center", sortable = FALSE,
      style = reactable::JS(sprintf(
        "function(rowInfo, colInfo, state) {
          const row = rowInfo.row;
          const val = row[colInfo.id];
          const id = row.Species + '||' + row.Population + '||' + '%s';
          const ref = state.meta.medians[id];
          const n = state.meta.ns[id];

          if (row.Site === 'n') { return { fontSize: '11px', color: '#666', fontStyle: 'italic', fontFamily: 'system-ui, sans-serif' }; }
          if (row.site_type === 'AOC') {
            if (val === null) { return { background: '#eeeeee', color: '#000000', fontWeight: 'bold', fontSize: '15px', fontFamily: 'system-ui, sans-serif' }; }
            if (n === undefined || n < 3 || ref === undefined || ref === null) {
              return { background: '#999999', color: '#ffffff', fontWeight: 'bold', fontSize: '15px', fontFamily: 'system-ui, sans-serif' }; }
            if (val < ref) {
              return { background: '#d80032', color: '#ffffff', fontWeight: 'bold', fontSize: '15px', fontFamily: 'system-ui, sans-serif' }; }
            return { background: '#4CAF50', color: '#ffffff', fontWeight: 'bold', fontSize: '15px', fontFamily: 'system-ui, sans-serif' }; }
          return { fontWeight: 'normal', fontSize: '13px', fontFamily: 'system-ui, sans-serif' }; }",
        col))
    )
  }
  
  reactable::reactable(
    display_data,
    columns = columns_list,
    defaultExpanded = FALSE,
    bordered = FALSE,
    striped = FALSE,
    highlight = TRUE,
    pagination = FALSE,
    sortable = FALSE,
    height = table_height,
    rowStyle = rowStyle_fn,
    style = list(
      fontFamily = "system-ui, sans-serif",
      fontSize = "12px",
      borderCollapse = "collapse",
      borderSpacing = "0",
      margin = "0 auto",
      width = "auto"
    ),
    details = function(index) {
      row <- display_data[index, ]
      if (row$Site == "Reference Median") {
        ref_rows <- ref_data %>% filter(Species == row$Species, Population == row$Population)
        ref_cols_present <- intersect(size_cols, names(ref_rows))
        ref_rows <- ref_rows %>% select(Site, all_of(ref_cols_present))
        reactable::reactable(ref_rows, compact = TRUE, bordered = TRUE, pagination = FALSE)
      } else NULL
    },
    meta = list(medians = prep$medians_map, ns = prep$n_map)
  )
}

build_t2_reactable <- function(prep, table_height = "1500px") {
  display_data <- prep$display_data
  ref_data     <- prep$ref_data
  size_cols    <- prep$size_cols
  
  columns_list <- make_common_column_defs()  #  existing helper
  
  rowStyle_fn <- function(index) {
    row <- display_data[index, ]
    prev_row <- if (index > 1) display_data[index - 1, ] else NULL
    next_row <- if (index < nrow(display_data)) display_data[index + 1, ] else NULL
    style <- list()
    if (is.null(prev_row) || prev_row$Species != row$Species) style$borderTop <- "2px solid #666"
    if (is.null(next_row) || next_row$Species != row$Species) style$borderBottom <- "2px solid #666"
    style
  }
  
  # per-size column style using your JS, fed by meta maps
  for (col in size_cols) {
    columns_list[[col]] <- reactable::colDef(
      name = col, align = "center", sortable = FALSE,
      minWidth = 50,
      maxWidth = 80,
      style = reactable::JS(sprintf(
        "function(rowInfo, colInfo, state) {
          const row = rowInfo.row;
          const val = row[colInfo.id];
          const id = row.Species + '||' + row.Population + '||' + '%s';
          const ref = state.meta.medians[id];
          const n = state.meta.ns[id];

          if (row.Site === 'n') { return { fontSize: '10px', color: '#666', fontStyle: 'italic', fontFamily: 'system-ui, sans-serif' }; }
          if (row.site_type === 'AOC') {
            if (val === null) { return { background: '#eeeeee', color: '#000000', fontWeight: 'bold', fontSize: '12px', fontFamily: 'system-ui, sans-serif' }; }
            if (n === undefined || n < 3 || ref === undefined || ref === null) {
              return { background: '#999999', color: '#ffffff', fontWeight: 'bold', fontSize: '11px', fontFamily: 'system-ui, sans-serif' }; }
            if (val < ref) {
              return { background: '#d80032', color: '#ffffff', fontWeight: 'bold', fontSize: '11px', fontFamily: 'system-ui, sans-serif' }; }
            return { background: '#4CAF50', color: '#ffffff', fontWeight: 'bold', fontSize: '11px', fontFamily: 'system-ui, sans-serif' }; }
          return { fontWeight: 'normal', fontSize: '13px', fontFamily: 'system-ui, sans-serif' }; }",
        col))
    )
  }
  
  reactable::reactable(
    display_data,
    columns = columns_list,
    defaultExpanded = FALSE,
    bordered = FALSE,
    striped = FALSE,
    highlight = TRUE,
    pagination = FALSE,
    sortable = FALSE,
    height = table_height,
    rowStyle = rowStyle_fn,
    style = list(
      fontFamily = "system-ui, sans-serif",
      fontSize = "11px",
      borderCollapse = "collapse",
      borderSpacing = "0",
      margin = "0 auto",
      width = "auto"
    ),
    details = function(index) {
      row <- display_data[index, ]
      if (row$Site == "Reference Median") {
        ref_rows <- ref_data %>% filter(Species == row$Species, Population == row$Population)
        ref_cols_present <- intersect(size_cols, names(ref_rows))
        ref_rows <- ref_rows %>% select(Site, all_of(ref_cols_present))
        reactable::reactable(ref_rows, compact = TRUE, bordered = TRUE, pagination = FALSE)
      } else NULL
    },
    meta = list(medians = prep$medians_map, ns = prep$n_map)
  )
}

render_t2_table_any <- function(prep = NULL,
                                df = NULL,
                                aoc_id = NULL,
                                reference_sites = NULL,
                                length_levels = NULL,
                                interest_species = NULL,
                                exclude_t1_passed = TRUE,
                                flags_df = NULL,
                                table_height = "1500px",
                                force = c("auto","reactable","flextable")) {
  force <- match.arg(force)
  if (force == "auto") {
    force <- if (knitr::is_html_output()) "reactable" else "flextable"
  }
  
  # Use prepped payload if provided; else build from df
  if (is.null(prep)) {
    stopifnot(!is.null(df), !is.null(aoc_id), !is.null(reference_sites))
    prep <- t2_prep_display(df, aoc_id, reference_sites, length_levels,
                            interest_species, exclude_t1_passed, flags_df)
  }
  
  if (is.null(prep$display_data) || nrow(prep$display_data) == 0) {
    if (force == "reactable") {
      return(reactable::reactable(data.frame(Message = "No advisory data available for selected filters.")))
    } else {
      return(flextable::flextable(data.frame(Message = "No advisory data available for selected filters.")))
    }
  }
  
  if (force == "reactable") {
    build_t2_reactable(prep, table_height = table_height)
  } else {
    build_t2_flextable(prep)
  }
}

