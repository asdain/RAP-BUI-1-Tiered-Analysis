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
  thresholds   <- if (!is.null(prep$threshold_map)) prep$threshold_map else list()
  aoc_vals     <- if (!is.null(prep$aoc_map)) prep$aoc_map else list()
  adv_palette  <- if (!is.null(adv_palette)) adv_palette else list()
  
  columns_list <- make_common_column_defs()
  
  rowStyle_fn <- function(index) {
    row <- display_data[index, ]
    prev_row <- if (index > 1) display_data[index - 1, ] else NULL
    next_row <- if (index < nrow(display_data)) display_data[index + 1, ] else NULL
    style <- list()
    if (is.null(prev_row) || prev_row$Species != row$Species) style$borderTop <- "2px solid #666"
    if (is.null(next_row) || next_row$Species != row$Species) style$borderBottom <- "2px solid #666"
    style
  }
  
  # per-size column style using JS, fed by meta maps
  for (col in size_cols) {
    columns_list[[col]] <- reactable::colDef(
      name = col, align = "center", sortable = FALSE,
      minWidth = 50,
      maxWidth = 80,
      style = reactable::JS(sprintf(
        "function(rowInfo, colInfo, state) {
  const row = rowInfo.row;
  const val = row[colInfo.id];
  const id  = row.Species + '||' + row.Population + '||' + '%s';
  const ref = state.meta.medians[id];
  const n   = state.meta.ns[id];
  const thr = state.meta.thresholds ? state.meta.thresholds[row.Species] : undefined;
  const aocVal = state.meta.aoc ? state.meta.aoc[id] : undefined;
  const pal = state.meta.palette;

  // 'n' row styling
  if (row.Site === 'n') {
    let style = {
      fontSize: '10px',
      color: '#666',
      fontStyle: 'italic',
      fontFamily: 'system-ui, sans-serif'
    };

    // If the AOC size passed T1, fade this n cell too
    if (thr !== undefined && thr !== null &&
        aocVal !== undefined && aocVal !== null && aocVal >= thr) {
      style.opacity = 0.4;
    }

    return style;
  }

  if (row.site_type === 'AOC') {
    // Missing AOC advisory
    if (val === null) {
      return {
        background: '#eeeeee',
        color: '#000000',
        fontWeight: 'bold',
        fontSize: '12px',
        fontFamily: 'system-ui, sans-serif'
      };
    }

    // 1) If this size class is already ABOVE the unrestrictive threshold,
    //    treat it as a Tier 1 pass and fade it out green.
    if (thr !== undefined && thr !== null && val >= thr) {
      return {
        background: pal.pass,
        color: '#ffffff',
        opacity: 0.4,
        fontWeight: 'normal',
        fontSize: '11px',
        fontFamily: 'system-ui, sans-serif'
      };
    }

    // 2) Otherwise, fall back to Tier 2 reference comparison logic:
    //    insufficient reference data
    if (n === undefined || n < 3 || ref === undefined || ref === null) {
      return {
        background: pal.insufficient,
        color: '#ffffff',
        fontWeight: 'bold',
        fontSize: '11px',
        fontFamily: 'system-ui, sans-serif'
      };
    }

    // more restrictive than reference median
    if (val < ref) {
      return {
        background: pal.fail,
        color: '#ffffff',
        fontWeight: 'bold',
        fontSize: '11px',
        fontFamily: 'system-ui, sans-serif'
      };
    }

    // not more restrictive than reference median
    return {
      background: pal.pass,
      color: '#ffffff',
      fontWeight: 'bold',
      fontSize: '11px',
      fontFamily: 'system-ui, sans-serif'
    };
  }

  // Reference rows
  if (row.site_type === 'Reference') {
    let style = {
      fontWeight: 'normal',
      fontSize: '13px',
      fontFamily: 'system-ui, sans-serif'
    };

    // If the AOC size passed T1, fade this reference cell too
    if (thr !== undefined && thr !== null &&
        aocVal !== undefined && aocVal !== null && aocVal >= thr) {
      style.opacity = 0.4;
    }

    return style;
  }

  // default (just in case)
  return {
    fontWeight: 'normal',
    fontSize: '13px',
    fontFamily: 'system-ui, sans-serif'
  };
}", col))
    )
  }
  
  reactable::reactable(
    display_data,
    columns = columns_list,
    columnGroups = list(
      colGroup(columns = c("Species_display", "Population_display", "Site"),
               sticky = "left")),
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
        ref_rows <- ref_data %>% dplyr::filter(Species == row$Species, Population == row$Population)
        ref_cols_present <- intersect(size_cols, names(ref_rows))
        ref_rows <- ref_rows %>% dplyr::select(Site, dplyr::all_of(ref_cols_present))
        reactable::reactable(ref_rows, compact = TRUE, bordered = TRUE, pagination = FALSE)
      } else NULL
    },
    meta = list(
      medians    = prep$medians_map,
      ns         = prep$n_map,
      thresholds = thresholds,
      aoc        = aoc_vals,
      palette = adv_palette
    )
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
                                threshold_df = NULL,
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

