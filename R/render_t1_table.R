render_t1_table <- function(df,
                            length_levels = NULL,
                            contaminant_shapes,
                            contaminant_colours,
                            generate_shape_fn = generate_shape,
                            shape_size = 12,
                            table_height = "1500px",
                            show_legend = TRUE,
                            use_pagination = FALSE,
                            default_page_size = 12) {
  
  if (is.null(length_levels)) {
    length_levels <- tryCatch(get("length_levels", envir = .GlobalEnv),
                              error = function(...) intersect(names(df), names(df)))
  }
  size_cols <- length_levels[length_levels %in% names(df)]
  
  
  
 # If dynamic unrestrictive thresrholds missing, default to 8
  if (!"Unrestrictive_Threshold" %in% names(df)) {
    df$Unrestrictive_Threshold <- 8
  }
  
  columns_list <- list()
  for (col_name in size_cols) {
    columns_list[[col_name]] <- colDef(
      name = col_name,
      html = TRUE,
      minWidth = 50,
      maxWidth = 80,
      cell = function(value, index) {
        row <- df[index, ]
        row_type <- row$Row_Label
        
        if (row_type == "Adv cause" && !is.na(value) && nzchar(value)) {
          causes <- trimws(unlist(strsplit(value, ",")))
          icons <- lapply(causes, function(cause) {
            shape <- contaminant_shapes[[cause]] %||% "circle"
            colour <- contaminant_colours[[cause]] %||% "gray"
            generate_shape_fn(shape, colour, size = shape_size)
          })
          return(do.call(htmltools::span, c(icons, list(style = "display:inline-flex;gap:6px;align-items:center;"))))
        }
        
        if (row_type != "Adv cause" && !is.na(value)) return(value)
        return("")
      },
      style = function(value, index) {
        row <- df[index, ]
        row_type <- row$Row_Label
        styles <- list(padding = "0", margin = "0", fontWeight = "bold", fontFamily = "system-ui, sans-serif")
        
        if (row_type == "Adv cause") {
          styles$paddingTop <- "0"
          styles$paddingBottom <- "10px"
        } else if (!is.na(value)) {
          val <- as.numeric(value)
          thr <- as.numeric(row$Unrestrictive_Threshold)
          # match your backend logic: restrictive = adv_level <= threshold
          styles$background <- if (!is.na(val) && !is.na(thr) && val <= thr) "#d80032" else "#4CAF50"
          styles$color <- "#ffffff"
        } else if ((is.na(value) || value == "") && row_type %in% c("General", "Sensitive")) {
          styles$background <- "#eeeeee"
          styles$color <- "#000000"
        }
        styles
      }
    )
  }
  
  columns_list$Row_Label <- colDef(
    name = "Population",
    minWidth = 90,
    style = function(value) {
      base <- list(fontSize = "12px", fontFamily = "system-ui, sans-serif")
      if (value == "Adv cause") base <- modifyList(base, list(fontSize = "10px", fontStyle = "italic"))
      base
    }
  )
  
  columns_list$Species <- colDef(show = FALSE)
  columns_list$Species_display <- colDef(
    name = "Species",
    minWidth = 140,
    maxWidth = 150,
    style = list(fontWeight = "bold", fontSize = "12px", fontFamily = "system-ui, sans-serif")
  )
  
  columns_list$Unrestrictive_Threshold <- colDef(
    name = "Desired Meals/Month",
    minWidth = 80,
    maxWidth = 100,
    align = "center",
    style = list(fontSize = "12px", fontFamily = "system-ui, sans-serif")
  )
  

  
  rowStyle_fn <- function(index) {
    row <- df[index, ]
    prev_row <- if (index > 1) df[index - 1, ] else NULL
    next_row <- if (index < nrow(df)) df[index + 1, ] else NULL
    
    style <- list()
    if (row$Row_Label %in% c("General", "Sensitive")) style$height <- "32px"
    if (is.null(prev_row) || prev_row$Species != row$Species) style$borderTop <- "2px solid #666"
    if (is.null(next_row) || next_row$Species != row$Species) style$borderBottom <- "2px solid #666"
    style
  }
  
  # Reorder the data frame columns (reactable has no columnOrder arg)
  ordered_cols <- c("Species_display", "Unrestrictive_Threshold", "Row_Label", size_cols)
  df_render <- df[, c(ordered_cols, setdiff(names(df), c(ordered_cols))), drop = FALSE]
  
  htmltools::div(
    htmltools::browsable(htmltools::tagList(
      if (isTRUE(show_legend)) {
        legend_items <- lapply(names(contaminant_shapes), function(contaminant) {
          shape <- contaminant_shapes[[contaminant]]
          colour <- contaminant_colours[[contaminant]]
          htmltools::div(
            style = "display: inline-block; margin-right: 12px; font-family: sans-serif; font-size: 13px;",
            generate_shape_fn(shape, colour, 12),
            contaminant
          )
        })
        htmltools::div(
          style = "margin-bottom: 10px; font-family: system-ui, sans-serif; font-size: inherit; color: black;",
          htmltools::strong("Advisory Cause:"),
          htmltools::div(style = "display: flex; flex-wrap: wrap; gap: 12px;", legend_items)
        )
      } else NULL,
      reactable::reactable(
        df_render,
        columns = columns_list,
        columnGroups = list(
          colGroup(columns = c("Row_Label", "Species_display", "Unrestrictive_Threshold"),
          sticky = "left")
        ),
        pagination = isTRUE(use_pagination),
        defaultPageSize = if (isTRUE(use_pagination)) default_page_size else 10,
        showPageSizeOptions = isTRUE(use_pagination),
        sortable = FALSE,
        height = if (isTRUE(use_pagination)) NULL else table_height,
        defaultColDef = colDef(
          sortable = FALSE,
          align = "center",
          minWidth = 50,
          style = list(
            padding = "0",
            margin  = "0",
            border  = "none",
            verticalAlign = "middle",
            fontFamily = "system-ui, sans-serif",
            fontSize   = "12px",
            width      = "100%"
          ),
          headerStyle = list(padding = "0px 0px", margin = "0")
        ),
        rowStyle = rowStyle_fn,
        bordered = FALSE,
        striped  = FALSE,
        highlight = FALSE,
        style = list(
          fontFamily = "system-ui, sans-serif",
          fontSize   = "12px",
          borderCollapse = "collapse",
          borderSpacing  = "0",
          margin     = "0 auto",
          width      = "100%"
        )
      )
    )),
    style = "max-width: 100%; overflow-x: auto;"
  )
}


# Another version of the function, to use either the original reactable table or flextable depending on HTML or Word
render_t1_table_any <- function(df,
                                length_levels = NULL,
                                contaminant_shapes = NULL,   # ignored for flextable
                                contaminant_colours = NULL,  # ignored for flextable
                                generate_shape_fn = NULL,    # ignored for flextable
                                shape_size = NULL, # ignored for flextable
                                table_height = "1500px",
                                show_legend = TRUE,
                                use_pagination = FALSE,
                                default_page_size = 12,
                                force = c("auto","reactable","flextable")) {
  force <- match.arg(force)
  
  is_html <- knitr::is_html_output()
  if (force == "auto") force <- if (is_html) "reactable" else "flextable"
  
  if (force == "reactable") {
    # your existing HTML/reactable function
    return(render_t1_table(
      df = df,
      length_levels = length_levels,
      contaminant_shapes = contaminant_shapes,
      contaminant_colours = contaminant_colours,
      generate_shape_fn = if (is.null(generate_shape_fn)) generate_shape else generate_shape_fn,
      shape_size = if (is.null(shape_size)) 12 else shape_size,
      table_height = table_height,
      show_legend = show_legend,
      use_pagination = use_pagination,
      default_page_size = default_page_size
    ))
  } else {
    # single non-HTML renderer
    return(build_t1_flextable(
      df = df,
      length_levels = length_levels,
      show_threshold_once = TRUE
    ))
  }
}

