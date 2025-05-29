render_t1_table <- function(df,
                            length_levels,
                                  contaminant_shapes,
                                  contaminant_colours,
                                  generate_shape_fn = generate_shape,
                                  restrict_threshold = 8,
                                  table_height = "1500px",
                                  show_legend = TRUE) {
  # Identify size class columns
  size_cols <- length_levels[length_levels %in% names(df)]

  
  # Generate column definitions
  columns_list <- list()
  
  for (col_name in size_cols) {
    columns_list[[col_name]] <- colDef(
      name = col_name,
      html = TRUE,
      cell = function(value, index) {
        row <- df[index, ]
        row_type <- row$Row_Label
        
        if (row_type == "Adv cause" && !is.na(value) && nzchar(value)) {
          causes <- trimws(unlist(strsplit(value, ",")))
          icons <- lapply(causes, function(cause) {
            shape <- contaminant_shapes[[cause]]
            colour <- contaminant_colours[[cause]]
            if (is.null(shape)) shape <- "circle"
            if (is.null(colour)) colour <- "gray"
            generate_shape_fn(shape, colour, size = 12)
          })
          return(htmltools::span(icons))
        }
        
        if (row_type != "Adv cause" && !is.na(value)) return(value)
        return("")
      },
      style = function(value, index) {
        row <- df[index, ]
        row_type <- row$Row_Label
        styles <- list(padding = "0", margin = "0", fontWeight = "bold")
        
        if (row_type == "Adv cause") {
          styles$paddingTop <- "0"
          styles$paddingBottom <- "10px"
        } else if (!is.na(value)) {
          val <- as.numeric(value)
          styles$background <- if (val < restrict_threshold) "#ff4d4d" else "#66cc66"
        } else if ((is.na(value) || value == "") && row_type %in% c("General", "Sensitive")) {
          styles$background <- "#eef2f7"
        }
        
        return(styles)
      }
    )
  }
  
  # Row_Label column
  columns_list$Row_Label <- colDef(
    name = "Population",
    sticky = "left",
    minWidth = 120,
    style = function(value) {
      if (value == "Adv cause") list(fontSize = "10px", fontStyle = "italic")
      else list(fontSize = "13px", fontWeight = "bold")
    },
    headerStyle = list(whiteSpace = "nowrap")
  )
  
  # Species column with hidden duplicates
  columns_list$Species = colDef(show = FALSE)
  
  columns_list$Species_display <- colDef(
    name = "Species",
    sticky = "left",
    minWidth = 140,
    style = list(fontWeight = "bold", fontSize = "13px")
  )
  
  
  # Row styling function
  rowStyle_fn <- function(index) {
    row <- df[index, ]
    prev_row <- if (index > 1) df[index - 1, ] else NULL
    next_row <- if (index < nrow(df)) df[index + 1, ] else NULL
    
    style <- list()
    if (row$Row_Label %in% c("General", "Sensitive")) {
      style$height <- "32px"
    }
    if (is.null(prev_row) || prev_row$Species != row$Species) style$borderTop <- "2px solid #666"
    if (is.null(next_row) || next_row$Species != row$Species) style$borderBottom <- "2px solid #666"
    return(style)
  }
  
  # Generate legend (if enabled)
  if (show_legend) {
    unique_contaminants <- names(contaminant_shapes)
    legend_items <- lapply(unique_contaminants, function(contaminant) {
      shape <- contaminant_shapes[[contaminant]]
      colour <- contaminant_colours[[contaminant]]
      htmltools::div(
        style = "display: inline-block; margin-right: 12px; font-family: sans-serif; font-size: 13px;",
        generate_shape_fn(shape, colour, 12),
        contaminant
      )
    })
    
    legend_html <- htmltools::div(
      style = "margin-bottom: 10px; font-family: sans-serif; font-size: inherit; color: black;",
      htmltools::strong("Advisory Cause:"),
      htmltools::div(
        style = "display: flex; flex-wrap: wrap; gap: 12px;",
        legend_items
      )
    )
    
  } else {
    legend_html <- NULL
  }
  # Ensure df uses correct column order
  ordered_df <- df[, c("Species_display", "Row_Label", size_cols)]
  # Final reactable widget
  htmltools::div(
    htmltools::browsable(htmltools::tagList(
      legend_html,
      reactable::reactable(
        df,
        columns = columns_list,
        defaultPageSize = 10,
        showPageSizeOptions = TRUE,
        pagination = FALSE,
        height = table_height,
        defaultColDef = colDef(
          sortable = TRUE,
          align = "center",
          minWidth = 80,
          style = list(padding = "0", margin = "0", border = "none", verticalAlign = "middle")
        ),
        rowStyle = rowStyle_fn,
        bordered = FALSE,
        striped = FALSE,
        highlight = TRUE,
        style = list(
          fontFamily = "sans-serif",
          fontSize = "13px",
          borderCollapse = "collapse",
          borderSpacing = "0",
          margin = "0 auto",
          width = "auto"
        )
      )
    )),
    style = "max-width: 100%; overflow-x: auto;"
  )
}
