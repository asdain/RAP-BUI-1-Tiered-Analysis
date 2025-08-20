#' Generate Tier 1 Advisory Table (AOC only)
#'
#' Displays MECP advisory levels and contaminant drivers by species, size, and population type.
#' Formats include colour-coded cells and icon-based contaminant indicators.
#'
#' @param df AOC-only data from make_restrict_table()
#' @param length_levels Ordered vector of size class labels
#' @param contaminant_shapes Named list mapping contaminant to shape
#' @param contaminant_colours Named list mapping contaminant to colour
#' @param generate_shape_fn Function to draw inline shape (default = generate_shape)
#' @param restrict_threshold Numeric threshold for advisory levels to flag as restrictive
#' @param table_height CSS height for table
#' @param show_legend Logical, whether to include contaminant legend
#' @return A reactable object with legend and formatted table



render_t1_table <- function(df,
                            length_levels = NULL,
                            interest_species = NULL,
                            contaminant_shapes,
                            contaminant_colours,
                            generate_shape_fn = generate_shape,
                            table_height = "1500px",
                            show_legend = TRUE,
                            restrict_threshold = 8,
                            use_pagination = FALSE,
                            default_page_size = 12) {
  
  if (is.null(length_levels)) {
    length_levels <- tryCatch(get("length_levels", envir = .GlobalEnv),
                              error = function(...) intersect(names(df), names(df)))
  }
  
  
  
  size_cols <- length_levels[length_levels %in% names(df)]
  
  if (!is.null(interest_species)) {
    if (exists("filter_interest_species", mode = "function")) {
      df <- filter_interest_species(df, interest_species)
    } else {
      df <- df[df$Species %in% interest_species, , drop = FALSE]
    }
  }
  
  
  
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
            shape <- contaminant_shapes[[cause]] %||% "circle"
            colour <- contaminant_colours[[cause]] %||% "gray"
            generate_shape_fn(shape, colour, size = 12)
          })
          # OLD: return(htmltools::span(icons))
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
          styles$background <- if (val < restrict_threshold) "#d80032" else "#4CAF50"
          styles$color <- "#ffffff"
        } else if ((is.na(value) || value == "") && row_type %in% c("General", "Sensitive")) {
          styles$background <- "#eeeeee"
          styles$color <- "#000000"
        }
        
        return(styles)
      }
    )
  }
  
  columns_list$Row_Label <- colDef(
    name = "Population",
    sticky = "left",
    minWidth = 120,
    style = function(value) {
      base <- list(fontSize = "13px", fontWeight = "bold", fontFamily = "system-ui, sans-serif")
      if (value == "Adv cause") base <- modifyList(base, list(fontSize = "10px", fontStyle = "italic"))
      return(base)
    },
    headerStyle = list(whiteSpace = "nowrap")
  )
  
  columns_list$Species <- colDef(show = FALSE)
  columns_list$Species_display <- colDef(
    name = "Species",
    sticky = "left",
    minWidth = 140,
    style = list(fontWeight = "bold", fontSize = "15px", fontFamily = "system-ui, sans-serif")
  )
  
  rowStyle_fn <- function(index) {
    row <- df[index, ]
    prev_row <- if (index > 1) df[index - 1, ] else NULL
    next_row <- if (index < nrow(df)) df[index + 1, ] else NULL
    
    style <- list()
    if (row$Row_Label %in% c("General", "Sensitive")) style$height <- "32px"
    if (is.null(prev_row) || prev_row$Species != row$Species) style$borderTop <- "2px solid #666"
    if (is.null(next_row) || next_row$Species != row$Species) style$borderBottom <- "2px solid #666"
    return(style)
  }
  
  legend_html <- if (show_legend) {
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
  } else {
    NULL
  }
  
  tbl <- reactable::reactable(
    df,
    columns = columns_list,
    pagination = isTRUE(use_pagination),
    defaultPageSize = if (isTRUE(use_pagination)) default_page_size else 10,
    showPageSizeOptions = isTRUE(use_pagination),
    sortable = FALSE,
    height = if (isTRUE(use_pagination)) NULL else table_height,
    defaultColDef = colDef(
      sortable = FALSE,
      align = "center",
      minWidth = 90,
      style = list(
        padding = "3px 6px",
        margin  = "0",
        border  = "none",
        verticalAlign = "middle",
        fontFamily = "system-ui, sans-serif",
        fontSize   = "12px"
      ),
      headerStyle = list(padding = "6px 6px")
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
      width      = "max-content"   # <-- key
    )
  )
  
  htmltools::tagList(
    legend_html,
    htmltools::div(
      tbl,
      style = "max-width: 100%; overflow-x: auto; overflow-y: hidden; scrollbar-gutter: stable;"
    )
  )
  
  
}
