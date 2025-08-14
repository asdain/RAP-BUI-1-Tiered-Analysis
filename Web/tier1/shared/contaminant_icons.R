#==============================
# Contaminant shape + colour utilities
#==============================

# Function to render inline shapes in reactable
generate_shape <- function(shape, colour = "gray", size = 12, border_width = 2) {
  if (is.null(colour) || is.na(colour) || nchar(colour) == 0) colour <- "gray"
  if (is.null(shape) || is.na(shape) || nchar(shape) == 0) shape <- "circle"
  
  common_style <- paste0(
    "width:", size, "px;",
    " height:", size, "px;",
    " display: inline-block;",
    " margin-right: 5px;",
    " vertical-align: middle;"
  )
  
  shape_style <- switch(shape,
                        "circle" = paste0("background-color:", colour, "; border-radius: 50%;"),
                        "square" = paste0("background-color:", colour, ";"),
                        "diamond" = paste0(
                          "background-color:", colour, ";",
                          " transform: rotate(45deg);",
                          " width:", size * 0.7, "px;",
                          " height:", size * 0.7, "px;"
                        ),
                        "cross" = paste0(
                          "background: linear-gradient(to right, ", colour, " 40%, ", colour, " 60%),",
                          "            linear-gradient(to bottom, ", colour, " 40%, ", colour, " 60%);",
                          " background-repeat: no-repeat;",
                          " background-position: center;",
                          " background-size: 100% 2px, 2px 100%;"
                        ),
                        "ring" = paste0(
                          "background-color: transparent;",
                          " border: ", border_width, "px solid ", colour, ";",
                          " border-radius: 50%;"
                        ),
                        "outline-square" = paste0(
                          "background-color: transparent;",
                          " border: ", border_width, "px solid ", colour, ";"
                        ),
                        "outline-diamond" = paste0(
                          "background-color: transparent;",
                          " border: ", border_width, "px solid ", colour, ";",
                          " transform: rotate(45deg);",
                          " width:", size * 0.7, "px;",
                          " height:", size * 0.7, "px;"
                        ),
                        ""
  )
  
  htmltools::tags$div(style = paste(common_style, shape_style))
}


# Function to extract all unique advisory causes from the wide-format reactable input
extract_unique_adv_causes <- function(t1_df) {
  size_cols <- setdiff(names(t1_df), c("Species", "Row_Label"))
  cause_strings <- t1_df %>%
    dplyr::filter(Row_Label == "Adv cause") %>%
    dplyr::select(dplyr::all_of(size_cols)) %>%
    unlist(use.names = FALSE) %>%
    as.character()
  
  unique(trimws(unlist(strsplit(cause_strings[!is.na(cause_strings)], ","))))
}

# Function to assign shape/colour pairs to contaminants
# If you wish to add additional shape or colour options, this is where to do it.
assign_contaminant_mappings <- function(contaminants,
                                        user_shapes = NULL,
                                        user_colours = NULL,
                                        shape_options = c("circle", "square", "diamond", "ring", "outline-square", "outline-diamond", "cross"),
                                        colour_options = c("goldenrod", "magenta", "skyblue", "purple", "darkorange", "seagreen", "maroon")) {
  n <- length(contaminants)
  shape_cycle <- rep(shape_options, length.out = n)
  colour_cycle <- rep(colour_options, length.out = n)
  
  shape_map <- setNames(shape_cycle, contaminants)
  colour_map <- setNames(colour_cycle, contaminants)
  
  # Optional user overrides
  if (!is.null(user_shapes)) shape_map[names(user_shapes)] <- user_shapes
  if (!is.null(user_colours)) colour_map[names(user_colours)] <- user_colours
  
  list(shapes = shape_map, colours = colour_map)
}
