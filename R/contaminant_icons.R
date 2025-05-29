#==============================
# Contaminant shape + colour utilities
#==============================

# Function to render inline shapes in reactable
generate_shape <- function(shape, colour = "gray", size = 12) {
  # Ensure safe defaults
  if (is.null(colour) || is.na(colour) || nchar(colour) == 0) colour <- "gray"
  if (is.null(shape) || is.na(shape) || nchar(shape) == 0) shape <- "circle"
  
  common_style <- paste0(
    "width:", size, "px;",
    " height:", size, "px;",
    " background-color:", colour, ";",  # NOTE: fixed spelling to "color"
    " display: inline-block;",
    " margin-right: 5px;",
    " vertical-align: middle;"
  )
  
  shape_style <- switch(shape,
                        "circle" = "border-radius: 50%;",
                        "square" = "",
                        "diamond" = paste0(
                          "transform: rotate(45deg);",
                          " width:", size * 0.7, "px;",
                          " height:", size * 0.7, "px;"
                        ),
                        "triangle" = paste0(
                          "width: 0; height: 0;",
                          " border-left:", size / 2, "px solid transparent;",
                          " border-right:", size / 2, "px solid transparent;",
                          " border-bottom:", size, "px solid ", colour, ";",
                          " background: none;"
                        ),
                        ""  # fallback
  )
  
  if (shape == "triangle") {
    htmltools::tags$div(style = shape_style)
  } else {
    htmltools::tags$div(style = paste(common_style, shape_style))
  }
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
                                        shape_options = c("circle", "square", "diamond", "triangle", "hexagon", "ring"),
                                        colour_options = c("goldenrod", "magenta", "skyblue", "purple", "darkorange", "seagreen")) {
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
