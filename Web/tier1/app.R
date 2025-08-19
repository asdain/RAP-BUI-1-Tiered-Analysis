library(shiny)
library(readr)
library(reactable)
library(rlang)
library(dplyr)
library(htmltools)
library(htmlwidgets)

# Globals
contaminant_shapes  <- list("Mercury"="circle","PCB"="square","PFAS"="triangle")
contaminant_colours <- list("Mercury"="#1f77b4","PCB"="#ff7f0e","PFAS"="#2ca02c")
length_levels <- c("15-20cm", "20-25cm", "25-30cm", "30-35cm", "35-40cm", "40-45cm",
                   "45-50cm", "50-55cm", "55-60cm", "60-65cm", "65-70cm", "70-75cm", ">75cm")

# Data path: prefer app-local for exported site; fallback for local dev


thr_path <- "data/consumption_threshold.csv"


if (file.exists("data/t1_wide.rds")) {
  t1_wide <- readRDS("data/t1_wide.rds")
} else {
  t1_wide <- readr::read_csv("data/t1_wide.csv", show_col_types = FALSE)
}

thr_df <- if (file.exists(thr_path)) {readr::read_csv(thr_path, show_col_types = FALSE)} else {tibble::tibble()}



# Validate data present
if (!file.exists("data/t1_wide.csv")) {
  stop("Missing data/t1_wide.csv. Ensure the data directory is included in the exported app.")
}

# After reading:
validate_cols <- c("Species","Species_display","Row_Label")
stopifnot(all(validate_cols %in% names(t1_wide)))




if (dir.exists("shared")) {
  r_files <- list.files("shared", pattern = "\\.R$", full.names = TRUE)
  lapply(r_files, source)
} else if (interactive() && dir.exists("../../R")) {
  r_files <- list.files("../../R", pattern = "\\.R$", full.names = TRUE)
  lapply(r_files, source)
}






## Example switch to URL reading (only if you’re sure files are web-served):
#t1_url  <- "./data/t1_wide.csv"
#thr_url <- "./data/consumption_threshold.csv"
#
#t1_wide <- readr::read_csv(t1_url, show_col_types = FALSE)
#thr_df  <- tryCatch(readr::read_csv(thr_url, show_col_types = FALSE),
#                    error = function(e) tibble::tibble())

#Consumption thresholds

get_threshold <- function(sp) {
  if (nrow(thr_df)) {
    v <- thr_df$threshold[match(sp, thr_df$Species)]
    ifelse(is.na(v), 8, v)
  } else 8
}


# Fallback generate_shape used by your renderer
generate_shape <- function(shape = "circle", colour = "gray", size = 12) {
  if (shape == "triangle") {
    tags$svg(width = size, height = size, viewBox = "0 0 20 20", `aria-hidden` = "true",
             tags$polygon(points = "10,3 18,17 2,17", fill = colour)
    )
  } else if (shape == "square") {
    tags$svg(width = size, height = size, viewBox = "0 0 20 20", `aria-hidden` = "true",
             tags$rect(x = 3, y = 3, width = 14, height = 14, fill = colour)
    )
  } else {
    tags$svg(width = size, height = size, viewBox = "0 0 20 20", `aria-hidden` = "true",
             tags$circle(cx = 10, cy = 10, r = 7, fill = colour)
    )
  }
}





if (!exists("render_t1_table")) {
  stop("render_t1_table() is not available. Did you include shared/*.R in the export?")
}


base_cols     <- c("Species","Species_display","Row_Label")
length_levels <- setdiff(names(t1_wide), base_cols)
species_choices <- sort(unique(t1_wide$Species))
default_species <- if ("Walleye" %in% species_choices) "Walleye" else species_choices[1]

ui <- fluidPage(
  tags$head(tags$style("main{max-width:1200px;margin:0 auto}")),
  h2("Tier 1 — Advisory Table (AOC)"),
  fluidRow(
    column(6, selectInput("species","Species",
                          choices = sort(unique(t1_wide$Species)),
                          selected = if ("Walleye" %in% t1_wide$Species) "Walleye" else sort(unique(t1_wide$Species))[1])),
    column(6, div(style="margin-top:28px;",
                  textOutput("threshold_text")))   # just display, not editable
  ),
  uiOutput("tbl")
)

server <- function(input, output) {
  filtered <- reactive({
    req(input$species)
    out <- subset(t1_wide, Species == input$species)
    req(nrow(out) > 0)
    out
  })
  
  
  output$threshold_text <- renderText({
    paste("Community desired consumption rate:", get_threshold(input$species), "meals/month")
  })
  
  output$tbl <- renderUI({
    render_t1_table(
      df = filtered(),
      length_levels = length_levels,
      contaminant_shapes = contaminant_shapes,
      contaminant_colours = contaminant_colours,
      generate_shape_fn = generate_shape,
      table_height = "1200px",
      show_legend = TRUE,
      restrict_threshold = get_threshold(input$species)
    )
  })
}


shinyApp(ui, server)

