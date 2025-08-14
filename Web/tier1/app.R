library(shiny)
library(readr)
library(reactable)
library(here)

# Bring in your renderer
source(here("R", "render_t1_table.R"))


#Consumption thresholds
thr_path <- here("Data","consumption_threshold.csv")
thr_df <- if (file.exists(thr_path)) readr::read_csv(thr_path, show_col_types = FALSE) else tibble::tibble()

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

contaminant_shapes  <- list("Mercury"="circle","PCB"="square","PFAS"="triangle")
contaminant_colours <- list("Mercury"="#1f77b4","PCB"="#ff7f0e","PFAS"="#2ca02c")

# Load precomputed CSV
t1_path <- here("web","tier1","data","t1_wide.csv")
stopifnot(file.exists(t1_path))
t1_wide <- readr::read_csv(t1_path, show_col_types = FALSE)

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
                          selected = if ("Walleye" %in% t1_wide$Species) "Walleye" else sort(unique(t1_data$Species))[1])),
    column(6, div(style="margin-top:28px;",
                  textOutput("threshold_text")))   # just display, not editable
  ),
  uiOutput("tbl")
)

server <- function(input, output, session) {
  filtered <- reactive({
    subset(t1_wide, Species == input$species)
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

