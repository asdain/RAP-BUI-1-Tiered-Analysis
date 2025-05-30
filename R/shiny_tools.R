#' Generate a waterbody selector input (Shiny-only)
#'
#' @param choices A character vector of waterbody names
#' @return A Shiny UI selectizeInput or an informative error if Shiny is not available
make_waterbody_selector <- function(choices) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Shiny is not installed. This input is only available in a Shiny app context.")
  }
  
  shiny::selectizeInput(
    inputId = "waterbody",
    label = "Select a waterbody",
    choices = choices,
    multiple = FALSE,
    options = list(
      placeholder = "Start typing a waterbody name...",
      maxOptions = 15
    )
  )
}

#' Render this input only in interactive mode (e.g., in dev or inside Shiny)
run_advisory_app = function(){
  if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
  library(shiny)
  
  shinyApp(
    ui = fluidPage(
      make_waterbody_selector(c("Lake St. Francis", "Lake Erie", "Ottawa River")),
      textOutput("selected")
    ),
    server = function(input, output, session) {
      output$selected <- renderText({ paste("You selected:", input$waterbody) })
    }
  )
  }
}
