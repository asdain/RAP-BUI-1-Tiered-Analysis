# Web/tier2/app.R

library(shiny)
library(reactable)

# ---- load precomputed bundle (preferred) ----
bundle_path <- "data/t2_bundle.rds"
if (!file.exists(bundle_path)) stop("Missing ", bundle_path)
bundle <- readRDS(bundle_path)

length_levels <- bundle$length_levels
display_all   <- bundle$display        # multiple species
ref_detail_all<- bundle$ref_detail     # multiple species
med_lookup    <- bundle$medians
n_lookup      <- bundle$ns

# ---- species choices & default (Walleye if present) ----
species_choices <- c("Bluegill",
                     "Freshwater Drum",
                     "Largemouth Bass",
                     "Northern Pike",
                     "Rock Bass",
                     "Smallmouth Bass",
                     "Walleye",
                     "White Perch",
                     "White Sucker",
                     "Yellow Perch")
default_species <- if ("Walleye" %in% species_choices) "Walleye" else species_choices[1]

# ---- UI ----
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      main{max-width:900px;margin:0 auto;}
      .Reactable .rt-th, .Reactable .rt-td { white-space: nowrap; }
      @media (max-width: 600px) {
        main{max-width:100%; padding: 0 8px;}
      }
    "))
  ),
  h2("Tier 2 — AOC vs Reference"),
  fluidRow(
    column(6, selectInput("species", "Species", choices = species_choices, selected = default_species))
  ),
  uiOutput("t2_table")
)

# ---- helpers: compact/paginated preset ----
table_paged <- function(data, columns_list, details_fn = NULL, meta = NULL) {
  reactable::reactable(
    data, columns = columns_list,
    pagination = TRUE,
    defaultPageSize = 12,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(10, 12, 15, 25),
    compact = TRUE,
    style = list(fontFamily="system-ui, sans-serif", fontSize="12px", width="100%"),
    defaultColDef = colDef(
      minWidth = 70,                       
      align    = "center",
      style        = list(padding = "3px 6px"),     
      headerStyle  = list(padding = "6px 6px")      
    ),details = details_fn,
    meta = meta
  )
}

# ---- server ----
server <- function(input, output, session) {
  
  # make columns (sticky species/pop labels, hide internals)
  make_columns <- function(size_cols) {
    cols <- list(
      Species_display    = colDef(name="Species",    sticky="left", minWidth=90, maxWidth = 120, html = TRUE,
                                  style = JS("
    function() {
      return {
        whiteSpace: 'normal',
        wordBreak: 'break-word',
        lineHeight: '1.2',
        fontWeight: 'bold',
        fontSize: '13px',
        fontFamily: 'system-ui, sans-serif'
      };
    }
  "),
                                  headerStyle = list(whiteSpace = "nowrap", padding = "6px 6px")
      ),
      Population_display = colDef(name="Population", sticky="left", minWidth=90, maxWidth = 100,
                                  style=list(fontWeight="bold", fontSize="12px")),
      Site       = colDef(sticky="left", minWidth=140),
      site_type  = colDef(show=FALSE),
      id         = colDef(show=FALSE),
      Species    = colDef(show=FALSE),
      Population = colDef(show=FALSE),
      site_order = colDef(show=FALSE)
    )
    # per-size columns (JS styling uses precomputed lookups)
    for (col in size_cols) {
      cols[[col]] <- colDef(
        name = col, align = "center", sortable = FALSE,
        style = JS(sprintf("
          function(rowInfo, colInfo, state) {
            const row = rowInfo.row;
            const val = row[colInfo.id];
            const id  = row.Species + '||' + row.Population + '||' + '%s';
            const ref = state.meta.medians[id];
            const n   = state.meta.ns[id];

            const base = { fontFamily: 'system-ui, sans-serif', fontWeight: 'bold', fontSize: '15px' };
            if (row.Site === 'n') return { ...base, fontSize: '11px', color:'#666', fontStyle:'italic' };
            if (row.site_type === 'AOC') {
              if (val === null)                              return { ...base, background:'#eee',  color:'#000' };
              if (n === undefined || n < 3 || ref == null)  return { ...base, background:'#999',  color:'#fff' };
              if (val < ref)                                 return { ...base, background:'#d80032', color:'#fff' };
              return { ...base, background:'#4CAF50', color:'#fff' };
            }
            return { fontFamily:'system-ui, sans-serif', fontSize:'13px' };
          }", col))
      )
    }
    cols
  }
  
  output$t2_table <- renderUI({
    sp <- input$species
    req(sp)
    
    # filter bundle to selected species
    display    <- display_all[display_all$Species == sp, , drop = FALSE]
    ref_detail <- ref_detail_all[ref_detail_all$Species == sp, , drop = FALSE]
    
    # visible size columns
    size_cols <- intersect(length_levels, names(display))
    
    # details panel (reference rows for this species/pop)
    details_fn <- function(i) {
      row <- display[i, ]
      if (identical(row$Site, "Reference Median")) {
        sub <- ref_detail[ref_detail$Species == row$Species &
                            ref_detail$Population == row$Population, , drop = FALSE]
        if (nrow(sub)) {
          keep <- c("Site", intersect(size_cols, names(sub)))
          reactable(
            sub[, keep, drop = FALSE],
            compact    = TRUE,
            bordered   = TRUE,
            pagination = FALSE,
            columns = list(
              Site = colDef(
                name = "Site",
                minWidth = 120,
                maxWidth = 220,   # cap width so it wraps
                html = TRUE,
                style = JS("
              function() {
                return {
                  whiteSpace: 'normal',
                  wordBreak: 'break-word',
                  lineHeight: '1.2',
                  fontSize: '12px',
                  fontFamily: 'system-ui, sans-serif'
                };
              }
            "),
                headerStyle = list(whiteSpace = "nowrap", padding = "4px 6px")
              )
            ),
            defaultColDef = colDef(
              minWidth = 80,
              align = "center",
              style = list(padding = "3px 6px")
            )
          )
        } else NULL
      } else NULL
    }
    
    
    columns_list <- make_columns(size_cols)
    
    columns_list$Site <- colDef(
      name = "Site",
      sticky = "left",
      minWidth = 120,
      maxWidth = 220,     # cap so it wraps sooner
      html = TRUE,
      style = JS("
    function() {
      return {
        whiteSpace: 'normal',
        wordBreak: 'break-word',
        lineHeight: '1.2'
      };
    }
  "),
      headerStyle = list(whiteSpace = "nowrap", padding = "6px 6px")
    )
    
    
    # render (paginated, compact)
    div(
      style = "max-width:100%; overflow-x:auto;",
      table_paged(
        data = display,
        columns_list = columns_list,
        details_fn = details_fn,
        meta = list(medians = med_lookup, ns = n_lookup)
      )
    )
  })
}

shinyApp(ui, server)

