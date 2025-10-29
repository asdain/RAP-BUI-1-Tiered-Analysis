

t3_fig <- function(idx, which = c("map","t3a_virtual","t3b_gam_plot","t3c_temporal", "t3c_projected"),
                   as_path = TRUE) {
  which <- match.arg(which)
  # We saved figure paths under idx$figs[[name]]
  pth <- idx$figs[[which]]
  if (is.null(pth)) stop("Figure not found in index: ", which)
  if (as_path) return(pth)
  
  # Optional: if your index stored ggplot objects instead of paths
  gp <- idx$figs_obj[[which]]
  if (!is.null(gp)) return(gp)
  stop("Figure stored as path only; set as_path = TRUE.")
}

t3_widget <- function(idx, name) {
  pth <- idx$html[[name]]
  if (is.null(pth)) stop("HTML widget not found: ", name)
  pth
}

t3_widget_png <- function(idx, name) {
  # If your analysis saved webshot snapshots, we also recorded png paths:
  pth <- idx$widgets_snapshots[[name]]
  if (is.null(pth)) {
    # Fallback: guess same name in widgets_dir
    paths <- attr(idx, "paths")
    guess <- fs::path(paths$widgets_dir, paste0(name, ".png"))
    if (file.exists(guess)) return(guess)
    stop("PNG snapshot not found for widget: ", name)
  }
  pth
}


t3_table <- function(idx, name) {
  # index tables are lists like list(csv=..., rds=...)
  ent <- idx$tables[[name]]
  if (is.null(ent)) stop("Table not in index: ", name)
  
  if (is.list(ent) && !is.null(ent$rds) && file.exists(ent$rds)) {
    return(readRDS(ent$rds))
  }
  if (is.list(ent) && !is.null(ent$csv) && file.exists(ent$csv)) {
    return(readr::read_csv(ent$csv, show_col_types = FALSE))
  }
  # Some pipelines put direct path strings:
  #if (is.character(ent) && grepl("\\.rds$", ent, ignore.case = TRUE)) return(readRDS(ent))
  #if (is.character(ent) && grepl("\\.csv$", ent, ignore.case = TRUE)) return(readr::read_csv(ent, show_col_types = FALSE))
  
  stop("Don’t know how to read table entry: ", name)
}


t3_text <- function(idx, name) {
  pth <- idx$text[[name]]
  if (is.null(pth)) stop("Text not in index: ", name)
  if (is.character(pth) && file.exists(pth)) {
    return(paste(readLines(pth, warn = FALSE), collapse = "\n"))
  }
  if (is.character(pth)) return(pth) # already a string
  stop("Unknown text entry format for: ", name)
}

t3_decision <- function(idx, name) {
  pth <- idx$decisions[[name]]
  if (is.null(pth)) stop("Text not in index: ", name)
  if (is.character(pth) && file.exists(pth)) {
    return(paste(readLines(pth, warn = FALSE), collapse = "\n"))
  }
  if (is.character(pth)) return(pth) # already a string
  stop("Unknown text entry format for: ", name)
}
