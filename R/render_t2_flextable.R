build_t2_flextable <- function(prep) {
  stopifnot(!is.null(prep$display_data), length(prep$size_cols) > 0)
  
  dfw <- prep$display_data
  size_cols <- prep$size_cols
  hdr_cols  <- c("Species_display", "Population", "Site")   # <-- use Species_display
  show_cols <- c(hdr_cols, size_cols)
  
  # Keep raw keys BEFORE display tweaks
  dfw$Species_raw    <- dfw$Species
  dfw$Population_raw <- dfw$Population
  
  # ---- Species shown once per contiguous species block ----
  sp_prev <- dplyr::lag(dfw$Species_raw)
  is_first_of_species <- is.na(sp_prev) | (dfw$Species_raw != sp_prev)
  dfw$Species_display <- ifelse(is_first_of_species, dfw$Species_raw, "")
  
  # ---- Population only on AOC row; blank on Reference Median and n ----
  dfw$Population[!(dfw$site_type == "AOC")] <- ""
  
  # Build table using Species_display
  ft <- flextable::flextable(dfw[, show_cols, drop = FALSE])
  
  # Headers
  ft <- flextable::set_header_labels(
    ft,
    Species_display = "Species",
    Population      = "Population",
    Site            = "Site"
  )
  
  # Header band + divider
  ft <- flextable::bg(ft, j = hdr_cols, bg = "#f5f5f5", part = "body")
  ft <- flextable::bold(ft, j = hdr_cols, bold = TRUE, part = "body")
  ft <- flextable::border(
    ft, j = "Site",
    border.right = officer::fp_border(color = "#666666", width = 2),
    part = "all"
  )
  
  # Alignments & sizing
  ft <- flextable::align(ft, j = NULL, align = "center", part = "all")
  ft <- flextable::align(ft, j = "Species_display", align = "left", part = "all")
  ft <- flextable::fontsize(ft, size = 9, part = "all")
  ft <- flextable::autofit(ft)
  
  # Style the 'n' rows
  idx_n <- which(dfw$Site == "n")
  if (length(idx_n)) {
    ft <- flextable::fontsize(ft, i = idx_n, size = 8, part = "body")
    ft <- flextable::italic(ft,  i = idx_n, j = "Site", italic = TRUE, part = "body")
    ft <- flextable::color(ft,   i = idx_n, j = NULL, color = "#444444", part = "body")
  }
  
  # ---- Coloring (unchanged logic; uses raw keys) ----
  n_map   <- prep$n_map
  med_map <- prep$medians_map
  key_id  <- function(i, col) paste(dfw$Species_raw[i], dfw$Population_raw[i], col, sep = "||")
  
  idx_aoc <- which(dfw$site_type == "AOC")
  
  med_row_for <- function(i) {
    which(
      dfw$Species_raw    == dfw$Species_raw[i] &
        dfw$Population_raw == dfw$Population_raw[i] &
        dfw$Site           == "Reference Median"
    )[1]
  }
  
  for (col in size_cols) {
    vals <- suppressWarnings(as.numeric(dfw[[col]]))
    
    aoc_missing <- aoc_badref <- aoc_restrict <- aoc_ok <- integer(0)
    
    for (i in idx_aoc) {
      v   <- vals[i]
      id  <- key_id(i, col)
      n   <- n_map[[id]]
      ref <- med_map[[id]]
      
      if (is.na(v)) {
        aoc_missing <- c(aoc_missing, i)
      } else if (is.null(n) || is.null(ref) || is.na(n) || n < 3 || is.na(ref)) {
        aoc_badref <- c(aoc_badref, i)
      } else if (v < ref) {
        aoc_restrict <- c(aoc_restrict, i)
      } else {
        aoc_ok <- c(aoc_ok, i)
      }
    }
    
    paint <- function(rows, bg, fg) {
      if (!length(rows)) return()
      ft <<- flextable::bg(ft, i = rows, j = col, bg = bg)
      ft <<- flextable::color(ft, i = rows, j = col, color = fg)
    }
    mirror <- function(src_rows, bg, fg) {
      if (!length(src_rows)) return()
      trg <- vapply(src_rows, med_row_for, integer(1))
      trg <- trg[!is.na(trg)]
      if (length(trg)) paint(trg, bg, fg)
    }
    
    paint(aoc_missing,  "#eeeeee", "#000000"); mirror(aoc_missing,  "#eeeeee", "#000000")
    paint(aoc_badref,   "#999999", "#ffffff"); mirror(aoc_badref,   "#999999", "#ffffff")
    paint(aoc_restrict, "#d80032", "#ffffff"); mirror(aoc_restrict, "#d80032", "#ffffff")
    paint(aoc_ok,       "#4CAF50", "#ffffff"); mirror(aoc_ok,       "#4CAF50", "#ffffff")
  }
  
  flextable::fix_border_issues(ft)
}
