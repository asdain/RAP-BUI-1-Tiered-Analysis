build_t2_flextable <- function(prep) {
  stopifnot(!is.null(prep$display_data), length(prep$size_cols) > 0)
  
  dfw       <- prep$display_data
  dfw$Site_display <- dplyr::recode(
    dfw$Site,
    "Reference Median" = "Ref. Median",
    .default = dfw$Site
  )
  size_cols <- prep$size_cols
  hdr_cols  <- c("Species_display", "Population", "Site_display")
  show_cols <- c(hdr_cols, size_cols)
  
  
  
  # thresholds from prep (named by Species)
  thresholds <- if (!is.null(prep$threshold_map)) prep$threshold_map else list()
  
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
    Site_display            = "Site"
  )
  
  # Header band + divider
  ft <- flextable::bg(ft, j = hdr_cols, bg = "#f5f5f5", part = "body")
  ft <- flextable::bold(ft, j = hdr_cols, bold = TRUE, part = "body")
  ft <- flextable::border(
    ft, j = "Site_display",
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
    ft <- flextable::italic(ft,  i = idx_n, j = "Site_display", italic = TRUE, part = "body")
    ft <- flextable::color(ft,   i = idx_n, j = NULL, color = "#444444", part = "body")
  }
  
  # ---- Coloring logic (Tier 2 + Tier 1 threshold) ----
  n_map   <- prep$n_map
  med_map <- prep$medians_map
  key_id  <- function(i, col) paste(dfw$Species_raw[i], dfw$Population_raw[i], col, sep = "||")
  
  idx_aoc <- which(dfw$site_type == "AOC")
  
  # For mirroring AOC colours onto "Reference Median" rows
  med_row_for <- function(i) {
    which(
      dfw$Species_raw    == dfw$Species_raw[i] &
        dfw$Population_raw == dfw$Population_raw[i] &
        dfw$Site           == "Reference Median"
    )[1]
  }
  
  for (col in size_cols) {
    vals <- suppressWarnings(as.numeric(dfw[[col]]))
    
    aoc_missing  <- integer(0)
    aoc_badref   <- integer(0)
    aoc_restrict <- integer(0)
    aoc_ok       <- integer(0)
    aoc_t1_pass  <- integer(0)  # Tier 1 pass based on threshold
    
    for (i in idx_aoc) {
      v   <- vals[i]
      id  <- key_id(i, col)
      n   <- n_map[[id]]
      ref <- med_map[[id]]
      
      # per-species threshold (if available)
      sp  <- dfw$Species_raw[i]
      thr <- thresholds[[sp]]
      
      if (is.na(v)) {
        aoc_missing <- c(aoc_missing, i)
      } else if (!is.null(thr) && !is.na(thr) && v >= thr) {
        # Tier 1 pass (unrestrictive size class)
        aoc_t1_pass <- c(aoc_t1_pass, i)
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
      ft <<- flextable::bg(ft, i = rows, j = col, bg = bg, part = "body")
      ft <<- flextable::color(ft, i = rows, j = col, color = fg, part = "body")
    }
    
    mirror <- function(src_rows, bg, fg) {
      if (!length(src_rows)) return()
      trg <- vapply(src_rows, med_row_for, integer(1))
      trg <- trg[!is.na(trg)]
      if (length(trg)) paint(trg, bg, fg)
    }
    
    # Core categories using the palette
    paint(aoc_missing,  adv_palette$nodata,       adv_palette$text_dark);  mirror(aoc_missing,  adv_palette$nodata,       adv_palette$text_dark)
    paint(aoc_badref,   adv_palette$insufficient, adv_palette$text_light); mirror(aoc_badref,   adv_palette$insufficient, adv_palette$text_light)
    paint(aoc_restrict, adv_palette$fail,         adv_palette$text_light); mirror(aoc_restrict, adv_palette$fail,         adv_palette$text_light)
    paint(aoc_ok,       adv_palette$pass,         adv_palette$text_light); mirror(aoc_ok,       adv_palette$pass,         adv_palette$text_light)
    
    # Tier 1 pass (v >= threshold): ghosted/excluded pass colour
    if (length(aoc_t1_pass)) {
      # AOC cells: lighter pass + white text + italics
      paint(aoc_t1_pass, adv_palette$pass_excl, adv_palette$text_light)
      ft <<- flextable::italic(ft, i = aoc_t1_pass, j = col, italic = TRUE, part = "body")
      
      # Mirror onto Reference Median row
      trg <- vapply(aoc_t1_pass, med_row_for, integer(1))
      trg <- trg[!is.na(trg)]
      if (length(trg)) {
        paint(trg, adv_palette$pass_excl, adv_palette$text_light)
        ft <<- flextable::italic(ft, i = trg, j = col, italic = TRUE, part = "body")
      }
    }
  }
  
  flextable::fix_border_issues(ft)
}
