build_t1_flextable <- function(df,
                                      length_levels = NULL,
                                      show_threshold_once = TRUE) {
  stopifnot("Unrestrictive_Threshold" %in% names(df))
  size_cols <- t1_size_cols(df, length_levels)
  
  # Encode causes as numeric indices (for the footer legend + display in Adv cause rows)
  enc <- encode_adv_causes_as_indices(df, size_cols)
  dfw <- enc$df
  legend_tbl <- enc$legend
  
  # Display tweaks: collapse Species/Threshold, hide threshold on Adv cause rows
  if (isTRUE(show_threshold_once) && nrow(dfw) > 0) {
    for (i in seq_len(nrow(dfw))) {
      if (i > 1 && dfw$Species[i] == dfw$Species[i - 1]) {
        dfw$Species_display[i] <- ""
        if (dfw$Row_Label[i] != "General") dfw$Unrestrictive_Threshold[i] <- ""
      }
      if (dfw$Row_Label[i] == "Adv cause") {
        dfw$Unrestrictive_Threshold[i] <- ""
      }
    }
  }
  
  hdr_cols <- c("Species_display","Unrestrictive_Threshold","Row_Label")
  show_cols <- c(hdr_cols, size_cols)
  
  ft <- flextable::flextable(dfw[, show_cols, drop = FALSE])
  
  # Headers
  ft <- flextable::set_header_labels(
    ft,
    Species_display = "Species",
    Unrestrictive_Threshold = "Unrestrictive Threshold",
    Row_Label = "Population"
  )
  
  # Alignments, font, layout
  ft <- flextable::align(ft, j = NULL, align = "center", part = "all")
  ft <- flextable::align(ft, j = "Species_display", align = "left", part = "all")
  ft <- flextable::fontsize(ft, size = 9, part = "all")
  ft <- flextable::autofit(ft)
  
  # Header-band style on first three body columns + vertical divider
  ft <- flextable::bg(ft, j = hdr_cols, bg = "#f5f5f5", part = "body")
  ft <- flextable::bold(ft, j = hdr_cols, bold = TRUE, part = "body")
  ft <- flextable::border(
    ft, j = "Row_Label",
    border.right = officer::fp_border(color = "#666666", width = 2),
    part = "all"
  )
  
  # Color fills for numeric advisory cells (General/Sensitive)
  is_adv_row <- dfw$Row_Label %in% c("General", "Sensitive")
  thr_vec <- suppressWarnings(as.numeric(df$Unrestrictive_Threshold))
  
  for (col in size_cols) {
    vals <- suppressWarnings(as.numeric(dfw[[col]]))
    
    idx_restrict <- which(is_adv_row & !is.na(vals) & !is.na(thr_vec) & vals < thr_vec)
    idx_ok       <- which(is_adv_row & !is.na(vals) & !is.na(thr_vec) & vals >=  thr_vec)
    idx_missing  <- which(is_adv_row &  is.na(vals))
    
    if (length(idx_restrict)) {
      ft <- flextable::bg(ft, i = idx_restrict, j = col, bg = adv_palette$fail)
      ft <- flextable::color(ft, i = idx_restrict, j = col, color = "white")
    }
    if (length(idx_ok)) {
      ft <- flextable::bg(ft, i = idx_ok, j = col, bg = adv_palette$pass)
      ft <- flextable::color(ft, i = idx_ok, j = col, color = "white")
    }
    if (length(idx_missing)) {
      ft <- flextable::bg(ft, i = idx_missing, j = col, bg = adv_palette$nodata)
      ft <- flextable::color(ft, i = idx_missing, j = col, color = "#000000")
    }
  }
  
  # --- Style the "Adv cause" rows as secondary (no superscripts) ---
  adv_cause_idx <- which(dfw$Row_Label == "Adv cause")
  if (length(adv_cause_idx)) {
    # smaller font on entire row
    ft <- flextable::fontsize(ft, i = adv_cause_idx, size = 8, part = "body")
    # italicize the "Population" cell text for those rows
    ft <- flextable::italic(ft, i = adv_cause_idx, j = "Row_Label", italic = TRUE, part = "body")
    # optional: tighten row padding a bit for compactness
    ft <- flextable::padding(ft, i = adv_cause_idx, padding.top = 0, padding.bottom = 1, part = "body")
  }
  
  # Footer legend for indices, if any
  if (nrow(legend_tbl) > 0) {
    legend_str <- paste0(
      "Advisory causes: ",
      paste(legend_tbl$idx, legend_tbl$contaminant, sep = " = ", collapse = "; ")
    )
    ft <- flextable::add_footer_lines(ft, values = legend_str)
    ft <- flextable::fontsize(ft, part = "footer", size = 8)
    ft <- flextable::align(ft, part = "footer", align = "left")
  }
  
  ft
}
