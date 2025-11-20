

# Key is ONLY species + contaminant
tier3_cache_key <- function(species, contaminant = "MERCURY") {
  paste(species, toupper(contaminant), sep = "__")
}

# Base-dir aware paths
tier3_paths <- function(base_dir, key) {
  base_dir   <- fs::path_abs(base_dir)
  species_dir <- fs::path(base_dir, key)
  
  # prefer INDEX.rds inside the species folder; allow index.rds too
  index_candidates <- c(
    fs::path(species_dir, "INDEX.rds"),
    fs::path(species_dir, "index.rds")
  )
  
  list(
    base        = base_dir,
    key         = key,
    species_dir = species_dir,
    index_file  = index_candidates,
    figs_dir    = fs::path(species_dir, "figs"),
    html_dir    = fs::path(species_dir, "html"),
    tables_dir  = fs::path(species_dir, "tables"),
    text_dir    = fs::path(species_dir, "text"),
    models_dir  = fs::path(species_dir, "models"),
    # legacy fallback (if you still have old runs)
    legacy_rds  = fs::path(base_dir, "t3_rds", paste0(key, ".rds"))
  )
}

t3_load_index <- function(base_dir, species, contaminant = "MERCURY", key = NULL) {
  base_dir   <- as.character(base_dir)                       # e.g., "Derived/SLR/Tier3" (book) or absolute (analysis)
  key        <- key %||% paste0(species, "__", contaminant)
  species_dir<- file.path(base_dir, key)
  
  # Look for index in species dir
  pick <- c(file.path(species_dir, "INDEX.rds"),
            file.path(species_dir, "index.rds"))[file.exists(c(
              file.path(species_dir, "INDEX.rds"),
              file.path(species_dir, "index.rds")))][1]
  if (is.na(pick)) stop("No index in ", species_dir)
  
  idx <- readRDS(pick)
  
  # If idx stores *relative* paths (as above), make them species-rooted for immediate use
  is_abs <- function(p) grepl("^[A-Za-z]:[/\\\\]|^/|^\\\\\\\\", p)
  rebase <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.list(x)) return(lapply(x, rebase))
    if (!is.character(x)) return(x)
    vapply(x, function(s) if (is_abs(s)) s else file.path(species_dir, s), character(1))
  }
  
  idx$figs   <- rebase(idx$figs)
  idx$html   <- rebase(idx$html)
  idx$tables <- rebase(idx$tables)
  idx$text   <- rebase(idx$text)
  idx$models <- rebase(idx$models)
  
  attr(idx, "paths") <- list(
    base        = base_dir,
    species_dir = species_dir,
    figs_dir    = file.path(species_dir, "figs"),
    html_dir    = file.path(species_dir, "html"),
    tables_dir  = file.path(species_dir, "tables"),
    text_dir    = file.path(species_dir, "text"),
    models_dir  = file.path(species_dir, "models")
  )
  idx
}


# same but uses relative paths
t3_load_index_rel <- function(base_dir, species, contaminant = "MERCURY", key = NULL) {
  base_dir <- as.character(base_dir)              # authoritative root
  key      <- key %||% tier3_cache_key(species, contaminant)
  species_dir <- file.path(base_dir, key)
  
  # Look for INDEX.rds in the species folder, then legacy
  candidates <- c(
    file.path(species_dir, "INDEX.rds"),
    file.path(species_dir, "index.rds"),
    file.path(base_dir, "t3_rds", paste0(key, ".rds"))  # legacy
  )
  pick <- candidates[file.exists(candidates)][1]
  if (is.na(pick)) {
    stop("Tier 3 index not found for key '", key, "'. Looked for:\n  ",
         paste(candidates, collapse = "\n  "))
  }
  
  idx <- readRDS(pick)
  
  # Expect that idx stores RELATIVE paths like "figs/map.png"
  # Convert any relative entry -> book/project relative using base_dir + key
  # Leave absolute strings untouched (in case any slipped in)
  is_abs_like <- function(p) grepl("^[A-Za-z]:[/\\\\]|^/|^\\\\\\\\", p)
  make_from_species <- function(p) {
    if (is.null(p)) return(NULL)
    if (is.list(p)) return(lapply(p, make_from_species))
    if (!is.character(p)) return(p)
    vapply(p, function(s) {
      if (is_abs_like(s)) s else file.path(species_dir, s)
    }, character(1))
  }
  
  idx$figs   <- make_from_species(idx$figs)
  idx$html   <- make_from_species(idx$html)
  idx$tables <- make_from_species(idx$tables)
  idx$text   <- make_from_species(idx$text)
  idx$models <- make_from_species(idx$models)
  
  # Attach handy dirs (consistent with the chosen base_dir)
  attr(idx, "key") <- key
  attr(idx, "paths") <- list(
    base        = base_dir,
    species_dir = species_dir,
    figs_dir    = file.path(species_dir, "figs"),
    html_dir    = file.path(species_dir, "html"),
    tables_dir  = file.path(species_dir, "tables"),
    text_dir    = file.path(species_dir, "text"),
    models_dir  = file.path(species_dir, "models")
  )
  
  idx
}


# ---- ensure dirs exist (uses the object returned by tier3_paths) ------------
ensure_dirs <- function(paths) {
  stopifnot(is.list(paths), !is.null(paths$base))
  fs::dir_create(fs::path_dir(paths$rds_file), recurse = TRUE)
  fs::dir_create(paths$figs_dir, recurse = TRUE)
  fs::dir_create(paths$widgets_dir, recurse = TRUE)
  invisible(paths)
}


# ---- exporters that use those paths ----------------------------------------
# save a ggplot to <figs_dir>/<name>.png
export_plot <- function(p, paths, name, width = 7, height = 4.5, dpi = 300) {
  stopifnot(is.list(paths), !is.null(paths$figs_dir))
  fs::dir_create(paths$figs_dir, recurse = TRUE)
  fn <- fs::path(paths$figs_dir, paste0(name, ".png"))
  ggplot2::ggsave(filename = fn, plot = p, width = width, height = height, dpi = dpi, bg = "white")
  fn
}

# write a list/data.frame/etc to the single RDS file slot
save_run_rds <- function(object, paths) {
  fs::dir_create(fs::path_dir(paths$rds_file), recurse = TRUE)
  saveRDS(object, paths$rds_file)
  paths$rds_file
}

# save a widget as HTML under widgets_dir and (optionally) snapshot to PNG
widget_save <- function(widget, paths, name,
                        snapshot_png = TRUE,
                        vwidth = 1200, vheight = 800) {
  stopifnot(is.list(paths), !is.null(paths$widgets_dir))
  fs::dir_create(paths$widgets_dir, recurse = TRUE)
  html_path <- fs::path(paths$widgets_dir, paste0(name, ".html"))
  htmlwidgets::saveWidget(widget, html_path, selfcontained = TRUE)
  
  if (isTRUE(snapshot_png)) {
    png_path <- fs::path(paths$widgets_dir, paste0(name, ".png"))
    # webshot2 will use CHROMOTE_CHROME if you set it earlier
    webshot2::webshot(html_path, file = png_path, vwidth = vwidth, vheight = vheight)
    return(list(html = html_path, png = png_path))
  }
  list(html = html_path)
}


widget_to_png <- function(htmlwidget, out_png, browser = Sys.getenv("CHROMOTE_CHROME")) {
  # Saves a snapshot of htmlwidget for PDF compatibility (webshot2)
  html_tmp <- sub("\\.png$", ".html", out_png)
  htmlwidgets::saveWidget(htmlwidget, html_tmp, selfcontained = TRUE)
  # use webshot2 + chromote path you already set in your Rmd
  webshot2::webshot(html_tmp, out_png, vwidth = 1200, vheight = 800)
  out_png
}

# ---- Tier 3 Virtual Advisory table renderer ----
# --- 1) prep: coerce bin columns numeric & build a Reference lookup per Population ---
.t3v_prep <- function(x) {
  # x = data.frame or path to RDS; must contain Region, Population, and size-bin columns
  df <- if (is.character(x) && file.exists(x)) readRDS(x) else as.data.frame(x)
  
  stopifnot(all(c("Region","Population") %in% names(df)))
  # order/pop factor not required, but tidy up common inconsistencies
  df$Region     <- as.character(df$Region)
  df$Population <- as.character(df$Population)
  
  size_cols <- setdiff(names(df), c("Region","Population"))
  # coerce size-bin columns to numeric (meals/month)
  for (c in size_cols) df[[c]] <- suppressWarnings(as.numeric(df[[c]]))
  
  # ---- build population-specific Reference medians (one value per Population × bin) ----
  ref_only <- df[df$Region == "Reference", , drop = FALSE]
  if (nrow(ref_only) == 0) {
    stop("No 'Reference' rows found for building comparison.")
  }
  ref_summary <- ref_only |>
    dplyr::group_by(Population) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(size_cols), ~ median(.x, na.rm = TRUE)), .groups = "drop")
  
  # named lookup: "Population||bin" -> numeric median
  ref_map <- list()
  for (i in seq_len(nrow(ref_summary))) {
    pop <- ref_summary$Population[i]
    for (bin in size_cols) {
      key <- paste0(pop, "||", bin)
      ref_map[[key]] <- ref_summary[[bin]][i]
    }
  }
  
  list(
    df        = df,
    size_cols = size_cols,
    ref_map   = ref_map
  )
}


# --- 2) HTML: reactable with Tier-2 look & simple coloring (AOC vs Reference) ---
.t3v_build_reactable <- function(prep, restrict_threshold = 8, table_height = "700px") {
  df        <- prep$df
  size_cols <- prep$size_cols
  ref_map   <- prep$ref_map
  
  # columns for Population + Region
  cols <- list(
    Population = reactable::colDef(name = "Population", sticky = "left"),
    Region     = reactable::colDef(name = "Site",       sticky = "left")
  )
  
  # per-bin cell coloring
  for (bin in size_cols) {
    cols[[bin]] <- reactable::colDef(
      name = bin, align = "center", sortable = FALSE, minWidth = 70,
      style = reactable::JS(sprintf(
        "function(rowInfo, colInfo, state) {
  const row = rowInfo.row;
  const val = row[colInfo.id];
  const pal = state.meta.palette;
  const thr = %f;  // numeric threshold baked in from R

  // Reference rows: header-like style
  if (row.Region === 'Reference') {
    return {
      fontWeight: 'bold',
      background: pal.nodata || '#f5f5f5',
      fontFamily: 'system-ui, sans-serif',
      fontSize: '13px'
    };
  }

  // Non-AOC, non-Reference rows: plain
  if (row.Region !== 'AOC') {
    return {
      fontFamily: 'system-ui, sans-serif',
      fontSize: '13px'
    };
  }

  // AOC row logic
  const key = row.Population + '||' + '%s';
  const ref = state.meta.ref_map[key];

  // no predicted advisory
  if (val === null) {
    return {
      background: pal.nodata || '#eeeeee',
      color: pal.text_dark || '#000000',
      fontWeight: 'bold',
      fontSize: '15px',
      fontFamily: 'system-ui, sans-serif'
    };
  }

  // no valid reference value
  if (ref === undefined || ref === null || Number.isNaN(ref)) {
    return {
      background: pal.insufficient || '#999999',
      color: pal.text_light || '#ffffff',
      fontWeight: 'bold',
      fontSize: '15px',
      fontFamily: 'system-ui, sans-serif'
    };
  }

  // FAIL: more restrictive than both the unrestrictive threshold and reference
  if (val < thr && val < ref) {
    return {
      background: pal.fail || '#E53935',
      color: pal.text_light || '#ffffff',
      fontWeight: 'bold',
      fontSize: '15px',
      fontFamily: 'system-ui, sans-serif'
    };
  }

  // PASS: meets or exceeds threshold or reference
  return {
    background: pal.pass || '#43C66F',
    color: pal.text_light || '#ffffff',
    fontWeight: 'bold',
    fontSize: '15px',
    fontFamily: 'system-ui, sans-serif'
  };
}", restrict_threshold, bin))
    )
  }
  
  # bold AOC rows
  rowStyle <- function(index) {
    r <- df[index, ]
    style <- list(fontFamily = "system-ui, sans-serif", fontSize = "13px")
    if (isTRUE(r$Region == "AOC")) style$fontWeight <- "bold"
    style
  }
  
  reactable::reactable(
    df,
    columns   = cols,
    pagination = FALSE,
    sortable   = FALSE,
    highlight  = TRUE,
    bordered   = FALSE,
    striped    = FALSE,
    height     = table_height,
    rowStyle   = rowStyle,
    style = list(
      fontFamily = "system-ui, sans-serif",
      fontSize   = "13px",
      borderCollapse = "collapse",
      margin     = "0 auto",
      width      = "auto"
    ),
    meta = list(
      ref_map  = ref_map,
      palette  = adv_palette   # <- this is the same R list you used elsewhere
    )
  )
}

# --- 3) Word/PDF: flextable with Tier-2 vibe & same coloring on AOC cells ---
.t3v_build_flextable <- function(prep, restrict_threshold = 8, caption = NULL) {
  df        <- prep$df
  size_cols <- prep$size_cols
  ref_map   <- prep$ref_map
  
  hdr_cols  <- c("Population","Region")
  show_cols <- c(hdr_cols, size_cols)
  
  ft <- flextable::flextable(df[, show_cols, drop = FALSE])
  
  # “Tier-2” feel
  ft <- flextable::bg(ft, j = hdr_cols, bg = "#f5f5f5", part = "body")
  ft <- flextable::bold(ft, j = hdr_cols, bold = TRUE, part = "body")
  ft <- flextable::border(
    ft, j = "Region",
    border.right = officer::fp_border(color = "#666666", width = 2),
    part = "all"
  )
  ft <- flextable::align(ft, j = NULL, align = "center", part = "all")
  ft <- flextable::fontsize(ft, size = 9, part = "all")
  ft <- flextable::autofit(ft)
  
  # bold AOC rows
  idx_aoc <- which(df$Region == "AOC")
  if (length(idx_aoc)) {
    ft <- flextable::bold(ft, i = idx_aoc, bold = TRUE, part = "body")
  }
  
  # color AOC cells with same logic as reactable
  for (bin in size_cols) {
    vals <- df[[bin]]
    aoc_missing  <- integer(0)
    aoc_badref   <- integer(0)
    aoc_restrict <- integer(0)
    aoc_ok       <- integer(0)
    
    for (i in idx_aoc) {
      v   <- vals[i]
      key <- paste0(df$Population[i], "||", bin)
      ref <- ref_map[[key]]
      
      if (is.na(v)) {
        aoc_missing <- c(aoc_missing, i)
      } else if (is.null(ref) || is.na(ref)) {
        aoc_badref  <- c(aoc_badref, i)
      } else if (v < restrict_threshold && v < ref) {
        aoc_restrict <- c(aoc_restrict, i)
      } else {
        aoc_ok <- c(aoc_ok, i)
      }
    }
    
    paint <- function(rows, bg, fg) {
      if (!length(rows)) return()
      ft <<- flextable::bg(ft,    i = rows, j = bin, bg = bg, part = "body")
      ft <<- flextable::color(ft, i = rows, j = bin, color = fg, part = "body")
      ft <<- flextable::bold(ft,  i = rows, j = bin, bold = TRUE, part = "body")
    }
    
    # use shared palette
    paint(aoc_missing,  adv_palette$nodata,       adv_palette$text_dark)
    paint(aoc_badref,   adv_palette$insufficient, adv_palette$text_light)
    paint(aoc_restrict, adv_palette$fail,         adv_palette$text_light)
    paint(aoc_ok,       adv_palette$pass,         adv_palette$text_light)
  }
  
  if (!is.null(caption)) {
    ft <- flextable::add_header_lines(ft, values = caption)
  }
  flextable::fix_border_issues(ft)
}


# --- 4) one entry point: auto-chooses output type ---
render_t3_virtual_any <- function(x,
                                  restrict_threshold = 8,
                                  caption = NULL,
                                  table_height = "700px",
                                  force = c("auto","reactable","flextable")) {
  force <- match.arg(force)
  if (force == "auto") {
    force <- if (knitr::is_html_output()) "reactable" else "flextable"
  }
  prep <- .t3v_prep(x)
  if (force == "reactable") {
    w <- .t3v_build_reactable(prep, restrict_threshold, table_height = table_height)
    if (is.null(caption)) w else htmltools::tagList(htmltools::tags$h4(caption), w)
  } else {
    .t3v_build_flextable(prep, restrict_threshold, caption = caption)
  }
}
