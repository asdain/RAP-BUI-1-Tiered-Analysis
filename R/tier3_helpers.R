tier3_cache_key <- function(species, contaminant, combine_ref, ref_1, ref_2, aoc_name) {
  # makes a unique but readable key
  paste(
    janitor::make_clean_names(species),
    tolower(contaminant),
    if (combine_ref) "ref_combined" else paste("ref", janitor::make_clean_names(ref_1), janitor::make_clean_names(ref_2), sep = "_"),
    janitor::make_clean_names(aoc_name),
    sep = "__"
  )
}

tier3_paths <- function(key) {
  list(
    rds   = here::here("Tier3", "cache", "t3_rds", paste0(key, ".rds")),
    figs  = here::here("Tier3", "cache", "figs",    key),
    wigs  = here::here("Tier3", "cache", "widgets", key)
  )
}

ensure_dirs <- function() {
  dir.create(here::here("Tier3", "cache", "t3_rds"),   showWarnings = FALSE, recursive = TRUE)
  dir.create(here::here("Tier3", "cache", "figs"),     showWarnings = FALSE, recursive = TRUE)
  dir.create(here::here("Tier3", "cache", "widgets"),  showWarnings = FALSE, recursive = TRUE)
}

export_plot <- function(p, path_base, name, width = 7, height = 4.5, dpi = 300) {
  fn <- file.path(path_base, paste0(name, ".png"))
  dir.create(path_base, showWarnings = FALSE, recursive = TRUE)
  ggplot2::ggsave(fn, p, width = width, height = height, dpi = dpi, bg = "white")
  fn
}

widget_to_png <- function(htmlwidget, out_png, browser = Sys.getenv("CHROMOTE_CHROME")) {
  # Saves a snapshot of htmlwidget for PDF compatibility (webshot2)
  html_tmp <- sub("\\.png$", ".html", out_png)
  htmlwidgets::saveWidget(htmlwidget, html_tmp, selfcontained = TRUE)
  # use webshot2 + chromote path you already set in your Rmd
  webshot2::webshot(html_tmp, out_png, vwidth = 1200, vheight = 800)
  out_png
}
