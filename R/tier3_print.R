t3_load <- function(species, contaminant = "MERCURY", combine_ref = TRUE, ref_1 = "Lake Ontario", ref_2 = "St. Lawrence River", aoc_name) {
  key   <- tier3_cache_key(species, contaminant, combine_ref, ref_1, ref_2, aoc_name)
  paths <- tier3_paths(key)
  readRDS(paths$rds)
}

t3_fig <- function(x, which, png = FALSE) {
  # which ∈ {"map","t3a","t3b","t3c"}
  p <- switch(which,
              map = x$ref$map,
              t3a = x$t3a$plot,
              t3b = x$t3b$fig,
              t3c = x$t3c$fig
  )
  if (!png) return(print(p))
  # render PNG fallback if you prefer
}

t3_table_png <- function(species_key, name = "t3b_table") {
  # convenient for PDF: include_graphics on widget snapshots
  file.path(tier3_paths(species_key)$wigs, paste0(name, ".png"))
}
