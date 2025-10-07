# ---- Batch render Tier 3 (SLR) reports by Species ----
library(rmarkdown)
library(dplyr)
library(stringr)
library(fs)

# 1) SET THIS: the Rmd you want to render for each species
#    (use your actual path; keep the YAML output: bookdown::html_document2)
report_rmd <- "Tier3/Tier3_walleye_SLR.Rmd"

# 2) Output directory (will be created if missing)
out_dir <- path_expand("~/R/RAP-BUI-1-Tiered-Analysis/Tier3/Output/SLR T3 Reports")
dir_create(out_dir, recurse = TRUE)

# 3) Species list from your current analysis data
species_vec <- raw_data |>
  dplyr::filter(Locname.Fishbase == "St. Lawrence River 15 - Lake St. Francis", Sample.Year >= 2014) |>
  dplyr::distinct(Specname) |>
  dplyr::filter(!is.na(Specname), Specname != "") |>
  dplyr::arrange(Specname) |>
  dplyr::pull(Specname) |>
  as.character()

# 4) Helper to make nice file stubs
stubify <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "")
}

# 5) Render loop
for (sp in species_vec) {
  title_str <- paste0("SLR BUI 1 Tier 3 Report - ", sp)
  out_file  <- paste0("tier3_", stubify(sp), "_slr.html")
  
  message("Rendering: ", sp, " → ", out_file)
  
  tryCatch({
    render(
      input          = report_rmd,
      output_file    = file.path(out_dir, out_file),
      # Use the format declared in YAML (bookdown::html_document2).
      # We only override the document title at render-time:
      output_options = list(
        pandoc_args = c("--metadata", paste0("title=", title_str))
      ),
      params        = list(species = sp),
      envir         = new.env()   # keep each run isolated/clean
    )
    message("✓ Done: ", sp)
  }, error = function(e) {
    message("⚠️ Skipped ", sp, " — ", conditionMessage(e))
  })
}

message("All renders complete. Output in: ", out_dir)
