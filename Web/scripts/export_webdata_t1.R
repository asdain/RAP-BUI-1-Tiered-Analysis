# RUN THIS LOCALLY after analysis builds `cons_data`, `length_levels`, and `aoc_id`
suppressPackageStartupMessages({ library(readr) })


source(here::here("Scripts", "Setup.R"))
source(here::here("Master Report.Rmd"))

aoc_id = 45087425

# Use existing function to get the wide AOC-only table
t1_df <- make_restrict_table(
  cons_data      = cons_data,      # your filtered data
  aoc_id         = params$AOC %||% aoc_id,   # whichever you use
  length_levels  = length_levels
)

# make sure the web folder exists
dir.create("Web/tier1/data", recursive = TRUE, showWarnings = FALSE)

# save exactly what render_t1_table expects (it already will, based on your code)
# Columns include: Species, Species_display, Row_Label, and each size column
readr::write_csv(t1_df, "web/tier1/data/t1_wide.csv")
cat("\n✓ Wrote web/tier1/data/t1_wide.csv\n")
