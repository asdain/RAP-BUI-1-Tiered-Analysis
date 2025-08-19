# RUN THIS LOCALLY after analysis builds `cons_data`, `length_levels`, and `aoc_id`
suppressPackageStartupMessages({ library(readr) })


source(here::here("Scripts", "Setup.R"))


aoc_id = 45087425

#Consumption thresholds
thr_path <- here("Data","consumption_threshold.csv")
thr_df <- if (file.exists(thr_path)) {readr::read_csv(thr_path, show_col_types = FALSE)} else {tibble::tibble()}



# Use existing function to get the wide AOC-only table
t1_df <- make_restrict_table(
  df      = cons_data,      # your filtered data
  aoc_id         = aoc_id,   # whichever you use
  length_levels  = length_levels,
  restrict_threshold = thr_df 
)

# make sure the web folder exists
dir.create("Web/tier1/data", recursive = TRUE, showWarnings = FALSE)

# save exactly what render_t1_table expects (it already will, based on your code)
# Columns include: Species, Species_display, Row_Label, and each size column

saveRDS(t1_df, "Web/tier1/data/t1_wide.rds", compress = "xz")

readr::write_csv(t1_df, "Web/tier1/data/t1_wide.csv")
cat("\n✓ Wrote Web/tier1/data/t1_wide.csv\n")

