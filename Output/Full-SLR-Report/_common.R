# Common setup for all chapters
library(here)
source(here::here("Scripts","setup.R"))


thr_path = here::here("Data", "consumption_threshold_slr.csv")

# Threshold dataframe for each species
thr_df <- if (file.exists(thr_path)) {readr::read_csv(thr_path, show_col_types = FALSE)} else {tibble::tibble()}


book_root <- here::here("Output", "Full-SLR-Report")
knitr::opts_knit$set(root.dir = book_root)

# Choose the AOC 
AOC_full <- "St. Lawrence River (Cornwall/Akwesasne)"

contaminant_label = "mercury"

# Load derived objects created by  analysis run for this AOC
# (run  existing parameterized analysis once to write these)
t1_df   <- readr::read_rds(here::here("Derived", "SLR", "t1_df.rds"))
flags   <- readr::read_rds(here::here("Derived", "SLR", "flags.rds"))            # t1/t2 flags
mappings <- readr::read_rds(here::here("Derived", "SLR","mappings.rds"))
ref_names <- readr::read_rds(here::here("Derived", "SLR", "t2_ref_sites.rds"))

region_colours <- c(AOC = "red", Reference = "cyan") # or read from RDS too

n_pass <- flags %>%
  summarise(
    t1_pass_n  = sum(t1_pass, na.rm = TRUE),
    t1_fail_n  = sum(!t1_pass, na.rm = TRUE),
    t1_total   = n(),
    t2_pass_n  = sum(t2_pass, na.rm = TRUE),
    t2_fail_n  = sum(!t2_pass, na.rm = TRUE),
    t2_total   = n()
  )

aoc_name = "St. Lawrence River (Lake St. Francis)"

