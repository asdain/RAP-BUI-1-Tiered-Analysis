# Common setup for all chapters
library(here)
source(here::here("Scripts","setup.R"))


thr_path = here::here("Data", "consumption_threshold_nr.csv")

# Threshold dataframe for each species
thr_df <- if (file.exists(thr_path)) {readr::read_csv(thr_path, show_col_types = FALSE)} else {tibble::tibble()}


book_root <- here::here("Output", "Full-NPCA-Report")
knitr::opts_knit$set(root.dir = book_root)

# Choose the AOC 
AOC_full <- "Niagara River"

contaminant_label = "PCBs"

# Load derived objects created by  analysis run for this AOC
# (run  existing parameterized analysis once to write these)
t1_df_ur   <- readr::read_rds(here::here("Derived", "NR_UR", "t1_df.rds"))
flags_ur   <- readr::read_rds(here::here("Derived", "NR_UR", "flags.rds"))            # t1/t2 flags
mappings_ur <- readr::read_rds(here::here("Derived", "NR_UR","mappings.rds"))
ref_names_ur <- readr::read_rds(here::here("Derived", "NR_UR", "t2_ref_sites.rds"))


t1_df_lr   <- readr::read_rds(here::here("Derived", "NR_LR", "t1_df.rds"))
flags_lr   <- readr::read_rds(here::here("Derived", "NR_LR", "flags.rds"))            # t1/t2 flags
mappings_lr <- readr::read_rds(here::here("Derived", "NR_LR","mappings.rds"))
ref_names_lr <- readr::read_rds(here::here("Derived", "NR_LR", "t2_ref_sites.rds"))

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

aoc_name = "Niagara River"

