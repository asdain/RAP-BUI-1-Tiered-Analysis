# Common setup for all chapters
library(here)
source(here::here("Scripts","setup.R"))


thr_path = here::here("Data", "consumption_threshold_boq.csv")

# Threshold dataframe for each species
thr_df <- if (file.exists(thr_path)) {readr::read_csv(thr_path, show_col_types = FALSE)} else {tibble::tibble()}


book_root <- here::here("Output", "Full-BOQ-Report")
knitr::opts_knit$set(root.dir = book_root)

# Choose the AOC 
AOC_full <- "Bay of Quinte"

contaminant_label = "PCB"

# Load derived objects created by  analysis run for this AOC (BOQ - Lake Ontario, Trenton Nearshore, Belleville Nearshore)
# (run  existing parameterized analysis once to write these)
t1_df_lo   <- readr::read_rds(here::here("Derived", "BOQ-LO", "t1_df.rds")) #Lake Ontario
flags_lo   <- readr::read_rds(here::here("Derived", "BOQ-LO", "flags.rds"))            # t1/t2 flags
mappings <- readr::read_rds(here::here("Derived", "BOQ-LO","mappings.rds"))
ref_names <- readr::read_rds(here::here("Derived", "BOQ-LO", "t2_ref_sites.rds"))

t1_df_tr   <- readr::read_rds(here::here("Derived", "BOQ-TR", "t1_df.rds")) # Trenton nearshore
flags_tr   <- readr::read_rds(here::here("Derived", "BOQ-TR", "flags.rds"))            # t1/t2 flags
t1_df_bel   <- readr::read_rds(here::here("Derived", "BOQ-BEL", "t1_df.rds")) # Belleville
flags_bel   <- readr::read_rds(here::here("Derived", "BOQ-BEL", "flags.rds"))            # t1/t2 flags
# Measure 2
t1_df_bel_m2   <- readr::read_rds(here::here("Derived", "BOQ-BEL-M2", "t1_df.rds")) # Belleville m2
flags_bel_m2   <- readr::read_rds(here::here("Derived", "BOQ-BEL-M2", "flags.rds"))            # t1/t2 flags
t1_df_tr_m2   <- readr::read_rds(here::here("Derived", "BOQ-TR-M2", "t1_df.rds")) # Trenton m2
flags_tr_m2   <- readr::read_rds(here::here("Derived", "BOQ-TR-M2", "flags.rds"))

region_colours <- c(AOC = "red", Reference = "cyan") # or read from RDS too

n_pass_lo <- flags_lo %>%
  summarise(
    t1_pass_n  = sum(t1_pass, na.rm = TRUE),
    t1_fail_n  = sum(!t1_pass, na.rm = TRUE),
    t1_total   = n(),
    t2_pass_n  = sum(t2_pass, na.rm = TRUE),
    t2_fail_n  = sum(!t2_pass, na.rm = TRUE),
    t2_total   = n()
  )

n_pass_tr <- flags_tr %>%
  summarise(
    t1_pass_n  = sum(t1_pass, na.rm = TRUE),
    t1_fail_n  = sum(!t1_pass, na.rm = TRUE),
    t1_total   = n(),
    t2_pass_n  = sum(t2_pass, na.rm = TRUE),
    t2_fail_n  = sum(!t2_pass, na.rm = TRUE),
    t2_total   = n()
  )

n_pass_bel <- flags_bel %>%
  summarise(
    t1_pass_n  = sum(t1_pass, na.rm = TRUE),
    t1_fail_n  = sum(!t1_pass, na.rm = TRUE),
    t1_total   = n(),
    t2_pass_n  = sum(t2_pass, na.rm = TRUE),
    t2_fail_n  = sum(!t2_pass, na.rm = TRUE),
    t2_total   = n()
  )

n_pass_bel_m2 <- flags_bel_m2 %>%
  summarise(
    t1_pass_n  = sum(t1_pass, na.rm = TRUE),
    t1_fail_n  = sum(!t1_pass, na.rm = TRUE),
    t1_total   = n(),
    t2_pass_n  = sum(t2_pass, na.rm = TRUE),
    t2_fail_n  = sum(!t2_pass, na.rm = TRUE),
    t2_total   = n()
  )
n_pass_bel_m2 <- flags_tr_m2 %>%
  summarise(
    t1_pass_n  = sum(t1_pass, na.rm = TRUE),
    t1_fail_n  = sum(!t1_pass, na.rm = TRUE),
    t1_total   = n(),
    t2_pass_n  = sum(t2_pass, na.rm = TRUE),
    t2_fail_n  = sum(!t2_pass, na.rm = TRUE),
    t2_total   = n()
  )

aoc_name = "Bay of Quinte"

