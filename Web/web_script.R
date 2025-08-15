setwd("~/R/RAP-BUI-1-Tiered-Analysis") # or similar


# Generate web
source("Web/scripts/export_webdata_t1.R")

if (!requireNamespace("here", quietly = TRUE)){ install.packages("here")}
library(here)

# 1) list all R helpers from root/R using absolute paths
r_files <- list.files(here("R"), pattern = "\\.R$", full.names = TRUE)
if (length(r_files) == 0) {stop("No .R files found in ", here("R"))}

# 2) create destination dir (absolute) and verify it is a directory
dest_dir <- here("Web", "tier1", "shared")
dir_create(dest_dir, recurse = T)
if (!dir.exists(dest_dir)) {stop("Destination directory was not created: ", dest_dir)}



# 3) copy all helpers into app-local shared/
ok <- file.copy(from = r_files, to = dest_dir, overwrite = TRUE)
if (!all(ok)) {
  bad <- r_files[!ok]
  stop("Failed to copy some files:\n", paste(" -", bad, collapse = "\n"))
}

message("✓ Copied ", length(r_files), " helper files into ", dest_dir)


# Do the same with the threshold data
threshold_path  <- here("Data", "consumption_threshold.csv")
data_dir  <- here("Web", "tier1", "data")
data_path <- file.path(data_dir, "consumption_threshold.csv")

if (!file.exists(threshold_path)) {
  stop("Source thresholds file not found at: ", threshold_path)
}

ok <- file.copy(from = threshold_path, to = data_path, overwrite = TRUE)

if (ok) {
  message("✓ Copied ", threshold_path, " to ", data_path)
} else {
  stop("Failed to copy ", threshold_path, " to ", data_path)
}








# AFTER shinylive::export(...)
if (!requireNamespace("fs", quietly = TRUE)) install.packages("fs")
library(fs)

# ensure the subdirs exist in the exported site
dir_create("docs/tier1_site_local/data")
dir_create("docs/tier1_site_local/shared")

# copy app-local assets from the source app folder
dir_copy("Web/tier1/data",   "docs/tier1_site_local/data",   overwrite = TRUE)
dir_copy("Web/tier1/shared", "docs/tier1_site_local/shared", overwrite = TRUE)




shinylive::export(
  appdir   = "Web/tier1",
  destdir  = "docs/tier1_site",
  basepath = "/RAP-BUI-1-Tiered-Analysis/tier1_site/"
)



shinylive::export(
  appdir   = "Web/tier1",
  destdir  = "docs/tier1_site_local",
  basepath = "/RAP-BUI-1-Tiered-Analysis/tier1_site_local/"          # <-- important for local preview
)

stopifnot(
  file_exists("docs/tier1_site_local/shinylive-sw.js"),
  file_exists("docs/tier1_site_local/index.html"),
  file_exists("docs/tier1_site_local/data/t1_wide.csv"),
  file_exists("docs/tier1_site_local/data/consumption_threshold.csv"),
  file_exists("docs/tier1_site_local/shared")
)

