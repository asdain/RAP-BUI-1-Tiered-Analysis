# Generate web
source("web/scripts/export_webdata_t1.R")

if (!requireNamespace("here", quietly = TRUE)){ install.packages("here")}
library(here)

# 1) list all R helpers from root/R using absolute paths
r_files <- list.files(here("R"), pattern = "\\.R$", full.names = TRUE)
if (length(r_files) == 0) {stop("No .R files found in ", here("R"))}

# 2) create destination dir (absolute) and verify it is a directory
dest_dir <- here("Web", "tier1", "shared")
dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(dest_dir)) {stop("Destination directory was not created: ", dest_dir)}

# 3) copy all helpers into app-local shared/
ok <- file.copy(from = r_files, to = dest_dir, overwrite = TRUE)
if (!all(ok)) {
  bad <- r_files[!ok]
  stop("Failed to copy some files:\n", paste(" -", bad, collapse = "\n"))
}

message("✓ Copied ", length(r_files), " helper files into ", dest_dir)




shinylive::export(
  appdir   = "Web/tier1",
  destdir  = "docs/tier1_site",
  basepath = "/RAP-BUI-1-Tiered-Analysis/tier1_site/"
)





