source("Web/scripts/export_webdata_t2.R")

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
library(here)
if (!requireNamespace("fs", quietly = TRUE)) install.packages("fs")
library(fs)

# 2) destination directories
dest_dir <- here("Web", "tier2", "shared")
data_dir <- here("Web", "tier2", "data")
dir_create(dest_dir, recurse = TRUE)
dir_create(data_dir, recurse = TRUE)



shinylive::export(
  appdir   = "Web/tier2",
  destdir  = "docs/tier2_site_local",
  basepath = "/"   # for local static server preview
)



# 5) Export to GitHub Pages dir
shinylive::export(
  appdir   = "Web/tier2",
  destdir  = "docs/tier2_site",
  basepath = "/RAP-BUI-1-Tiered-Analysis/tier2_site/"
)
