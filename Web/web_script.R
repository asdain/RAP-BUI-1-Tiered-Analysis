# Generate web
source("web/scripts/export_webdata_t1.R")

shinylive::run("web/tier1")
shinylive::export(appdir = "web/tier1", destdir = "docs/tier1_site")

