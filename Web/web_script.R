# Generate web
source("web/scripts/export_webdata_t1.R")


r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
file.copy(r_files, "web/tier1/shared/", overwrite = TRUE)
shinylive::run("web/tier1")
shinylive::export(
  appdir   = "web/tier1",
  destdir  = "docs/tier1_site",
  basepath = "/<repo-name>/tier1_site/"
)





# Optional: clean up after export
unlink(list.files("web/tier1/shared", full.names = TRUE))
