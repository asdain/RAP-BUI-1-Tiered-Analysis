preview_full_slr <- function(file) {
  o <- setwd(file.path("Output", "Full-SLR-Report"))
  on.exit(setwd(o), add = TRUE)
  bookdown::preview_chapter(file)
}

build_full_slr <- function(fmt = c("gitbook","word"), ...) {
  fmt <- match.arg(fmt)
  o <- setwd(file.path("Output", "Full-SLR-Report"))
  on.exit(setwd(o), add = TRUE)
  bookdown::render_book("index.Rmd",
                        switch(fmt,
                               gitbook = bookdown::gitbook(),
                               word    = bookdown::word_document2()
                        ),
                        ...
  )
}


serve_full_slr <- function(
    output_dir = "_book",
    preview = TRUE,           # only recompile changed chapters
    in_session = FALSE,       # safer/reproducible; TRUE shares your current R session
    port = 4321,              # change if you have a conflict
    daemon = TRUE,            # keep serving without blocking your console
    quiet = FALSE,
    ...
) {
  # set working directory to book root (where index.Rmd and _bookdown.yml live)
  
  bookdown::serve_book(
    dir = ".",
    output_dir = output_dir,
    preview = preview,
    in_session = in_session,
    quiet = quiet,
    port = port,
    daemon = daemon,
    ...
  )
  # Notes:
  # - Visit http://127.0.0.1:4321 (change if you set a different port).
  # - If using RStudio, it opens in the Viewer by default.
}

# Stop all running book servers (from servr)
stop_served_books <- function() {
  if ("servr" %in% .packages(all.available = TRUE)) {
    try(servr::daemon_stop(), silent = TRUE)
  }
}
