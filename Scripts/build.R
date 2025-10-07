# Set working directory to your book folder
old <- setwd("Output/Full-SLR-Report")
on.exit(setwd(old), add = TRUE)

# Build the book (change output format as needed)
bookdown::render_book("index.Rmd", "bookdown::gitbook")