# Generates table captions for proper formatting across output types
make_tab_cap <- function(x){
  if (knitr::is_html_output()) {
    cap = paste("<caption>",x,"</caption>")
  } else{
    cap = paste(x)
  }
}


tbl_caption <- function(label, ref_id = label) {
  if (knitr::is_html_output()) {
    knitr::asis_output(
      sprintf(
        "<span class=\"tbl-anchor\">(#tab:%s)</span>\n<p class=\"caption\"><span class=\"caption-number\">Table \\@ref(tab:%s):</span> <span class=\"caption-text\">(ref:%s)</span></p>\n\n",
        label, label, ref_id
      )
    )
  } else {
    NULL
  }
}



