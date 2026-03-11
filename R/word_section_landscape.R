word_section <- function(orient = c("portrait", "landscape"),
                         type = c("continuous", "nextPage"),
                         margins_twips = list(top=1440, right=1440, bottom=1440, left=1440)) {
  
  orient <- match.arg(orient)
  type   <- match.arg(type)
  
  # Letter size in twips (1 in = 1440 twips)
  if (orient == "portrait") {
    w <- 12240; h <- 15840
  } else {
    w <- 15840; h <- 12240
  }
  
  xml <- sprintf(
    '<w:p><w:pPr><w:sectPr>
       <w:pgSz w:w="%d" w:h="%d" w:orient="%s"/>
       <w:pgMar w:top="%d" w:right="%d" w:bottom="%d" w:left="%d"/>
       <w:type w:val="%s"/>
     </w:sectPr></w:pPr></w:p>',
    w, h, orient,
    margins_twips$top, margins_twips$right, margins_twips$bottom, margins_twips$left,
    type
  )
  
  knitr::asis_output(paste0("\n```{=openxml}\n", xml, "\n```\n"))
}


word_sectpr <- function(orient = c("portrait", "landscape"),
                        type = c("nextPage", "continuous"),
                        margins_twips = list(top=1440, right=1440, bottom=1440, left=1440)) {
  orient <- match.arg(orient)
  type   <- match.arg(type)
  
  # Letter in twips (1 in = 1440 twips)
  if (orient == "portrait") { w <- 12240; h <- 15840 } else { w <- 15840; h <- 12240 }
  
  xml <- sprintf(
    '<w:p><w:pPr><w:sectPr>
       <w:pgSz w:w="%d" w:h="%d" w:orient="%s"/>
       <w:pgMar w:top="%d" w:right="%d" w:bottom="%d" w:left="%d"/>
       <w:type w:val="%s"/>
     </w:sectPr></w:pPr></w:p>',
    w, h, orient,
    margins_twips$top, margins_twips$right, margins_twips$bottom, margins_twips$left,
    type
  )
  
  knitr::asis_output(paste0("\n```{=openxml}\n", xml, "\n```\n"))
}