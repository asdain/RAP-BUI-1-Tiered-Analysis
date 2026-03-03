# Setup file for BUI 1 Data Analysis.
# Loads necessary data, packages, and creates convenient data objects.

# Packages ------------
packages_list = c("dplyr", 
                  "car", 
                  "janitor", 
                  "tidyr", 
                  "ggplot2", 
                  "tidyverse", 
                  "forcats", 
                  "factoextra",
                  "ggpubr",
                  "RColorBrewer",
                  "ordinal",
                  "knitr",
                  "kableExtra",
                  "reactable",
                  "htmltools",
                  "stringr",
                  "reactablefmtr",
                  "htmlwidgets",
                  "sf",
                  "purrr",
                  "glue",
                  "scales",
                  "mgcv",
                  "emmeans",
                  "broom",
                  "webshot2",
                  "flextable",
                  "here",
                  "readr",
                  "fs",
                  "bookdown",
                  "maptiles",
                  "ggspatial",
                  "servr",
                  "officer",
                  "officedown",
                  "gratia")





installed_packages <- packages_list %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages_list[!installed_packages])
}

lapply(packages_list, library, character.only = TRUE)

# Input data --------------------------
# Loading the MNR fish consumption advisory data csv 
cons_data = read.csv(here::here("Data","Guide_to_Eating_Ontario_Fish_Advisory_Database_2025.csv")) %>%
  clean_names()

# Separating east lake into two labels
cons_data = cons_data %>%
  mutate(guide_locname_eng = ifelse(	
    guide_locdesc == "Kenora Dist.|Distr. de Kenora", "East Lake (Kenora)", guide_locname_eng),
    guide_locname_eng = ifelse(
      guide_locdesc == "Studholme Twp., Cochrane Dist.|Canton de Studholme, distr. de Cochrane", "West Lake (Cochrane)", guide_locname_eng
    ))

length_levels <- c("15-20 cm", "20-25 cm", "25-30 cm", "30-35 cm", "35-40 cm", "40-45 cm",
                   "45-50 cm", "50-55 cm", "55-60 cm", "60-65 cm", "65-70 cm", "70-75 cm", ">75 cm")



`%||%` <- function(x, y) if (is.null(x)) y else x

# Visualization constants ------------------
# Custom ggplot theme
theme_aaron = function(){
  font = "Arial"
  
  theme_classic() %+replace%
    theme(
      panel.grid=element_blank(),
      panel.border=element_blank(),
      axis.text = element_text(color="black", size = 12),
      axis.title = element_text(size = 16),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16),
      plot.margin = margin(1,1,1,1, "cm")
    )
}

# Loading all custom script
tool_files = list.files(path = here::here("R"), pattern = "\\.R$", full.names = TRUE)
invisible(lapply(tool_files, source))

adv_palette <- list(
  pass        = "#27C48F",
  pass_excl   = "#9DECD1",
  fail        = "#C4275B",
  insufficient = "#BDBABB",
  nodata      = "#EEEEEE",
  text_light  = "#E9FCF5",
  text_dark   = "#1F2933"
)

