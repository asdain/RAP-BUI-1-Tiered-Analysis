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
                  "webshot2")


installed_packages <- packages_list %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages_list[!installed_packages])
}

lapply(packages_list, library, character.only = TRUE)

# Input data --------------------------
# Loading the MNR fish consumption advisory data csv 
cons_data = read.csv("Data/mnr_fish_consumption_advisory_data_2024.csv") %>%
  clean_names()

length_levels <- c("15-20cm", "20-25cm", "25-30cm", "30-35cm", "35-40cm", "40-45cm",
                   "45-50cm", "50-55cm", "55-60cm", "60-65cm", "65-70cm", "70-75cm", ">75cm")




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
tool_files = list.files(path = "R", pattern = "\\.R$", full.names = TRUE)
invisible(lapply(tool_files, source))

