# Setup file for BUI 1 Data Analysis.
# Loads necessary data, packages, and creates convenient data objects.

# Packages ------------
library(dplyr)
library(car)
library(janitor)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(forcats)
library(factoextra)
library(ggpubr)
library(RColorBrewer)
library(ordinal)
library(knitr)
library(kableExtra)
library(reactable)
library(htmltools)
library(stringr)
library(reactablefmtr)
library(htmlwidgets)

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

# Color palette
coul = brewer.pal(7,"BuPu")[2:7]
coul = colorRampPalette(coul)(13)


# Reactable elements
# Load Font Awesome
fa_css <- tags$head(
  tags$link(
    rel = "stylesheet",
    href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
  )
)

# Contaminant colors
contaminant_colors <- c(
  PFAS = "magenta",
  Mercury = "goldenrod",
  PCB = "skyblue"
)


# Font Awesome icons
contaminant_icons <- c(
  PFAS = "fa-square",
  Mercury = "fa-circle",
  PCB = "fa-diamond"
)

