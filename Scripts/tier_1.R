

###############################
source("Scripts/setup.R")
###############################

# TIER 1: ARE FISH ADVISORIES IN THE AOC CONSIDERED RESTRICTIVE?

# FISH -> FISH OF INTEREST
# RESTRICTIVE -> BELOW A CERTAIN THRESHOLD OF MEALS PER MONTH

# LOADING DATASET ----------

## Dataset "cons_data", which contains the MECP fish consumption advisory data, is pre-loaded by the setup script.
### In this analysis, it is filtered to exclude advisories driven by PFAS.
#cons_data = cons_data %>%
  #filter(adv_cause_desc != "PFAS")
### ACTUALLY DON'T DO THIS, TIER 1 DOES NOT CARE ABOUT ADVISORIES. WILL BE HANDLED IN TIER 3-4

## Dataset "cons_data_aoc" restricts the dataset to the SLR AOC
cons_data_aoc = cons_data %>%
  filter(guide_locname_eng == "St. Lawrence River (Lake St. Francis)")

## Filtered dataset to relevant variables
dat_aoc = cons_data_aoc %>%
  select(c(spec = specname,
           pop_id = population_type_id,
           pop_name = population_type_desc,
           length_id = length_category_id,
           length_name = length_category_label,
           adv_level = adv_level,
           adv_cause = adv_cause_multiple_name
           ))

# DEFINING TERMS -----------

## RESTRICTION THRESHOLD -> change value to adjust restriction threshold. 
### Currently set to 8 meals/month as recommended by other AOC reports
restrict_threshold = 8


# ANALYSIS ----------

## ALL FISH AND SIZE CLASSES
### For first go-around, since we do not have survey results, I will create a table and visualization for all species and size classes that are restrictive
restrict_aoc = dat_aoc %>%
  mutate(restrictive = adv_level <= restrict_threshold)

### Making a wide table to show where there are restrictions for each species and size category
### Wide format is less useful for analysis, but translates better for visualization in table form
restrict_aoc_wide = spread(restrict_aoc, length_name, adv_level, adv_cause_multiple_name) %>%
  select(-c(length_id, restrictive, pop_id)) %>%
  group_by(spec, pop_name) %>%
  summarize(across(everything(), ~ na.omit(.)[1]), .groups = "drop")

### Saving this as a csv
write.csv(restrict_aoc_wide, "Output/tier_1_restrictive_table.csv")


write.csv(restrict_aoc, "Output/tier_1_table.csv")







### ALTERNATIVE, NEW WORKFLOW -------------
### USING REACTABLE FOR INTERACTIVE TABLE
cons_data_aoc = cons_data %>%
  filter(guide_locname_eng == "St. Lawrence River (Lake St. Francis)")

dat_aoc = cons_data_aoc %>%
  select(c(spec = specname,
           pop_id = population_type_id,
           pop_name = population_type_desc,
           length_id = length_category_id,
           length_name = length_category_label,
           adv_level = adv_level,
           adv_cause = adv_cause_multiple_name
  ))

restrict_aoc = dat_aoc %>%
  mutate(restrictive = adv_level <= restrict_threshold)


# Define length category levels for refactoring
length_levels = c("15-20cm",
                  "20-25cm",
                  "25-30cm",
                  "30-35cm",
                  "35-40cm",
                  "40-45cm",
                  "45-50cm",
                  "50-55cm",
                  "55-60cm",
                  "60-65cm",
                  "65-70cm",
                  "70-75cm",
                  ">75cm"
)

restrict_aoc$length_name = factor(restrict_aoc$length_name, levels = length_levels, ordered = T)


# Pivot table 

restrict_aoc_long = restrict_aoc %>%
  mutate(adv_level = as.character(adv_level)) %>%
  arrange(length_name, factor(length_name, levels = c())) %>%
  pivot_longer(
  cols = c(adv_level, adv_cause),
  names_to = "Variable", values_to = "Value") %>%
  mutate(VarPop = paste(pop_name, Variable, sep = "_"))

restrict_aoc_wide = restrict_aoc_long %>%
  select(spec, length_name, VarPop, Value) %>%
  pivot_wider(names_from = length_name, values_from = Value) %>%
  arrange()

restrict_aoc_final <- restrict_aoc_wide %>%
  arrange(spec, factor(VarPop, levels = c(
    "General_adv_level", "General_adv_cause",
    "Sensitive_adv_level", "Sensitive_adv_cause"
  ))) %>%
  mutate(Row_Label = case_when(
    VarPop == "General_adv_level" ~ "General",
    VarPop == "General_adv_cause" ~ "Adv cause",
    VarPop == "Sensitive_adv_level" ~ "Sensitive",
    VarPop == "Sensitive_adv_cause" ~ "Adv cause"
  )) %>%
  select(spec, Row_Label, everything(), -VarPop) %>%
  rename(Species = spec)



#### Making interactive tablee with Reactable
# Define a color function
get_color <- function(value) {
  level <- suppressWarnings(as.numeric(value))
  if (is.na(level)) return(NULL)
  if (level < 8) "tomato"
  else "seagreen"
}

col_defs <- setNames(
  lapply(length_levels, function(col) {
    colDef(
      style = function(value) {
        color <- get_color(value)
        if (!is.null(color)) list(background = color, color = "white")
        else NULL
      }
    )
  }),
  length_levels
)

reactable(
  restrict_aoc_final,
  columns = col_defs,
  striped = TRUE,
  bordered = TRUE,
  highlight = TRUE,
  defaultPageSize = nrow(restrict_aoc_final),
  defaultColDef = colDef(align = "center")
)


# Build the table with visual contaminant indicators instead of text
# ---------------------------
# Define your contaminant colors
# ---------------------------
contaminant_colors <- c(
  "PFAS" = "#e6194B",    # Red
  "Mercury" = "#f9c74f", # Yellow
  "PCB" = "#4363d8"      # Blue
)
# As dots
render_contaminant_dots <- function(cause_text) {
  if (is.na(cause_text) || cause_text == "") return("")
  causes <- trimws(unlist(strsplit(cause_text, ",")))
  spans <- lapply(causes, function(cause) {
    color <- contaminant_colors[[cause]]
    if (is.null(color)) return("")  # Skip if not one of the known contaminants
    div(style = paste0(
      "display:inline-block; width:10px; height:10px; border-radius:50%; margin-right:4px; background-color:", color, ";"
    ))
  })
  div(style = "display:flex; justify-content:center;", spans)
}

# As bars
render_contaminant_bars <- function(cause_text) {
  if (is.na(cause_text) || cause_text == "") return("")
  causes <- trimws(unlist(strsplit(cause_text, ",")))
  bars <- lapply(causes, function(cause) {
    color <- contaminant_colors[[cause]]
    if (is.null(color)) return("")
    div(style = paste0(
      "width: 100%; height: 6px; background-color:", color, "; margin: 1px 0; border-radius: 4px;"
    ))
  })
  div(style = "display:flex; flex-direction:column; justify-content:center; height:100%;", bars)
}

# Identify size columns
size_cols <- names(restrict_aoc_final)[!(names(restrict_aoc_final) %in% c("Species", "Row_Label"))]

# Define column styles based on Row_Label
columns_list <- lapply(size_cols, function(col_name) {
  colDef(
    name = col_name,
    cell = function(value, index, name) {
      row <- restrict_aoc_final[index, ]
      row_type <- row$Row_Label
      
      if (row_type == "Adv cause") {
        render_contaminant_bars(value)
      } else if (!is.na(as.numeric(value))) {
        value_num <- as.numeric(value)
        color <- if (value_num < 8) "#ff4d4d" else if (value_num >= 24) "#66cc66" else "#ffd966"
        div(style = paste0(
          "background-color:", color, "; color: black; font-weight: bold; border-radius: 16px;",
          "padding: 3px 6px; display: inline-block;"
        ), value)
      } else {
        value
      }
    },
    html = TRUE,
    style = function(value, index) {
      row <- restrict_aoc_final[index, ]
      row_type <- row$Row_Label
      
      # Reduce height of contaminant rows
      if (row_type == "Adv cause") {
        return(list(fontSize = "10px", paddingTop = "2px", paddingBottom = "2px"))
      }
      
      # Add spacing before Sensitive group
      if (row_type == "Sensitive" && index > 1 && restrict_aoc_final$Row_Label[index - 1] == "Adv cause") {
        return(list(paddingTop = "12px"))
      }
      
      return(NULL)
    }
  )
})
names(columns_list) <- size_cols

# Build the reactable
reactable(
  restrict_aoc_final,
  columns = c(
    list(
      Species = colDef(minWidth = 120, style = list(fontWeight = "bold")),
      Row_Label = colDef(name = "Population", minWidth = 100, style = list(fontStyle = "italic"))
    ),
    columns_list
  ),
  defaultColDef = colDef(align = "center", minWidth = 90),
  bordered = FALSE,  # 🔥 Removes table borders
  striped = FALSE,
  highlight = TRUE,
  style = list(
    fontFamily = "sans-serif",
    fontSize = "13px",
    border = "none",
    margin = "0 auto",
    width = "auto"
  )
)



library(reactable)
library(htmltools)
library(dplyr)
library(stringr)

# ---------------------------
# Define your contaminant colors
# ---------------------------
contaminant_colors <- c(
  "PFAS" = "#e6194B",    # Red
  "Mercury" = "#f9c74f", # Yellow
  "PCB" = "#4363d8"      # Blue
)

# ---------------------------
# Function to render stacked color bars for contaminants
# ---------------------------
render_contaminant_bars <- function(cause_text) {
  if (is.na(cause_text) || cause_text == "") return("")
  
  # Split comma-separated contaminants
  causes <- trimws(unlist(strsplit(cause_text, ",")))
  
  # Create a vertical stack of bars
  bars <- lapply(causes, function(cause) {
    color <- contaminant_colors[[cause]]
    if (is.null(color)) return("")  # Skip unknowns
    div(style = paste0(
      "width: 100%; height: 6px; background-color:", color,
      "; margin: 1px 0; border-radius: 3px;"
    ))
  })
  
  # Wrap bars in a vertically stacked flex container
  div(style = "display: flex; flex-direction: column; justify-content: center;border: none; padding: 0; margin: 0; gap: 0;", bars)
}

# ---------------------------
# Prepare column definitions for advisory sizes
# ---------------------------
size_cols <- names(restrict_aoc_final)[!(names(restrict_aoc_final) %in% c("Species", "Row_Label"))]

# Generate column definitions for each size bin
columns_list <- lapply(size_cols, function(col_name) {
  colDef(
    name = col_name,
    html = TRUE,
    cell = function(value, index) {
      row <- restrict_aoc_final[index, ]
      row_type <- row$Row_Label
      
      if (row_type == "Adv cause") {
        # Show stacked bars for contaminant cause
        render_contaminant_bars(value)
      } else if (!is.na(as.numeric(value))) {
        # Show advisory level as colored bubble
        value_num <- as.numeric(value)
        color <- if (value_num < 8) "#ff4d4d" else if (value_num >= 24) "#66cc66" else "#ffd966"
        div(style = paste0(
          "background-color:", color,
          "; color: black; font-weight: bold; border-radius: 8px;",
          "padding: 3px 6px; display: inline-block; font-size: 12px;"
        ), value)
      } else {
        value
      }
    },
    style = function(value, index) {
      row_type <- restrict_aoc_final$Row_Label[index]
      
      # Make contaminant rows shorter
      if (row_type == "Adv cause") {
        return(list(fontSize = "10px", paddingTop = "2px", paddingBottom = "2px"))
      }
      
      # Add spacing between General/Adv cause and Sensitive/Adv cause groups
      if (row_type == "Sensitive" && index > 1 &&
          restrict_aoc_final$Row_Label[index - 1] == "Adv cause") {
        return(list(paddingTop = "12px"))
      }
      
      return(list())
    }
  )
})
names(columns_list) <- size_cols

# ---------------------------
# Add formatting for the Species and Row_Label columns
# ---------------------------


columns_list$Row_Label <- colDef(
  name = "Population",
  style = function(value) {
    if (value == "Adv cause") {
      list(fontSize = "10px", fontStyle = "italic")
    } else {
      list(fontSize = "13px", fontWeight = "bold")
    }
  }
)

# ---------------------------
# Render the final table (RStudio Viewer-compatible)
# ---------------------------
reactable(
  restrict_aoc_final,
  columns = columns_list,
  groupBy = "Species",  # Merge Species cells
  defaultColDef = colDef(
    align = "center",
    minWidth = 80
  ),
  bordered = FALSE,  # Remove full table borders
  striped = FALSE,
  highlight = TRUE,
  style = list(
    fontFamily = "sans-serif",
    fontSize = "13px",
    border = "none",
    margin = "0 auto"
  )
)




# Round 3


# ---------------------------
# Define contaminant colors
# ---------------------------
contaminant_colors <- c(
  "PFAS" = "#e6194B",
  "Mercury" = "#f9c74f",
  "PCB" = "#4363d8"
)

# ---------------------------
# Render stacked vertical bars for contaminants
# ---------------------------
render_contaminant_bars <- function(cause_text) {
  if (is.na(cause_text) || cause_text == "") return("")
  causes <- trimws(unlist(strsplit(cause_text, ",")))
  
  bars <- lapply(causes, function(cause) {
    color <- contaminant_colors[[cause]]
    if (is.null(color)) return("")
    
    # Bar styles — no margin, no padding, border-box
    div(style = paste0(
      "width: 100%; height: 6px; background-color:", color, 
      "; margin: 0; padding: 0; box-sizing: border-box;"
    ))
  })
  
  # Flex container — no gap, no margin, no padding
  div(style = "display: flex; flex-direction: column; gap: 0; margin: 0; padding: 0; box-sizing: border-box;", bars)
}


# ---------------------------
# Get size columns
# ---------------------------
size_cols <- names(restrict_aoc_final)[!(names(restrict_aoc_final) %in% c("Species", "Row_Label"))]

# ---------------------------
# Create column definitions
# ---------------------------
columns_list <- list()

# Advisory level and contaminant columns
for (col_name in size_cols) {
  columns_list[[col_name]] <- colDef(
    name = col_name,
    html = TRUE,
    cell = function(value, index) {
      row <- restrict_aoc_final[index, ]
      row_type <- row$Row_Label
      
      if (row_type == "Adv cause") {
        render_contaminant_bars(value)
      } else if (!is.na(as.numeric(value))) {
        value_num <- as.numeric(value)
        color <- if (value_num < 8) "#ff4d4d" else if (value_num >= 24) "#66cc66" else "#ffd966"
        div(style = paste0(
          "background-color:", color,
          "; width: 100%; height: 100%; padding: 6px 0; margin: 0; font-weight: bold; box-sizing: border-box;"
        ), value)
      } else {
        value
      }
    },
    style = function(value, index) {
      row_type <- restrict_aoc_final$Row_Label[index]
      
      # Shorter row height for Adv cause
      if (row_type == "Adv cause") {
        return(list(fontSize = "10px", paddingTop = "2px", paddingBottom = "2px"))
      }
      
      # Add spacing *after* Adv cause rows (i.e., before General of next group)
      if (index > 1 &&
          restrict_aoc_final$Row_Label[index - 1] == "Adv cause" &&
          row_type == "General") {
        return(list(paddingTop = "12px"))
      }
      
      return(NULL)
    }
  )
}

# Row_Label / Population column
columns_list$Row_Label <- colDef(
  name = "Population",
  minWidth = 120,
  style = function(value) {
    if (value == "Adv cause") {
      list(fontSize = "10px", fontStyle = "italic")
    } else {
      list(fontSize = "13px", fontWeight = "bold")
    }
  },
  headerStyle = list(whiteSpace = "nowrap")  # Prevent column header from wrapping
)

# Species column: use JavaScript to hide repeated values (merge cells visually)
columns_list$Species <- colDef(
  name = "Species",
  minWidth = 140,
  style = htmlwidgets::JS("
    function(rowInfo, column, state) {
      const firstSorted = state.sorted[0];
      if (!firstSorted || firstSorted.id === 'Species') {
        const prevRow = state.pageRows[rowInfo.viewIndex - 1];
        if (prevRow && rowInfo.values['Species'] === prevRow['Species']) {
          return { visibility: 'hidden' };
        }
      }
    }
  ")
)

# ---------------------------
# Render the table
# ---------------------------
reactable(
  restrict_aoc_final,
  columns = columns_list,
  defaultColDef = colDef(
    align = "center",
    minWidth = 80
  ),
  bordered = FALSE,
  striped = FALSE,
  highlight = TRUE,
  style = list(
    fontFamily = "sans-serif",
    fontSize = "13px",
    border = "none",
    margin = "0",
    padding = "0",
    borderSpacing = "0",
    borderCollapse = "collapse",
    boxSizing = "border-box"
  )
)



# Round 4

# -----------------------------------
# Render stacked vertical bars for contaminants
# -----------------------------------
render_contaminant_bars <- function(cause_text) {
  if (is.na(cause_text) || cause_text == "") return("")
  
  causes <- trimws(unlist(strsplit(cause_text, ",")))
  bars <- lapply(causes, function(cause) {
    color <- contaminant_colors[[cause]]
    if (is.null(color)) return("")
    
    div(style = paste0(
      "background-color:", color, "; ",
      "width: 100%; height: 6px; margin: 0; padding: 0; ",
      "border-radius: 3px; display: block; line-height: 1;"
    ))
  })
  
  div(style = "height: 100%; display: block; margin: 0; padding: 0;", bars)
}



# -----------------------------------
# Column setup
# -----------------------------------
size_cols <- names(restrict_aoc_final)[!(names(restrict_aoc_final) %in% c("Species", "Row_Label"))]
columns_list <- list()

for (col_name in size_cols) {
  columns_list[[col_name]] <- colDef(
    name = col_name,
    html = TRUE,
    cell = function(value, index) {
      row <- restrict_aoc_final[index, ]
      row_type <- row$Row_Label
      
      if (row_type == "Adv cause") {
        render_contaminant_bars(value)
      } else if (is.na(value) || value == "") {
        div(style = "background-color: #eef2f7; height: 100%; width: 100%;")
      } else if (!is.na(as.numeric(value))) {
        value_num <- as.numeric(value)
        color <- if (value_num < 8) "#ff4d4d" else if (value_num >= 24) "#66cc66" else "#ffd966"
        div(style = paste0(
          "background-color:", color, "; ",
          "width: 100%; height: 100%; display: flex; align-items: center; justify-content: center; ",
          "font-weight: bold; line-height: 1; margin: 0; padding: 0;"
        ), value)
        
        
        
      } else {
        value
      }
    },
    style = list(
      padding = "0px",
      margin = "0px",
      borderRight = "1px solid #ddd",
      borderBottom = "1px solid #ddd"
    )
  )
}

# Row label styling
columns_list$Row_Label <- colDef(
  name = "Population",
  minWidth = 120,
  style = function(value) {
    if (value == "Adv cause") {
      list(fontSize = "10px", fontStyle = "italic")
    } else {
      list(fontSize = "13px", fontWeight = "bold")
    }
  },
  headerStyle = list(whiteSpace = "nowrap")
)

# Merge Species cells with visibility hack
columns_list$Species <- colDef(
  name = "Species",
  minWidth = 140,
  style = JS("
    function(rowInfo, column, state) {
      const thisSpecies = rowInfo.values['Species'];
      const prevRow = state.pageRows[rowInfo.viewIndex - 1];
      const nextRow = state.pageRows[rowInfo.viewIndex + 1];

      const visibility = (prevRow && thisSpecies === prevRow['Species']) ? 'hidden' : 'visible';

      return {
        visibility: visibility
      };
    }
  ")
)

# -----------------------------------
# Define row spacing and top/bottom borders via rowStyle
# -----------------------------------
rowStyle_fn <- function(index) {
  row <- restrict_aoc_final[index, ]
  prev_row <- if (index > 1) restrict_aoc_final[index - 1, ] else NULL
  next_row <- if (index < nrow(restrict_aoc_final)) restrict_aoc_final[index + 1, ] else NULL
  
  style <- list()
  
  # Spacing between grouped species (after Adv cause)
  if (row$Row_Label == "Adv cause" &&
      !is.null(next_row) && next_row$Row_Label == "General") {
    style$paddingBottom <- "12px"
  }
  
  # Thick top/bottom border for species blocks
  if (is.null(prev_row) || prev_row$Species != row$Species) {
    style$borderTop <- "2px solid #666"
  }
  if (is.null(next_row) || next_row$Species != row$Species) {
    style$borderBottom <- "2px solid #666"
  }
  
  return(style)
}


# -----------------------------------
# Final table render
# -----------------------------------
reactable(
  restrict_aoc_final,
  columns = columns_list,
  defaultColDef = colDef(
    align = "center",
    minWidth = 80,
    style = list(
      padding = "0px",
      margin = "0px",
      verticalAlign = "top"
      # no border!
    )
  )
  ,
  rowStyle = rowStyle_fn,
  bordered = FALSE,
  striped = FALSE,
  highlight = TRUE,
  style = list(
    fontFamily = "sans-serif",
    fontSize = "13px",
    borderCollapse = "collapse",  # Important: removes spacing between cells
    borderSpacing = "0px",
    margin = "0 auto",
    width = "auto"
  )
)

# Round 5 ---------------
library(reactable)
library(dplyr)

# ---------------------------
# Define contaminant colors
# ---------------------------
contaminant_colors <- c(
  "PFAS" = "#e6194B",
  "Mercury" = "#f9c74f",
  "PCB" = "#4363d8"
)

# ---------------------------
# Get the size columns
# ---------------------------
size_cols <- names(restrict_aoc_final)[!(names(restrict_aoc_final) %in% c("Species", "Row_Label"))]

# ---------------------------
# Create column definitions
# ---------------------------
columns_list <- list()

for (col_name in size_cols) {
  columns_list[[col_name]] <- colDef(
    name = col_name,
    html = TRUE,
    
    cell = function(value, index) {
      row <- restrict_aoc_final[index, ]
      row_type <- row$Row_Label
      
      # Show bars for Adv cause rows
      if (row_type == "Adv cause" && !is.na(value) && value != "") {
        causes <- trimws(unlist(strsplit(value, ",")))
        colors <- c("PFAS" = "#e6194B", "Mercury" = "#f9c74f", "PCB" = "#4363d8")
        bars <- lapply(causes, function(cause) {
          color <- colors[[cause]]
          if (is.null(color)) return("")
          paste0(
            "<div style='width: 100%; height: 6px; padding: 0; margin: 0; background:", color,
            "; box-sizing: border-box; display: block'></div>"
          )
        })
        return(HTML(paste0(
          "<div style='line-height: 0; padding: 0; margin: 0;'>",
          paste(bars, collapse = ""),
          "</div>"
          )))
      }
      
      # Show advisory level value in other rows
      if (row_type != "Adv cause" && !is.na(value)) return(value)
      return("")
    },
    
    style = function(value, index) {
      row <- restrict_aoc_final[index, ]
      row_type <- row$Row_Label
      base <- list(
        padding = "0px",
        margin = "0px",
        verticalAlign = "middle",
        textAlign = "center",
        fontWeight = "bold"
      )
      
      # Style background for advisory levels
      if (row_type != "Adv cause" && !is.na(value)) {
        val <- as.numeric(value)
        if (val < 8) {
          base$background <- "#ff4d4d"
        } else if (val >= 24) {
          base$background <- "#66cc66"
        } else {
          base$background <- "#ffd966"
        }
      }
      
      # Grey background for NA
      if ((is.na(value) || value == "") && row_type %in% c("General", "Sensitive")) {
        base$background <- "#eef2f7"
      }
      
      
      return(base)
    }
  )
  
}

# ---------------------------
# Define Row_Label (Population) column
# ---------------------------
columns_list$Row_Label <- colDef(
  name = "Population",
  minWidth = 120,
  style = function(value) {
    if (value == "Adv cause") {
      list(fontSize = "10px", fontStyle = "italic")
    } else {
      list(fontSize = "13px", fontWeight = "bold")
    }
  },
  headerStyle = list(whiteSpace = "nowrap")
)

# ---------------------------
# Merge Species cells using visibility trick
# ---------------------------
columns_list$Species <- colDef(
  name = "Species",
  minWidth = 140,
  style = JS("
    function(rowInfo, column, state) {
      const thisSpecies = rowInfo.values['Species'];
      const prevRow = state.pageRows[rowInfo.viewIndex - 1];
      const nextRow = state.pageRows[rowInfo.viewIndex + 1];

      const visibility = (prevRow && thisSpecies === prevRow['Species']) ? 'hidden' : 'visible';

      return {
        visibility: visibility
      };
    }
  ")
)

# ---------------------------
# Row-level spacing and borders
# ---------------------------
rowStyle_fn <- function(index) {
  row <- restrict_aoc_final[index, ]
  prev_row <- if (index > 1) restrict_aoc_final[index - 1, ] else NULL
  next_row <- if (index < nrow(restrict_aoc_final)) restrict_aoc_final[index + 1, ] else NULL
  
  style <- list(height = "32px")  # Keeps rows uniform
  
  ## Add spacing after an "Adv cause" row only if it's the **last in a group**
  #if (row$Row_Label == "Adv cause" &&
  #    !is.null(next_row) && next_row$Row_Label == "General") {
  #  style$paddingBottom <- "12px"
  #}
  
  # Apply thick borders between species blocks
  if (is.null(prev_row) || prev_row$Species != row$Species) {
    style$borderTop <- "2px solid #666"
  }
  if (is.null(next_row) || next_row$Species != row$Species) {
    style$borderBottom <- "2px solid #666"
  }
  
  return(style)
}


# ---------------------------
# Render final reactable
# ---------------------------
reactable(
  restrict_aoc_final,
  columns = columns_list,
  defaultColDef = colDef(
    align = "center",
    minWidth = 80,
    style = list(
      padding = "0px",
      margin = "0px",
      border = "none"
    )
  ),
  rowStyle = rowStyle_fn,
  bordered = FALSE,
  striped = FALSE,
  highlight = TRUE,
  style = list(
    fontFamily = "sans-serif",
    fontSize = "13px",
    borderCollapse = "collapse",
    borderSpacing = "0",
    margin = "0 auto",
    width = "auto"
  )
)

# Round 6 - full reformat
 # ✅ Step 1: Expand the dataset to include stacked contaminant rows


# Define contaminants in the order you want them stacked
contaminants <- c("Buffer", "PCB", "Mercury", "PFAS")

# Function to expand one advisory row into 5 rows
expand_advisory_row <- function(advisory_row, cause_row) {
  advisory_row <- as.data.frame(advisory_row)  # Ensure data frame format
  cause_row <- as.data.frame(cause_row)
  
  # Make 5 copies of the advisory row
  expanded_rows <- advisory_row[rep(1, 5), ]
  expanded_rows$Row_Label <- c("Buffer", "PCB", "Mercury", "PFAS", advisory_row$Row_Label)
  
  # Fill contaminant rows based on matching cause text
  for (i in 2:4) {
    contam <- contaminants[i]
    expanded_rows[i, !(names(expanded_rows) %in% c("Species", "Row_Label"))] <- lapply(
      cause_row[1, !(names(cause_row) %in% c("Species", "Row_Label"))],
      function(x) if (!is.na(x) && grepl(contam, x)) 1 else NA
    )
  }
  
  # Buffer row remains all NA
  expanded_rows[1, !(names(expanded_rows) %in% c("Species", "Row_Label"))] <- NA
  
  return(expanded_rows)
}

# Pull out advisory level rows (General + Sensitive)
advisory_rows <- restrict_aoc_final %>% filter(Row_Label %in% c("General", "Sensitive"))

# Pull out cause rows
cause_rows <- restrict_aoc_final %>% filter(Row_Label == "Adv cause")

# Loop and expand each advisory row based on species-matched cause row
expanded_all <- bind_rows(lapply(1:nrow(advisory_rows), function(i) {
  advisory_row <- advisory_rows[i, ]
  cause_row <- cause_rows %>% filter(Species == advisory_row$Species) %>% dplyr::slice(1)
  expand_advisory_row(advisory_row, cause_row)
}))

expanded_all <- expanded_all %>%
  mutate(Species = advisory_rows$Species[rep(1:nrow(advisory_rows), each = 5)]) %>%
  select(Species, Row_Label, everything())
expanded_all <- expanded_all %>%
  mutate(across(where(is.factor), as.character)) %>%
  mutate(across(where(is.character), ~ ifelse(. == "1", 1, .)))


# Step 2 Reactable


# Define contaminant colors
contaminant_colors <- c(
  "PFAS" = "#e6194B",
  "Mercury" = "#f9c74f",
  "PCB" = "#4363d8"
)

# Get all size columns
size_cols <- names(expanded_all)[!(names(expanded_all) %in% c("Species", "Row_Label"))]

# Column definitions
columns_list <- list()

for (col_name in size_cols) {
  columns_list[[col_name]] <- colDef(
    name = col_name,
    html = TRUE,
    cell = function(value, index) {
      row <- expanded_all[index, ]
      row_type <- row$Row_Label
      
      if (row_type %in% names(contaminant_colors)) {
        # For contaminant bar rows, draw a colored block if 1 is present
        if (!is.na(value) && value == 1) {
          color <- contaminant_colors[[row_type]]
          return(HTML(paste0(
            "<div style='width:100%; height:100%; background:", color, ";'></div>"
          )))
        } else {
          return("")  # no bar if not 1
        }
      }
      
      # Normal advisory level cells
      if (row_type %in% c("General", "Sensitive") && !is.na(value)) {
        return(value)
      }
      
      return("")  # Empty fallback
    }
    ,
    style = function(value, index) {
      row <- expanded_all[index, ]
      row_type <- row$Row_Label
      style <- list(
        padding = "0px",
        margin = "0px",
        textAlign = "center",
        verticalAlign = "middle",
        fontWeight = "bold"
      )
      
      # Advisory level background colors
      if (row_type %in% c("General", "Sensitive") && !is.na(value)) {
        val <- as.numeric(value)
        if (val < 8) style$background <- "#ff4d4d"
        else if (val >= 24) style$background <- "#66cc66"
        else style$background <- "#ffd966"
      }
      
      # Grey for NA in advisory level rows only
      if (row_type %in% c("General", "Sensitive") && (is.na(value) || value == "")) {
        style$background <- "#eef2f7"
      }
      
      return(style)
    }
  )
}

# Row_Label column
columns_list$Row_Label <- colDef(
  name = "Population",
  minWidth = 120,
  style = function(value) {
    if (value %in% c("Buffer", "PCB", "Mercury", "PFAS")) {
      list(fontSize = "1px", lineHeight = "1")
    } else if (value == "Adv cause") {
      list(fontSize = "10px", fontStyle = "italic")
    } else {
      list(fontSize = "13px", fontWeight = "bold")
    }
  },
  headerStyle = list(whiteSpace = "nowrap")
)

# Merge Species cells
columns_list$Species <- colDef(
  name = "Species",
  minWidth = 140,
  style = JS("
    function(rowInfo, column, state) {
      const thisSpecies = rowInfo.values['Species'];
      const prevRow = state.pageRows[rowInfo.viewIndex - 1];
      const visibility = (prevRow && thisSpecies === prevRow['Species']) ? 'hidden' : 'visible';
      return { visibility: visibility };
    }
  ")
)

# Row styling
rowStyle_fn <- function(index) {
  row <- expanded_all[index, ]
  prev_row <- if (index > 1) expanded_all[index - 1, ] else NULL
  next_row <- if (index < nrow(expanded_all)) expanded_all[index + 1, ] else NULL
  
  style <- list(height = if (row$Row_Label %in% c("Buffer", "PCB", "Mercury", "PFAS")) "6px" else "32px")
  
  # Add spacing after PFAS row if next row is General
  if (row$Row_Label == "PFAS" &&
      !is.null(next_row) && next_row$Row_Label == "General") {
    style$paddingBottom <- "10px"
  }
  
  # Thick border above and below species group
  if (is.null(prev_row) || prev_row$Species != row$Species) {
    style$borderTop <- "2px solid #666"
  }
  if (is.null(next_row) || next_row$Species != row$Species) {
    style$borderBottom <- "2px solid #666"
  }
  
  return(style)
}

# Render final table
reactable(
  expanded_all,
  columns = columns_list,
  rowStyle = rowStyle_fn,
  defaultColDef = colDef(
    align = "center",
    minWidth = 80,
    style = list(padding = "0px", margin = "0px", border = "none")
  ),
  bordered = FALSE,
  striped = FALSE,
  highlight = TRUE,
  style = list(
    fontFamily = "sans-serif",
    fontSize = "13px",
    borderCollapse = "collapse",
    borderSpacing = "0",
    margin = "0 auto"
  )
)

# ROUND 7 ----------------
## Refreshed and streamlined reactable rendering code
library(reactable)
library(dplyr)

# Contaminant colors
contaminant_colors <- c(
  PFAS = "#e6194B",
  Mercury = "#f9c74f",
  PCB = "#4363d8"
)

# Get size bin columns
size_cols <- setdiff(names(restrict_aoc_final), c("Species", "Row_Label"))

# Create column definitions
columns_list <- list()

for (col_name in size_cols) {
  columns_list[[col_name]] <- colDef(
    name = col_name,
    html = TRUE,
    cell = function(value, index) {
      row <- restrict_aoc_final[index, ]
      row_type <- row$Row_Label
      
      # Render contaminant bars for Adv cause
      if (row_type == "Adv cause" && !is.na(value) && nzchar(value)) {
        causes <- trimws(unlist(strsplit(value, ",")))
        bars <- paste0(
          "<div style='display: flex; flex-direction: column; height: 100%; width: 100%; margin: 0; padding: 0;'>",
          paste0(
            lapply(causes, function(cause) {
              color <- contaminant_colors[[cause]]
              if (!is.null(color)) paste0(
                "<div style='flex: 1; background: ", color, "; margin: 0; padding: 0;'></div>"
              ) else ""
            }),
            collapse = ""
          ),
          "</div>"
        )
        return(HTML(bars))
      }
      
      # Display advisory level
      if (row_type != "Adv cause" && !is.na(value)) return(value)
      return("")
    },
    style = function(value, index) {
      row <- restrict_aoc_final[index, ]
      row_type <- row$Row_Label
      styles <- list(padding = "0", margin = "0", fontWeight = "bold")
      
      # Color advisory levels
      if (row_type != "Adv cause" && !is.na(value)) {
        val <- as.numeric(value)
        styles$background <- if (val < 8) "#ff4d4d" else "#66cc66"
      } else if ((is.na(value) || value == "") && row_type %in% c("General", "Sensitive")) {
        styles$background <- "#eef2f7"
      }
      
      return(styles)
    }
  )
}

# Row_Label (Population) column
columns_list$Row_Label <- colDef(
  name = "Population",
  minWidth = 120,
  style = function(value) {
    if (value == "Adv cause") list(fontSize = "10px", fontStyle = "italic")
    else list(fontSize = "13px", fontWeight = "bold")
  },
  headerStyle = list(whiteSpace = "nowrap")
)

# Species column with hidden duplicates
columns_list$Species <- colDef(
  name = "Species",
  minWidth = 140,
  style = JS("function(rowInfo, column, state) {
    const prev = state.pageRows[rowInfo.viewIndex - 1];
    if (prev && rowInfo.values['Species'] === prev['Species']) {
      return { visibility: 'hidden' };
    }
    return {};
  }")
)

# Row-level styling
rowStyle_fn <- function(index) {
  row <- restrict_aoc_final[index, ]
  prev_row <- if (index > 1) restrict_aoc_final[index - 1, ] else NULL
  next_row <- if (index < nrow(restrict_aoc_final)) restrict_aoc_final[index + 1, ] else NULL
  
  style <- list(height = "32px")
  if (is.null(prev_row) || prev_row$Species != row$Species) style$borderTop <- "2px solid #666"
  if (is.null(next_row) || next_row$Species != row$Species) style$borderBottom <- "2px solid #666"
  return(style)
}

# Final render
htmltools::browsable(
  htmltools::tagList(
    htmltools::tags$style(HTML("
      .reactable-table td {
        padding: 0 !important;
        margin: 0 !important;
        line-height: 0 !important;
        vertical-align: top !important;
      }
    ")),
    reactable(
      restrict_aoc_final,
      columns = columns_list,
      defaultColDef = colDef(
        align = "center",
        minWidth = 80,
        style = list(padding = "0", margin = "0", border = "none")
      ),
      rowStyle = rowStyle_fn,
      bordered = FALSE,
      striped = FALSE,
      highlight = TRUE,
      style = list(
        fontFamily = "sans-serif",
        fontSize = "13px",
        borderCollapse = "collapse",
        borderSpacing = "0",
        margin = "0 auto",
        width = "auto"
      )
    )
  )
)


# ROUND 8 ----------------------------
## Reactablefmtr icon assign
# Refreshed and streamlined reactable rendering code with Unicode icons
library(reactable)
library(dplyr)
library(reactablefmtr)

# Contaminant colors
contaminant_colors <- c(
  PFAS = "#e6194B",
  Mercury = "#f9c74f",
  PCB = "#4363d8"
)

# Unicode shapes for each contaminant
contaminant_shapes <- c(
  PFAS = "\u25A0",    # ■ square
  Mercury = "\u25CF", # ● circle
  PCB = "\u25B2"       # ▲ diamond
)

# Get size bin columns
size_cols <- setdiff(names(restrict_aoc_final), c("Species", "Row_Label"))

# Create column definitions
columns_list <- list()

for (col_name in size_cols) {
  columns_list[[col_name]] <- colDef(
    name = col_name,
    html = TRUE,
    cell = function(value, index) {
      row <- restrict_aoc_final[index, ]
      row_type <- row$Row_Label
      
      # Use Unicode symbols for Adv cause
      if (row_type == "Adv cause" && !is.na(value) && nzchar(value)) {
        causes <- trimws(unlist(strsplit(value, ",")))
        icons <- lapply(causes, function(cause) {
          color <- contaminant_colors[[cause]]
          shape <- contaminant_shapes[[cause]]
          if (!is.null(color) && !is.null(shape)) {
            paste0("<span style='color:", color, "; font-size: 16px; margin-right: 4px;'>", shape, "</span>")
          } else {
            ""
          }
        })
        return(HTML(paste(icons, collapse = " ")))
      }
      
      # Display advisory level
      if (row_type != "Adv cause" && !is.na(value)) return(value)
      return("")
    },
    style = function(value, index) {
      row <- restrict_aoc_final[index, ]
      row_type <- row$Row_Label
      styles <- list(padding = "0", margin = "0", fontWeight = "bold")
      
      # Color advisory levels
      if (row_type != "Adv cause" && !is.na(value)) {
        val <- as.numeric(value)
        styles$background <- if (val < 8) "#ff4d4d" else "#66cc66"
      } else if ((is.na(value) || value == "") && row_type %in% c("General", "Sensitive")) {
        styles$background <- "#eef2f7"
      }
      
      return(styles)
    }
  )
}

# Row_Label (Population) column
columns_list$Row_Label <- colDef(
  name = "Population",
  minWidth = 120,
  style = function(value) {
    if (value == "Adv cause") list(fontSize = "10px", fontStyle = "italic")
    else list(fontSize = "13px", fontWeight = "bold")
  },
  headerStyle = list(whiteSpace = "nowrap")
)

# Species column with hidden duplicates
columns_list$Species <- colDef(
  name = "Species",
  minWidth = 140,
  style = JS("function(rowInfo, column, state) {
    const prev = state.pageRows[rowInfo.viewIndex - 1];
    if (prev && rowInfo.values['Species'] === prev['Species']) {
      return { visibility: 'hidden' };
    }
    return {};
  }")
)

# Row-level styling
rowStyle_fn <- function(index) {
  row <- restrict_aoc_final[index, ]
  prev_row <- if (index > 1) restrict_aoc_final[index - 1, ] else NULL
  next_row <- if (index < nrow(restrict_aoc_final)) restrict_aoc_final[index + 1, ] else NULL
  
  style <- list(height = "32px")
  if (is.null(prev_row) || prev_row$Species != row$Species) style$borderTop <- "2px solid #666"
  if (is.null(next_row) || next_row$Species != row$Species) style$borderBottom <- "2px solid #666"
  return(style)
}

# Final render
# Final render
reactable(
  pagination = TRUE,
  showPageSizeOptions = TRUE,
  defaultPageSize = 10,
  restrict_aoc_final,
  columns = columns_list,
  defaultColDef = colDef(
    sortable = TRUE,
    align = "center",
    minWidth = 80,
    style = list(padding = "0", margin = "0", border = "none")
  ),
  rowStyle = rowStyle_fn,
  bordered = FALSE,
  striped = FALSE,
  highlight = TRUE,
  style = list(
    fontFamily = "sans-serif",
    fontSize = "13px",
    borderCollapse = "collapse",
    borderSpacing = "0",
    margin = "0 auto",
    width = "auto"
  )
)

# Legend for advisory causes
legend_html <- htmltools::HTML(paste(
  "<div style='margin-bottom: 10px;'>",
  "<strong>Advisory Cause Legend:</strong><br>",
  "<span style='color:#e6194B; font-size: 16px;'>■</span> PFAS &nbsp;&nbsp;",
  "<span style='color:#f9c74f; font-size: 16px;'>●</span> Mercury &nbsp;&nbsp;",
  "<span style='color:#4363d8; font-size: 16px;'>▲</span> PCB",
  "</div>",
  sep = ""
))

# Final render
htmltools::browsable(tagList(
  legend_html,
  reactable(
    pagination = TRUE,
    showPageSizeOptions = TRUE,
    defaultPageSize = 10,
    restrict_aoc_final,
    columns = columns_list,
    defaultColDef = colDef(
      sortable = TRUE,
      align = "center",
      minWidth = 80,
      style = list(padding = "0", margin = "0", border = "none")
    ),
    rowStyle = rowStyle_fn,
    bordered = FALSE,
    striped = FALSE,
    highlight = TRUE,
    style = list(
      fontFamily = "sans-serif",
      fontSize = "13px",
      borderCollapse = "collapse",
      borderSpacing = "0",
      margin = "0 auto",
      width = "auto"
    )
  )
))


## Round 9 -------------
# Reactable with Font Awesome icons for advisory causes
library(reactable)
library(dplyr)
library(reactablefmtr)
library(htmltools)

# Load Font Awesome
fa_css <- tags$head(
  tags$link(
    rel = "stylesheet",
    href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
  )
)

# Contaminant colors
contaminant_colors <- c(
  PFAS = "#e6194B",
  Mercury = "#f9c74f",
  PCB = "#4363d8"
)

# Font Awesome icons
contaminant_icons <- c(
  PFAS = "fa-square",
  Mercury = "fa-circle",
  PCB = "fa-diamond"
)

# Get size bin columns
size_cols <- setdiff(names(restrict_aoc_final), c("Species", "Row_Label"))

# Create column definitions
columns_list <- list()

for (col_name in size_cols) {
  columns_list[[col_name]] <- colDef(
    name = col_name,
    html = TRUE,
    cell = function(value, index) {
      row <- restrict_aoc_final[index, ]
      row_type <- row$Row_Label

      if (row_type == "Adv cause" && !is.na(value) && nzchar(value)) {
        causes <- trimws(unlist(strsplit(value, ",")))
        icons <- lapply(causes, function(cause) {
          icon_class <- contaminant_icons[[cause]]
          color <- contaminant_colors[[cause]]
          if (!is.null(icon_class) && !is.null(color)) {
            paste0("<i class='fas ", icon_class, "' style='color:", color, "; margin-right: 6px;'></i>")
          } else {
            ""
          }
        })
        return(HTML(paste(icons, collapse = " ")))
      }

      if (row_type != "Adv cause" && !is.na(value)) return(value)
      return("")
    },
    style = function(value, index) {
      row <- restrict_aoc_final[index, ]
      row_type <- row$Row_Label
      styles <- list(padding = "0", margin = "0", fontWeight = "bold")

      if (row_type != "Adv cause" && !is.na(value)) {
        val <- as.numeric(value)
        styles$background <- if (val < 8) "#ff4d4d" else "#66cc66"
      } else if ((is.na(value) || value == "") && row_type %in% c("General", "Sensitive")) {
        styles$background <- "#eef2f7"
      }

      return(styles)
    }
  )
}

# Row_Label column
columns_list$Row_Label <- colDef(
  name = "Population",
  minWidth = 120,
  style = function(value) {
    if (value == "Adv cause") list(fontSize = "10px", fontStyle = "italic")
    else list(fontSize = "13px", fontWeight = "bold")
  },
  headerStyle = list(whiteSpace = "nowrap")
)

# Species column with hidden duplicates
columns_list$Species <- colDef(
  name = "Species",
  minWidth = 140,
  style = JS("function(rowInfo, column, state) {
    const prev = state.pageRows[rowInfo.viewIndex - 1];
    if (prev && rowInfo.values['Species'] === prev['Species']) {
      return { visibility: 'hidden' };
    }
    return {};
  }")
)

# Row styling
rowStyle_fn <- function(index) {
  row <- restrict_aoc_final[index, ]
  prev_row <- if (index > 1) restrict_aoc_final[index - 1, ] else NULL
  next_row <- if (index < nrow(restrict_aoc_final)) restrict_aoc_final[index + 1, ] else NULL

  style <- list(height = "32px")
  if (is.null(prev_row) || prev_row$Species != row$Species) style$borderTop <- "2px solid #666"
  if (is.null(next_row) || next_row$Species != row$Species) style$borderBottom <- "2px solid #666"
  return(style)
}

# Legend
legend_html <- HTML(paste(
  "<div style='margin-bottom: 10px;'>",
  "<strong>Advisory Cause Legend:</strong><br>",
  "<i class='fas fa-square' style='color:#e6194B;'></i> PFAS &nbsp;&nbsp;",
  "<i class='fas fa-circle' style='color:#f9c74f;'></i> Mercury &nbsp;&nbsp;",
  "<i class='fas fa-diamond' style='color:#4363d8;'></i> PCB",
  "</div>",
  sep = ""
))

# Render with Font Awesome and legend
browsable(tagList(
  fa_css,
  legend_html,
  reactable(
    restrict_aoc_final,
    columns = columns_list,
    defaultPageSize = 10,
    showPageSizeOptions = TRUE,
    pagination = TRUE,
    defaultColDef = colDef(
      sortable = TRUE,
      align = "center",
      minWidth = 80,
      style = list(padding = "0", margin = "0", border = "none")
    ),
    rowStyle = rowStyle_fn,
    bordered = FALSE,
    striped = FALSE,
    highlight = TRUE,
    style = list(
      fontFamily = "sans-serif",
      fontSize = "13px",
      borderCollapse = "collapse",
      borderSpacing = "0",
      margin = "0 auto",
      width = "auto"
    )
  )
))




# Round 10 -----------
# Reactable with Font Awesome icons for advisory causes
library(reactable)
library(dplyr)
library(reactablefmtr)
library(htmltools)

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

# Get size bin columns
size_cols <- setdiff(names(restrict_aoc_final), c("Species", "Row_Label"))

# Create column definitions
columns_list <- list()

for (col_name in size_cols) {
  columns_list[[col_name]] <- colDef(
    name = col_name,
    html = TRUE,
    cell = function(value, index) {
      row <- restrict_aoc_final[index, ]
      row_type <- row$Row_Label
      
      if (row_type == "Adv cause" && !is.na(value) && nzchar(value)) {
        causes <- trimws(unlist(strsplit(value, ",")))
        icons <- lapply(causes, function(cause) {
          icon_class <- contaminant_icons[[cause]]
          color <- contaminant_colors[[cause]]
          if (!is.null(icon_class) && !is.null(color)) {
            paste0("<i class='fas ", icon_class, "' style='color:", color, "; margin-right: 6px;'></i>")
          } else {
            ""
          }
        })
        return(HTML(paste(icons, collapse = " ")))
      }
      
      if (row_type != "Adv cause" && !is.na(value)) return(value)
      return("")
    },
    style = function(value, index) {
      row <- restrict_aoc_final[index, ]
      row_type <- row$Row_Label
      styles <- list(padding = "0", margin = "0", fontWeight = "bold")
      
      if (row_type == "Adv cause") {
        styles$paddingTop <- "0"
        styles$paddingBottom <- "10px"
      } else if (!is.na(value)) {
        val <- as.numeric(value)
        styles$background <- if (val < 8) "#ff4d4d" else "#66cc66"
      } else if ((is.na(value) || value == "") && row_type %in% c("General", "Sensitive")) {
        styles$background <- "#eef2f7"
      }
      
      return(styles)
    }
  )
}

# Row_Label column
columns_list$Row_Label <- colDef(
  name = "Population",
  minWidth = 120,
  style = function(value) {
    if (value == "Adv cause") list(fontSize = "10px", fontStyle = "italic")
    else list(fontSize = "13px", fontWeight = "bold")
  },
  headerStyle = list(whiteSpace = "nowrap")
)

# Species column with hidden duplicates
columns_list$Species <- colDef(
  name = "Species",
  minWidth = 140,
  style = JS("function(rowInfo, column, state) {
    const prev = state.pageRows[rowInfo.viewIndex - 1];
    if (prev && rowInfo.values['Species'] === prev['Species']) {
      return { visibility: 'hidden' };
    }
    return {};
  }")
)

# Row styling
rowStyle_fn <- function(index) {
  row <- restrict_aoc_final[index, ]
  prev_row <- if (index > 1) restrict_aoc_final[index - 1, ] else NULL
  next_row <- if (index < nrow(restrict_aoc_final)) restrict_aoc_final[index + 1, ] else NULL
  
  style <- list(height = "32px")
  if (is.null(prev_row) || prev_row$Species != row$Species) style$borderTop <- "2px solid #666"
  if (is.null(next_row) || next_row$Species != row$Species) style$borderBottom <- "2px solid #666"
  return(style)
}

# Legend
legend_html <- HTML(paste(
  "<div style='margin-bottom: 10px; font-family: sans-serif; font-size: 13px;'>",
  "<strong>Advisory Cause Legend:</strong><br>",
  "<i class='fas fa-square' style='color:magenta;'></i> PFAS &nbsp;&nbsp;",
  "<i class='fas fa-circle' style='color:goldenrod;'></i> Mercury &nbsp;&nbsp;",
  "<i class='fas fa-diamond' style='color:skyblue;'></i> PCB",
  "</div>",
  sep = ""
))

# Render with Font Awesome and legend
browsable(tagList(
  fa_css,
  legend_html,
  reactable(
    restrict_aoc_final,
    columns = columns_list,
    defaultPageSize = 10,
    showPageSizeOptions = TRUE,
    pagination = TRUE,
    defaultColDef = colDef(
      sortable = TRUE,
      align = "center",
      minWidth = 80,
      style = list(padding = "0", margin = "0", border = "none")
    ),
    rowStyle = rowStyle_fn,
    bordered = FALSE,
    striped = FALSE,
    highlight = TRUE,
    style = list(
      fontFamily = "sans-serif",
      fontSize = "13px",
      borderCollapse = "collapse",
      borderSpacing = "0",
      margin = "0 auto",
      width = "auto"
    )
  )
))



  