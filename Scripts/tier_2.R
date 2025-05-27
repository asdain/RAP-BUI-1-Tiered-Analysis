###############################
source("Scripts/setup.R")
###############################

# TIER 2: ARE FISH ADVISORIES IN THE AOC MORE RESTRICTIVE THAN SUITABLE REFERENCE SITES?

# FISH -> FISH OF INTEREST THAT FAILED TIER 1
# RESTRICTIVE -> BELOW A CERTAIN THRESHOLD OF MEALS PER MONTH
# SUITABLE REFERENCE SITES -> NON-AOC, SIMILAR WATERS. KEN DROUILLARD RECOMMENDS MINIMUM 3
## Upper St. Lawrencce (Kingston-1000 Islands)
## Non-AOC Lake Ontario?


## Filtered dataset to relevant variables
dat = cons_data %>%
  select(c(spec = specname,
           pop_id = population_type_id,
           pop_name = population_type_desc,
           length_id = length_category_id,
           length_name = length_category_label,
           adv_level = adv_level,
           adv_cause = adv_cause_multiple_name,
           loc = guide_locname_eng
  ))

# Now, what to do about suitable reference sites?
# Let's take a look at a few for now:
## SLR (Kingston to Cornwall) -> The obvious upstream site
## All Great Lakes non-AOC sites (as per Detroit River):
###  Lake Superior
#### S4 Black Bay
#### S7 Schreiber Point
#### S10 Agawa and Bachwana
#### S11 Goulais Bay
### Lake Huron
#### GB1 Georgian Bay
#### GB3 Georgian Bay
#### GB4 Georgian Bay
#### H3 Lake Huron
#### H4 Lake Huron
#### H5 Lake Huron
#### NC1 North Channel
#### NC2 North Channel
#### NC2a North Channel
### Lake Ontario
#### 2 Western Lake Ontario
#### 2a Jordan Harbour
#### 6 Northwestern Ontario
#### 6a Frenchman Bay
#### 6B Whitby Harbour
#### Northeastern Lake Ontario
### Lake Erie
#### 1 Western Lake Erie
#### 2 Cnetral Lake Erie
#### 2a Rondeau Bay
#### 3 Long Point Bay
#### Eastern Lake Erie
### Lake St Clair

# Not all these locations seem to exist in the 2024 csv. Just using the map for now.

dat_refs = dat %>%
  filter(loc %in% c("St. Lawrence River (Lake St. Francis)",
         "St. Lawrence River (Lake Ontario to Cornwall)",
         "Black Bay area (Lake Superior)",
         "Lake Superior (Eastern Basin)",
         "Lake Superior (Western Basin)",
         "North Channel (Lake Huron)",
         "Georgian Bay (Lake Huron)",
         "Lake Huron (Main Basin)",
         "Lake Ontario (Western Basin)",
         "Lake Ontario (Eastern Basin)",
         "Whitby Harbour (Lake Ontario)",
         "Frenchman Bay (Lake Ontario)",
         "Lake Erie",
         "Rondeau Bay (Lake Erie)",
         "Lake St. Clair"
         ))

lsf_spec = dat %>%
  filter(loc == "St. Lawrence River (Lake St. Francis)") %>% # Applies cases across the same species found in LSF
  distinct(spec)

dat_refs=dat_refs %>%
  semi_join(lsf_spec, by = "spec") 

dat_refs = dat_refs %>%
  mutate(ref = (ifelse(loc == "St. Lawrence River (Lake St. Francis)", "Target", "Reference")))




ref_medians = dat_refs %>%
  filter(ref == "Reference") %>%
  group_by(spec, length_name, pop_name) %>% #Groups variables by waterbody and advisory population type (general or sensitive)
  summarize(n = nrow(loc),median = median(adv_level)) #Summarizes by advisory level per species 

comparison_dat = dat_refs %>%
  filter(ref == "Target") %>%
  left_join(ref_medians) %>%
  mutate(is_more_restrictive = ifelse(adv_level <= median, TRUE, FALSE))

comparison_brief = comparison_dat %>%
  select(c(spec, length_name, pop_name, adv_level, median, is_more_restrictive))

table = 

table = kbl(comparison_brief) %>%
  kable_styling() %>%
  column_spec(table, is_more_restrictive, color = ifelse(table$is_more_restrictive == TRUE, "red", "green"))
table

table_wide = spread(comparison_brief, length_name, adv_level, median, is_more_restrictive) %>%
  #select(-c(length_name, adv_level, pop_id)) %>%
  group_by(spec, pop_name) %>%
  summarize(across(everything(), ~ na.omit(.)[1]), .groups = "drop")

table_wide = comparison_brief %>%
  pivot_wider(names_from = length_name)


## REACTABLE WORKFLOW -------------------
dat = cons_data %>%
  select(c(spec = specname,
           pop_id = population_type_id,
           pop_name = population_type_desc,
           length_id = length_category_id,
           length_name = length_category_label,
           adv_level = adv_level,
           adv_cause = adv_cause_multiple_name,
           loc = guide_locname_eng
  ))

### REACTABLE: ALL REFERENCE SITES
reference_sites = c("St. Lawrence River (Lake Ontario to Cornwall)",
                    # Superior
                    "Black Bay area (Lake Superior)",
                    "Lake Superior (Eastern Basin)",
                    "Lake Superior (Western Basin)",
                    # Huron
                    "North Channel (Lake Huron)",
                    "Georgian Bay (Lake Huron)",
                    "Lake Huron (Main Basin)",
                    # Ontario
                    "Lake Ontario (Western Basin)",
                    "Lake Ontario (Eastern Basin)",
                    "Whitby Harbour (Lake Ontario)",
                    "Frenchman Bay (Lake Ontario)",
                    # Erie
                    "Lake Erie",
                    "Rondeau Bay (Lake Erie)",
                    "Lake St. Clair",
                    ## Inlets
                    ## Ontario
                    "West Lake",
                    "East Lake",
                    "Consecon Lake",
                    "McLaughlin Bay (Lake Ontario)",
                    "Oshawa Harbour (Lake Ontario)",
                    "Whitby Harbour (Lake Ontario)",
                    "Frenchman Bay (Lake Ontario)",
                    "Jordan Harbour (Lake Ontario)",
                    ## Erie
                    "Wheatley Harbour (Lake Erie)",
                    ## Huron
                    "Collingwood Harbour (Georgian Bay - Lake Huron)",
                    "Severn Sound (Georgian Bay - Lake Huron)",  # Former AOC
                    ## Superior
                    "Nipigon Bay (Lake Superior)",
                    "Jackfish Bay Lake Superior" # AOC in recovery
  )
# Inlets - included bays and stuff, did not include marshes


# Filter reference sites and get median advisory level
ref_dat <- cons_data %>%
  filter(guide_locname_eng %in% reference_sites) %>%
  filter(!is.na(adv_level)) %>%
  group_by(specname, population_type_desc, length_category_label) %>%
  summarize(median_ref = median(adv_level), .groups = "drop")

# Join reference medians to AOC table
restrict_aoc_with_ref <- restrict_aoc %>%
  left_join(ref_dat, 
            by = c("spec" = "specname", 
                   "pop_name" = "population_type_desc", 
                   "length_name" = "length_category_label")) %>%
  mutate(
    is_lower_than_ref = ifelse(!is.na(adv_level) & !is.na(median_ref), adv_level < median_ref, FALSE)
  )

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

restrict_aoc_with_ref$length_name = factor(restrict_aoc_with_ref$length_name, levels = length_levels, ordered = T)

# Prepare reshaped table with comparison flags


# Convert adv_level to character (for consistency with existing workflow)

restrict_aoc_with_ref <- restrict_aoc_with_ref %>%
  mutate(
    adv_level = as.character(adv_level),
    adv_cause = as.character(adv_cause),
    median_ref = as.character(round(median_ref, 1))
  )

restrict_aoc_long <- restrict_aoc_with_ref %>%
  pivot_longer(
    cols = c(adv_level, adv_cause, median_ref),
    names_to = "Variable", values_to = "Value"
  ) %>%
  mutate(VarPop = paste(pop_name, Variable, sep = "_"))
restrict_aoc_long$length_name <- factor(restrict_aoc_long$length_name, levels = length_levels, ordered = TRUE)

# Wide format
restrict_aoc_wide <- restrict_aoc_long %>%
  select(spec, length_name, VarPop, Value) %>%
  pivot_wider(names_from = length_name, values_from = Value)

ordered_size_cols <- length_levels[length_levels %in% names(restrict_aoc_final)]
restrict_aoc_final <- restrict_aoc_final[, c("Species", "Row_Label", ordered_size_cols)]
# Now assign Row_Label *intentionally*
restrict_aoc_final <- restrict_aoc_wide %>%
  mutate(Row_Label = case_when(
    VarPop == "General_adv_level"     ~ "General",
    VarPop == "General_adv_cause"     ~ "Adv cause",
    VarPop == "General_median_ref"    ~ "Ref median",
    VarPop == "Sensitive_adv_level"   ~ "Sensitive",
    VarPop == "Sensitive_adv_cause"   ~ "Adv cause",
    VarPop == "Sensitive_median_ref"  ~ "Ref median",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Row_Label)) %>%  # get rid of anything else
  select(spec, Row_Label, everything(), -VarPop) %>%
  rename(Species = spec)
# Identify present size columns
ordered_size_cols <- length_levels[length_levels %in% names(restrict_aoc_final)]

# Reorder columns in final table (Species + Row_Label + sizes)
restrict_aoc_final <- restrict_aoc_final %>%
  select(Species, Row_Label, all_of(ordered_size_cols))

# Create table in Reactable
# Set up contaminant colors
contaminant_colors <- c(
  "PFAS" = "#e6194B",
  "Mercury" = "#f9c74f",
  "PCB" = "#4363d8"
)

# Identify size columns
size_cols <- names(restrict_aoc_final)[!(names(restrict_aoc_final) %in% c("Species", "Row_Label"))]

# Build columns_list dynamically
columns_list <- list()

for (col_name in size_cols) {
  columns_list[[col_name]] <- colDef(
    name = col_name,
    html = TRUE,
    
    # Display target advisory with median below it
    cell = function(value, index) {
      row <- restrict_aoc_final[index, ]
      row_type <- row$Row_Label
      
      # Show contaminant dots
      if (row_type == "Adv cause" && !is.na(value) && value != "") {
        causes <- trimws(unlist(strsplit(value, ",")))
        dots <- lapply(causes, function(cause) {
          color <- contaminant_colors[[cause]]
          if (is.null(color)) return("")
          paste0(
            "<span style='display:inline-block; width:8px; height:8px; margin: 1px; border-radius:50%; background:", color, ";'></span>"
          )
        })
        return(HTML(paste(dots, collapse = "")))
      }
      
      # Display value directly (General, Sensitive, Ref median, fallback)
      if (!is.na(value)) return(value)
      return("")
    }
    ,
    
    # Color based on comparison to reference median
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
      
      # Only apply color to advisory cells
      if (row_type %in% c("General", "Sensitive") && !is.na(value)) {
        val <- suppressWarnings(as.numeric(value))
        ref_index <- index + 2
        
        if (ref_index <= nrow(restrict_aoc_final)) {
          ref_row <- restrict_aoc_final[ref_index, ]
          
          if (ref_row$Row_Label == "Ref median") {
            ref_val_raw <- ref_row[[col_name]]
            ref_val <- suppressWarnings(as.numeric(ref_val_raw))
            
            if (!is.na(val) && !is.na(ref_val)) {
              if (val < ref_val) {
                base$background <- "#ff4d4d"
              } else {
                base$background <- "#66cc66"
              }
            }
          }
        }
      }
      
      # Grey background for NA cells
      if ((is.na(value) || value == "") && row_type %in% c("General", "Sensitive")) {
        base$background <- "#eef2f7"
      }
      
      return(base)
    }
    
  )
}

# Species column: hide repeated labels
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

# Population column
columns_list$Row_Label <- colDef(
  name = "Population",
  minWidth = 120,
  style = function(value) {
    if (value == "Adv cause") {
      list(fontSize = "10px", fontStyle = "italic")
    } else if (value == "Ref median") {
      list(fontSize = "11px", color = "#555")
    } else {
      list(fontSize = "13px", fontWeight = "bold")
    }
  },
  headerStyle = list(whiteSpace = "nowrap")
)

# Row spacing and species group borders
rowStyle_fn <- function(index) {
  row <- restrict_aoc_final[index, ]
  prev_row <- if (index > 1) restrict_aoc_final[index - 1, ] else NULL
  next_row <- if (index < nrow(restrict_aoc_final)) restrict_aoc_final[index + 1, ] else NULL
  
  style <- list(height = "32px")
  
  # Extra spacing below Adv cause rows
  if (row$Row_Label == "Adv cause" &&
      !is.null(next_row) && next_row$Row_Label %in% c("Ref median", "Sensitive")) {
    style$paddingBottom <- "10px"
  }
  
  # Species group border
  if (is.null(prev_row) || prev_row$Species != row$Species) {
    style$borderTop <- "2px solid #666"
  }
  if (is.null(next_row) || next_row$Species != row$Species) {
    style$borderBottom <- "2px solid #666"
  }
  
  return(style)
}

# Final table
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



###------------------------------------------
t2_full_sites = c("St. Lawrence River (Lake St. Francis)",
  "St. Lawrence River (Lake Ontario to Cornwall)",
                    # Superior
                    "Black Bay area (Lake Superior)",
                    "Lake Superior (Eastern Basin)",
                    "Lake Superior (Western Basin)",
                    # Huron
                    "North Channel (Lake Huron)",
                    "Georgian Bay (Lake Huron)",
                    "Lake Huron (Main Basin)",
                    # Ontario
                    "Lake Ontario (Western Basin)",
                    "Whitby Harbour (Lake Ontario)",
                    "Frenchman Bay (Lake Ontario)",
                    # Erie
                    "Lake Erie",
                    "Rondeau Bay (Lake Erie)",
                    "Lake St. Clair",
                    ## Inlets
                    ## Ontario
                    "West Lake",
                    "East Lake",
                    "Consecon Lake",
                    "McLaughlin Bay (Lake Ontario)",
                    "Oshawa Harbour (Lake Ontario)",
                    "Whitby Harbour (Lake Ontario)",
                    "Frenchman Bay (Lake Ontario)",
                    "Jordan Harbour (Lake Ontario)",
                    ## Erie
                    "Wheatley Harbour (Lake Erie)",
                    ## Huron
                    "Collingwood Harbour (Georgian Bay - Lake Huron)",
                    "Severn Sound (Georgian Bay - Lake Huron)",  # Former AOC
                    ## Superior
                    "Nipigon Bay (Lake Superior)",
                    "Jackfish Bay Lake Superior" # AOC in recovery
)

t2_ontario_sites = c("St. Lawrence River (Lake St. Francis)",
                     "St. Lawrence River (Lake Ontario to Cornwall)",
                     "Lake Ontario (Western Basin)",
                     "Whitby Harbour (Lake Ontario)",
                     "Frenchman Bay (Lake Ontario)",
                     "West Lake",
                     "East Lake",
                     "Consecon Lake",
                     "McLaughlin Bay (Lake Ontario)",
                     "Oshawa Harbour (Lake Ontario)",
                     "Whitby Harbour (Lake Ontario)",
                     "Frenchman Bay (Lake Ontario)",
                     "Jordan Harbour (Lake Ontario)"
)


lsf_spec = dat %>%
  filter(loc == "St. Lawrence River (Lake St. Francis)") %>% # Applies cases across the same species found in LSF
  distinct(spec)

dat_refs = dat %>%
  filter(loc %in% t2_sites)
  
dat_refs=dat_refs %>%
  semi_join(lsf_spec, by = "spec") 

dat_aoc_and_refs = dat_refs %>%
  mutate(ref = (ifelse(loc == "St. Lawrence River (Lake St. Francis)", "Target", "Reference")))

# Clean up and reduce to necessary fields
clean_data <- dat_aoc_and_refs %>%
  mutate(
    spec = trimws(spec),
    pop_name = trimws(pop_name),
    length_name = trimws(length_name)
  ) %>%
  select(
    Species = spec,
    Population = pop_name,
    Size = length_name,
    Advisory = adv_level,
    Contaminants = adv_cause,
    ref
  )

# Pivot target site (AOC) rows to wide format
aoc_wide <- clean_data %>%
  filter(ref == "Target") %>%
  select(Species, Population, Size, Advisory) %>%
  pivot_wider(names_from = Size, values_from = Advisory) %>%
  arrange(Species, Population)

# Reactable 
# Step 1: Filter to only the Target site rows
library(dplyr)
library(reactable)
library(htmltools)


# Step 1: Filter to only the Target site rows
aoc_rows <- dat_aoc_and_refs %>%
  filter(ref == "Target") %>%
  rename(
    Species = spec,
    Population = pop_name,
    Size = length_name,
    Advisory = adv_level,
    Contaminants = adv_cause
  )

# Step 2: Define the table
reactable(
  aoc_rows,
  groupBy = c("Species", "Population"),
  searchable = TRUE,
  filterable = TRUE,
  defaultPageSize = 15,
  columns = list(
    Species = colDef(name = "Species"),
    Size = colDef(name = "Size"),
    Advisory = colDef(
      name = "Advisory (Target)",
      style = function(value, index) {
        row_data <- aoc_rows[index, ]
        
        # Find matching reference advisories
        ref_vals <- dat_aoc_and_refs %>%
          filter(
            ref == "Reference",
            spec == row_data$Species,
            length_name == row_data$Size,
            pop_name == row_data$Population
          ) %>%
          pull(adv_level)
        
        median_val <- median(ref_vals, na.rm = TRUE)
        
        if (!is.na(value) && !is.na(median_val)) {
          if (value < median_val) {
            list(background = "#ff4d4d", fontWeight = "bold")
          } else {
            list(background = "#66cc66", fontWeight = "bold")
          }
        } else {
          list(background = "#eef2f7")
        }
      }
    )
    ,
  details = function(index) {
    row <- aoc_rows[index, ]
    
    # Pull matching reference site rows
    ref_subset <- dat_aoc_and_refs %>%
      filter(
        ref == "Reference",
        spec == row$Species,
        length_name == row$Size,
        pop_name == row$Population
      ) %>%
      select(Site = loc, Advisory = adv_level, Contaminants = adv_cause)
    
    # Add reference median row at top
    median_val <- median(ref_subset$Advisory, na.rm = TRUE)
    ref_subset <- bind_rows(
      tibble(Site = "Median (Ref)", Advisory = median_val, Contaminants = NA),
      ref_subset
    )
    
    reactable(ref_subset, compact = TRUE, bordered = TRUE, defaultPageSize = 5)
  }
)

