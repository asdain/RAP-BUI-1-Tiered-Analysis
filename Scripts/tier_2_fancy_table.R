# =======================================
# Fish Advisory Comparison Table Generator
# (Semi-wide format for reactable aggregate)
# =======================================

library(dplyr)
library(tidyr)
library(reactable)
library(scales)

# Try one---------------

get_reference_site_ids <- function(cons_data, reference_site_names) {
  cons_data %>%
    filter(guide_locname_eng %in% reference_site_names) %>%
    distinct(guide_locname_eng, waterbody_group, guide_locdesc) %>%
    arrange(guide_locname_eng)
}

get_reference_site_ids(cons_data, t2_ontario_sites)

# Reference sites list (same as your code)
t2_full_sites <- c(
  "St. Lawrence River (Lake Ontario to Cornwall)", "Black Bay area (Lake Superior)", "Lake Superior (Eastern Basin)",
  "Lake Superior (Western Basin)", "North Channel (Lake Huron)", "Georgian Bay (Lake Huron)", "Lake Huron (Main Basin)",
  "Lake Ontario (Western Basin)", "Whitby Harbour (Lake Ontario)", "Frenchman Bay (Lake Ontario)", "Lake Erie",
  "Rondeau Bay (Lake Erie)", "Lake St. Clair", "West Lake", "East Lake", "Consecon Lake",
  "McLaughlin Bay (Lake Ontario)", "Oshawa Harbour (Lake Ontario)", "Jordan Harbour (Lake Ontario)",
  "Wheatley Harbour (Lake Erie)", "Collingwood Harbour (Georgian Bay - Lake Huron)",
  "Severn Sound (Georgian Bay - Lake Huron)", "Nipigon Bay (Lake Superior)")

t2_ontario_sites <- c(
  "St. Lawrence River (Lake Ontario to Cornwall)",
  "Lake Ontario (Western Basin)", "Whitby Harbour (Lake Ontario)",
  "Frenchman Bay (Lake Ontario)", "West Lake", "East Lake",
  "Consecon Lake", "McLaughlin Bay (Lake Ontario)",
  "Oshawa Harbour (Lake Ontario)", "Jordan Harbour (Lake Ontario)")

# Reference sites list (example placeholder)
reference_waterbody_groups_ontario <- c("44007727", 
                                     "43557712",
                                     "43497905",
                                     "43117922",
                                     "43397900",
                                     "43517847",
                                     "43527851",
                                     "44397535",
                                     "43557717",
                                     "43527856"
                                     )  # <-- you will add your IDs here
reference_waterbody_groups_full = c()

slr_waterbody_group = "45087425"    # <-- and here for AOC site(s)

length_levels <- c("15-20cm", "20-25cm", "25-30cm", "30-35cm", "35-40cm", "40-45cm",
                   "45-50cm", "50-55cm", "55-60cm", "60-65cm", "65-70cm", "70-75cm", ">75cm")

# =======================================
# Helper function to get waterbody_group IDs for site names
# =======================================
get_waterbody_groups <- function(cons_data, site_names) {
  cons_data %>%
    filter(guide_locname_eng %in% site_names) %>%
    distinct(guide_locname_eng, waterbody_group) %>%
    arrange(guide_locname_eng)
}

get_waterbody_groups(cons_data, t2_ontario_sites)

## =======================================
# Main function for comparison table
# =======================================
# =======================================
# Main function for comparison table
# =======================================
make_comparison_table <- function(cons_data, reference_groups, aoc_groups, title) {
  
  # Get AOC species
  aoc_species <- cons_data %>%
    filter(waterbody_group %in% aoc_groups,
           population_type_desc %in% c("General", "Sensitive")) %>%
    pull(specname) %>%
    unique()
  
  # Filter data by waterbody_group and species
  filtered_data <- cons_data %>%
    filter(waterbody_group %in% c(reference_groups, aoc_groups),
           population_type_desc %in% c("General", "Sensitive"),
           specname %in% aoc_species) %>%
    mutate(Size = factor(length_category_label, levels = length_levels, ordered = TRUE),
           site_type = if_else(waterbody_group %in% aoc_groups, "AOC", "Reference"))
  
  # Deduplicate just in case
  filtered_data <- filtered_data %>%
    group_by(Species = specname, Site = guide_locname_eng, waterbody_group, site_type, Population = population_type_desc, Size) %>%
    summarise(advisory = max(adv_level, na.rm = TRUE), .groups = "drop")
  
  # Pivot to semi-wide format
  wide_data <- filtered_data %>%
    pivot_wider(names_from = Size, values_from = advisory)
  
  size_cols <- length_levels[length_levels %in% names(wide_data)]
  
  # Define columns list with median aggregate function for reference rows
  columns_list <- list(
    Species = colDef(minWidth = 150),
    Site = colDef(minWidth = 150),
    site_type = colDef(),
    Population = colDef()
  )
  
  for (col in size_cols) {
    columns_list[[col]] <- colDef(
      name = col,
      align = "center",
      aggregate = "median",
      style = function(value) {
        if (is.null(value) || is.na(value)) return(list(background = "#eeeeee", color = "#000000"))
        val <- as.numeric(value)
        if (val == 0) color <- "#d80032"
        else if (val <= 4) color <- "#f9c74f"
        else color <- "#2b2d42"
        list(background = color, color = "#ffffff")
      }
    )
  }
  
  # Build reactable
  reactable(
    wide_data,
    groupBy = c("Species", "Population", "site_type"),
    columns = columns_list,
    bordered = TRUE,
    striped = TRUE,
    highlight = TRUE,
    pagination = FALSE,
    defaultExpanded = TRUE,
    style = list(fontFamily = "sans-serif", fontSize = "13px")
  )
}
# Example usage (replace cons_data with your dataset)
 lake_st_francis_comparison <- make_comparison_table(
  cons_data, reference_groups = reference_waterbody_groups_ontario,
  aoc_groups = slr_waterbody_group,
   title = "Lake St. Francis"
 )

 lake_st_francis_comparison

# Try 2 -----------------
 # =======================================
 # Fish Advisory Comparison Table Generator
 # Expandable Reference Medians version (with grouped AOC + Median rows)
 # =======================================
 
 library(dplyr)
 library(tidyr)
 library(reactable)
 library(scales)
 
 # Reference sites list (example placeholder)
 reference_waterbody_groups <- c("44007727", 
                                 "43557712",
                                 "43497905",
                                 "43117922",
                                 "43397900",
                                 "43517847",
                                 "43527851",
                                 "44397535",
                                 "43557717",
                                 "43527856"
 )
 aoc_waterbody_groups = "45087425"    # <-- and here for AOC site(s)
 
 length_levels <- c("15-20cm", "20-25cm", "25-30cm", "30-35cm", "35-40cm", "40-45cm",
                    "45-50cm", "50-55cm", "55-60cm", "60-65cm", "65-70cm", "70-75cm", ">75cm")
 
 # =======================================
 # Helper function to get waterbody_group IDs for site names
 # =======================================
 get_waterbody_groups <- function(cons_data, site_names) {
   cons_data %>%
     filter(guide_locname_eng %in% site_names) %>%
     distinct(guide_locname_eng, waterbody_group) %>%
     arrange(guide_locname_eng)
 }
 
 # =======================================
 # Main function for comparison table with medians + expandable detail rows
 # =======================================
 make_comparison_table <- function(cons_data, reference_groups, aoc_groups, title) {
   
   # Get AOC species
   aoc_species <- cons_data %>%
     filter(waterbody_group %in% aoc_groups,
            population_type_desc %in% c("General", "Sensitive")) %>%
     pull(specname) %>%
     unique()
   
   # Filter relevant data
   filtered_data <- cons_data %>%
     filter(waterbody_group %in% c(reference_groups, aoc_groups),
            population_type_desc %in% c("General", "Sensitive"),
            specname %in% aoc_species) %>%
     mutate(Size = factor(length_category_label, levels = length_levels, ordered = TRUE),
            site_type = if_else(waterbody_group %in% aoc_groups, "AOC", "Reference"))
   
   # Deduplicate and reshape
   base_data <- filtered_data %>%
     group_by(Species = specname, Site = guide_locname_eng, site_type, Population = population_type_desc, Size) %>%
     summarise(advisory = max(adv_level, na.rm = TRUE), .groups = "drop")
   
   # AOC rows
   aoc_data <- base_data %>%
     filter(site_type == "AOC") %>%
     pivot_wider(names_from = Size, values_from = advisory)
   
   # Reference site rows
   ref_data <- base_data %>%
     filter(site_type == "Reference") %>%
     pivot_wider(names_from = Size, values_from = advisory)
   
   # Median rows
   ref_medians <- ref_data %>%
     group_by(Species, Population) %>%
     summarise(across(all_of(length_levels), ~median(.x, na.rm = TRUE)), .groups = "drop") %>%
     mutate(Site = "Reference Median", site_type = "Reference")
   
   # Bind AOC + Median in correct order
   aoc_median_combo <- aoc_data %>%
     bind_rows(ref_medians) %>%
     mutate(group_key = paste(Species, Population))
   
   # Final display table: keep grouping logic to sort properly
   display_data <- aoc_median_combo %>%
     arrange(Species, Population, Site != "Reference Median") %>%
     mutate(
       id = row_number(),
       Species = ifelse(duplicated(paste(Species, Population)), "", Species),
       Population = ifelse(duplicated(paste(Species, Population)), "", Population)
     )
   
   size_cols <- length_levels[length_levels %in% names(display_data)]
   
   # Column definitions
   columns_list <- list(
     Species = colDef(minWidth = 150),
     Population = colDef(minWidth = 100),
     Site = colDef(minWidth = 150),
     site_type = colDef(show = FALSE),
     id = colDef(show = FALSE)
   )
   
   for (col in size_cols) {
     columns_list[[col]] <- colDef(
       name = col,
       align = "center",
       style = function(value) {
         if (is.null(value) || is.na(value)) return(list(background = "#eeeeee", color = "#000000"))
         val <- as.numeric(value)
         if (val == 0) color <- "#d80032"
         else if (val <= 4) color <- "#f9c74f"
         else color <- "#2b2d42"
         list(background = color, color = "#ffffff")
       }
     )
   }
   
   # Build main table with expandable median rows
   reactable(
     display_data,
     columns = columns_list,
     defaultExpanded = FALSE,
     details = function(index) {
       row <- display_data[index, ]
       if (row$Site == "Reference Median") {
         ref_rows <- ref_data %>%
           filter(Species == row$Species, Population == row$Population)
         reactable(ref_rows[, c("Site", size_cols)], compact = TRUE, bordered = TRUE, pagination = FALSE)
       } else {
         NULL
       }
     },
     bordered = TRUE,
     striped = TRUE,
     highlight = TRUE,
     pagination = FALSE,
     style = list(fontFamily = "sans-serif", fontSize = "13px")
   )
 }
 
 # Example usage (replace cons_data with your dataset)
 lake_st_francis_comparison <- make_comparison_table(
   cons_data, reference_groups = reference_waterbody_groups,
   aoc_groups = aoc_waterbody_groups,
   title = "Lake St. Francis"
 )
 
 lake_st_francis_comparison
 
 
# Try manual --------------
 
 # Reference sites list (example placeholder)
 reference_waterbody_groups_ontario <- c("44007727", 
                                         "43557712",
                                         "43497905",
                                         "43117922",
                                         "43397900",
                                         "43517847",
                                         "43527851",
                                         "44397535",
                                         "43557717",
                                         "43527856"
 )  # <-- you will add your IDs here
 reference_waterbody_groups_full = c()
 
 slr_waterbody_group = "45087425"    # <-- and here for AOC site(s)
 
 length_levels <- c("15-20cm", "20-25cm", "25-30cm", "30-35cm", "35-40cm", "40-45cm",
                    "45-50cm", "50-55cm", "55-60cm", "60-65cm", "65-70cm", "70-75cm", ">75cm")
 
 # =======================================
 # Helper function to get waterbody_group IDs for site names
 # =======================================
 get_waterbody_groups <- function(cons_data, site_names) {
   cons_data %>%
     filter(guide_locname_eng %in% site_names) %>%
     distinct(guide_locname_eng, waterbody_group) %>%
     arrange(guide_locname_eng)
 }
 
 get_waterbody_groups(cons_data, t2_ontario_sites)
 
 ## =======================================
 # Main function for comparison table
 # =======================================
 # =======================================
 # Main function for comparison table
 # =======================================
 make_comparison_table <- function(cons_data, reference_groups, aoc_groups, title) {
   
   # Get AOC species
   aoc_species <- cons_data %>%
     filter(waterbody_group %in% aoc_groups,
            population_type_desc %in% c("General", "Sensitive")) %>%
     pull(specname) %>%
     unique()
   
   # Filter data by waterbody_group and species
   filtered_data <- cons_data %>%
     filter(waterbody_group %in% c(reference_groups, aoc_groups),
            population_type_desc %in% c("General", "Sensitive"),
            specname %in% aoc_species) %>%
     mutate(Size = factor(length_category_label, levels = length_levels, ordered = TRUE),
            site_type = if_else(waterbody_group %in% aoc_groups, "AOC", "Reference"))
   
   # Deduplicate just in case
   filtered_data <- filtered_data %>%
     group_by(Species = specname, Site = guide_locname_eng, waterbody_group, site_type, Population = population_type_desc, Size) %>%
     summarise(advisory = max(adv_level, na.rm = TRUE), .groups = "drop")
   
   # Pivot to semi-wide format
   wide_data <- filtered_data %>%
     pivot_wider(names_from = Size, values_from = advisory)
   
   size_cols <- length_levels[length_levels %in% names(wide_data)]
   
   # Define columns list with median aggregate function for reference rows
   columns_list <- list(
     Species = colDef(minWidth = 150, aggregate = "unique"),
     Site = colDef(minWidth = 150),
     site_type = colDef(aggregate = "unique"),
     Population = colDef(aggregate = "unique")
   )
   
   for (col in size_cols) {
     columns_list[[col]] <- colDef(
       name = col,
       align = "center",
       aggregate = "median",
       style = function(value) {
         if (is.null(value) || is.na(value)) return(list(background = "#eeeeee", color = "#000000"))
         val <- as.numeric(value)
         if (val == 0) color <- "#d80032"
         else if (val <= 4) color <- "#f9c74f"
         else color <- "#2b2d42"
         list(background = color, color = "#ffffff")
       }
     )
   }
   
   # Build reactable
   reactable(
     wide_data,
     groupBy = c("Species", "Population","site_type"),
     columns = columns_list,
     bordered = TRUE,
     striped = TRUE,
     highlight = TRUE,
     pagination = FALSE,
     defaultExpanded = TRUE,
     style = list(fontFamily = "sans-serif", fontSize = "13px")
   )
 }
 # Example usage (replace cons_data with your dataset)
 lake_st_francis_comparison <- make_comparison_table(
   cons_data, reference_groups = reference_waterbody_groups_ontario,
   aoc_groups = slr_waterbody_group,
   title = "Lake St. Francis"
 )
 
 lake_st_francis_comparison
 
 
 ## New try ----------------------------
 # =======================================
 # Fish Advisory Comparison Table (Word-style layout)
 # =======================================
 
 library(dplyr)
 library(tidyr)
 library(reactable)
 library(scales)
 
 reference_waterbody_groups <- c("44007727", "43557712", "43497905", "43117922", "43397900",
                                 "43517847", "43527851", "44397535", "43557717", "43527856")
 aoc_waterbody_groups <- "45087425"
 
 length_levels <- c("15-20cm", "20-25cm", "25-30cm", "30-35cm", "35-40cm", "40-45cm",
                    "45-50cm", "50-55cm", "55-60cm", "60-65cm", "65-70cm", "70-75cm", ">75cm")
 
 make_flat_table <- function(cons_data, reference_groups, aoc_groups) {
   # Get species found in AOC
   aoc_species <- cons_data %>%
     filter(waterbody_group %in% aoc_groups,
            population_type_desc %in% c("General", "Sensitive")) %>%
     pull(specname) %>%
     unique()
   
   # Filter and tag
   filtered <- cons_data %>%
     filter(waterbody_group %in% c(reference_groups, aoc_groups),
            population_type_desc %in% c("General", "Sensitive"),
            specname %in% aoc_species) %>%
     mutate(
       Size = factor(length_category_label, levels = length_levels, ordered = TRUE),
       Site = guide_locname_eng,
       site_type = if_else(waterbody_group %in% aoc_groups, "AOC", "Reference"),
       Species = specname,
       Population = population_type_desc
     )
   
   # Base long-format
   long_data <- filtered %>%
     select(Species, Population, site_type, Site, Size, advisory = adv_level)
   
   # Calculate medians per species/pop/size
   medians <- long_data %>%
     filter(site_type == "Reference") %>%
     group_by(Species, Population, Size) %>%
     summarise(advisory = median(advisory, na.rm = TRUE), .groups = "drop") %>%
     mutate(Site = "Median", site_type = "Reference")
   
   # Wide format for reference sites
   reference_wide <- long_data %>%
     filter(site_type == "Reference") %>%
     pivot_wider(names_from = Size, values_from = advisory)
   
   # Wide format for AOC
   aoc_wide <- long_data %>%
     filter(site_type == "AOC") %>%
     pivot_wider(names_from = Size, values_from = advisory)
   
   # Wide format for median (summary row)
   median_wide <- medians %>%
     pivot_wider(names_from = Size, values_from = advisory)
   
   # Build nested reference section
   build_reference_group <- function(spec, pop) {
     ref_sites <- reference_wide %>%
       filter(Species == spec, Population == pop)
     med_row <- median_wide %>%
       filter(Species == spec, Population == pop)
     
     reactable(
       bind_rows(med_row, ref_sites),
       columns = columns,
       defaultExpanded = FALSE,
       compact = TRUE,
       bordered = TRUE,
       showPagination = FALSE,
       outlined = TRUE,
       style = list(fontSize = "12px")
     )
   }
   
   # Combine AOC and median rows with expandable reference rows
   final <- bind_rows(aoc_wide, median_wide) %>%
     arrange(Species, Population, Site)
   
   size_cols <- length_levels[length_levels %in% names(final)]
   
   columns <- list(
     Species = colDef(name = "Species", minWidth = 150),
     Population = colDef(name = "Population", minWidth = 120),
     site_type = colDef(show = FALSE),
     Site = colDef(name = "Waterbody", minWidth = 200, cell = function(value) {
       if (value == "Median") "Median ↓" else value
     })
   )
   for (col in size_cols) {
     columns[[col]] <- colDef(name = col)
   }
   
   reactable(
     final,
     columns = columns,
     groupBy = c("Species", "Population"),
     details = function(index) {
       row <- final[index, ]
       if (row$Site == "Median") {
         build_reference_group(row$Species, row$Population)
       } else {
         NULL
       }
     },
     defaultExpanded = TRUE,
     highlight = TRUE,
     bordered = TRUE,
     striped = TRUE,
     compact = TRUE,
     pagination = FALSE,
     style = list(fontFamily = "sans-serif", fontSize = "13px")
   )
 }
 
 
 # Example call:
 make_flat_table(cons_data, reference_waterbody_groups, aoc_waterbody_groups)
 
 

## Retrying aggregate ----------------
 # =======================================
 # Fish Advisory Comparison Table (Flat layout with aggregate Median row)
 # =======================================
 
 make_flat_table <- function(cons_data, reference_groups, aoc_groups) {
   # Get species found in AOC
   aoc_species <- cons_data %>%
     filter(waterbody_group %in% aoc_groups,
            population_type_desc %in% c("General", "Sensitive")) %>%
     pull(specname) %>%
     unique()
   
   # Filter and tag
   filtered <- cons_data %>%
     filter(waterbody_group %in% c(reference_groups, aoc_groups),
            population_type_desc %in% c("General", "Sensitive"),
            specname %in% aoc_species) %>%
     mutate(
       Size = factor(length_category_label, levels = length_levels, ordered = TRUE),
       Site = guide_locname_eng,
       site_type = if_else(waterbody_group %in% aoc_groups, "AOC", "Reference"),
       Species = specname,
       Population = population_type_desc
     )
   
   # Base wide-format
   wide_data <- filtered %>%
     select(Species, Population, site_type, Site, Size, advisory = adv_level) %>%
     pivot_wider(names_from = Size, values_from = advisory)
   
   size_cols <- length_levels[length_levels %in% names(wide_data)]
   
   # Define column list with aggregate for Reference
   columns <- list(
     Species = colDef(name = "Species", minWidth = 150),
     Population = colDef(name = "Population", minWidth = 120),
     site_type = colDef(name = "Site Type", minWidth = 100),
     Site = colDef(name = "Waterbody", minWidth = 200)
   )
   
   for (col in size_cols) {
     columns[[col]] <- colDef(
       name = col,
       aggregate = JS(sprintf(
         "function(values, rows) {
          let ref = rows.filter(r => r['site_type'] === 'Reference')
                        .map(r => r['%s'])
                        .filter(v => v != null);
          ref.sort((a, b) => a - b);
          let mid = Math.floor(ref.length / 2);
          if (ref.length === 0) return null;
          return ref.length %% 2 === 0 ? (ref[mid - 1] + ref[mid]) / 2 : ref[mid];
        }",
         col
       ))
     )
   }
   
   reactable(
     wide_data,
     columns = columns,
     groupBy = c("Species", "Population", "site_type"),
     defaultExpanded = TRUE,
     highlight = TRUE,
     bordered = TRUE,
     striped = TRUE,
     compact = TRUE,
     pagination = FALSE,
     style = list(fontFamily = "sans-serif", fontSize = "13px")
   )
 }  
 
 # Example usage:
 make_flat_table(cons_data, reference_waterbody_groups, aoc_waterbody_groups)

 
 
## Retrying details ------------
 # =======================================
 # Fish Advisory Comparison Table (Expandable with Reference Medians)
 # =======================================
 
 
 make_comparison_table <- function(cons_data, reference_groups, aoc_groups) {
   aoc_combinations <- cons_data %>%
     filter(waterbody_group %in% aoc_groups,
            population_type_desc %in% c("General", "Sensitive")) %>%
     distinct(Species = specname, Size = length_category_label, Population = population_type_desc) %>%
     mutate(Size = factor(Size, levels = length_levels, ordered = TRUE))
   
   filtered_data <- cons_data %>%
     filter(waterbody_group %in% c(reference_groups, aoc_groups),
            population_type_desc %in% c("General", "Sensitive")) %>%
     mutate(Species = specname,
            Size = factor(length_category_label, levels = length_levels, ordered = TRUE),
            Population = population_type_desc,
            site_type = if_else(waterbody_group %in% aoc_groups, "AOC", "Reference")) %>%
     semi_join(aoc_combinations, by = c("Species", "Size", "Population"))
   
   base_data <- filtered_data %>%
     group_by(Species, Site = guide_locname_eng, site_type, Population, Size) %>%
     summarise(advisory = max(adv_level, na.rm = TRUE), .groups = "drop")
   
   aoc_data <- base_data %>%
     filter(site_type == "AOC") %>%
     pivot_wider(names_from = Size, values_from = advisory) %>%
     mutate(site_order = 1)
   
   ref_long <- base_data %>%
     filter(site_type == "Reference")
   
   ref_medians <- ref_long %>%
     group_by(Species, Population, Size) %>%
     summarise(Median = median(advisory, na.rm = TRUE), .groups = "drop") %>%
     pivot_wider(names_from = Size, values_from = Median) %>%
     mutate(Site = "Reference Median", site_type = "Reference", site_order = 2)
   
   ref_data <- ref_long %>%
     pivot_wider(names_from = Size, values_from = advisory)
   
   aoc_median_combo <- bind_rows(aoc_data, ref_medians)
   
   display_data <- aoc_median_combo %>%
     arrange(Species, Population, site_order) %>%
     mutate(
       id = row_number(),
       Species_display = ifelse(duplicated(Species), "", Species),
       Population_display = ifelse(duplicated(paste(Species, Population)), "", Population)
     ) %>%
     select(Species_display, Population_display, Site, site_type, id, Species, Population, site_order, all_of(length_levels))
   
   size_cols <- length_levels[length_levels %in% names(display_data)]
   
   comparison_lookup <- ref_medians %>%
     select(Species, Population, all_of(size_cols)) %>%
     pivot_longer(cols = all_of(size_cols), names_to = "Size", values_to = "Median")
   
   columns_list <- list(
     Species_display = colDef(name = "Species", minWidth = 150),
     Population_display = colDef(name = "Population", minWidth = 100),
     Site = colDef(minWidth = 150),
     site_type = colDef(show = FALSE),
     id = colDef(show = FALSE),
     Species = colDef(show = FALSE),
     Population = colDef(show = FALSE),
     site_order = colDef(show = FALSE)
   )
   
   for (col in size_cols) {
     columns_list[[col]] <- colDef(
       name = col,
       align = "center",
       style = function(value, index) {
         if (is.null(value) || is.na(value)) return(list(background = "#eeeeee", color = "#000000"))
         row <- display_data[index + 1, ]
         med_val <- comparison_lookup %>%
           filter(Species == row$Species, Population == row$Population, Size == col) %>%
           pull(Median)
         if (length(med_val) == 0 || is.na(med_val)) return(list(background = "#eeeeee", color = "#000000"))
         if (row$Site == "Reference Median" || row$site_type == "AOC") {
           if (as.numeric(value) < as.numeric(med_val)) {
             return(list(background = "#d80032", color = "#ffffff"))
           } else {
             return(list(background = "#4CAF50", color = "#ffffff"))
           }
         } else {
           return(NULL)
         }
       }
     )
   }
   
   reactable(
     display_data,
     columns = columns_list,
     defaultExpanded = FALSE,
     details = function(index) {
       row <- display_data[index, ]
       if (row$Site == "Reference Median") {
         ref_rows <- ref_data %>%
           filter(Species == row$Species, Population == row$Population) %>%
           select(Site, all_of(size_cols))
         reactable(ref_rows, compact = TRUE, bordered = TRUE, pagination = FALSE)
       } else {
         NULL
       }
     },
     bordered = TRUE,
     striped = TRUE,
     highlight = TRUE,
     pagination = FALSE,
     style = list(fontFamily = "sans-serif", fontSize = "13px")
   )
 }
 

 
 
 # Example usage:
 make_comparison_table(cons_data, reference_waterbody_groups, aoc_waterbody_groups)
 
 
# Checkpoint - colouring logic --------------------
 make_comparison_table <- function(cons_data, reference_groups, aoc_groups) {
   aoc_combinations <- cons_data %>%
     filter(waterbody_group %in% aoc_groups,
            population_type_desc %in% c("General", "Sensitive")) %>%
     distinct(Species = specname, Size = length_category_label, Population = population_type_desc) %>%
     mutate(Size = factor(Size, levels = length_levels, ordered = TRUE))
   
   filtered_data <- cons_data %>%
     filter(waterbody_group %in% c(reference_groups, aoc_groups),
            population_type_desc %in% c("General", "Sensitive")) %>%
     mutate(Species = specname,
            Size = factor(length_category_label, levels = length_levels, ordered = TRUE),
            Population = population_type_desc,
            site_type = if_else(waterbody_group %in% aoc_groups, "AOC", "Reference")) %>%
     semi_join(aoc_combinations, by = c("Species", "Size", "Population"))
   
   base_data <- filtered_data %>%
     group_by(Species, Site = guide_locname_eng, site_type, Population, Size) %>%
     summarise(advisory = max(adv_level, na.rm = TRUE), .groups = "drop")
   
   ref_long <- base_data %>%
     filter(site_type == "Reference")
   
   ref_medians <- ref_long %>%
     group_by(Species, Population, Size) %>%
     summarise(Median = median(advisory, na.rm = TRUE), .groups = "drop") %>%
     pivot_wider(names_from = Size, values_from = Median) %>%
     mutate(Site = "Reference Median", site_type = "Reference", site_order = 2)
   
   ref_data <- ref_long %>%
     pivot_wider(names_from = Size, values_from = advisory)
   
   ref_medians_long <- ref_medians %>%
     pivot_longer(cols = all_of(length_levels[length_levels %in% names(ref_medians)]),
                  names_to = "Size", values_to = "Median") %>%
     select(Species, Population, Size, Median)
   
   base_with_comparison <- base_data %>%
     left_join(ref_medians_long, by = c("Species", "Population", "Size")) %>%
     mutate(
       comparison_pass = case_when(
         site_type == "AOC" & (is.na(advisory) | is.na(Median)) ~ NA,
         site_type == "AOC" & advisory >= Median ~ TRUE,
         site_type == "AOC" & advisory < Median ~ FALSE,
         TRUE ~ NA
       )
     )
   
   aoc_data <- base_data %>%
     filter(site_type == "AOC") %>%
     pivot_wider(names_from = Size, values_from = advisory) %>%
     mutate(site_order = 1)
   
   aoc_median_combo <- bind_rows(aoc_data, ref_medians)
   
   display_data <- aoc_median_combo %>%
     arrange(Species, Population, site_order) %>%
     mutate(
       id = row_number(),
       Species_display = ifelse(duplicated(Species), "", Species),
       Population_display = ifelse(duplicated(paste(Species, Population)), "", Population)
     ) %>%
     select(Species_display, Population_display, Site, site_type, id, Species, Population, site_order, all_of(length_levels)) %>%
     mutate(across(c(Species, Population, Site), as.character))
   
   flag_table <- base_with_comparison %>%
     filter(site_type == "AOC") %>%
     select(Species, Population, Site, Size, comparison_pass) %>%
     pivot_wider(
       names_from = Size,
       values_from = comparison_pass,
       names_glue = "{Size}_flag"
     ) %>%
     mutate(across(c(Species, Population, Site), as.character))
   
   display_data <- display_data %>%
     left_join(flag_table, by = c("Species", "Population", "Site"))
   
   size_cols <- length_levels[length_levels %in% names(display_data)]
   
   columns_list <- list(
     Species_display = colDef(name = "Species", minWidth = 150),
     Population_display = colDef(name = "Population", minWidth = 100),
     Site = colDef(minWidth = 150),
     site_type = colDef(show = FALSE),
     id = colDef(show = FALSE),
     Species = colDef(show = FALSE),
     Population = colDef(show = FALSE),
     site_order = colDef(show = FALSE)
   )
   
   for (col in size_cols) {
     columns_list[[col]] <- colDef(
       name = col,
       align = "center",
       style = function(value, index) {
         if (is.null(value) || is.na(value)) return(list(background = "#eeeeee", color = "#000000"))
         flag <- display_data[[paste0(col, "_flag")]][index + 1]
         if (length(flag) == 0 || is.na(flag)) {
           return(list(background = "#eeeeee", color = "#000000"))
         } else if (flag) {
           return(list(background = "#4CAF50", color = "#ffffff"))
         } else {
           return(list(background = "#d80032", color = "#ffffff"))
         }
       }
     )
   }
   
   reactable(
     display_data,
     columns = columns_list,
     defaultExpanded = FALSE,
     details = function(index) {
       row <- display_data[index, ]
       if (row$Site == "Reference Median") {
         ref_rows <- ref_data %>%
           filter(Species == row$Species, Population == row$Population) %>%
           select(Site, all_of(size_cols))
         reactable(ref_rows, compact = TRUE, bordered = TRUE, pagination = FALSE)
       } else {
         NULL
       }
     },
     bordered = TRUE,
     striped = TRUE,
     highlight = TRUE,
     pagination = FALSE,
     style = list(fontFamily = "sans-serif", fontSize = "13px")
   )
 }
 
 
 
 
 
 # Example usage:
make_comparison_table(cons_data, reference_waterbody_groups, aoc_waterbody_groups)



## Simplified colouring method -------------
make_comparison_table <- function(cons_data, reference_groups, aoc_groups) {
  aoc_combinations <- cons_data %>%
    filter(waterbody_group %in% aoc_groups,
           population_type_desc %in% c("General", "Sensitive")) %>%
    distinct(Species = specname, Size = length_category_label, Population = population_type_desc) %>%
    mutate(Size = factor(Size, levels = length_levels, ordered = TRUE))
  
  filtered_data <- cons_data %>%
    filter(waterbody_group %in% c(reference_groups, aoc_groups),
           population_type_desc %in% c("General", "Sensitive")) %>%
    mutate(Species = specname,
           Size = factor(length_category_label, levels = length_levels, ordered = TRUE),
           Population = population_type_desc,
           site_type = if_else(waterbody_group %in% aoc_groups, "AOC", "Reference")) %>%
    semi_join(aoc_combinations, by = c("Species", "Size", "Population"))
  
  base_data <- filtered_data %>%
    group_by(Species, Site = guide_locname_eng, site_type, Population, Size) %>%
    summarise(advisory = max(adv_level, na.rm = TRUE), .groups = "drop")
  
  ref_long <- base_data %>%
    filter(site_type == "Reference")
  
  ref_medians <- ref_long %>%
    group_by(Species, Population, Size) %>%
    summarise(Median = median(advisory, na.rm = TRUE), .groups = "drop") %>%
    complete(Species, Population, Size = factor(length_levels, levels = length_levels, ordered = TRUE))
  
  ref_data <- ref_long %>%
    pivot_wider(names_from = Size, values_from = advisory)
  
  ref_medians_wide <- ref_medians %>%
    pivot_wider(names_from = Size, values_from = Median) %>%
    mutate(Site = "Reference Median", site_type = "Reference", site_order = 2)
  
  aoc_data <- base_data %>%
    filter(site_type == "AOC") %>%
    pivot_wider(names_from = Size, values_from = advisory) %>%
    mutate(site_order = 1)
  
  display_data <- bind_rows(aoc_data, ref_medians_wide) %>%
    arrange(Species, Population, site_order) %>%
    mutate(
      id = row_number(),
      Species_display = ifelse(duplicated(Species), "", Species),
      Population_display = ifelse(duplicated(paste(Species, Population)), "", Population)
    ) %>%
    select(Species_display, Population_display, Site, site_type, id, Species, Population, site_order, all_of(length_levels))
  
  size_cols <- length_levels[length_levels %in% names(display_data)]
  
  columns_list <- list(
    Species_display = colDef(name = "Species", minWidth = 150),
    Population_display = colDef(name = "Population", minWidth = 100),
    Site = colDef(minWidth = 150),
    site_type = colDef(show = FALSE),
    id = colDef(show = FALSE),
    Species = colDef(show = FALSE),
    Population = colDef(show = FALSE),
    site_order = colDef(show = FALSE)
  )
  
  for (col in size_cols) {
    columns_list[[col]] <- colDef(
      name = col,
      align = "center",
      style = function(value, index) {
        if (is.null(value) || is.na(value)) {
          return(list(background = "#eeeeee", color = "#000000"))
        }
        
        row <- display_data[index, ]
        if (!is.na(row$site_type) && row$site_type == "AOC") {
          ref_row <- display_data[
            display_data$Species == row$Species &
              display_data$Population == row$Population &
              display_data$Site == "Reference Median"
          , ]
          
          ref_value <- if (nrow(ref_row) == 1) ref_row[[col]] else NA
          
          if (is.na(ref_value)) {
            return(list(background = "#eeeeee", color = "#000000"))
          } else if (value >= ref_value) {
            return(list(background = "#4CAF50", color = "#ffffff"))
          } else {
            return(list(background = "#d80032", color = "#ffffff"))
          }
        } else {
          return(list(background = "#eeeeee", color = "#000000"))
        }
        print(paste("Row:", row$Species, row$Population, "col:", col, "value:", value))
        print(ref_row)
        
      }
    )
  }
  
  reactable(
    display_data,
    columns = columns_list,
    defaultExpanded = FALSE,
    details = function(index) {
      row <- display_data[index, ]
      if (row$Site == "Reference Median") {
        ref_rows <- ref_data %>%
          filter(Species == row$Species, Population == row$Population) %>%
          select(Site, all_of(size_cols))
        reactable(ref_rows, compact = TRUE, bordered = TRUE, pagination = FALSE)
      } else {
        NULL
      }
    },
    bordered = TRUE,
    striped = TRUE,
    highlight = TRUE,
    pagination = FALSE,
    style = list(fontFamily = "sans-serif", fontSize = "13px")
  )
}

make_comparison_table(cons_data, reference_waterbody_groups, aoc_waterbody_groups)

# Working coloured version ------------------


make_comparison_table <- function(cons_data, reference_groups, aoc_groups) {
  aoc_combinations <- cons_data %>%
    filter(waterbody_group %in% aoc_groups,
           population_type_desc %in% c("General", "Sensitive")) %>%
    distinct(Species = specname, Size = length_category_label, Population = population_type_desc) %>%
    mutate(Size = factor(Size, levels = length_levels, ordered = TRUE))
  
  filtered_data <- cons_data %>%
    filter(waterbody_group %in% c(reference_groups, aoc_groups),
           population_type_desc %in% c("General", "Sensitive")) %>%
    mutate(Species = specname,
           Size = factor(length_category_label, levels = length_levels, ordered = TRUE),
           Population = population_type_desc,
           site_type = if_else(waterbody_group %in% aoc_groups, "AOC", "Reference")) %>%
    semi_join(aoc_combinations, by = c("Species", "Size", "Population"))
  
  base_data <- filtered_data %>%
    group_by(Species, Site = guide_locname_eng, site_type, Population, Size) %>%
    summarise(advisory = max(adv_level, na.rm = TRUE), .groups = "drop")
  
  aoc_data <- base_data %>%
    filter(site_type == "AOC") %>%
    pivot_wider(names_from = Size, values_from = advisory) %>%
    mutate(site_order = 1)
  
  ref_long <- base_data %>%
    filter(site_type == "Reference")
  
  ref_medians <- ref_long %>%
    group_by(Species, Population, Size) %>%
    summarise(Median = median(advisory, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = Size, values_from = Median) %>%
    mutate(Site = "Reference Median", site_type = "Reference", site_order = 2)
  
  ref_data <- ref_long %>%
    pivot_wider(names_from = Size, values_from = advisory)
  
  aoc_median_combo <- bind_rows(aoc_data, ref_medians)
  
  display_data <- aoc_median_combo %>%
    arrange(Species, Population, site_order) %>%
    mutate(
      id = row_number(),
      Species_display = ifelse(duplicated(Species), "", Species),
      Population_display = ifelse(duplicated(paste(Species, Population)), "", Population)
    ) %>%
    select(Species_display, Population_display, Site, site_type, id, Species, Population, site_order, all_of(length_levels))
  
  size_cols <- length_levels[length_levels %in% names(display_data)]
  
  comparison_lookup <- ref_medians %>%
    select(Species, Population, all_of(size_cols)) %>%
    pivot_longer(cols = all_of(size_cols), names_to = "Size", values_to = "Median") %>%
    mutate(id = paste(Species, Population, Size, sep = "||"))
  
  # Prepare lookup as JS-usable named list
  comparison_medians <- setNames(as.list(comparison_lookup$Median), comparison_lookup$id)
  
  columns_list <- list(
    Species_display = colDef(name = "Species", minWidth = 150),
    Population_display = colDef(name = "Population", minWidth = 100),
    Site = colDef(minWidth = 150),
    site_type = colDef(show = FALSE),
    id = colDef(show = FALSE),
    Species = colDef(show = FALSE),
    Population = colDef(show = FALSE),
    site_order = colDef(show = FALSE)
  )
  
  for (col in size_cols) {
    columns_list[[col]] <- colDef(
      name = col,
      align = "center",
      style = JS(sprintf(
        "function(rowInfo, colInfo, state) {
          const row = rowInfo.row;
          if (row.site_type !== 'AOC') return null;
          const id = row.Species + '||' + row.Population + '||' + '%s';
          const ref = state.meta.medians[id];
          if (ref === undefined || ref === null || row[colInfo.id] === null) {
            return { background: '#eeeeee', color: '#000000' };
          } else if (row[colInfo.id] < ref) {
            return { background: '#d80032', color: '#ffffff' };
          } else {
            return { background: '#4CAF50', color: '#ffffff' };
          }
        }",
        col
      ))
    )
  }
  
  reactable(
    display_data,
    columns = columns_list,
    defaultExpanded = FALSE,
    details = function(index) {
      row <- display_data[index, ]
      if (row$Site == "Reference Median") {
        ref_rows <- ref_data %>%
          filter(Species == row$Species, Population == row$Population) %>%
          select(Site, all_of(size_cols))
        reactable(ref_rows, compact = TRUE, bordered = TRUE, pagination = FALSE)
      } else {
        NULL
      }
    },
    meta = list(medians = comparison_medians),
    bordered = TRUE,
    striped = TRUE,
    highlight = TRUE,
    pagination = FALSE,
    style = list(fontFamily = "sans-serif", fontSize = "13px")
  )
}

make_comparison_table(cons_data, reference_waterbody_groups, aoc_waterbody_groups)




## New formatting -------------
# =======================================
# AOC Fish Advisory Table: Embed Median Inside Cells + Expandable Reference Sites
# =======================================
# =======================================
# AOC Fish Advisory Table: Embedded Median + Dropdowns + Only AOC Rows
# =======================================

make_comparison_table <- function(cons_data, reference_groups, aoc_groups) {
  aoc_combinations <- cons_data %>%
    filter(waterbody_group %in% aoc_groups,
           population_type_desc %in% c("General", "Sensitive")) %>%
    distinct(Species = specname, Size = length_category_label, Population = population_type_desc) %>%
    mutate(Size = factor(Size, levels = length_levels, ordered = TRUE))
  
  filtered_data <- cons_data %>%
    filter(waterbody_group %in% c(reference_groups, aoc_groups),
           population_type_desc %in% c("General", "Sensitive")) %>%
    mutate(Species = specname,
           Size = factor(length_category_label, levels = length_levels, ordered = TRUE),
           Population = population_type_desc,
           site_type = if_else(waterbody_group %in% aoc_groups, "AOC", "Reference")) %>%
    semi_join(aoc_combinations, by = c("Species", "Size", "Population"))
  
  base_data <- filtered_data %>%
    group_by(Species, Site = guide_locname_eng, site_type, Population, Size) %>%
    summarise(advisory = max(adv_level, na.rm = TRUE), .groups = "drop")
  
  # Keep only AOC rows for display table
  aoc_display <- base_data %>%
    filter(site_type == "AOC") %>%
    pivot_wider(names_from = Size, values_from = advisory) %>%
    arrange(Species, Population) %>%
    mutate(
      id = row_number(),
      Species_display = ifelse(duplicated(Species), "", Species),
      Population_display = ifelse(duplicated(paste(Species, Population)), "", Population)
    ) %>%
    select(Species_display, Population_display, Site, site_type, id, Species, Population, all_of(length_levels))
  
  size_cols <- length_levels[length_levels %in% names(aoc_display)]
  
  medians_long <- base_data %>%
    filter(site_type == "Reference") %>%
    group_by(Species, Population, Size) %>%
    summarise(Median = median(advisory, na.rm = TRUE), .groups = "drop") %>%
    mutate(id = paste(Species, Population, Size, sep = "||"))
  
  median_lookup <- setNames(as.list(medians_long$Median), medians_long$id)
  
  ref_data <- base_data %>%
    filter(site_type == "Reference") %>%
    pivot_wider(names_from = Size, values_from = advisory)
  
  columns_list <- list(
    Species_display = colDef(name = "Species", minWidth = 150),
    Population_display = colDef(name = "Population", minWidth = 100),
    Site = colDef(minWidth = 150),
    site_type = colDef(show = FALSE),
    id = colDef(show = FALSE),
    Species = colDef(show = FALSE),
    Population = colDef(show = FALSE)
  )
  
  for (col in size_cols) {
    columns_list[[col]] <- colDef(
      name = col,
      align = "center",
      html = TRUE,
      cell = JS(sprintf(
        "function(cellInfo, state) {
          const row = cellInfo.row;
          const val = cellInfo.value;
          const id = row.Species + '||' + row.Population + '||' + '%s';
          const median = state.meta.medians[id];
          if (val === null) return null;
          if (median === undefined || median === null) {
            return `<div style='color: white; background: #999; padding: 2px 4px; border-radius: 2px;'>
                      <div style='font-weight: bold;'>${val}</div>
                      <div style='font-size: 0.75rem;'>NA</div>
                    </div>`;
          }
          const color = val < median ? '#d80032' : '#4CAF50';
          return `<div style='color: white; background: ${color}; padding: 2px 4px; border-radius: 2px;'>
                    <div style='font-weight: bold;'>${val}</div>
                    <div style='font-size: 0.75rem;'>${median}</div>
                  </div>`;
        }",
        col
      ))
    )
  }
  
  reactable(
    aoc_display,
    columns = columns_list,
    defaultExpanded = FALSE,
    details = function(index) {
      row <- aoc_display[index, ]
      ref_rows <- ref_data %>%
        filter(Species == row$Species, Population == row$Population) %>%
        select(Site, all_of(size_cols))
      reactable(ref_rows, compact = TRUE, bordered = TRUE, pagination = FALSE)
    },
    meta = list(medians = median_lookup),
    bordered = TRUE,
    striped = TRUE,
    highlight = TRUE,
    pagination = FALSE,
    style = list(fontFamily = "sans-serif", fontSize = "13px")
  )
}


## Newer formatting -------------
# =======================================
# AOC Fish Advisory Table: Embedded Median + Median Row w/ n + Dropdowns
# =======================================
make_comparison_table <- function(cons_data, reference_groups, aoc_groups) {
  # ===== Filter AOC Species/Size/Population Combinations =====
  aoc_combinations <- cons_data %>%
    filter(
      waterbody_group %in% aoc_groups,
      population_type_desc %in% c("General", "Sensitive")
    ) %>%
    distinct(
      Species = specname,
      Size = length_category_label,
      Population = population_type_desc
    ) %>%
    mutate(Size = factor(Size, levels = length_levels, ordered = TRUE))
  
  # ===== Filter Full Dataset and Join with AOC Combinations =====
  filtered_data <- cons_data %>%
    filter(
      waterbody_group %in% c(reference_groups, aoc_groups),
      population_type_desc %in% c("General", "Sensitive")
    ) %>%
    mutate(
      Species = specname,
      Size = factor(length_category_label, levels = length_levels, ordered = TRUE),
      Population = population_type_desc,
      site_type = if_else(waterbody_group %in% aoc_groups, "AOC", "Reference")
    ) %>%
    semi_join(aoc_combinations, by = c("Species", "Size", "Population"))
  
  # ===== Collapse to Max Advisory Level per Group =====
  base_data <- filtered_data %>%
    group_by(Species, Site = guide_locname_eng, site_type, Population, Size) %>%
    summarise(advisory = max(adv_level, na.rm = TRUE), .groups = "drop")
  
  # ===== Create Wide Table for AOC and Reference Medians =====
  aoc_data <- base_data %>%
    filter(site_type == "AOC") %>%
    pivot_wider(names_from = Size, values_from = advisory) %>%
    mutate(site_order = 1)
  
  ref_long <- base_data %>%
    filter(site_type == "Reference")
  
  ref_medians <- ref_long %>%
    group_by(Species, Population, Size) %>%
    summarise(Median = median(advisory, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = Size, values_from = Median) %>%
    mutate(Site = "Reference Median", site_type = "Reference", site_order = 2)
  
  ref_data <- ref_long %>%
    pivot_wider(names_from = Size, values_from = advisory)
  
  aoc_median_combo <- bind_rows(aoc_data, ref_medians)
  
  # ===== Format Final Display Table =====
  display_data <- aoc_median_combo %>%
    arrange(Species, Population, site_order) %>%
    mutate(
      id = row_number(),
      Species_display = ifelse(duplicated(Species), "", Species),
      Population_display = ifelse(duplicated(paste(Species, Population)), "", Population)
    ) %>%
    select(
      Species_display, Population_display, Site, site_type, id,
      Species, Population, site_order, all_of(length_levels)
    )
  
  # ===== Prepare Size Columns and Comparison Data =====
  size_cols <- length_levels[length_levels %in% names(display_data)]
  
  comparison_lookup <- ref_medians %>%
    select(Species, Population, all_of(size_cols)) %>%
    pivot_longer(cols = all_of(size_cols), names_to = "Size", values_to = "Median") %>%
    mutate(id = paste(Species, Population, Size, sep = "||"))
  
  comparison_medians <- setNames(as.list(comparison_lookup$Median), comparison_lookup$id)
  
  # ===== Define Column Styles =====
  columns_list <- list(
    Species_display = colDef(
      name = "Species",
      minWidth = 150,
      style = JS("
      function(rowInfo) {
        if (rowInfo.row.Site === 'Reference Median') {
          return { fontWeight: 'normal', fontSize: '13px' };
        }
        return { fontWeight: 'bold', fontSize: '15px' };
      }
    ")
    ),
    Population_display = colDef(
      name = "Population",
      minWidth = 100,
      style = JS("
      function(rowInfo) {
        if (rowInfo.row.Site === 'Reference Median') {
          return { fontWeight: 'normal', fontSize: '13px' };
        }
        return { fontWeight: 'bold', fontSize: '15px' };
      }
    ")
    ),
    Site = colDef(
      minWidth = 150,
      style = JS("
      function(rowInfo) {
        if (rowInfo.row.Site === 'Reference Median') {
          return { fontWeight: 'normal', fontSize: '13px' };
        }
        return { fontWeight: 'bold', fontSize: '15px' };
      }
    ")
    ),
    site_type = colDef(show = FALSE),
    id = colDef(show = FALSE),
    Species = colDef(show = FALSE),
    Population = colDef(show = FALSE),
    site_order = colDef(show = FALSE)
  )
  
  # ===== Add Conditional Cell Coloring and Styling for Size Columns =====
  for (col in size_cols) {
    columns_list[[col]] <- colDef(
      name = col,
      align = "center",
      style = JS(sprintf(
        "function(rowInfo, colInfo, state) {
        const row = rowInfo.row;
        const val = row[colInfo.id];
        const isRefMed = row.Site === 'Reference Median';
        const id = row.Species + '||' + row.Population + '||' + '%s';
        const ref = state.meta.medians[id];

        let baseStyle = isRefMed
          ? { fontWeight: 'normal', fontSize: '13px' }
          : { fontWeight: 'bold', fontSize: '15px' };

        if (row.site_type !== 'AOC' || ref === undefined || ref === null || val === null) {
          return Object.assign({}, baseStyle, { background: '#eeeeee', color: '#000000' });
        } else if (val < ref) {
          return Object.assign({}, baseStyle, { background: '#d80032', color: '#ffffff' });
        } else {
          return Object.assign({}, baseStyle, { background: '#4CAF50', color: '#ffffff' });
        }
      }", col
      ))
    )
  }
  
  # ===== Render Final Reactable Table =====
  reactable(
    display_data,
    columns = columns_list,
    defaultExpanded = FALSE,
    bordered = TRUE,
    striped = TRUE,
    highlight = TRUE,
    pagination = FALSE,
    style = list(fontFamily = "system-ui, sans-serif"),
    details = function(index) {
      row <- display_data[index, ]
      if (row$Site == "Reference Median") {
        ref_rows <- ref_data %>%
          filter(Species == row$Species, Population == row$Population) %>%
          select(Site, all_of(size_cols))
        reactable(ref_rows, compact = TRUE, bordered = TRUE, pagination = FALSE)
      } else {
        NULL
      }
    },
    meta = list(medians = comparison_medians)
  )
}


make_comparison_table(cons_data, reference_waterbody_groups, aoc_waterbody_groups)



### Final version of table ----------------------------
# Enhanced Comparison Table with n-values under medians
# Updated function: Adds 'n' row, styles advisory rows based on sample size and medians,
# improves formatting, disables sorting, and enables sticky headers and first 4 columns with fixed size
make_comparison_table <- function(cons_data, reference_groups, aoc_groups) {
  aoc_combinations <- cons_data %>%
    filter(waterbody_group %in% aoc_groups,
           population_type_desc %in% c("General", "Sensitive")) %>%
    distinct(Species = specname, Size = length_category_label, Population = population_type_desc) %>%
    mutate(Size = factor(Size, levels = length_levels, ordered = TRUE))
  
  filtered_data <- cons_data %>%
    filter(waterbody_group %in% c(reference_groups, aoc_groups),
           population_type_desc %in% c("General", "Sensitive")) %>%
    mutate(Species = specname,
           Size = factor(length_category_label, levels = length_levels, ordered = TRUE),
           Population = population_type_desc,
           site_type = if_else(waterbody_group %in% aoc_groups, "AOC", "Reference")) %>%
    semi_join(aoc_combinations, by = c("Species", "Size", "Population"))
  
  base_data <- filtered_data %>%
    group_by(Species, Site = guide_locname_eng, site_type, Population, Size) %>%
    summarise(advisory = max(adv_level, na.rm = TRUE), .groups = "drop")
  
  aoc_data <- base_data %>%
    filter(site_type == "AOC") %>%
    pivot_wider(names_from = Size, values_from = advisory) %>%
    mutate(site_order = 1)
  
  ref_long <- base_data %>%
    filter(site_type == "Reference")
  
  ref_medians <- ref_long %>%
    group_by(Species, Population, Size) %>%
    summarise(Median = median(advisory, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = Size, values_from = Median) %>%
    mutate(Site = "Reference Median", site_type = "Reference", site_order = 2)
  
  ref_n <- ref_long %>%
    group_by(Species, Population, Size) %>%
    summarise(n = n(), .groups = "drop") %>%
    pivot_wider(names_from = Size, values_from = n) %>%
    mutate(Site = "n", site_type = "Reference", site_order = 3)
  
  ref_data <- ref_long %>%
    pivot_wider(names_from = Size, values_from = advisory)
  
  aoc_median_combo <- bind_rows(aoc_data, ref_medians, ref_n)
  
  display_data <- aoc_median_combo %>%
    arrange(Species, Population, site_order) %>%
    mutate(
      id = row_number(),
      Species_display = ifelse(duplicated(Species), "", Species),
      Population_display = ifelse(duplicated(paste(Species, Population)), "", Population)
    ) %>%
    select(Species_display, Population_display, Site, site_type, id,
           Species, Population, site_order, all_of(length_levels))
  
  size_cols <- length_levels[length_levels %in% names(display_data)]
  
  comparison_lookup <- ref_medians %>%
    select(Species, Population, all_of(size_cols)) %>%
    pivot_longer(cols = all_of(size_cols), names_to = "Size", values_to = "Median") %>%
    mutate(id = paste(Species, Population, Size, sep = "||"))
  
  n_lookup <- ref_n %>%
    select(Species, Population, all_of(size_cols)) %>%
    pivot_longer(cols = all_of(size_cols), names_to = "Size", values_to = "n") %>%
    mutate(id = paste(Species, Population, Size, sep = "||"))
  
  comparison_medians <- setNames(as.list(comparison_lookup$Median), comparison_lookup$id)
  sample_ns <- setNames(as.list(n_lookup$n), n_lookup$id)
  
  columns_list <- list(
    Species_display = colDef(
      name = "Species", minWidth = 150, sortable = FALSE, sticky = "left",
      style = JS("function(rowInfo) { return { fontWeight: 'bold', fontSize: '15px' }; }")
    ),
    Population_display = colDef(
      name = "Population", minWidth = 100, sortable = FALSE, sticky = "left",
      style = JS("function(rowInfo) { return { fontWeight: 'bold', fontSize: '15px' }; }")
    ),
    Site = colDef(
      minWidth = 150, sortable = FALSE, sticky = "left", align = "center",
      style = JS(
        "function(rowInfo) {
          const site = rowInfo.row.Site;
          if (site === 'n') {
            return { fontSize: '13px', fontStyle: 'italic' };
          }
          if (site === 'Reference Median') {
            return { fontSize: '13px' };
          }
          if (rowInfo.row.site_type === 'AOC') {
            return { fontWeight: 'bold', fontSize: '15px' };
          }
          return {};
        }"
      )
    ),
    site_type = colDef(show = FALSE),
    id = colDef(show = FALSE),
    Species = colDef(show = FALSE),
    Population = colDef(show = FALSE),
    site_order = colDef(show = FALSE)
  )
  
  for (col in size_cols) {
    columns_list[[col]] <- colDef(
      name = col,
      align = "center",
      sortable = FALSE,
      style = JS(sprintf(
        "function(rowInfo, colInfo, state) {
          const row = rowInfo.row;
          const val = row[colInfo.id];
          const id = row.Species + '||' + row.Population + '||' + '%s';
          const ref = state.meta.medians[id];
          const n = state.meta.ns[id];

          if (row.Site === 'n') {
            return { fontSize: '11px', color: '#666', fontStyle: 'italic' };
          }

          if (row.site_type === 'AOC') {
            if (val === null) {
              return { background: '#eeeeee', color: '#000000', fontWeight: 'bold', fontSize: '15px' };
            } else if (n === undefined || n <= 3 || ref === undefined || ref === null) {
              return { background: '#999999', color: '#ffffff', fontWeight: 'bold', fontSize: '15px' };
            } else if (val < ref) {
              return { background: '#d80032', color: '#ffffff', fontWeight: 'bold', fontSize: '15px' };
            } else {
              return { background: '#4CAF50', color: '#ffffff', fontWeight: 'bold', fontSize: '15px' };
            }
          }

          return { fontWeight: 'normal', fontSize: '13px' };
        }",
        col
      ))
    )
  }
  
  reactable(
    display_data,
    columns = columns_list,
    defaultExpanded = FALSE,
    bordered = TRUE,
    striped = TRUE,
    highlight = TRUE,
    pagination = FALSE,
    style = list(
      fontFamily = "system-ui, sans-serif",
      height = "90%",
      width = "100%",
      overflowY = "auto",
      overflowX = "auto",
      margin = "0 auto"
    ),
    details = function(index) {
      row <- display_data[index, ]
      if (row$Site == "Reference Median") {
        ref_rows <- ref_data %>%
          filter(Species == row$Species, Population == row$Population) %>%
          select(Site, all_of(size_cols))
        reactable(ref_rows, compact = TRUE, bordered = TRUE, pagination = FALSE)
      } else {
        NULL
      }
    },
    meta = list(medians = comparison_medians, ns = sample_ns)
  )
}



slr_t2_table = make_comparison_table(cons_data, reference_waterbody_groups, aoc_waterbody_groups)
slr_t2_table

# Making display table for RStudio Viewer
slr_t2_display <- browsable(
  tagList(
    h3("St. Lawrence River Tier 2 Consumption Analysis", style = "margin-bottom: 0.5em; font-family: system-ui, sans-serif;"),
    slr_t2_table,
    div(
      "Rows indicate MECP consumption advisory levels in the AOC, compared to the reference median. Advisory levels are reported by species, size class, and population type (general or sensitive). Reference sites include all non-AOC fishing zones in Lake Ontario. Red: AOC < reference median; Green: AOC ≥ reference median; Dark grey = low sample size (n ≤ 3). Drop down tables show species advisory levels in reference sites with available data.",
      style = "margin-top: 0.5em; font-size: 13px; font-family: system-ui, sans-serif; color: #222;"
    )
  )
)
slr_t2_display



slr_t2_widget <- slr_t2_table %>%
  prependContent(
    tags$h3("St. Lawrence River Tier 2 Consumption Analysis",
            style = "margin-bottom: 0.5em; font-family: system-ui, sans-serif;")
  ) %>%
  appendContent(
    tags$div(
      "Rows indicate MECP consumption advisory levels in the AOC, compared to the reference median. Advisory levels are reported by species, size class, and population type (general or sensitive). Reference sites include all non-AOC fishing zones in Lake Ontario. Red: AOC < reference median; Green: AOC ≥ reference median; Dark grey = low sample size (n ≤ 3). Drop down tables show species advisory levels in reference sites with available data.",
      style = "margin-top: 0.5em; font-size: 13px; font-family: system-ui, sans-serif; color: #222;"
    )
  )

saveWidget(
  widget = slr_t2_widget,
  file = "Output/Figures/slr_t2_comparison_table.html",
  selfcontained = TRUE
)



export_reactable <- function(widget,
                             file,
                             title = "Table Title",
                             caption = "Figure caption goes here.") {
  styled_widget <- widget %>%
    htmlwidgets::prependContent(
      htmltools::tags$h3(
        title,
        style = "margin-bottom: 0.5em; font-family: system-ui, sans-serif; text-align: center;"
      )
    ) %>%
    htmlwidgets::appendContent(
      htmltools::tags$div(
        caption,
        style = "width: 90%; margin: 0.5em auto 0 auto; font-size: 13px; font-family: system-ui, sans-serif; color: #222; text-align: left-align;"
      )
    )
  
  htmlwidgets::saveWidget(
    widget = styled_widget,
    file = file,
    selfcontained = TRUE
  )
}

export_reactable(slr_t2_table,
                 file = "Output/Figures/SLR_T2_Table.html",
                 title = "St. Lawrence River Tier 2 Consumption Analysis",
                 caption = "Rows indicate MECP consumption advisory levels in the AOC, compared to the reference median. Advisory levels are reported by species, size class, and population type (general or sensitive). Reference sites include all non-AOC fishing zones in Lake Ontario. Red: AOC < reference median; Green: AOC ≥ reference median; Dark grey = low sample size (n ≤ 3). Drop down tables show species advisory levels in reference sites with available data.")
