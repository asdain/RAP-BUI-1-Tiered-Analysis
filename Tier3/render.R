setwd("RAP-BUI-1-Tiered-Analysis")  # or wherever your true root is



species_params <- c("Walleye" = 16, 
                    "Black Crappie" = 8,
                    "Brown Bullhead" = 6, 
                    "Bluegill" = 6,
                    "Freshwater Drum" = 6,
                    "Largemouth Bass" = 6,
                    "White Bass" = 8,
                    "Rock Bass" = 6,
                    "Pumpkinseed" = 6,
                    "Chinook Salmon" = 8, 
                    "Northern Pike" = 4,
                    "Rainbow Smelt" = 8,
                    "Smallmouth Bass" = 8,
                    "White Perch" = 6,
                    "Channel Catfish" = 8,
                    "Lake Whitefish" = 8,
                    "White Sucker" = 8)  

species_params = c("Yellow Perch" = 6)

library(rmarkdown)
library(here)
library(stringr)



for (sp in names(species_params)) {
  threshold <- species_params[[sp]]
  file_stub <- str_to_lower(str_replace_all(sp, "\\s+", "_"))
  output_name <- paste0("tier3_", file_stub, "_boq.html")
  
  message("Rendering report for: ", sp)
  
  tryCatch({
    render(
      input = here("Tier3", "Output",  "t3_walleye_boq.Rmd"),
      
      output_file = here("Tier3", "Output","Bay of Quinte T3 Reports", output_name),
      params = list(
        species = sp,
        restrict_threshold = threshold,
        raw_csv = here("Data", "Great Lakes Data to Ken 2024-12 PCB-Hg(Data).csv")
      ),
      envir = new.env()
    )
    message("✓ Report completed for ", sp, "\n")
  }, error = function(e) {
    message("⚠️ Skipped ", sp, ": ", conditionMessage(e), "\n")
  })
}


slr_spec = c("Yellow Perch",
             "Walleye",
             "Largemouth Bass",
             "Smallmouth Bass",
             "Bluegill",
             "Channel Catfish",
             "Fallfish",
             "Freshwater Drum",
             "Northern Pike",
             "Rock Bass",
             "White Perch",
             "White Sucker")

for (sp in slr_spec) {
  rm(list = setdiff(ls(), c("slr_spec", "sp")), envir = globalenv())  # optional safety
  gc()  # free memory if needed
  file_stub <- str_to_lower(str_replace_all(sp, "\\s+", "_"))
  output_name <- paste0("tier3_", file_stub, "_slr.html")
  
  message("Rendering report for: ", sp)
  
  tryCatch({
    render(
      input = here("Tier3", "Output","t3_bluegill_slr.Rmd"),
      
      output_file = here("Tier3", "Output","SLR T3 Reports", output_name),
      params = list(
        species = sp,
        
        raw_csv = here("Data", "Great Lakes Data to Ken 2024-12 PCB-Hg(Data).csv")
      )
    )
    message("✓ Report completed for ", sp, "\n")
  }, error = function(e) {
    message("⚠️ Skipped ", sp, ": ", conditionMessage(e), "\n")
  })
}

