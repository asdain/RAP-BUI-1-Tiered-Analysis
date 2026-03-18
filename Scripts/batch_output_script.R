
species_list_ur <- c("Brown Trout",
                     "Freshwater Drum",
                    "Largemouth Bass",
                     "Rainbow Trout",
                     "Rock Bass",
                     "Walleye",
)


run_log <- tibble::tibble(
  species = character(),
  status  = character(),
  message = character()
)

for (sp in species_list_ur) {
  
  message("Running species: ", sp)
  
  res <- tryCatch(
    {
      rmarkdown::render(
        input = here::here("Analysis","Tier3_analyis.Rmd"),
        params = list(species = sp,
                      AOC_name = 'Upper Niagara River',
                      aoc_shapefile = "Data/Canadian_Niagara_River_AOC/Upper_NR_Shapefile.shp",
                      ref_1= 'Lake Erie',
                      ref_2= 'Lake Erie',
                      add_AOC= 'Lake Ontario 1a',
                      out_dir_base = "Output/Full-NPCA-Report/Derived/NR/Tier3/UR"),
        envir = new.env(parent = globalenv()),
        knit_root_dir = here::here(),
        quiet = TRUE,
        output_file = NULL
      )
      
      list(status = "success", message = NA_character_)
    },
    error = function(e) {
      list(status = "error", message = conditionMessage(e))
    }
  )
  
  run_log <- dplyr::add_row(
    run_log,
    species = sp,
    status = res$status,
    message = res$message
  )
}

run_log



species_list_lr = c(
  "Brown Trout",
  "Chinook Salmon",
  "Coho Salmon",
  "Freshwater Drum",
  "Lake Trout",
  "Largemouth Bass",
  "Rainbow Smelt",
  "Rainbow Trout",
  "Smallmouth Bass",
  "Walleye",
  "White Perch",
  "Yellow Perch"
)

for (sp in species_list_lr) {
  
  message("Running species: ", sp)
  
  res <- tryCatch(
    {
      rmarkdown::render(
        input = here::here("Analysis","Tier3_analyis.Rmd"),
        params = list(species = sp,
                      AOC_name = "Lower Niagara River",
                      aoc_shapefile = "Data/Canadian_Niagara_River_AOC/Lower_NR_Shapefile.shp",
                      ref_1= "Lake Ontario",
                      ref_2= 'Lake Ontario',
                      add_AOC= "Lake Ontario 1b",
                      out_dir_base = "Output/Full-NPCA-Report/Derived/NR/Tier3/LR"),
        envir = new.env(parent = globalenv()),
        knit_root_dir = here::here(),
        quiet = TRUE,
        output_file = NULL
      )
      
      list(status = "success", message = NA_character_)
    },
    error = function(e) {
      list(status = "error", message = conditionMessage(e))
    }
  )
  
  run_log <- dplyr::add_row(
    run_log,
    species = sp,
    status = res$status,
    message = res$message
  )
}

run_log
