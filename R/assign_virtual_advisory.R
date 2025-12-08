#' Assign MECP fish consumption advisory levels based on contaminant concentration
#'
#' @param conc Numeric vector of contaminant concentrations (µg/g for Hg, ng/g for PCBs)
#' @param population Character, either "General" or "Sensitive"
#' @param contaminant Character, currently supports "Mercury" or "PCBs"
#'
#' @return Integer vector of advisory levels (number of meals/month)
#' @export
library(dplyr)

assign_advisory <- function(conc, population = "General", contaminant = c("MERCURY", "PCBs", "TEQWH4")) {
  contaminant <- match.arg(contaminant)
  
  if (!population %in% c("General", "Sensitive")) {
    stop("population must be 'General' or 'Sensitive'")
  }
  
  if (contaminant == "MERCURY") {
    if (population == "Sensitive") {
      return(case_when(
        conc > 0.50 ~ 0L,
        conc > 0.25 ~ 4L,
        conc > 0.16 ~ 8L,
        conc > 0.12 ~ 12L,
        conc > 0.06 ~ 16L,
        TRUE        ~ 32L
      ))
    } else {  # General
      return(case_when(
        conc > 1.8  ~ 0L,
        conc > 1.2  ~ 2L,
        conc > 0.6  ~ 4L,
        conc > 0.4  ~ 8L,
        conc > 0.3  ~ 12L,
        conc > 0.15 ~ 16L,
        TRUE        ~ 32L
      ))
    }
  }
  
  if (contaminant == "PCBs") {
    return(case_when(
      conc > 844 ~ 0L,
      conc > 422 ~ 1L,
      conc > 211 ~ 2L,
      conc > 105 ~ 4L,
      conc > 70  ~ 8L,
      conc > 53  ~ 12L,
      conc > 26  ~ 16L,
      TRUE       ~ 32L
    ))
  }
  
  if (contaminant == "TEQWH4") {
    # Dioxin/Furan/dlPCB TEQ (pg/g)
    return(case_when(
      conc > 21.6 ~ 0L,
      conc > 10.8 ~ 1L,
      conc > 5.4  ~ 2L,
      conc > 2.7  ~ 4L,
      conc > 1.8  ~ 8L,
      conc > 1.3  ~ 12L,
      conc > 0.7  ~ 16L,
      TRUE        ~ 32L
    ))
  }


  
  
  #  Fallback to prevent NULL return if no match above (safety net)
  stop("Unrecognized input combination. Check contaminant and population.")
}
