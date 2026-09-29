# Tier 3 model using all species

## Model terms:
# log(Concentration/threshold) / log_conc_minus_threshold = log(conc) - log(threshold)
## This way, directly linked to advisories; 0 = at threshold, < 0 = below threshold, > 0 above threshold


# Niagara Tier 3 pooled PCB dataset builde--------------
# For multi-species GAM using log_ratio = log(PCB / 105)

library(here)
library(dplyr)
library(stringr)
library(sf)
library(readr)
library(tidyr)
library(forcats)
library(purrr)
library(mgcv)

source(here::here("Scripts", "setup.R"))


# User settings ----------------

raw_csv <- here::here("Data", "Great Lakes Data to Ken 2024-12 PCB-Hg(Data).csv")

upper_aoc_shp <- here::here("Data", "Canadian_Niagara_River_AOC", "Upper_NR_Shapefile")
lower_aoc_shp <- here::here("Data", "Canadian_Niagara_River_AOC", "Lower_NR_Shapefile")

target_crs <- 4326
recent_year <- 2006

# fixed Tier 3 PCB threshold for 8 meals/month
pcb_threshold_ng_g <- 105

# optional name-based helpers
# keep only if these are genuinely useful in your Niagara data
add_upper_aoc <- c(
  "Lake Ontario 1a",
  "Upper Niagara River",
  "Upper NR"
)

add_lower_aoc <- c(
  "Lake Ontario 1b",
  "Lower Niagara River",
  "Lower NR",
  "Lake Ontario 1b"
)

reference_patterns <- c(
  "Lake Ontario",
  "Lake Erie"
)


exclude_sites <- c(
  "Hamilton Harbour",
  "Trent River",      
  "Toronto Waterfront",
  "Detroit River",
  "Bay of Quinte",
  "Creek",
  "River",
  "Marsh",
  "Belleville Nearshore",
  "Trenton Nearshore",
  "Lake Ontario 4a",
  "Trenton",
  "Big Bay",
  "Long Branch"
)

species_list = c(
  "Rock Bass",
  "Brown Bullhead",
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

ur_species = c(
  "Rock Bass",
  #"Brown Trout", #no recent AOC values
  "Freshwater Drum",
  "Largemouth Bass",
  "Rainbow Trout",
  "Walleye",
  "White Perch"
)

lr_species = c(
  "Rock Bass",
  "Brown Bullhead",
  "Brown Trout",
  "Chinook Salmon",
  #"Coho Salmon", # no recent reference values
  "Freshwater Drum",
  "Lake Trout",
  "Largemouth Bass",
  "Rainbow Smelt",
  "Rainbow Trout",
  "Smallmouth Bass",
  "Walleye",
  #"White Perch", # no recent aoc values
  "Yellow Perch"
)


# Read raw data----------
raw_data <- read.csv(raw_csv)



# Tier 3B model -------------------------


## Initial contaminant filter
dat0 <- raw_data %>%
  filter(
    Contaminant == "PCBs",
    !is.na(Value),
    Value > 0
  ) %>%
  filter(
    Specname %in% species_list
  ) %>%
  mutate(
    Species   = as.character(Specname),
    site_name = as.character(Locname.Fishbase),
    year      = as.integer(Sample.Year),
    long      = as.numeric(Longitude.Decimal),
    lat       = as.numeric(Latitude.Decimal),
    length_cm = as.numeric(Length),
    weight_g  = as.numeric(Weight),
    conc_ng_g = as.numeric(Value)
  ) 

dat0 <- dat0 %>%
  mutate(
    site_name = iconv(site_name, from = "", to = "UTF-8", sub = "")
  )


## Read AOC shapefiles--------

upper_aoc <- st_read(upper_aoc_shp, quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(target_crs) %>%
  mutate(aoc_zone = "Upper Niagara River")

lower_aoc <- st_read(lower_aoc_shp, quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(target_crs) %>%
  mutate(aoc_zone = "Lower Niagara River")

aoc_polys <- bind_rows(
  upper_aoc %>% select(aoc_zone, geometry),
  lower_aoc %>% select(aoc_zone, geometry)
)


# Spatial assignment for rows with coordinates

dat_has_coords <- dat0 %>%
  filter(!is.na(long), !is.na(lat)) %>%
  st_as_sf(coords = c("long", "lat"), crs = target_crs, remove = FALSE) %>%
  st_make_valid()

# join points to AOC polygons
joined <- st_join(dat_has_coords, aoc_polys, join = st_within, left = TRUE)

# drop sf geometry for later dplyr work
joined_df <- joined %>%
  st_drop_geometry() %>%
  mutate(aoc_zone = as.character(aoc_zone))

# rows without coordinates
no_coords_df <- dat0 %>%
  filter(is.na(long) | is.na(lat)) %>%
  mutate(aoc_zone = NA_character_)

dat1 <- bind_rows(joined_df, no_coords_df)

dat1 <- dat1 %>%
  mutate(length_cm = ifelse(length_cm >= 900, NA, length_cm))


# Name-based fallback assignment

collapse_patterns <- function(x) {
  str_c(x, collapse = "|")
}

upper_pat <- collapse_patterns(add_upper_aoc)
lower_pat <- collapse_patterns(add_lower_aoc)
ref_pat   <- collapse_patterns(reference_patterns)

dat2 <- dat1 %>%
  mutate(
    aoc_zone = case_when(
      !is.na(aoc_zone) ~ aoc_zone,
      
      str_detect(site_name, regex(upper_pat, ignore_case = TRUE)) ~ "Upper Niagara River",
      str_detect(site_name, regex(lower_pat, ignore_case = TRUE)) ~ "Lower Niagara River",
      
      TRUE ~ NA_character_
    ),
    Zone = case_when(
      aoc_zone == "Upper Niagara River" ~ "Upper Niagara River",
      aoc_zone == "Lower Niagara River" ~ "Lower Niagara River",
      str_detect(site_name, regex(ref_pat, ignore_case = TRUE)) ~ "Reference",
      TRUE ~ NA_character_
    ),
    region = case_when(
      Zone %in% c("Upper Niagara River", "Lower Niagara River") ~ "AOC",
      Zone == "Reference" ~ "Reference",
      TRUE ~ NA_character_
    )
  )


# Exclusion filter
# safer than one big regex if any site names have punctuation
if (length(exclude_sites) > 0) {
  
  exclude_pattern <- exclude_sites %>%
    stringr::str_replace_all("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1") %>%
    stringr::str_c(collapse = "|")
  
  dat2 <- dat2 %>%
    filter(
      region == "AOC" |
        !stringr::str_detect(site_name, stringr::regex(exclude_pattern, ignore_case = TRUE))
    )
}


## Final modeling variables ----------

niagara_t3_dat <- dat2 %>%
  filter(!is.na(region)) %>%
  mutate(
    Zone = factor(
      Zone,
      levels = c("Upper Niagara River", "Lower Niagara River", "Reference")
    ),
    region = factor(region, levels = c("AOC", "Reference")),
    Species = fct_infreq(factor(Species)),
    site_name = factor(site_name),
    
    threshold_ng_g = pcb_threshold_ng_g,
    log_conc = log(conc_ng_g),
    log_ratio = log(conc_ng_g / threshold_ng_g),
    above_threshold = conc_ng_g > threshold_ng_g,
    
    recent_flag = year >= recent_year
  ) %>%
  filter(
    !is.na(year),
    !is.na(length_cm),
    !is.na(conc_ng_g)
  )


## recent and full datasets --------------
niagara_t3_recent <- niagara_t3_dat %>%
  filter(recent_flag)

niagara_t3_model <- niagara_t3_dat %>%
  filter(
    !is.na(long),
    !is.na(lat)
  )


## Quick summaries
summary_by_zone_species <- niagara_t3_recent %>%
  count(Zone, Species, sort = TRUE)

summary_by_zone <- niagara_t3_recent %>%
  summarise(
    n = n(),
    n_species = n_distinct(Species),
    pct_above_105 = mean(above_threshold, na.rm = TRUE) * 100,
    .by = Zone
  )

print(summary_by_zone)
print(summary_by_zone_species)


## Save datasets -----------

saveRDS(niagara_t3_dat,   here::here("Derived", "NR", "Tier3", "niagara_t3_pooled_full.rds"))
saveRDS(niagara_t3_recent, here::here("Derived", "NR", "Tier3", "niagara_t3_pooled_recent.rds"))
saveRDS(niagara_t3_model,  here::here("Derived", "NR", "Tier3", "niagara_t3_pooled_model_coords.rds"))

readr::write_csv(niagara_t3_dat,    here::here("Derived", "NR", "Tier3", "niagara_t3_pooled_full.csv"))
readr::write_csv(niagara_t3_recent, here::here("Derived", "NR", "Tier3", "niagara_t3_pooled_recent.csv"))
readr::write_csv(summary_by_zone,   here::here("Derived", "NR", "Tier3", "niagara_t3_summary_by_zone.csv"))



# Checkpoint: Load datasets --------------------

niagara_t3_dat = read_rds(here::here("Derived", "NR", "Tier3", "niagara_t3_pooled_full.rds"))
niagara_t3_recent = read_rds(here::here("Derived", "NR", "Tier3", "niagara_t3_pooled_recent.rds"))
niagara_t3_model = read_rds(here::here("Derived", "NR", "Tier3", "niagara_t3_pooled_model_coords.rds"))


species_counts <- niagara_t3_dat %>%
  filter(
    year >= recent_year,
    Species %in% species_list
  ) %>%
  count(Zone, Species, name = "n") %>%
  arrange(Species, Zone)



## Initial models --------------

niagara_t3_model$Zone <- relevel(niagara_t3_model$Zone, ref = "Reference")

m1 <- gam(
  log_ratio ~
    Zone +
    s(year, k = 10) +
    s(length_cm, k = 6) +
    s(Species, bs = "re") +
    s(site_name, bs = "re") +
    te(long, lat, k = c(8, 8)),
  data = niagara_t3_model,
  method = "REML",
  family = scat()
)

m1_nolatlong <- gam(
  log_ratio ~
    Zone +
    s(year, k = 10) +
    s(length_cm, k = 6) +
    s(Species, bs = "re") +
    s(site_name, bs = "re"),
  data = niagara_t3_model,
  method = "REML",
  family = scat()
)

summary(m1)

AIC(m1, m1_nolatlong)
# Spatial structure adds a lot


m2 <- gam(
  log_ratio ~
    Zone +
    s(year, k = 10) +
    s(length_cm, Species, bs = "fs", k = 5) +
    s(Species, bs = "re") +
    s(site_name, bs = "re"),
  data = niagara_t3_model,
  method = "REML",
  family = scat()
)
# Difference: adding species-specific curves to smooth

summary(m2)

# Takeaway: species-specific deviations from the shared length curve are weak / not strongly supported. Paying a huge complexity cost (edf ~28, Ref.df 59) for very little gain
# Differences between AOC and reference are not strongly species-specific.

AIC(m1,m2)
# Not much difference


gam.check(m1)
plot(m1, select = 5)  # assuming te(long,lat) is term 5



## Separating UR and LR ------------------
upper_ref_patterns <- c(
  "Lake Erie"
)

lower_ref_patterns <- c(
  "Lake Ontario"
)

collapse_patterns <- function(x) {
  stringr::str_c(x, collapse = "|")
}

escape_for_regex <- function(x) {
  stringr::str_replace_all(x, "([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1")
}

upper_pat      <- collapse_patterns(escape_for_regex(add_upper_aoc))
lower_pat      <- collapse_patterns(escape_for_regex(add_lower_aoc))
upper_ref_pat  <- collapse_patterns(escape_for_regex(upper_ref_patterns))
lower_ref_pat  <- collapse_patterns(escape_for_regex(lower_ref_patterns))
exclude_pat    <- collapse_patterns(escape_for_regex(exclude_sites))

dat_split <- dat1 %>%
  mutate(
    # Step 1: assign AOC zone first
    aoc_zone = case_when(
      !is.na(aoc_zone) ~ aoc_zone,
      str_detect(site_name, regex(upper_pat, ignore_case = TRUE)) ~ "Upper Niagara River",
      str_detect(site_name, regex(lower_pat, ignore_case = TRUE)) ~ "Lower Niagara River",
      TRUE ~ NA_character_
    ),
    
    # Step 2: assign reference system only if not already AOC
    ref_system = case_when(
      !is.na(aoc_zone) ~ NA_character_,
      str_detect(site_name, regex(upper_ref_pat, ignore_case = TRUE)) ~ "Lake Erie",
      str_detect(site_name, regex(lower_ref_pat, ignore_case = TRUE)) ~ "Lake Ontario",
      TRUE ~ NA_character_
    )
  )


dat_split <- dat_split %>%
  filter(
    !str_detect(site_name, regex(exclude_pat, ignore_case = TRUE)) | !is.na(aoc_zone)
  )

###  Upper analysis: Upper NR + Lake Erie refs---------------
niagara_upper_dat <- dat_split %>%
  filter(
    aoc_zone == "Upper Niagara River" | ref_system == "Lake Erie",
    Species %in% ur_species
  ) %>%
  mutate(
    region = case_when(
      aoc_zone == "Upper Niagara River" ~ "AOC",
      ref_system == "Lake Erie" ~ "Reference",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    region = factor(region, levels = c("Reference", "AOC")),
    Species = fct_infreq(factor(Species)),
    site_name = factor(site_name),
    threshold_ng_g = pcb_threshold_ng_g,
    log_conc = log(conc_ng_g),
    log_ratio = log(conc_ng_g / threshold_ng_g),
    above_threshold = conc_ng_g > threshold_ng_g,
    recent_flag = year >= recent_year,
    length_cm = ifelse(
      is.na(length_cm),
      median(length_cm, na.rm = TRUE),
      length_cm
    )
  ) %>%
  filter(
    !is.na(year),
    !is.na(length_cm),
    !is.na(conc_ng_g),
    !is.na(long),
    !is.na(lat)
  ) %>% group_by(Species) %>%
  filter(any(region == "AOC")) %>%
  ungroup() %>%
  filter(!is.na(region)) 

### Lower analysis: Lower NR + Lake Ontario refs--------------
niagara_lower_dat <- dat_split %>%
  filter(
    aoc_zone == "Lower Niagara River" | ref_system == "Lake Ontario",
    Species %in% lr_species
  ) %>%
  mutate(
    region = case_when(
      aoc_zone == "Lower Niagara River" ~ "AOC",
      ref_system == "Lake Ontario" ~ "Reference",
      TRUE ~ NA_character_
    )
  ) %>%
  
  mutate(
    region = factor(region, levels = c("Reference", "AOC")),
    Species = fct_infreq(factor(Species)),
    site_name = factor(site_name),
    threshold_ng_g = pcb_threshold_ng_g,
    log_conc = log(conc_ng_g),
    log_ratio = log(conc_ng_g / threshold_ng_g),
    above_threshold = conc_ng_g > threshold_ng_g,
    recent_flag = year >= recent_year,
    length_cm = ifelse(
      is.na(length_cm),
      median(length_cm, na.rm = TRUE),
      length_cm
    )
  ) %>%
  filter(
    !is.na(year),
    !is.na(length_cm),
    !is.na(conc_ng_g),
    !is.na(long),
    !is.na(lat)
  )  %>% group_by(Species) %>%
  filter(any(region == "AOC")) %>%
  ungroup() %>%
  filter(!is.na(region)) 




## Separate models --------
m_upper <- gam(
  log_ratio ~
    region +
    s(year, k = 10) +
    s(length_cm, k = 6) +
    Species +
    s(site_name, bs = "re"),
  data = niagara_upper_dat,
  method = "REML",
  family = scat()
)

m_lower <- gam(
  log_ratio ~
    region +
    s(year, k = 10) +
    s(length_cm, k = 6) +
    Species +
    s(site_name, bs = "re") +
    te(long, lat, k = c(8, 8)),
  data = niagara_lower_dat,
  method = "REML",
  family = scat()
)

summary(m_upper)


summary(m_lower)

# Primary analysis retains all eligible species; sparse AOC coverage remains
# a limitation to assess using coverage summaries and sensitivity analyses.



### Model testing ------------
source("Scripts/NR_model_defensibility.R")

NR_tests <- run_niagara_defensibility(
  upper_dat = niagara_upper_dat,
  lower_dat = niagara_lower_dat
)

# Start with these results
print(NR_tests$primary_comparison, n = Inf, width = Inf)

# Changes across data subsets
print(NR_tests$sensitivity_comparison, n = Inf, width = Inf)

# Influence of individual AOC sites
print(NR_tests$site_influence, n = Inf, width = Inf)

# Inspect a particular fitted model
summary(NR_tests$models$Upper$C_species)


## Model C is the preferred choice as it had the best support and lower complexity than spatial model


## Checkpoint: For simplicity, this is the chosen model:------------

# Restrict data to 2006 onward
upper_c_dat <- niagara_upper_dat %>%
  filter(year >= 2006) %>%
  prepare_model_c("Upper") %>%
  purrr::pluck("data") %>%
  droplevels()

lower_c_dat <- niagara_lower_dat %>%
  filter(year >= 2006) %>%
  prepare_model_c("Lower") %>%
  purrr::pluck("data") %>%
  droplevels()

# Retain the year smooth while accommodating fewer sampled years
k_upper <- min(10L, n_distinct(upper_c_dat$year))
k_lower <- min(10L, n_distinct(lower_c_dat$year))

stopifnot(k_upper >= 3L, k_lower >= 3L)

m_upper <- gam(
  log_ratio ~
    region +
    Species +
    s(year, k = k_upper) +
    s(length_cm, by = Species, k = 6, id = 1) +
    s(site_name, bs = "re"),
  data = upper_c_dat,
  method = "REML",
  family = scat(),
  na.action = na.fail
)

m_lower <- gam(
  log_ratio ~
    region +
    Species +
    s(year, k = k_lower) +
    s(length_cm, by = Species, k = 6, id = 1) +
    s(site_name, bs = "re"),
  data = lower_c_dat,
  method = "REML",
  family = scat(),
  na.action = na.fail
)

# Regenerate the results from these model objects
model_c_results <- bind_rows(
  model_c_region_result(m_upper, "Upper"),
  model_c_region_result(m_lower, "Lower")
)

print(model_c_results)

# Optional: inspect the complete summaries
summary(m_upper)
summary(m_lower)

## New better figures: Raw data scatter plots --------

# Measured PCB concentration versus measured fish length, by species.
# Uses dat_split BEFORE the length imputation in the modelling datasets.
# Descriptive figures: no model predictions, fitted curves or significance tests.


# This file runs the model and produces the scatter plots all-in-one.
# Also gives figure captions and some diagnostics
source("Scripts/NR_Model_C_scatter_curves.R")


scatter_lower_c$support
scatter_upper_c$support


scatter_lower_c$plot
scatter_upper_c$plot

model_c_results






## All-species analysis: compatibility names for downstream code --------
# Run the two all-species model fits above before running this section.
# These assignments update BOTH the data and fitted-model aliases.
niagara_upper_ind <- droplevels(niagara_upper_dat)
niagara_lower_ind <- droplevels(niagara_lower_dat)
m_upper_ind <- m_upper
m_lower_ind <- m_lower

## Prediction grids for recent years -------------

library(dplyr)
library(tidyr)

# Average the prediction MATRIX, then propagate the coefficient covariance.
# Rows from one fitted model are correlated; neither sd(fit)/sqrt(n) nor
# sqrt(mean(se.fit^2)) is the standard error of their average.
# Exponentiation gives a geometric average of conditional median ratios,
# not an arithmetic mean concentration or an individual-fish prediction interval.
average_gam_predictions <- function(mod, newdata, by,
                                    exclude = NULL, weight_col = NULL) {
  grouped <- dplyr::group_by(newdata, dplyr::across(dplyr::all_of(by)))
  groups <- dplyr::group_data(grouped)
  beta <- coef(mod)
  V <- vcov(mod)
  estimates <- lapply(groups$.rows, function(i) {
    X <- predict(mod, newdata = newdata[i, , drop = FALSE],
                 type = "lpmatrix", exclude = exclude)
    w <- if (is.null(weight_col)) rep(1, length(i)) else newdata[[weight_col]][i]
    if (any(!is.finite(X)) || any(!is.finite(w)) ||
        any(w < 0) || sum(w) <= 0) {
      stop("Non-finite predictions or invalid averaging weights.")
    }
    Xbar <- matrix(as.numeric(crossprod(w / sum(w), X)), nrow = 1)
    fit <- as.numeric(Xbar %*% beta)
    se <- sqrt(max(0, as.numeric(Xbar %*% V %*% t(Xbar))))
    tibble::tibble(log_ratio = fit, se = se,
                   ratio = exp(fit), lower = exp(fit - 1.96 * se),
                   upper = exp(fit + 1.96 * se))
  })
  dplyr::bind_cols(dplyr::select(groups, -dplyr::all_of(".rows")),
                   dplyr::bind_rows(estimates))
}

# The requested period is retained explicitly. Years after the available
# observations are extrapolations of s(year), not observed recent conditions.
recent_years <- 2006:2024

# representative length (use median)
med_length <- median(niagara_upper_ind$length_cm, na.rm = TRUE)

# get species levels used in model
species_levels <- levels(niagara_upper_ind$Species)


## UR Prediction plot ---------

pred_grid_upper <- expand.grid(
  region = c("Reference", "AOC"),
  year = recent_years,
  length_cm = med_length,
  Species = levels(niagara_upper_ind$Species)
) %>%
  mutate(
    site_name = levels(niagara_upper_ind$site_name)[1]
  )

summary_upper <- average_gam_predictions(
  m_upper_ind, pred_grid_upper, by = "region", exclude = "s(site_name)"
) %>%
  rename(mean_log_ratio = log_ratio, se_log_ratio = se)

upper_diff <- summary_upper %>%
  select(region, mean_log_ratio) %>%
  tidyr::pivot_wider(names_from = region, values_from = mean_log_ratio) %>%
  mutate(diff_log = AOC - Reference, diff_ratio = exp(diff_log))

med_length_lower <- median(niagara_lower_dat$length_cm, na.rm = TRUE)
species_levels_lower <- levels(niagara_lower_dat$Species)

pred_grid_lower <- expand.grid(
  region = c("Reference", "AOC"),
  year = recent_years,
  length_cm = med_length_lower,
  Species = levels(niagara_lower_ind$Species)
) %>%
  mutate(
    site_name = factor(levels(niagara_lower_ind$site_name)[1],
                       levels = levels(niagara_lower_ind$site_name)),
    long = median(niagara_lower_ind$long, na.rm = TRUE),
    lat  = median(niagara_lower_ind$lat, na.rm = TRUE),
    Species = factor(Species, levels = levels(niagara_lower_ind$Species)),
    region = factor(region, levels = levels(niagara_lower_ind$region))
  )

# Both the summary and length figure set site/spatial smooth terms to zero.
# This is a standardized model profile, not a prediction at a physical site.
summary_lower <- average_gam_predictions(
  m_lower_ind, pred_grid_lower, by = "region",
  exclude = c("s(site_name)", "te(long,lat)")
) %>%
  rename(mean_log_ratio = log_ratio, se_log_ratio = se)

dir.create("Derived/NR/Tier3/UR", recursive = TRUE, showWarnings = FALSE)
dir.create("Derived/NR/Tier3/LR", recursive = TRUE, showWarnings = FALSE)
plot_colours = c(AOC = "red", Reference = "royalblue")

# Equal species/year weights at EVERY length preserve the original estimand.
# This is a hypothetical pooled profile: species may be evaluated outside
# their own observed length/year ranges. Adding species can change its level
# and refitting can change the shared smooth. Do not interpret it as the
# observed species mixture at each size or as species-specific advisories.
# Use species-specific prediction panels to investigate unsupported tails.
# Use observed AOC lengths from the full upper dataset
upper_lengths <- niagara_upper_ind %>%
  filter(region == "AOC") %>%
  pull(length_cm) %>%
  sort() %>%
  unique()

pred_grid_upper <- expand.grid(
  region = levels(niagara_upper_ind$region),
  year = recent_years,
  length_cm = upper_lengths,
  Species = levels(niagara_upper_ind$Species)
) %>%
  mutate(
    region = factor(region, levels = levels(niagara_upper_ind$region)),
    Species = factor(Species, levels = levels(niagara_upper_ind$Species)),
    site_name = factor(
      levels(niagara_upper_ind$site_name)[1],
      levels = levels(niagara_upper_ind$site_name)
    )
  )

pred_upper_df <- average_gam_predictions(
  m_upper_ind, pred_grid_upper, by = c("region", "length_cm"),
  exclude = "s(site_name)"
)


library(dplyr)
library(mgcv)


length_seq <- seq(
  floor(min(niagara_upper_ind$length_cm[niagara_upper_ind$region == "AOC"], na.rm = TRUE)),
  ceiling(max(niagara_upper_ind$length_cm[niagara_upper_ind$region == "AOC"], na.rm = TRUE)),
  by = 1
)

species_levels <- levels(niagara_upper_ind$Species)

base_grid_upper <- expand.grid(
  year = recent_years,
  length_cm = length_seq,
  Species = species_levels
) %>%
  mutate(
    site_name = factor(levels(niagara_upper_ind$site_name)[1],
                       levels = levels(niagara_upper_ind$site_name))
  )

new_AOC <- base_grid_upper %>%
  mutate(region = factor("AOC", levels = levels(niagara_upper_ind$region)))

new_REF <- base_grid_upper %>%
  mutate(region = factor("Reference", levels = levels(niagara_upper_ind$region)))

Xp_AOC <- predict(m_upper_ind, newdata = new_AOC, type = "lpmatrix",
                  exclude = "s(site_name)")
Xp_REF <- predict(m_upper_ind, newdata = new_REF, type = "lpmatrix",
                  exclude = "s(site_name)")

Xp_diff <- Xp_AOC - Xp_REF

beta <- coef(m_upper_ind)
Vb   <- vcov(m_upper_ind)

diff_fit <- as.vector(Xp_diff %*% beta)
diff_se  <- sqrt(rowSums((Xp_diff %*% Vb) * Xp_diff))

contrast_upper <- base_grid_upper %>%
  mutate(
    diff_log_ratio = diff_fit,
    se = diff_se,
    lower = diff_log_ratio - 1.96 * se,
    upper = diff_log_ratio + 1.96 * se,
    diff_ratio = exp(diff_log_ratio),
    lower_ratio = exp(lower),
    upper_ratio = exp(upper)
  )

Xbar_diff <- matrix(colMeans(Xp_diff), nrow = 1)

avg_diff <- as.numeric(Xbar_diff %*% beta)
avg_se   <- sqrt(as.numeric(Xbar_diff %*% Vb %*% t(Xbar_diff)))





avg_result_upper <- tibble::tibble(
  diff_log_ratio = avg_diff,
  se = avg_se,
  lower = avg_diff - 1.96 * avg_se,
  upper = avg_diff + 1.96 * avg_se,
  diff_ratio = exp(avg_diff),
  lower_ratio = exp(avg_diff - 1.96 * avg_se),
  upper_ratio = exp(avg_diff + 1.96 * avg_se),
  z = avg_diff / avg_se,
  p_value = 2 * pnorm(abs(avg_diff / avg_se), lower.tail = FALSE)
)

avg_result_upper



ur_pred_plot = ggplot(pred_upper_df, aes(x = length_cm, y = ratio, colour = region, fill = region)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_manual(values = plot_colours) +
  scale_fill_manual(values = plot_colours) +
  labs(
    x = "Length (cm)",
    y = "Predicted PCB / threshold ratio",
    title = "Upper Niagara River: predicted recent PCB ratio by length",
    subtitle = "Equal species/year weights, 2006–2024; standardized model profile.",
    caption = "Shading: approximate 95% confidence intervals. Some species-length/year combinations are extrapolated.",
    colour = "Region",
    fill = "Region"
  ) +
  theme_classic()

ur_pred_plot

ggsave("Derived/NR/Tier3/UR/ur_pcb_pred_plot.png", ur_pred_plot, dpi = 300, height = 8, width = 10)



## LR Prediction plot ------------

lower_lengths <- niagara_lower_ind %>%
  filter(region == "AOC") %>%
  pull(length_cm) %>%
  sort() %>%
  unique()

pred_grid_lower <- expand.grid(
  region = levels(niagara_lower_ind$region),
  year = recent_years,
  length_cm = lower_lengths,
  Species = levels(niagara_lower_ind$Species)
) %>%
  mutate(
    region = factor(region, levels = levels(niagara_lower_ind$region)),
    Species = factor(Species, levels = levels(niagara_lower_ind$Species)),
    site_name = factor(
      levels(niagara_lower_ind$site_name)[1],
      levels = levels(niagara_lower_ind$site_name)
    ),
    long = median(niagara_lower_ind$long, na.rm = TRUE),
    lat = median(niagara_lower_ind$lat, na.rm = TRUE)
  )

pred_lower_df <- average_gam_predictions(
  m_lower_ind, pred_grid_lower, by = c("region", "length_cm"),
  exclude = c("s(site_name)", "te(long,lat)")
)

lr_pred_plot = ggplot(pred_lower_df, aes(x = length_cm, y = ratio, colour = region, fill = region)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = plot_colours) +
  scale_fill_manual(values = plot_colours) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(
    x = "Length (cm)",
    y = "Predicted PCB / threshold ratio",
    title = "Lower Niagara River: predicted recent PCB ratio by length",
    subtitle = "Equal species/year weights, 2006–2024; standardized model profile.",
    caption = "Shading: approximate 95% confidence intervals. Some species-length/year combinations are extrapolated.",
    colour = "Region",
    fill = "Region"
  ) +
  theme_classic()

lr_pred_plot

ggsave("Derived/NR/Tier3/LR/lr_pcb_pred_plot.png", lr_pred_plot, dpi = 300, height = 8, width = 10)



# use observed AOC length range
length_seq_lower <- seq(
  floor(min(niagara_lower_ind$length_cm[niagara_lower_ind$region == "AOC"], na.rm = TRUE)),
  ceiling(max(niagara_lower_ind$length_cm[niagara_lower_ind$region == "AOC"], na.rm = TRUE)),
  by = 1
)

species_levels_lower <- levels(niagara_lower_ind$Species)

base_grid_lower <- expand.grid(
  year = recent_years,
  length_cm = length_seq_lower,
  Species = species_levels_lower
) %>%
  mutate(
    Species = factor(Species, levels = levels(niagara_lower_ind$Species)),
    site_name = factor(
      levels(niagara_lower_ind$site_name)[1],
      levels = levels(niagara_lower_ind$site_name)
    ),
    long = median(niagara_lower_ind$long, na.rm = TRUE),
    lat  = median(niagara_lower_ind$lat, na.rm = TRUE)
  )

new_AOC_lower <- base_grid_lower %>%
  mutate(region = factor("AOC", levels = levels(niagara_lower_ind$region)))

new_REF_lower <- base_grid_lower %>%
  mutate(region = factor("Reference", levels = levels(niagara_lower_ind$region)))

Xp_AOC_lower <- predict(
  m_lower_ind,
  newdata = new_AOC_lower,
  type = "lpmatrix",
  exclude = c("s(site_name)", "te(long,lat)")
)

Xp_REF_lower <- predict(
  m_lower_ind,
  newdata = new_REF_lower,
  type = "lpmatrix",
  exclude = c("s(site_name)", "te(long,lat)")
)

Xp_diff_lower <- Xp_AOC_lower - Xp_REF_lower

beta_lower <- coef(m_lower_ind)
Vb_lower   <- vcov(m_lower_ind)

# pointwise contrasts 
diff_fit_lower <- as.vector(Xp_diff_lower %*% beta_lower)
diff_se_lower  <- sqrt(rowSums((Xp_diff_lower %*% Vb_lower) * Xp_diff_lower))

contrast_lower <- base_grid_lower %>%
  mutate(
    diff_log_ratio = diff_fit_lower,
    se = diff_se_lower,
    lower = diff_log_ratio - 1.96 * se,
    upper = diff_log_ratio + 1.96 * se,
    diff_ratio = exp(diff_log_ratio),
    lower_ratio = exp(lower),
    upper_ratio = exp(upper)
  )

# average contrast over the whole recent grid
Xbar_diff_lower <- matrix(colMeans(Xp_diff_lower), nrow = 1)

avg_diff_lower <- as.numeric(Xbar_diff_lower %*% beta_lower)
avg_se_lower   <- sqrt(as.numeric(Xbar_diff_lower %*% Vb_lower %*% t(Xbar_diff_lower)))

avg_result_lower <- tibble(
  diff_log_ratio = avg_diff_lower,
  se = avg_se_lower,
  lower = avg_diff_lower - 1.96 * avg_se_lower,
  upper = avg_diff_lower + 1.96 * avg_se_lower,
  diff_ratio = exp(avg_diff_lower),
  lower_ratio = exp(avg_diff_lower - 1.96 * avg_se_lower),
  upper_ratio = exp(avg_diff_lower + 1.96 * avg_se_lower),
  z = avg_diff_lower / avg_se_lower,
  p_value = 2 * pnorm(abs(avg_diff_lower / avg_se_lower), lower.tail = FALSE)
)

avg_result_lower

# Although a large proportion of observed fish exceeded the PCB threshold, this pattern is strongly size-dependent. Model-based predictions indicate that, after accounting for fish length, species, and site effects, PCB concentrations in the AOC are not consistently elevated relative to appropriate reference systems.


##  Site effects-------------------
library(dplyr)
library(stringr)
library(gratia)
library(ggplot2)


# Upper Niagara

upper_site_effects <- smooth_estimates(m_upper_ind, select = "s(site_name)") %>%
  mutate(
    multiplier = exp(.estimate),
    lower = exp(.estimate - 2 * .se),
    upper = exp(.estimate + 2 * .se),
    site_name = str_to_title(as.character(site_name))
  ) %>%
  select(site_name, .estimate, .se, multiplier, lower, upper) %>%
  arrange(desc(multiplier))

# add sample size per site
upper_site_effects <- upper_site_effects %>%
  left_join(
    niagara_upper_ind %>% count(site_name, name = "n") %>%
      mutate(site_name = str_to_title(as.character(site_name))),
    by = "site_name"
  )



# Lower Niagara

lower_site_effects <- smooth_estimates(m_lower_ind, select = "s(site_name)") %>%
  mutate(
    multiplier = exp(.estimate),
    lower = exp(.estimate - 2 * .se),
    upper = exp(.estimate + 2 * .se),
    site_name = str_to_title(as.character(site_name))
  ) %>%
  select(site_name, .estimate, .se, multiplier, lower, upper) %>%
  arrange(desc(multiplier))

# add sample size per site
lower_site_effects <- lower_site_effects %>%
  left_join(
    niagara_lower_ind %>% count(site_name, name = "n") %>%
      mutate(site_name = str_to_title(as.character(site_name))),
    by = "site_name"
  )


# Screen for elevated sites (lower > 1)
upper_site_effects %>%
  filter(lower > 1) %>%
  arrange(desc(multiplier))

lower_site_effects %>%
  filter(lower > 1) %>%
  arrange(desc(multiplier))


# Adjusted site predictions --------------------------------------------
# Use the SAME species-length mixture at every site, evaluated at the same
# year. Each species gets equal total weight; its observed AOC lengths retain
# their within-species frequency. All AOC years supply this length template
# so species lacking recent AOC samples are not silently dropped.
# These are model-standardized site comparisons. Species need not have been
# observed at every site, and the target year can be beyond a site's data.
# Original preprocessing may already have imputed missing fish lengths.
make_standardized_site_predictions <- function(mod, dat, target_year) {
  fitted_data <- model.frame(mod)
  template <- fitted_data %>%
    filter(region == "AOC") %>%
    select(Species, length_cm) %>%
    group_by(Species) %>%
    mutate(.weight = 1 / n()) %>%
    ungroup()
  if (!nrow(template)) stop("No fitted AOC rows for site standardization.")
  
  site_meta <- dat %>%
    filter(as.character(site_name) %in% as.character(fitted_data$site_name)) %>%
    group_by(site_name) %>%
    summarise(
      n_regions = n_distinct(region), region = first(region),
      long = median(long, na.rm = TRUE), lat = median(lat, na.rm = TRUE),
      .groups = "drop"
    )
  if (any(site_meta$n_regions != 1)) {
    stop("A site belongs to multiple regions; use unique site identifiers.")
  }
  rows <- lapply(seq_len(nrow(site_meta)), function(i) {
    nd <- template
    nd$site_name <- factor(as.character(site_meta$site_name[i]),
                           levels = levels(fitted_data$site_name))
    nd$region <- factor(as.character(site_meta$region[i]),
                        levels = levels(fitted_data$region))
    nd$year <- target_year
    nd$long <- site_meta$long[i]
    nd$lat <- site_meta$lat[i]
    nd
  })
  newdata <- bind_rows(rows)
  estimates <- average_gam_predictions(
    mod, newdata, by = c("site_name", "region"), weight_col = ".weight"
  ) %>%
    rename(log_fit = log_ratio, log_se = se, ratio_fit = ratio) %>%
    left_join(fitted_data %>% count(site_name, name = "n"), by = "site_name") %>%
    mutate(site_name = as.character(site_name), elevated = lower > 1) %>%
    arrange(desc(ratio_fit))
  list(newdata = newdata, estimates = estimates)
}

upper_site_output <- make_standardized_site_predictions(
  m_upper_ind, niagara_upper_ind, max(recent_years)
)
lower_site_output <- make_standardized_site_predictions(
  m_lower_ind, niagara_lower_ind, max(recent_years)
)
upper_newdata <- upper_site_output$newdata
lower_newdata <- lower_site_output$newdata
upper_site_pred <- upper_site_output$estimates
lower_site_pred <- lower_site_output$estimates

# Descriptive benchmark: equal-site geometric mean of the SAME standardized
# predictions plotted below; not the old average over historical raw rows.
overall_upper <- exp(mean(upper_site_pred$log_fit))
overall_lower <- exp(mean(lower_site_pred$log_fit))

### Forest plot-------------

okabe_ito <- c(
  "#000000", "#E69F00", "#56B4E9", "#009E73",
  "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
)

upper_site_pred2 <- upper_site_pred %>%
  mutate(
    region = as.factor(region),
    elevated = as.logical(elevated)
  ) %>%
  arrange(ratio_fit)

p_forest_upper <- ggplot(
  upper_site_pred2,
  aes(x = ratio_fit, y = reorder(site_name, ratio_fit))
) +
  geom_errorbar(
    aes(xmin = lower, xmax = upper),
    height = 0.2, linewidth = 0.4, alpha = 0.85
  ) +
  geom_point(
    aes(colour = region, shape = region,
        stroke = ifelse(elevated, 1.4, 0.4)),
    size = 2.7
  ) +
  geom_vline(xintercept = overall_upper, linetype = "dashed", linewidth = 0.6) +
  geom_vline(xintercept = 1, linetype = "dotted", linewidth = 0.6) +
  scale_shape_manual(values = c(16, 17)) +
  scale_colour_manual(values = c("Reference" = "royalblue", "AOC" = "red")) +
  labs(
    x = "Adjusted PCB / threshold ratio",
    subtitle = paste0("Common AOC species-length mixture; year ", max(recent_years)),
    caption = "Equal species weights. Dashed: equal-site geometric mean; dotted: threshold.",
    y = "Site",
    colour = "Region",
    shape = "Region"
  ) +
  theme_classic()

p_forest_upper

ggsave("Derived/NR/Tier3/UR/ur_pcb_site_plot.png", p_forest_upper, dpi = 300, height = 8, width = 10)




lower_site_pred2 <- lower_site_pred %>%
  mutate(
    region = as.factor(region),
    elevated = as.logical(elevated)
  ) %>%
  arrange(ratio_fit)

p_forest_lower <- ggplot(
  lower_site_pred2,
  aes(x = ratio_fit, y = reorder(site_name, ratio_fit))
) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.2, linewidth = 0.4, alpha = 0.85
  ) +
  geom_point(
    aes(colour = region, shape = region,
        stroke = ifelse(elevated, 1.4, 0.4)),
    size = 2.7
  ) +
  geom_vline(xintercept = overall_lower, linetype = "dashed", linewidth = 0.6) +
  geom_vline(xintercept = 1, linetype = "dotted", linewidth = 0.6) +
  scale_shape_manual(values = c(16, 17)) +
  scale_colour_manual(values = c("Reference" = "royalblue", "AOC" = "red")) +
  labs(
    x = "Adjusted PCB / threshold ratio",
    subtitle = paste0("Common AOC species-length mixture; year ", max(recent_years)),
    caption = "Equal species weights. Dashed: equal-site geometric mean; dotted: threshold.",
    y = "Site",
    colour = "Region",
    shape = "Region"
  ) +
  theme_classic()

p_forest_lower

ggsave("Derived/NR/Tier3/LR/lr_pcb_site_plot.png", p_forest_lower, dpi = 300, height = 8, width = 10)


# Tier 3A ---------------

## Heatmap for size-exceedance summary -------------
library(dplyr)
library(ggplot2)
library(scales)

# define bins (adjust if needed)

breaks <- c(0, 30, 50, 70, 100)

# Upper
heat_df_upper <- niagara_upper_dat %>%
  filter(year >= 2006) %>%
  mutate(
    size_bin = cut(
      length_cm,
      breaks = c(0, 30, 50, 70, Inf),
      labels = c("0–30 cm", "30–50 cm", "50–70 cm", ">70 cm"),
      include.lowest = TRUE,
      right = TRUE
    ),
    above = conc_ng_g > 105
  ) %>%
  group_by(region, Species, size_bin) %>%
  summarise(
    n = n(),
    n_above = sum(above),
    pct_above = n_above / n,
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0(
      n_above, " of ", n,
      "\n(", percent(pct_above, accuracy = 1), ")"
    ),
    ,
    region = factor(region, levels = c("AOC", "Reference"))
  )  %>% group_by(Species) %>%
  filter(any(region == "AOC")) %>%
  ungroup() %>%
  filter(!is.na(region)) 

ur_heatmap = ggplot(heat_df_upper, aes(x = size_bin, y = Species, fill = pct_above)) +
  geom_tile(color = "white") +
  geom_text(aes(label = label), size = 3) +
  scale_fill_gradient(
    low = "lightblue",
    high = "red",
    limits = c(0, 1),
    labels = percent
  ) +
  facet_wrap(~region) +
  labs(
    x = "Fish length",
    y = "Species",
    fill = "% above threshold",
    title = "Proportion of fish exceeding PCB threshold by species and size (2006-2024)"
  ) +
  theme_classic()

ur_heatmap

ggsave("Derived/NR/Tier3/UR/ur_t3_heatmap.png", ur_heatmap, dpi = 300, height = 8, width = 10)


# Lower
heat_df_lower <- niagara_lower_dat %>%
  filter(year >= 2006) %>%
  mutate(
    size_bin = cut(
      length_cm,
      breaks = c(0, 30, 50, 70, Inf),
      labels = c("0–30 cm", "30–50 cm", "50–70 cm", ">70 cm"),
      include.lowest = TRUE,
      right = TRUE
    ),
    above = conc_ng_g > 105
  ) %>%
  group_by(region, Species, size_bin) %>%
  summarise(
    n = n(),
    n_above = sum(above),
    pct_above = n_above / n,
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0(
      n_above, " of ", n,
      "\n(", percent(pct_above, accuracy = 1), ")"
    ),
    region = factor(region, levels = c("AOC", "Reference"))
  ) %>% group_by(Species) %>%
  filter(any(region == "AOC")) %>%
  ungroup() %>%
  filter(!is.na(region)) 

lr_heatmap = ggplot(heat_df_lower, aes(x = size_bin, y = Species, fill = pct_above)) +
  geom_tile(color = "white") +
  geom_text(aes(label = label), size = 3) +
  scale_fill_gradient(
    low = "lightblue",
    high = "red",
    limits = c(0, 1),
    labels = scales::percent
  ) +
  facet_wrap(~region) +
  labs(
    x = "Fish length",
    y = "Species",
    fill = "% above threshold",
    title = "Proportion of fish exceeding PCB threshold by species and size (2006-2024)"
  ) +
  theme_classic()

lr_heatmap

ggsave("Derived/NR/Tier3/LR/lr_t3_heatmap.png", lr_heatmap, dpi = 300, height = 8, width = 10)

## Comparison tables ---------
library(tidyverse)
library(scales)

size_levels <- c("0–30 cm", "30–50 cm", "50–70 cm", ">70 cm")
region_levels <- c("Reference", "AOC")


# Summarize observed data
size_df_obs <- niagara_lower_dat %>%
  filter(year >= 2006) %>%
  mutate(
    size_bin = cut(
      length_cm,
      breaks = c(0, 30, 50, 70, Inf),
      labels = size_levels,
      include.lowest = TRUE,
      right = TRUE
    ),
    above = conc_ng_g > 105
  ) %>%
  group_by(region, Species, size_bin) %>%
  summarise(
    n = n(),
    n_above = sum(above),
    pct_above = n_above / n,
    .groups = "drop"
  ) %>%
  group_by(Species) %>%
  filter(any(region == "AOC")) %>%
  ungroup()



aoc_species <- size_df_obs %>%
  filter(region == "AOC") %>%
  distinct(Species)



# Complete all AOC/reference x size-bin cells for species retained
size_df <- size_df_obs %>%
  semi_join(aoc_species, by = "Species") %>%
  complete(
    Species,
    region = region_levels,
    size_bin = factor(size_levels, levels = size_levels),
    fill = list(n = NA_integer_, n_above = NA_integer_, pct_above = NA_real_)
  )

# Comparison flags by species-size bin
size_compare <- size_df %>%
  select(Species, size_bin, region, pct_above) %>%
  pivot_wider(names_from = region, values_from = pct_above) %>%
  mutate(
    matched_comparison = !is.na(AOC) & !is.na(Reference),
    compare_flag = case_when(
      !matched_comparison ~ "No comparison",
      AOC > Reference ~ "AOC higher",
      TRUE ~ "Reference equal or higher"
    )
  ) %>%
  select(Species, size_bin, compare_flag, matched_comparison)

size_df <- size_df %>%
  left_join(size_compare, by = c("Species", "size_bin")) %>%
  mutate(
    panel_group = "Size-specific",
    x_group = as.character(size_bin),
    label = case_when(
      is.na(n) ~ "No data",
      !matched_comparison ~ paste0(
        n_above, " of ", n,
        "\n(", percent(pct_above, accuracy = 1), ")",
        "\nNo comp."
      ),
      TRUE ~ paste0(
        n_above, " of ", n,
        "\n(", percent(pct_above, accuracy = 1), ")"
      )
    )
  )

# Overall totals using ONLY matched size classes
overall_df <- size_df %>%
  filter(!is.na(n)) %>%
  filter(
    region == "AOC" |
      (region == "Reference" & matched_comparison)
  ) %>%
  group_by(region, Species) %>%
  summarise(
    n = sum(n),
    n_above = sum(n_above),
    pct_above = n_above / n,
    .groups = "drop"
  ) %>%
  mutate(
    panel_group = "Overall",
    x_group = "Overall",
    label = paste0(
      n_above, " of ", n,
      "\n(", percent(pct_above, accuracy = 1), ")"
    )
  )

overall_compare <- overall_df %>%
  select(Species, region, pct_above) %>%
  pivot_wider(names_from = region, values_from = pct_above) %>%
  mutate(
    compare_flag = case_when(
      is.na(AOC) | is.na(Reference) ~ "No comparison",
      AOC > Reference ~ "AOC higher",
      TRUE ~ "Reference equal or higher"
    )
  ) %>%
  select(Species, compare_flag)

overall_df <- overall_df %>%
  left_join(overall_compare, by = "Species") %>%
  mutate(matched_comparison = TRUE)

plot_df <- bind_rows(
  size_df %>%
    select(region, Species, panel_group, x_group, label, compare_flag,
           matched_comparison, n, n_above, pct_above),
  overall_df %>%
    select(region, Species, panel_group, x_group, label, compare_flag,
           matched_comparison, n, n_above, pct_above)
) %>%
  mutate(
    region = factor(region, levels = c("Reference", "AOC")),
    panel_group = factor(panel_group, levels = c("Size-specific", "Overall")),
    x_group = factor(x_group, levels = c(size_levels, "Overall")),
    
    fill_group = case_when(
      is.na(n) ~ "No data",
      region == "Reference" ~ "Reference",
      compare_flag == "AOC higher" ~ "AOC higher",
      compare_flag == "Reference equal or higher" ~ "AOC equal/lower",
      TRUE ~ "No comparison"
    ),
    
    label_plot = case_when(
      is.na(n) ~ "n.d.",
      compare_flag == "No comparison" ~ paste0(
        n_above, " of ", n,
        "\n(", percent(pct_above, accuracy = 1), ")",
        "\nn.c."
      ),
      TRUE ~ paste0(
        n_above, " of ", n,
        "\n(", percent(pct_above, accuracy = 1), ")"
      )
    )
  )


lr_compare_facet2 <- ggplot(
  plot_df,
  aes(x = x_group, y = region, fill = fill_group)
) +
  geom_tile(color = "white", linewidth = 0.5) +
  
  geom_text(
    aes(label = label_plot),
    size = 3,
    lineheight = 0.85,
    na.rm = TRUE
  ) +
  
  facet_grid(
    Species ~ panel_group,
    switch = "y",
    scales = "free_x",
    space = "free_x"
  ) +
  scale_fill_manual(
    values = c(
      "AOC higher" = "indianred2",
      "AOC equal/lower" = "lightblue3",
      "Reference" = "white",
      "No comparison" = "grey85",
      "No data" = "grey95"
    ),
    drop = FALSE
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    x = "Fish length",
    y = NULL,
    fill = NULL,
    title = "PCB threshold exceedance by species, size class, and region (2006–2024)"
  ) +
  theme_classic() +
  theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 0, face = "bold"),
    strip.text.x = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    panel.spacing.x = unit(0.8, "lines"),
    panel.spacing.y = unit(0.5, "lines"),
    axis.line = element_blank(),
    panel.grid = element_blank()
  )


lr_compare_facet2


ggsave("Derived/NR/Tier3/LR/lr_t3_comparison.png", lr_compare_facet2, dpi = 300, height = 10, width = 10)



## UPPER --------------------

library(tidyverse)
library(scales)

size_levels <- c("0–30 cm", "30–50 cm", "50–70 cm", ">70 cm")
region_levels <- c("Reference", "AOC")


# Summarize observed data
size_df_obs <- niagara_upper_dat %>%
  filter(year >= 2006) %>%
  mutate(
    Species = as.character(Species),
    region = as.character(region),
    size_bin = cut(
      length_cm,
      breaks = c(0, 30, 50, 70, Inf),
      labels = size_levels,
      include.lowest = TRUE,
      right = TRUE
    ),
    above = conc_ng_g > 105
  ) %>%
  group_by(region, Species, size_bin) %>%
  summarise(
    n = n(),
    n_above = sum(above),
    pct_above = n_above / n,
    .groups = "drop"
  ) %>%
  group_by(Species) %>%
  filter(any(region == "AOC")) %>%
  ungroup()



aoc_species <- size_df_obs %>%
  filter(region == "AOC") %>%
  distinct(Species)



# Complete all AOC/reference x size-bin cells for species retained
size_df <- size_df_obs %>%
  semi_join(aoc_species, by = "Species") %>%
  complete(
    Species,
    region = region_levels,
    size_bin = factor(size_levels, levels = size_levels),
    fill = list(n = NA_integer_, n_above = NA_integer_, pct_above = NA_real_)
  ) %>%
  mutate(as.factor(Species))

# Comparison flags by species-size bin
size_compare <- size_df %>%
  select(Species, size_bin, region, pct_above) %>%
  pivot_wider(names_from = region, values_from = pct_above) %>%
  mutate(
    matched_comparison = !is.na(AOC) & !is.na(Reference),
    compare_flag = case_when(
      !matched_comparison ~ "No comparison",
      AOC > Reference ~ "AOC higher",
      TRUE ~ "Reference equal or higher"
    )
  ) %>%
  select(Species, size_bin, compare_flag, matched_comparison)

size_df <- size_df %>%
  left_join(size_compare, by = c("Species", "size_bin")) %>%
  mutate(
    panel_group = "Size-specific",
    x_group = as.character(size_bin),
    label = case_when(
      is.na(n) ~ "No data",
      !matched_comparison ~ paste0(
        n_above, " of ", n,
        "\n(", percent(pct_above, accuracy = 1), ")",
        "\nNo comp."
      ),
      TRUE ~ paste0(
        n_above, " of ", n,
        "\n(", percent(pct_above, accuracy = 1), ")"
      )
    )
  )

# Overall totals using ONLY matched size classes
overall_df <- size_df %>%
  filter(!is.na(n)) %>%
  filter(
    region == "AOC" |
      (region == "Reference" & matched_comparison)
  ) %>%
  group_by(region, Species) %>%
  summarise(
    n = sum(n),
    n_above = sum(n_above),
    pct_above = n_above / n,
    .groups = "drop"
  ) %>%
  mutate(
    panel_group = "Overall",
    x_group = "Overall",
    label = paste0(
      n_above, " of ", n,
      "\n(", percent(pct_above, accuracy = 1), ")"
    )
  )

overall_compare <- overall_df %>%
  select(Species, region, pct_above) %>%
  pivot_wider(names_from = region, values_from = pct_above) %>%
  mutate(
    compare_flag = case_when(
      is.na(AOC) | is.na(Reference) ~ "No comparison",
      AOC > Reference ~ "AOC higher",
      TRUE ~ "Reference equal or higher"
    )
  ) %>%
  select(Species, compare_flag)

overall_df <- overall_df %>%
  left_join(overall_compare, by = "Species") %>%
  mutate(matched_comparison = TRUE)

plot_df <- bind_rows(
  size_df %>%
    select(region, Species, panel_group, x_group, label, compare_flag,
           matched_comparison, n, n_above, pct_above),
  overall_df %>%
    select(region, Species, panel_group, x_group, label, compare_flag,
           matched_comparison, n, n_above, pct_above)
) %>%
  mutate(
    region = factor(region, levels = c("Reference", "AOC")),
    panel_group = factor(panel_group, levels = c("Size-specific", "Overall")),
    x_group = factor(x_group, levels = c(size_levels, "Overall")),
    
    fill_group = case_when(
      is.na(n) ~ "No data",
      region == "Reference" ~ "Reference",
      compare_flag == "AOC higher" ~ "AOC higher",
      compare_flag == "Reference equal or higher" ~ "AOC equal/lower",
      TRUE ~ "No comparison"
    ),
    
    label_plot = case_when(
      is.na(n) ~ "n.d.",
      compare_flag == "No comparison" ~ paste0(
        n_above, " of ", n,
        "\n(", percent(pct_above, accuracy = 1), ")",
        "\nn.c."
      ),
      TRUE ~ paste0(
        n_above, " of ", n,
        "\n(", percent(pct_above, accuracy = 1), ")"
      )
    )
  )


ur_compare_facet2 <- ggplot(
  plot_df,
  aes(x = x_group, y = region, fill = fill_group)
) +
  geom_tile(color = "white", linewidth = 0.5) +
  
  geom_text(
    aes(label = label_plot),
    size = 3,
    lineheight = 0.85,
    na.rm = TRUE
  ) +
  
  facet_grid(
    Species ~ panel_group,
    switch = "y",
    scales = "free_x",
    space = "free_x"
  ) +
  scale_fill_manual(
    values = c(
      "AOC higher" = "indianred2",
      "AOC equal/lower" = "lightblue3",
      "Reference" = "white",
      "No comparison" = "grey85",
      "No data" = "grey95"
    ),
    drop = FALSE
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    x = "Fish length",
    y = NULL,
    fill = NULL,
    title = "PCB threshold exceedance by species, size class, and region (2006–2024)"
  ) +
  theme_classic() +
  theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 0, face = "bold"),
    strip.text.x = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    panel.spacing.x = unit(0.8, "lines"),
    panel.spacing.y = unit(0.5, "lines"),
    axis.line = element_blank(),
    panel.grid = element_blank()
  )


ur_compare_facet2

ggsave("Derived/NR/Tier3/UR/ur_t3_comparison.png", ur_compare_facet2, dpi = 300, height = 6, width = 10)




# Tier 3C Temporal trends ------------
# Requires: dat_split BEFORE length imputation, niagara_upper_dat,
#           niagara_lower_dat (used only to identify your retained species).
# One temporal model per reach; species panels are predictions from that model.
# All species share one log-linear temporal slope. Curves are conditional medians.

# ---- Controls ------------------------------------------------------------
t3c_cfg <- list(
  year_origin = 2006, # Numerical centering ONLY; does not filter sampling years.
  representative_start_year = 2006, # Recent fish lengths used for projection profiles.
  threshold = 105,
  k_length = 6L,
  n_draws = 5000L,
  seed = 2006L,
  decision_years = 10,
  plot_horizon = 30L,
  extend_to_crossings = TRUE,
  maximum_plot_horizon = 50L,
  log_y = TRUE,
  ncol = 3L,
  run_species_loo = TRUE,
  out_dir = "Derived/NR/Tier3/Tier3C_species"
)

# Separate measured lengths from upstream imputed modelling lengths.
# Species filters track the current reach datasets, including your exclusions.
prepare_t3c_aoc <- function(raw, included_species, zone, cfg) {
  needed <- c("aoc_zone", "Species", "year", "length_cm", "conc_ng_g", "site_name")
  if (!all(needed %in% names(raw))) stop("dat_split lacks: ",
                                         paste(setdiff(needed, names(raw)), collapse = ", "))
  d0 <- raw %>%
    mutate(.source_row = seq_len(nrow(raw))) %>%
    filter(aoc_zone == zone, Species %in% included_species, is.finite(year))
  if (!nrow(d0)) stop(zone, ": no AOC observations in the selected period.")
  # dat_split in the supplied script is pre-imputation. If an explicit
  # original-length column exists, prefer it; exclude known imputed lengths.
  if ("length_cm_original" %in% names(d0)) d0$length_cm <- d0$length_cm_original
  known_imputed <- if ("length_was_missing" %in% names(d0))
    d0$length_was_missing %in% TRUE else rep(FALSE, nrow(d0))
  keep <- is.finite(d0$length_cm) & d0$length_cm > 0 &
    is.finite(d0$conc_ng_g) & d0$conc_ng_g > 0 &
    !is.na(d0$site_name) & nzchar(trimws(as.character(d0$site_name))) & !known_imputed
  excluded <- d0[!keep, , drop = FALSE]
  d <- d0[keep, , drop = FALSE] %>%
    mutate(Species = factor(as.character(Species)),
           site_name = factor(as.character(site_name)),
           region = factor("AOC"),
           log_conc = log(conc_ng_g),
           year_c = year - cfg$year_origin) %>%
    droplevels()
  if (!nrow(d)) stop(zone, ": no usable measured-length observations.")
  coverage <- d %>% group_by(Species) %>% summarise(
    n = n(), n_years = n_distinct(year), first_year = min(year),
    last_year = max(year), n_sites = n_distinct(site_name),
    n_lengths = n_distinct(length_cm),
    min_length = min(length_cm), max_length = max(length_cm), .groups = "drop")
  coverage$sparse <- coverage$n < 10 | coverage$n_years < 3 | coverage$n_lengths < 6
  audit <- tibble(reach = zone, eligible_AOC_all_years = nrow(d0),
                  excluded_invalid_or_imputed = nrow(excluded), fitted_n = nrow(d),
                  first_year = min(d$year), last_year = max(d$year),
                  n_sites = n_distinct(d$site_name), n_species = n_distinct(d$Species))
  stopifnot(all(is.finite(d$year)), all(d$region == "AOC"))
  list(data = d, coverage = coverage, audit = audit, excluded = excluded)
}

# Explicit screens: do not silently drop sparse species or change to another
# length model. A species with inadequate length support needs review.
fit_t3c <- function(dat, cfg) {
  dat <- droplevels(as.data.frame(dat))
  if (n_distinct(dat$year_c) < 3L) stop("Fewer than three sampled years for estimating a trend.")
  unique_lengths <- tapply(dat$length_cm, dat$Species, function(x) length(unique(x)))
  weak <- names(unique_lengths)[unique_lengths < 3L]
  if (length(weak)) stop("Fewer than three distinct measured lengths for: ",
                         paste(weak, collapse = ", "), ". Review coverage; no species were silently removed.")
  if (n_distinct(dat$length_cm) < cfg$k_length) stop("Too few distinct lengths for the selected k_length.")
  nsp <- nlevels(dat$Species)
  use_site <- nlevels(dat$site_name) >= 2L
  length_term <- if (nsp > 1L)
    sprintf("Species + s(length_cm, by = Species, k = %d, id = 1)", cfg$k_length) else
      sprintf("s(length_cm, k = %d)", cfg$k_length)
  f <- as.formula(paste("log_conc ~ year_c +", length_term,
                        if (use_site) '+ s(site_name, bs = "re")' else ""),
                  env = asNamespace("mgcv"))
  mod <- gam(f, data = dat, method = "REML", family = scat(), na.action = na.fail)
  if (!isTRUE(mod$converged)) stop("GAM did not converge.")
  if (!is.null(mod$outer.info$conv) && mod$outer.info$conv != "full convergence")
    stop("Outer optimization: ", mod$outer.info$conv)
  if (mod$rank < length(coef(mod))) stop("Rank-deficient model; review year/species/length coverage.")
  attr(mod, "t3c_has_site") <- use_site
  attr(mod, "t3c_time_window") <- "all_available"
  mod
}

t3c_covariance <- function(mod) {
  V <- if (!is.null(mod$Vc)) mod$Vc else mod$Vp
  if (any(!is.finite(V))) stop("Non-finite model coefficient covariance.")
  (V + t(V)) / 2
}

# Correlated coefficient draws preserve uncertainty in the anchor estimate,
# species/length effects AND the shared temporal slope, including covariance.
draw_t3c_coefficients <- function(mod, n_draws, seed) {
  set.seed(seed)
  V <- t3c_covariance(mod)
  ev <- eigen(V, symmetric = TRUE)
  tol <- 1e-8 * max(1, max(abs(ev$values)))
  if (min(ev$values) < -tol) stop("Coefficient covariance is not positive semidefinite.")
  L <- sweep(ev$vectors, 2, sqrt(pmax(ev$values, 0)), "*")
  z <- matrix(rnorm(nrow(V) * n_draws), nrow = nrow(V), ncol = n_draws)
  sweep(L %*% z, 1, coef(mod), "+")
}

# Inverse empirical CDF avoids interpolation involving Inf. Infinite crossing
# times are retained, not discarded to obtain an artificially finite interval.
t3c_quantile <- function(x, p) {
  if (anyNA(x)) stop("Unexpected missing values in simulation output.")
  as.numeric(quantile(x, probs = p, type = 1, names = FALSE))
}

t3c_crossing <- function(log_start, slope, log_target) {
  out <- rep(Inf, length(log_start))
  current <- log_start <= log_target
  out[current] <- 0
  declining <- !current & slope < 0
  out[declining] <- (log_start[declining] - log_target) / (-slope[declining])
  out
}

extract_t3c_rate <- function(mod, label, draws = NULL) {
  j <- match("year_c", names(coef(mod)))
  if (is.na(j)) stop("Missing linear year_c coefficient.")
  b <- unname(coef(mod)[j]); se <- sqrt(t3c_covariance(mod)[j, j])
  half <- if (b < 0) log(2) / -b else Inf
  hc <- c(NA_real_, NA_real_)
  if (!is.null(draws)) {
    hd <- rep(Inf, ncol(draws)); declining <- draws[j, ] < 0
    hd[declining] <- log(2) / -draws[j, declining]
    hc <- t3c_quantile(hd, c(0.025, 0.975))
  }
  tibble(river_section = label, slope = b, slope_se = se, k = -b,
         annual_change = 100 * expm1(b),
         annual_lwr = 100 * expm1(b - 1.96 * se),
         annual_upr = 100 * expm1(b + 1.96 * se),
         p_value = 2 * pnorm(-abs(b / se)),
         half_life_years = half, half_life_lwr = hc[1], half_life_upr = hc[2],
         covariance = if (!is.null(mod$Vc)) "Smoothing uncertainty corrected" else "Conditional")
}

# Each species contributes its own RECENT measured length quartiles. The model
# itself uses the full record. If a species has no recent samples, use its
# historical lengths and explicitly flag this fallback in the exported table.
# Keep the full
# numeric values in predictions; round ONLY for the display table/facet label.
get_rep_lengths_quartiles <- function(dat, cfg) {
  roles <- c("Lower quartile", "Median", "Upper quartile")
  bind_rows(lapply(levels(dat$Species), function(sp) {
    ds <- dat[dat$Species == sp, , drop = FALSE]
    recent <- ds[ds$year >= cfg$representative_start_year, , drop = FALSE]
    historical_fallback <- nrow(recent) == 0L
    lengths_dat <- if (historical_fallback) ds else recent
    if (historical_fallback) message(sp, ": no AOC samples from ",
                                     cfg$representative_start_year, " onward; representative lengths use historical samples (flagged in rep_info).")
    tibble(Species = sp, role = roles,
           length_cm = as.numeric(quantile(lengths_dat$length_cm, c(0.25, 0.5, 0.75), names = FALSE)),
           species_first_year = min(ds$year), species_last_year = max(ds$year),
           n_lengths = nrow(lengths_dat),
           length_reference_first_year = min(lengths_dat$year),
           length_reference_last_year = max(lengths_dat$year),
           historical_length_fallback = historical_fallback)
  })) %>% mutate(role = factor(role, levels = roles))
}

t3c_newdata <- function(mod, species, lengths, years, cfg) {
  mf <- model.frame(mod)
  nd <- data.frame(length_cm = lengths, year_c = years - cfg$year_origin)
  if ("Species" %in% names(mf)) nd$Species <- factor(species, levels = levels(mf$Species))
  if ("site_name" %in% names(mf))
    nd$site_name <- factor(levels(mf$site_name)[1], levels = levels(mf$site_name))
  nd
}

t3c_lpmatrix <- function(mod, newdata) {
  omitted <- if (isTRUE(attr(mod, "t3c_has_site"))) "s(site_name)" else NULL
  predict(mod, newdata = newdata, type = "lpmatrix", exclude = omitted)
}

compute_years_to_threshold_pcb <- function(mod, dat, rep_info, draws, cfg, label) {
  anchor <- max(dat$year)
  nd <- t3c_newdata(mod, rep_info$Species, rep_info$length_cm,
                    rep(anchor, nrow(rep_info)), cfg)
  X <- t3c_lpmatrix(mod, nd)
  eta <- as.numeric(X %*% coef(mod))
  V <- t3c_covariance(mod)
  se <- sqrt(pmax(0, rowSums((X %*% V) * X)))
  log_draws <- X %*% draws
  j <- match("year_c", names(coef(mod)))
  slope <- unname(coef(mod)[j])
  point_times <- t3c_crossing(eta, rep(slope, length(eta)), log(cfg$threshold))
  bind_rows(lapply(seq_len(nrow(rep_info)), function(i) {
    times <- t3c_crossing(log_draws[i, ], draws[j, ], log(cfg$threshold))
    ci <- t3c_quantile(times, c(0.025, 0.975))
    tibble(river_section = label, Species = as.character(rep_info$Species[i]),
           role = rep_info$role[i], length_cm = rep_info$length_cm[i],
           anchor_year = anchor, species_last_year = rep_info$species_last_year[i],
           anchor_extrapolated_for_species = anchor > rep_info$species_last_year[i],
           predicted_conc = exp(eta[i]), conc_lwr = exp(eta[i] - 1.96 * se[i]),
           conc_upr = exp(eta[i] + 1.96 * se[i]), target_conc = cfg$threshold,
           years_to_target = point_times[i], target_year = anchor + point_times[i],
           years_lwr = ci[1], years_upr = ci[2],
           fraction_draws_below_at_anchor = mean(log_draws[i, ] <= log(cfg$threshold)),
           decision_horizon_years = cfg$decision_years,
           fraction_draws_reaching_within_horizon = mean(times <= cfg$decision_years),
           fraction_draws_no_finite_crossing = mean(is.infinite(times)),
           outcome = if (point_times[i] <= cfg$decision_years) "Supportive" else "Unsupportive",
           uncertainty_note = if (is.infinite(ci[2]))
             "Upper time bound is unbounded; some coefficient draws do not reach the target." else
               "Finite approximate 95% interval, conditional on a constant log-linear trend.")
  }))
}

make_t3c_prediction_grid <- function(mod, dat, rep_info, table, cfg) {
  anchor <- max(dat$year)
  end_year <- anchor + cfg$plot_horizon
  finite_targets <- table$target_year[is.finite(table$target_year)]
  if (cfg$extend_to_crossings && length(finite_targets))
    end_year <- max(end_year, ceiling(max(finite_targets)))
  end_year <- min(end_year, anchor + cfg$maximum_plot_horizon)
  if (any(table$target_year > end_year & is.finite(table$target_year)))
    message("Some point-estimate crossings exceed the plotting horizon; retain their values in the table.")
  # Include every species' last sampled year in both the solid and extrapolated
  # segments so there is no gap where the line type changes.
  grid <- bind_rows(lapply(seq_len(nrow(rep_info)), function(i) {
    rr <- rep_info[i, ]
    inside <- seq(rr$species_first_year, rr$species_last_year, by = 1)
    future <- seq(rr$species_last_year, end_year, by = 1)
    bind_rows(
      tibble(year = inside, period = "Fitted"),
      tibble(year = future, period = "Extrapolated")
    ) %>% mutate(Species = as.character(rr$Species), role = rr$role,
                 length_cm = rr$length_cm)
  }))
  nd <- t3c_newdata(mod, grid$Species, grid$length_cm, grid$year, cfg)
  X <- t3c_lpmatrix(mod, nd)
  eta <- as.numeric(X %*% coef(mod))
  se <- sqrt(pmax(0, rowSums((X %*% t3c_covariance(mod)) * X)))
  grid %>% mutate(conc = exp(eta), lwr = exp(eta - 1.96 * se),
                  upr = exp(eta + 1.96 * se),
                  role = factor(role, levels = c("Lower quartile", "Median", "Upper quartile")),
                  period = factor(period, levels = c("Fitted", "Extrapolated")))
}

colour_vals <- c("Lower quartile" = "#696969", "Median" = "black", "Upper quartile" = "#ADADAD")

plot_t3c_species <- function(pred_df, dat, rep_info, cfg, observed_only = FALSE) {
  if (!"axes" %in% names(formals(ggplot2::facet_wrap)))
    stop("Update ggplot2 to use repeated x axes via facet_wrap(axes = 'all_x').")
  d <- if (observed_only) filter(pred_df, period == "Fitted") else pred_df
  # Species-specific actual lengths are printed in facet strips, avoiding
  # a misleading common length next to the shared LQ/median/UQ legend.
  strip_labels <- vapply(split(rep_info, rep_info$Species), function(x) {
    x <- x[match(c("Lower quartile", "Median", "Upper quartile"), as.character(x$role)), ]
    paste0(x$Species[1], "\nLQ / median / UQ: ",
           paste(sprintf("%.1f", x$length_cm), collapse = " / "), " cm")
  }, character(1))
  ribbon_df <- distinct(d, Species, role, year, .keep_all = TRUE)
  p <- ggplot(d, aes(year, conc, colour = role)) +
    geom_ribbon(data = ribbon_df,
                aes(ymin = lwr, ymax = upr, fill = role, group = interaction(Species, role)),
                alpha = 0.10, colour = NA, show.legend = FALSE) +
    geom_line(aes(linetype = period, group = interaction(Species, role, period)), linewidth = 0.8) +
    geom_hline(yintercept = cfg$threshold, linetype = "dotted", colour = "grey35") +
    facet_wrap(~ Species, ncol = cfg$ncol, scales = "fixed",
               labeller = labeller(Species = strip_labels), axes = "all_x", axis.labels = "all_x") +
    scale_colour_manual(values = colour_vals, drop = FALSE) +
    scale_fill_manual(values = colour_vals, drop = FALSE) +
    scale_linetype_manual(values = c(Fitted = "solid", Extrapolated = "22"), drop = TRUE) +
    scale_x_continuous(breaks = scales::breaks_pretty(n = 5)) +
    labs(x = "Year", y = "Predicted PCB concentration (ng/g)",
         colour = "Representative length", linetype = NULL,
         title = NULL, subtitle = NULL, caption = NULL) +
    theme_classic(base_size = 12) +
    theme(legend.position = "bottom", strip.background = element_blank(),
          strip.text = element_text(size = 10), panel.spacing = grid::unit(1, "lines"))
  if (cfg$log_y) p <- p + scale_y_log10(labels = scales::label_number(big.mark = ",")) else
    p <- p + scale_y_continuous(labels = scales::label_number(big.mark = ","))
  p
}

make_t3c_caption <- function(dat, cfg, label, observed_only = FALSE) {
  paste0("Modelled PCB concentrations in ", label,
         " Niagara River AOC fish at species-specific lower-quartile, median and upper-quartile lengths. ",
         "The model used AOC observations from ", min(dat$year), " to ", max(dat$year),
         ", using the full available record for the retained species; reference fish were excluded. ",
         "Representative lengths were calculated from measured AOC fish sampled from ",
         cfg$representative_start_year, " onward. For species without samples in that period, historical lengths were used and flagged in the representative-length table. ",
         "Each panel shows one species, with its representative measured lengths listed above the panel. ",
         "Curves represent conditional median concentrations from one pooled model with species-specific length effects ",
         "and a common constant proportional annual change across species. ",
         if (n_distinct(dat$site_name) > 1) "Site random effects were set to zero for prediction. " else
           "Only one recorded site was represented; no site random effect was fitted. ",
         "Shading shows approximate pointwise 95% confidence intervals for the curves, conditional on the selected lengths. ",
         if (!observed_only) paste0("Solid curves cover each species' sampled year range; dashed curves extend beyond its last sampling year. ",
                                    "Extrapolation assumes the log-linear trend estimated over the full sampling record continues. ") else
                                      "Curves are shown only within each species' sampled year range. ",
         "The dotted horizontal line marks ", cfg$threshold, " ng/g. ",
         if (cfg$log_y) "The concentration axis is logarithmic. " else "",
         "The panels do not estimate independent species-specific rates of temporal change.")
}

# ---- Leave-one-species-out sensitivity: same data and model structure ------
run_species_loo <- function(dat, full_mod, river_section, cfg) {
  base <- extract_t3c_rate(full_mod, river_section) %>% mutate(
    model = "Full pooled model", omitted_species = NA_character_,
    n_removed = 0L, status = "ok", error = "", fit_warnings = "")
  if (n_distinct(dat$Species) < 2L) return(base)
  others <- lapply(levels(dat$Species), function(sp) {
    message(river_section, " sensitivity: excluding ", sp)
    dd <- droplevels(dat[dat$Species != sp, , drop = FALSE])
    notes <- character()
    ans <- tryCatch(withCallingHandlers({
      mm <- fit_t3c(dd, cfg)
      extract_t3c_rate(mm, river_section) %>% mutate(
        model = paste("Exclude", sp), omitted_species = sp,
        n_removed = sum(dat$Species == sp), status = "ok", error = "")
    }, warning = function(w) {
      notes <<- c(notes, conditionMessage(w)); invokeRestart("muffleWarning")
    }), error = function(e) tibble(river_section = river_section,
                                   model = paste("Exclude", sp), omitted_species = sp,
                                   n_removed = sum(dat$Species == sp), status = "failed", error = conditionMessage(e)))
    ans$fit_warnings <- paste(unique(notes), collapse = " | ")
    ans
  })
  bind_rows(c(list(base), others))
}

plot_species_loo <- function(loo_dat) {
  d <- filter(loo_dat, status == "ok", !is.na(omitted_species))
  if (!nrow(d)) return(NULL)
  b <- loo_dat$annual_change[loo_dat$model == "Full pooled model"][1]
  d$omitted_species <- reorder(d$omitted_species, d$annual_change)
  ggplot(d, aes(x = annual_change, y = omitted_species)) +
    geom_vline(xintercept = 0, colour = "grey75") +
    geom_vline(xintercept = b, colour = "red", linetype = "dashed") +
    geom_segment(aes(x = annual_lwr, xend = annual_upr, yend = omitted_species)) +
    geom_point(aes(size = n_removed), shape = 21, fill = "white") +
    labs(x = "Estimated annual change in PCB concentration (%)", y = "Species omitted",
         size = "Observations removed", title = NULL, subtitle = NULL, caption = NULL) +
    theme_classic(base_size = 12)
}

# ---- Fit each reach and generate predictions ------------------------------
run_t3c_reach <- function(raw, included_species, zone, label, cfg, seed) {
  prepared <- prepare_t3c_aoc(raw, included_species, zone, cfg)
  dat <- prepared$data
  if (n_distinct(dat$site_name) == 1L) message(label,
                                               ": one recorded AOC site. Fitting without a site random effect; interpret as the sampled site's trend.")
  sparse <- prepared$coverage$Species[prepared$coverage$sparse]
  if (length(sparse)) message(label, ": sparse coverage for ", paste(sparse, collapse = ", "),
                              ". Review the coverage table and uncertainty before interpreting projections.")
  message("Fitting Tier 3C ", label, ": ", nrow(dat), " AOC observations, ",
          n_distinct(dat$Species), " species, ", min(dat$year), "-", max(dat$year))
  mod <- fit_t3c(dat, cfg)
  reps <- get_rep_lengths_quartiles(dat, cfg)
  draws <- draw_t3c_coefficients(mod, cfg$n_draws, seed)
  rate <- extract_t3c_rate(mod, label, draws)
  tab <- compute_years_to_threshold_pcb(mod, dat, reps, draws, cfg, label)
  pred <- make_t3c_prediction_grid(mod, dat, reps, tab, cfg)
  loo <- if (cfg$run_species_loo) run_species_loo(dat, mod, label, cfg) else NULL
  list(model = mod, data = dat, coverage = prepared$coverage, audit = prepared$audit,
       excluded = prepared$excluded, rep_info = reps, rate = rate, threshold_table = tab,
       predictions = pred,
       plot = plot_t3c_species(pred, dat, reps, cfg),
       observed_plot = plot_t3c_species(pred, dat, reps, cfg, observed_only = TRUE),
       caption = make_t3c_caption(dat, cfg, label),
       observed_caption = make_t3c_caption(dat, cfg, label, observed_only = TRUE),
       loo = loo, loo_plot = if (!is.null(loo)) plot_species_loo(loo) else NULL)
}

t3c_upper <- run_t3c_reach(dat_split, unique(as.character(niagara_upper_dat$Species)),
                           "Upper Niagara River", "Upper", t3c_cfg, t3c_cfg$seed)
t3c_lower <- run_t3c_reach(dat_split, unique(as.character(niagara_lower_dat$Species)),
                           "Lower Niagara River", "Lower", t3c_cfg, t3c_cfg$seed + 1L)


m_t3c_pcb_upper <- t3c_upper$model
m_t3c_pcb_lower <- t3c_lower$model
niagara_upper_t3c <- t3c_upper$data
niagara_lower_t3c <- t3c_lower$data
rep_info_upper <- t3c_upper$rep_info
rep_info_lower <- t3c_lower$rep_info
hl_tab <- bind_rows(t3c_upper$rate, t3c_lower$rate)
t3c_upper_tbl <- t3c_upper$threshold_table
t3c_lower_tbl <- t3c_lower$threshold_table
t3c_sensitivity_tbl <- bind_rows(t3c_upper_tbl, t3c_lower_tbl)
p_t3c_proj_upper <- t3c_upper$plot
p_t3c_proj_lower <- t3c_lower$plot
p_t3c_obs_upper <- t3c_upper$observed_plot
p_t3c_obs_lower <- t3c_lower$observed_plot
loo_upper <- t3c_upper$loo
loo_lower <- t3c_lower$loo
p_loo_upper <- t3c_upper$loo_plot
p_loo_lower <- t3c_lower$loo_plot

t3c_format_time <- function(x) ifelse(is.infinite(x), "No finite crossing", sprintf("%.1f", x))
t3c_summary_table <- t3c_sensitivity_tbl %>% transmute(
  River = river_section, Species, `Representative group` = role,
  `Length (cm)` = round(length_cm, 1), `Anchor year` = anchor_year,
  `Predicted PCB at anchor (ng/g)` = round(predicted_conc, 1),
  `Threshold (ng/g)` = target_conc,
  `Years to threshold` = t3c_format_time(years_to_target),
  `95% interval (years)` = paste0(t3c_format_time(years_lwr), " to ", t3c_format_time(years_upr)),
  `Decision horizon (years)` = decision_horizon_years,
  `Draws reaching within horizon (%)` = round(100 * fraction_draws_reaching_within_horizon, 1),
  `No finite crossing draws (%)` = round(100 * fraction_draws_no_finite_crossing, 1),
  `Point-estimate outcome` = outcome
)
print(hl_tab, width = Inf)
print(t3c_summary_table, n = Inf, width = Inf)
print(p_t3c_proj_upper)
print(p_t3c_proj_lower)
if (!is.null(p_loo_upper)) print(p_loo_upper)
if (!is.null(p_loo_lower)) print(p_loo_lower)
cat("\nUPPER TIER 3C CAPTION\n", t3c_upper$caption, "\n\n", sep = "")
cat("LOWER TIER 3C CAPTION\n", t3c_lower$caption, "\n", sep = "")

# ---- Export figures, tables, model summaries and diagnostics ---------------
dir.create(t3c_cfg$out_dir, recursive = TRUE, showWarnings = FALSE)
for (reach in c("Upper", "Lower")) {
  res <- if (reach == "Upper") t3c_upper else t3c_lower
  stem <- file.path(t3c_cfg$out_dir, paste0("NR_", reach, "_Tier3C"))
  h <- 1.2 + 2.7 * ceiling(n_distinct(res$data$Species) / t3c_cfg$ncol)
  ggsave(paste0(stem, "_projections.png"), res$plot, width = 12, height = h, dpi = 300, bg = "white")
  ggsave(paste0(stem, "_fitted_period.png"), res$observed_plot, width = 12, height = h, dpi = 300, bg = "white")
  writeLines(res$caption, paste0(stem, "_caption.txt"))
  writeLines(res$observed_caption, paste0(stem, "_fitted_period_caption.txt"))
  for (item in c("data", "coverage", "audit", "excluded", "rep_info", "threshold_table", "predictions"))
    write.csv(res[[item]], paste0(stem, "_", item, ".csv"), row.names = FALSE)
  writeLines(capture.output(summary(res$model)), paste0(stem, "_model_summary.txt"))
  saveRDS(res$model, paste0(stem, "_model.rds"))
  if (!is.null(res$loo)) write.csv(res$loo, paste0(stem, "_species_loo.csv"), row.names = FALSE)
  if (!is.null(res$loo_plot)) {
    ggsave(paste0(stem, "_species_loo.png"), res$loo_plot, width = 9,
           height = max(4, 1.8 + 0.4 * n_distinct(res$data$Species)), dpi = 300, bg = "white")
    writeLines("Points and 95% intervals show the shared annual change after omitting each species. The red dashed line marks the full-model estimate; the grey line marks no change. Failed fits are retained in the CSV, but not plotted. This assesses influence, not species-specific rates.",
               paste0(stem, "_species_loo_caption.txt"))
  }
  # These plots are checks to review, not automatic validation of projections.
  local({
    pdf(paste0(stem, "_diagnostics.pdf"), width = 9, height = 7)
    on.exit(dev.off())
    par(mfrow = c(2, 2))
    gam.check(res$model)
    par(mfrow = c(1, 1))
    plot(res$data$year, residuals(res$model, type = "deviance"),
         xlab = "Year", ylab = "Deviance residual", main = "Check for remaining temporal structure")
    abline(h = 0, lty = 2)
  })
}
write.csv(hl_tab, file.path(t3c_cfg$out_dir, "Tier3C_rates_and_half_lives.csv"), row.names = FALSE)
write.csv(t3c_summary_table, file.path(t3c_cfg$out_dir, "Tier3C_summary_table.csv"), row.names = FALSE)
saveRDS(t3c_summary_table, file.path(t3c_cfg$out_dir, "t3c_summary_table.rds"))
saveRDS(list(config = t3c_cfg, Upper = t3c_upper, Lower = t3c_lower),
        file.path(t3c_cfg$out_dir, "Tier3C_results.rds"))

# Interpretation:
# - Temporal fits use ALL available sampling years for the retained AOC species.
#   Representative lengths use recent samples where available; this does NOT
#   restrict the model-fitting period. Ensure dat_split still contains history.
# - The projected annual rate is the whole-record rate, not a post-remediation
#   rate. Review residual temporal structure before interpreting extrapolation.
# - Half-life is descriptive. Supportive/Unsupportive uses the point estimate
#   of time to 105 ng/g (<=10 years), NOT whether half-life is <=10 years.
# - An interval ending in "No finite crossing" is unbounded under coefficient
#   uncertainty. Do not discard those draws or treat a point outcome as certain.
# - A curve below the threshold at the anchor has years_to_target=0. If the
#   slope is positive, this does not mean it will remain below the threshold.
# - All panels share one temporal rate within a reach. Species-specific length
#   effects and baseline concentrations do not imply species-specific slopes.
# - Quartiles are fixed measured-length summaries, not bootstrapped quantities.
# - Time projections describe conditional medians, not individual-fish safety.
# - Review diagnostics and site/temporal coverage before reporting forecasts.
# - To replot, use plot_t3c_species() with t3c_upper/t3c_lower stored outputs.


# Tier 3C: earlier of the Tier 1 and Tier 2 targets --------------------------

# Keep the existing species-specific LQ / median / UQ lengths and fitted curves.
# Required Tier 2 files: one reach-specific t2_prep.rds at each path below.
# The uploaded example has display_data, ref_data, size_cols and medians_map.
t3c_targets_cfg <- list(
  t2_paths = c(Upper = "Derived/NR_UR/t2_prep.rds",
               Lower = "Derived/NR_LR/t2_prep.rds"),
  fitted_results_path = "Derived/NR/Tier3/Tier3C_species/Tier3C_results.rds",
  populations = c("General", "Sensitive"), # Separate results, never pooled.
  tier1_target = 105,
  decision_years = 10,
  n_draws = 5000L,
  seed = 2006L,
  # Boundary convention: [15,20], (20,25], ..., (70,75], (75,Inf).
  # Check against the convention in your Tier 2 analysis. No length rounding.
  size_class_right = TRUE,
  mark_first_target_on_plots = TRUE,
  out_dir = "Derived/NR/Tier3/Tier3C_dual_targets"
)

# PCB lookup supplied for this assessment, in ng/g. The 0-meal row is a
# category marker, NOT an upper concentration bound. Do not use 845 as a target.
t3c_pcb_lookup <- data.frame(
  meals = c(32, 16, 12, 8, 4, 2, 1, 0),
  conc = c(26, 53, 70, 105, 211, 422, 844, 845)
)

dual_meals_to_target <- function(meals, lookup = t3c_pcb_lookup) {
  positive <- lookup[lookup$meals > 0, , drop = FALSE]
  if (anyDuplicated(positive$meals) || any(!is.finite(positive$conc)) ||
      any(positive$conc <= 0)) stop("Invalid PCB lookup.")
  do.call(rbind, lapply(meals, function(m) {
    if (is.na(m)) return(data.frame(required_meals = NA_real_, target = NA_real_,
                                    mapping_note = "Reference median unavailable"))
    if (!is.finite(m) || m < 0 || m > max(positive$meals))
      stop("Invalid reference-median advisory: ", m)
    if (m == 0) return(data.frame(required_meals = 0, target = Inf,
                                  mapping_note = "Zero-meal reference: no positive-advisory requirement; not evidence of improvement"))
    # Match OR EXCEED the median number of meals. If the median is between
    # categories (e.g. 10), require the next category (12), not interpolation.
    required <- min(positive$meals[positive$meals >= m])
    data.frame(required_meals = required,
               target = positive$conc[match(required, positive$meals)],
               mapping_note = if (required == m) "Exact advisory category" else
                 "Next available meal category at or above the reference median")
  }))
}

dual_read_t2 <- function(path, reach, populations) {
  if (!file.exists(path)) stop(reach, ": cannot find ", path,
                               ". Set t3c_targets_cfg$t2_paths to your reach-specific files.")
  prep <- readRDS(path)
  if (!is.list(prep) || !all(c("display_data", "size_cols") %in% names(prep)))
    stop(reach, ": expected t2_prep$display_data and t2_prep$size_cols.")
  d <- as_tibble(prep$display_data)
  cols <- as.character(prep$size_cols)
  if (!all(c("Species", "Population", "Site", cols) %in% names(d)))
    stop(reach, ": Tier 2 display_data is missing required columns.")
  # Use the SAVED Tier 2 median, not medians recomputed from different sites,
  # and not threshold_map (which is not the species-by-size reference median).
  ref <- d %>% filter(trimws(as.character(Site)) == "Reference Median") %>%
    transmute(Species = trimws(as.character(Species)),
              Population = trimws(as.character(Population)), across(all_of(cols)))
  if (!all(populations %in% unique(ref$Population)))
    stop(reach, ": requested population absent from Reference Median rows.")
  ref <- ref %>% filter(Population %in% populations) %>%
    pivot_longer(all_of(cols), names_to = "size_class", values_to = "reference_meals")
  old <- ref$reference_meals
  ref$reference_meals <- suppressWarnings(as.numeric(as.character(old)))
  if (any(!is.na(old) & is.na(ref$reference_meals)))
    stop(reach, ": nonnumeric reference advisory; inspect display_data.")
  if (anyDuplicated(ref[c("Species", "Population", "size_class")]))
    stop(reach, ": duplicate reference-median species/population/size keys.")
  key <- paste(ref$Species, ref$Population, ref$size_class, sep = "||")
  ref$n_reference_sites <- vapply(key, function(k) {
    value <- prep$n_map[[k]]
    if (is.null(value) || !length(value)) NA_real_ else as.numeric(value)[1]
  }, numeric(1))
  mapped <- dual_meals_to_target(ref$reference_meals)
  ref <- bind_cols(ref, mapped) %>% rename(tier2_target = target) %>%
    mutate(reach = reach, source_file = path)
  sites <- if (!is.null(prep$ref_data) && "Site" %in% names(prep$ref_data))
    sort(unique(as.character(prep$ref_data$Site))) else character()
  if (any(grepl("Lake Erie", sites, ignore.case = TRUE)) &&
      any(grepl("Lake Ontario", sites, ignore.case = TRUE)))
    message(reach, ": the Tier 2 reference set includes sites labelled Lake Erie AND Lake Ontario. ",
            "Saved Tier 2 medians are retained; review the exported site list if you intend a single-lake target.")
  list(targets = ref, size_cols = cols, reference_sites = sites, path = path)
}

dual_size_class <- function(lengths, size_cols, right = TRUE) {
  # Parse the actual labels instead of generating a possibly mismatched join.
  labels <- gsub("[\u2013\u2014]", "-", trimws(size_cols))
  labels <- gsub("\\s*cm\\s*$", "", labels, ignore.case = TRUE)
  regular <- grepl("^[0-9.]+\\s*-\\s*[0-9.]+$", labels)
  open <- grepl("^>\\s*[0-9.]+$", labels)
  if (any(!regular & !open) || sum(open) > 1)
    stop("Unrecognised Tier 2 size labels; edit dual_size_class explicitly.")
  lo <- hi <- numeric(length(labels))
  for (i in seq_along(labels)) {
    z <- as.numeric(regmatches(labels[i], gregexpr("[0-9.]+", labels[i]))[[1]])
    lo[i] <- z[1]; hi[i] <- if (open[i]) Inf else z[2]
  }
  ord <- order(lo); lo <- lo[ord]; hi <- hi[ord]; size_cols <- size_cols[ord]
  if (any(hi <= lo) || (length(lo) > 1 && any(lo[-1] != head(hi, -1))))
    stop("Tier 2 size classes must be contiguous and nonoverlapping.")
  as.character(cut(lengths, breaks = c(lo[1], hi), labels = size_cols,
                   right = right, include.lowest = TRUE))
}

dual_crossing <- function(log_start, slope, target) {
  n <- max(length(log_start), length(slope), length(target))
  z <- rep_len(log_start, n); b <- rep_len(slope, n); t <- rep_len(target, n)
  out <- rep(NA_real_, n)
  valid <- !is.na(t) & t > 0 & is.finite(z) & is.finite(b)
  out[valid] <- Inf
  met <- valid & z <= log(t)
  out[met] <- 0
  decline <- valid & !met & b < 0
  out[decline] <- (z[decline] - log(t[decline])) / -b[decline]
  out
}

dual_first <- function(t1, t2) {
  if (is.na(t2)) return("Unknown: Tier 2 unavailable")
  if (is.infinite(t1) && is.infinite(t2)) return("Neither under fitted trend")
  if (isTRUE(all.equal(t1, t2, tolerance = 1e-8)))
    return(if (t1 == 0) "Both already met" else "Both together")
  if (t1 < t2) "Tier 1" else "Tier 2"
}

# Calendar year of equality on the fitted curve. Unlike dual_crossing(), this
# is NOT a remaining-time calculation and does not clamp historical dates.
dual_calendar_crossing <- function(log_start, slope, anchor, target) {
  n <- max(length(log_start), length(slope), length(anchor), length(target))
  z <- rep_len(log_start, n); b <- rep_len(slope, n)
  a <- rep_len(anchor, n); t <- rep_len(target, n)
  out <- rep(NA_real_, n)
  valid <- is.finite(z) & is.finite(b) & b != 0 & is.finite(a) &
    is.finite(t) & t > 0
  out[valid] <- a[valid] + (log(t[valid]) - z[valid]) / b[valid]
  out
}

dual_curve_marker <- function(log_start, slope, anchor, tier1, tier2) {
  c1 <- dual_calendar_crossing(log_start, slope, anchor, tier1)
  c2 <- dual_calendar_crossing(log_start, slope, anchor, tier2)
  target <- if (is.na(tier2)) NA_real_ else max(tier1, tier2)
  # Upward crossings do not mark attainment of a concentration <= target.
  label <- if (is.na(tier2)) NA_character_ else if (!is.finite(tier2))
    NA_character_ else if (tier1 == tier2) "Both" else if (tier1 > tier2)
      "Tier 1 first" else "Tier 2 first"
  note <- if (is.na(tier2)) "Tier 2 unavailable: first crossing unresolved" else
    if (!is.finite(tier2)) "Zero-meal reference: no finite threshold intersection" else
      if (slope == 0) "Flat curve: no unique crossing year" else
        if (slope > 0) "Increasing curve: upward crossing is not target attainment" else
          "Modelled downward intersection; may precede the latest sampling year"
  year <- if (is.finite(target) && slope < 0)
    dual_calendar_crossing(log_start, slope, anchor, target) else NA_real_
  data.frame(curve_tier1_crossing_year = c1, curve_tier2_crossing_year = c2,
             curve_first_crossing_year = year, curve_crossing_conc = target,
             curve_first_target = label, curve_crossing_note = note)
}

dual_interval <- function(x) {
  if (all(is.na(x))) return(c(NA_real_, NA_real_))
  if (anyNA(x)) stop("Unexpected partially missing crossing draws.")
  # Retain infinite times: discarding them gives misleadingly finite intervals.
  as.numeric(quantile(x, c(0.025, 0.975), type = 1, names = FALSE))
}

dual_model_inputs <- function(res, cfg, seed) {
  mod <- res$model; dat <- res$data; reps <- res$rep_info
  if (is.null(mod) || is.null(dat) || is.null(reps))
    stop("Expected a species-specific Tier 3C result with model, data and rep_info.")
  if (!all(c("Species", "role", "length_cm") %in% names(reps)))
    stop("rep_info must contain species-specific representative lengths.")
  if (!"year_c" %in% names(coef(mod))) stop("Expected a linear year_c temporal coefficient.")
  if ("region" %in% names(dat) && any(dat$region != "AOC"))
    stop("This update expects an AOC-only temporal fit.")
  if (!identical(attr(mod, "t3c_time_window"), "all_available"))
    stop("Rerun the updated NR_Tier3C_temporal_replacement.R first. ",
         "This object has not been marked as an all-available-years temporal fit.")
  offset <- unique(dat$year - dat$year_c)
  if (length(offset) != 1 || !is.finite(offset)) stop("Cannot determine year centering.")
  anchor <- max(dat$year)
  mf <- model.frame(mod)
  nd <- data.frame(length_cm = reps$length_cm, year_c = anchor - offset)
  if ("Species" %in% names(mf))
    nd$Species <- factor(as.character(reps$Species), levels = levels(mf$Species))
  if ("site_name" %in% names(mf))
    nd$site_name <- factor(levels(mf$site_name)[1], levels = levels(mf$site_name))
  X <- predict(mod, nd, type = "lpmatrix",
               exclude = if ("site_name" %in% names(mf)) "s(site_name)" else NULL)
  V <- if (!is.null(mod$Vc)) mod$Vc else mod$Vp
  V <- (V + t(V)) / 2
  if (any(!is.finite(V))) stop("Nonfinite coefficient covariance.")
  ev <- eigen(V, symmetric = TRUE)
  if (min(ev$values) < -1e-8 * max(1, max(abs(ev$values))))
    stop("Coefficient covariance is not positive semidefinite.")
  set.seed(seed)
  L <- sweep(ev$vectors, 2, sqrt(pmax(ev$values, 0)), "*")
  draws <- sweep(L %*% matrix(rnorm(nrow(V) * cfg$n_draws), nrow(V)), 1, coef(mod), "+")
  j <- match("year_c", names(coef(mod)))
  list(reps = reps, anchor = anchor, eta = as.numeric(X %*% coef(mod)),
       se = sqrt(pmax(0, rowSums((X %*% V) * X))),
       log_draws = X %*% draws, slopes = draws[j, ], slope = unname(coef(mod)[j]),
       covariance = if (!is.null(mod$Vc)) "Smoothing uncertainty corrected" else "Conditional")
}

dual_compare_targets <- function(inputs, t2, reach, population, cfg) {
  rr <- as_tibble(inputs$reps) %>% mutate(Species = as.character(Species),
                                          Population = population,
                                          size_class = dual_size_class(length_cm, t2$size_cols, cfg$size_class_right)) %>%
    left_join(t2$targets %>% filter(Population == population) %>%
                select(Species, Population, size_class, reference_meals, required_meals,
                       n_reference_sites, tier2_target, mapping_note),
              by = c("Species", "Population", "size_class"))
  if (nrow(rr) != nrow(inputs$reps)) stop("Tier 2 join changed the prediction row count.")
  bind_rows(lapply(seq_len(nrow(rr)), function(i) {
    t2target <- rr$tier2_target[i]
    available <- !is.na(t2target)
    pt1 <- dual_crossing(inputs$eta[i], inputs$slope, cfg$tier1_target)
    pt2 <- dual_crossing(inputs$eta[i], inputs$slope, t2target)
    d1 <- dual_crossing(inputs$log_draws[i, ], inputs$slopes, cfg$tier1_target)
    d2 <- dual_crossing(inputs$log_draws[i, ], inputs$slopes, t2target)
    de <- pmin(d1, d2) # Intentionally NA if Tier 2 is unavailable.
    pe <- min(pt1, pt2)
    ci1 <- dual_interval(d1); ci2 <- dual_interval(d2); cie <- dual_interval(de)
    # EITHER target is sufficient. For concentration <= target, the union is
    # concentration <= max(T1,T2), NOT min(T1,T2).
    effective <- if (available) max(cfg$tier1_target, t2target) else NA_real_
    decisive <- if (!available) "Tier 2 unavailable" else if (is.infinite(t2target))
      "Tier 2: zero-meal reference" else if (t2target > cfg$tier1_target)
        "Tier 2" else if (t2target < cfg$tier1_target) "Tier 1" else "Same target"
    outcome <- if (available) {
      if (pe <= cfg$decision_years) "Supportive" else "Unsupportive"
    } else if (pt1 <= cfg$decision_years) "Supportive via Tier 1; Tier 2 unavailable" else
      "Not assessed: Tier 2 unavailable"
    note <- if (is.na(rr$size_class[i])) "Length outside Tier 2 size classes" else
      if (!available) "No reference median for this species, population and size class" else
        rr$mapping_note[i]
    curve_marker <- dual_curve_marker(inputs$eta[i], inputs$slope, inputs$anchor,
                                      cfg$tier1_target, t2target)
    bind_cols(rr[i, ], curve_marker, tibble(reach = reach, anchor_year = inputs$anchor,
                                            predicted_conc = exp(inputs$eta[i]),
                                            conc_lwr = exp(inputs$eta[i] - 1.96 * inputs$se[i]),
                                            conc_upr = exp(inputs$eta[i] + 1.96 * inputs$se[i]),
                                            tier1_target = cfg$tier1_target, effective_target = effective,
                                            less_stringent_target = decisive, comparison_complete = available,
                                            reference_zero_meals = available && rr$reference_meals[i] == 0,
                                            years_to_tier1 = pt1, tier1_year = inputs$anchor + pt1,
                                            tier1_years_lwr = ci1[1], tier1_years_upr = ci1[2],
                                            years_to_tier2 = pt2, tier2_year = inputs$anchor + pt2,
                                            tier2_years_lwr = ci2[1], tier2_years_upr = ci2[2],
                                            first_target = dual_first(pt1, pt2), years_to_either = pe,
                                            first_target_year = inputs$anchor + pe,
                                            either_years_lwr = cie[1], either_years_upr = cie[2],
                                            fraction_draws_tier1_within_horizon = mean(d1 <= cfg$decision_years),
                                            fraction_draws_tier2_within_horizon = mean(d2 <= cfg$decision_years),
                                            fraction_draws_either_within_horizon = mean(de <= cfg$decision_years),
                                            fraction_draws_no_finite_either = if (available) mean(is.infinite(de)) else NA_real_,
                                            decision_horizon_years = cfg$decision_years, outcome = outcome,
                                            target_note = note, covariance = inputs$covariance,
                                            uncertainty_scope = "Model coefficients only; reference median and representative lengths held fixed"))
  }))
}

dual_annotate_plot <- function(res, tab, inputs) {
  if (is.null(res$plot) || is.null(res$predictions)) return(NULL)
  # Keep the original concentration curves and their species-specific lengths.
  # Points are exactly at (calendar crossing year, threshold concentration).
  # Never move an earlier intersection to the latest sampling year. A point
  # outside its species/length curve's displayed range stays in the table only.
  bounds <- res$predictions %>% group_by(Species, role) %>%
    summarise(plot_first = min(year), plot_last = max(year), .groups = "drop")
  marks <- tab %>% left_join(bounds, by = c("Species", "role")) %>%
    filter(is.finite(curve_first_crossing_year), !is.na(curve_first_target),
           curve_first_crossing_year >= plot_first,
           curve_first_crossing_year <= plot_last)
  res$plot + geom_point(data = marks, inherit.aes = FALSE,
                        aes(x = curve_first_crossing_year, y = curve_crossing_conc,
                            colour = role, shape = curve_first_target),
                        size = 2.8, stroke = 1.1) +
    scale_shape_manual(name = "First criterion met", values = c("Tier 1 first" = 16,
                                                                "Tier 2 first" = 17, "Both" = 8))
}

dual_caption <- function(res, population, cfg) paste0(res$caption,
                                                      " Symbols mark the actual modelled downward intersection with the first of the Tier 1 target (",
                                                      cfg$tier1_target, " ng/g) or the PCB-equivalent Tier 2 target for the ", population,
                                                      " population, the same species and the size class containing the representative length. ",
                                                      "Crossing years can precede the latest sampling year and are not clamped to that year. ",
                                                      "These are fitted intersections, not observed dates of advisory change. ",
                                                      "A zero-meal reference imposes no positive-advisory requirement and has no finite intersection marker. ",
                                                      "Increasing or flat curves have no downward-attainment marker. Missing Tier 2 medians remain unresolved. ",
                                                      "Intersections outside each displayed curve's year range are retained in the table without being moved onto the figure. ",
                                                      "Reference advisories are held constant into the future. Intervals exclude uncertainty in reference medians. ",
                                                      "These are PCB-equivalent advisory targets, not measured reference PCB concentrations or predictions of advisories driven by other contaminants.")

# Basic decision-rule checks. They do not replace checks of the fitted models.
dual_rule_checks <- function() {
  m <- dual_meals_to_target(c(4, 8, 16, 10, 0, NA_real_))
  stopifnot(isTRUE(all.equal(m$target, c(211, 105, 53, 70, Inf, NA_real_))))
  b <- log(0.9)
  t1 <- dual_crossing(log(300), b, 105)
  t2 <- dual_crossing(log(300), b, 211)
  stopifnot(t2 < t1, dual_first(t1, t2) == "Tier 2",
            dual_crossing(log(300), b, 53) > t1,
            dual_crossing(log(300), b, Inf) == 0,
            dual_crossing(log(80), 0.02, 105) == 0,
            is.infinite(dual_crossing(log(300), 0.02, 105)),
            is.na(dual_crossing(log(300), b, NA_real_)),
            dual_first(0, 0) == "Both already met",
            dual_first(Inf, Inf) == "Neither under fitted trend")
  bins <- c("15-20 cm", "20-25 cm", "25-30 cm", ">30 cm")
  stopifnot(identical(dual_size_class(c(15, 20, 20.1, 30, 31, 14), bins),
                      c(bins[1], bins[1], bins[2], bins[3], bins[4], NA_character_)))
  # A curve that crossed 105 in 2010 must be marked in 2010 even if the
  # remaining-time calculation at the 2023 anchor is zero.
  z <- log(105) + b * (2023 - 2010)
  historical <- dual_curve_marker(z, b, 2023, 105, 53)
  stopifnot(abs(historical$curve_first_crossing_year - 2010) < 1e-8,
            historical$curve_crossing_conc == 105,
            dual_crossing(z, b, 105) == 0,
            abs(exp(z + b * (historical$curve_first_crossing_year - 2023)) - 105) < 1e-8)
  earlier <- dual_curve_marker(z, b, 2023, 105, 211)
  stopifnot(earlier$curve_first_crossing_year < 2010,
            earlier$curve_first_target == "Tier 2 first",
            abs(exp(z + b * (earlier$curve_first_crossing_year - 2023)) - 211) < 1e-8,
            is.na(dual_curve_marker(z, b, 2023, 105, Inf)$curve_first_crossing_year),
            is.na(dual_curve_marker(z, 0, 2023, 105, 53)$curve_first_crossing_year),
            is.na(dual_curve_marker(z, 0.1, 2023, 105, 53)$curve_first_crossing_year),
            is.na(dual_curve_marker(z, b, 2023, 105, NA_real_)$curve_first_crossing_year))
  invisible(TRUE)
}

# ---- Run without refitting ------------------------------------------------
# To load functions only (e.g. tests), set options(NR.dual_targets.run = FALSE)
# before sourcing this file. The default below performs the complete update.
if (isTRUE(getOption("NR.dual_targets.run", TRUE))) {
  dual_rule_checks()
  if (all(vapply(c("t3c_upper", "t3c_lower"), exists, logical(1), inherits = TRUE))) {
    fitted <- list(Upper = t3c_upper, Lower = t3c_lower)
  } else {
    if (!file.exists(t3c_targets_cfg$fitted_results_path))
      stop("Run the species-specific Tier 3C script once, or set fitted_results_path to its saved results.")
    saved <- readRDS(t3c_targets_cfg$fitted_results_path)
    fitted <- saved[c("Upper", "Lower")]
  }
  if (identical(normalizePath(t3c_targets_cfg$t2_paths[["Upper"]], mustWork = FALSE),
                normalizePath(t3c_targets_cfg$t2_paths[["Lower"]], mustWork = FALSE)))
    stop("Upper and Lower must point to their own Tier 2 files.")
  # Read both Tier 2 files before calculating results for either reach.
  t2_sources <- lapply(c("Upper", "Lower"), function(reach)
    dual_read_t2(t3c_targets_cfg$t2_paths[[reach]], reach, t3c_targets_cfg$populations))
  names(t2_sources) <- c("Upper", "Lower")
  t3c_dual <- list()
  for (reach in c("Upper", "Lower")) {
    inputs <- dual_model_inputs(fitted[[reach]], t3c_targets_cfg,
                                t3c_targets_cfg$seed + match(reach, c("Upper", "Lower")))
    t3c_dual[[reach]] <- list()
    for (pop in t3c_targets_cfg$populations) {
      tab <- dual_compare_targets(inputs, t2_sources[[reach]], reach, pop, t3c_targets_cfg)
      t3c_dual[[reach]][[pop]] <- list(table = tab,
                                       plot = if (t3c_targets_cfg$mark_first_target_on_plots)
                                         dual_annotate_plot(fitted[[reach]], tab, inputs) else fitted[[reach]]$plot,
                                       caption = if (t3c_targets_cfg$mark_first_target_on_plots)
                                         dual_caption(fitted[[reach]], pop, t3c_targets_cfg) else fitted[[reach]]$caption)
    }
  }
  t3c_dual_table <- bind_rows(lapply(t3c_dual, function(reach)
    bind_rows(lapply(reach, function(pop) pop$table))))
  fmt <- function(x) ifelse(is.na(x), "Unavailable",
                            ifelse(is.infinite(x), "No finite crossing", sprintf("%.1f", x)))
  t3c_dual_summary <- t3c_dual_table %>% transmute(
    Reach = reach, Population, Species, `Length group` = role,
    `Length (cm)` = round(length_cm, 1), `Tier 2 size class` = size_class,
    `Reference median (meals/month)` = reference_meals,
    `Tier 2 target (ng/g)` = ifelse(is.infinite(tier2_target),
                                    "No concentration ceiling (0 meals)", as.character(tier2_target)),
    `Years to Tier 1` = fmt(years_to_tier1), `Years to Tier 2` = fmt(years_to_tier2),
    `First criterion met` = first_target, `Years to either` = fmt(years_to_either),
    `95% interval for time to either` = paste(fmt(either_years_lwr), fmt(either_years_upr), sep = " to "),
    `Anchor year` = anchor_year, `Year either is met (anchor or later)` = fmt(first_target_year),
    `Curve crossing year` = fmt(curve_first_crossing_year),
    `Curve crossing target` = curve_first_target,
    `Curve crossing note` = curve_crossing_note,
    Outcome = outcome, Note = target_note)
  print(t3c_dual_summary, n = Inf, width = Inf)
  dir.create(t3c_targets_cfg$out_dir, recursive = TRUE, showWarnings = FALSE)
  write.csv(t3c_dual_table, file.path(t3c_targets_cfg$out_dir, "Tier3C_dual_targets_full.csv"), row.names = FALSE)
  write.csv(t3c_dual_summary, file.path(t3c_targets_cfg$out_dir, "Tier3C_dual_targets_summary.csv"), row.names = FALSE)
  for (reach in names(t3c_dual)) {
    stem <- file.path(t3c_targets_cfg$out_dir, paste0("NR_", reach))
    write.csv(t2_sources[[reach]]$targets, paste0(stem, "_Tier2_target_lookup.csv"), row.names = FALSE)
    writeLines(t2_sources[[reach]]$reference_sites, paste0(stem, "_Tier2_reference_sites.txt"))
    for (pop in names(t3c_dual[[reach]])) {
      ans <- t3c_dual[[reach]][[pop]]
      if (!is.null(ans$plot)) {
        ggsave(paste0(stem, "_", pop, "_dual_targets.png"), ans$plot, width = 12,
               height = 1.7 + 2.7 * ceiling(n_distinct(ans$table$Species) / 3), dpi = 300, bg = "white")
        writeLines(ans$caption, paste0(stem, "_", pop, "_caption.txt"))
      }
    }
  }
  saveRDS(list(config = t3c_targets_cfg, results = t3c_dual, table = t3c_dual_table,
               summary = t3c_dual_summary, tier2_sources = t2_sources),
          file.path(t3c_targets_cfg$out_dir, "Tier3C_dual_targets.rds"))
  cat("\nFull comparison: t3c_dual_table\nReadable comparison: t3c_dual_summary\n",
      "Example figure: t3c_dual$Lower$General$plot\n",
      "Example caption: cat(t3c_dual$Lower$General$caption)\n", sep = "")
}

# Interpretation:
# - For EITHER criterion, use the HIGHER concentration target (max), not min.
# - Crossing times count forward from the latest AOC sampling year in the fit.
#   Plot markers instead use the actual fitted intersection year, including
#   historical dates. The separate curve_* columns record those intersections.
# - Zero means already met at that anchor, not sustained attainment or safety.
# - A zero-meal reference gives immediate Tier 2 attainment under a literal
#   "at least the reference advisory" rule. Do not describe this as recovery.
# - A missing reference median is not the same as a zero-meal reference.
# - Confidence intervals propagate correlated model coefficients through BOTH
#   crossing times and their minimum; reference-target uncertainty is excluded.
# - Reference targets are fixed at the saved Tier 2 assessment values. This
#   does not predict when a changing future reference population will be matched.
# - PCB equivalents of meal categories are not actual reference concentrations;
#   t2_prep has no contaminant-driver field. Other contaminants can limit meals.
# - No across-species recovery verdict is assigned from these individual rows.

