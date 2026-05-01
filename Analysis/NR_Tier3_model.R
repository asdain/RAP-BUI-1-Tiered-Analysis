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
  "Brown Trout",
  "Freshwater Drum",
  "Largemouth Bass",
  "Rainbow Trout",
  "Walleye",
  "White Perch"
)

lr_species = c(
  "Rock Bass",
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

# Too many species without much data in upper -> data is pulled to reference. hard to interpret. Should limit species with decent # of obs




## Limited indicator species approach -------------

indicator_spec <- c(
  "Yellow Perch",
  "Smallmouth Bass",
  "Rainbow Trout",
  "Freshwater Drum",
  "Lake Trout",
  "Rock Bass"
)

niagara_upper_ind <- niagara_upper_dat %>%
  filter(Species %in% indicator_spec) %>%
  group_by(Species) %>%
  filter(any(region == "AOC")) %>%
  ungroup() %>%
  droplevels()

niagara_lower_ind <- niagara_lower_dat %>%
  filter(Species %in% indicator_spec) %>%
  group_by(Species) %>%
  filter(any(region == "AOC")) %>%
  ungroup() %>%
  droplevels()


m_upper_ind <- gam(
  log_ratio ~
    region +
    s(year, k = 10) +
    s(length_cm, k = 6) +
    Species +
    s(site_name, bs = "re"),
  data = niagara_upper_ind,
  method = "REML",
  family = scat()
)

m_lower_ind <- gam(
  log_ratio ~
    region +
    s(year, k = 10) +
    s(length_cm, k = 6) +
    Species +
    s(site_name, bs = "re") +
    te(long, lat, k = c(8, 8)),
  data = niagara_lower_ind,
  method = "REML",
  family = scat()
)

summary(m_upper_ind)


summary(m_lower_ind)




## Prediction grids for recent years -------------

library(dplyr)
library(tidyr)

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

pred_upper <- predict(
  m_upper_ind,
  newdata = pred_grid_upper,
  se.fit = TRUE,
  exclude = "s(site_name)"
)

pred_grid_upper <- pred_grid_upper %>%
  mutate(
    log_ratio = pred_upper$fit,
    se = pred_upper$se.fit
  )

summary_upper <- pred_grid_upper %>%
  group_by(region) %>%
  summarise(
    mean_log_ratio = mean(log_ratio),
    se_log_ratio = sd(log_ratio) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    ratio = exp(mean_log_ratio),
    lower = exp(mean_log_ratio - 2 * se_log_ratio),
    upper = exp(mean_log_ratio + 2 * se_log_ratio)
  )

upper_diff <- summary_upper %>%
  tidyr::pivot_wider(names_from = region, values_from = mean_log_ratio) %>%
  mutate(
    diff_log = AOC - Reference,
    diff_ratio = exp(diff_log)
  )


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

pred_lower <- predict(
  m_lower_ind,
  newdata = pred_grid_lower,
  se.fit = TRUE,
  exclude = "s(site_name)"
)

pred_grid_lower <- pred_grid_lower %>%
  mutate(
    log_ratio = pred_lower$fit,
    se = pred_lower$se.fit
  )

summary_lower <- pred_grid_lower %>%
  group_by(region) %>%
  summarise(
    mean_log_ratio = mean(log_ratio),
    se_log_ratio = sd(log_ratio) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    ratio = exp(mean_log_ratio),
    lower = exp(mean_log_ratio - 2 * se_log_ratio),
    upper = exp(mean_log_ratio + 2 * se_log_ratio)
  )



plot_colours = c(AOC = "red", Reference = "royalblue")

# use observed AOC lengths from the upper indicator dataset
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

pred_upper <- predict(
  m_upper_ind,
  newdata = pred_grid_upper,
  se.fit = TRUE,
  exclude = "s(site_name)"
)

pred_upper_df <- pred_grid_upper %>%
  mutate(
    log_ratio = pred_upper$fit,
    se_fit = pred_upper$se.fit
  ) %>%
  group_by(region, length_cm) %>%
  summarise(
    log_ratio = mean(log_ratio),
    se = sqrt(mean(se_fit^2)),
    .groups = "drop"
  ) %>%
  mutate(
    ratio = exp(log_ratio),
    lower = exp(log_ratio - 2 * se),
    upper = exp(log_ratio + 2 * se)
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
    subtitle = "Predictions averaged over 2006–2024 and indicator species",
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

pred_lower <- predict(
  m_lower_ind,
  newdata = pred_grid_lower,
  se.fit = TRUE,
  exclude = c("s(site_name)", "te(long,lat)")
)

pred_lower_df <- pred_grid_lower %>%
  mutate(
    log_ratio = pred_lower$fit,
    se_fit = pred_lower$se.fit
  ) %>%
  group_by(region, length_cm) %>%
  summarise(
    log_ratio = mean(log_ratio),
    se = sqrt(mean(se_fit^2)),
    .groups = "drop"
  ) %>%
  mutate(
    ratio = exp(log_ratio),
    lower = exp(log_ratio - 2 * se),
    upper = exp(log_ratio + 2 * se)
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
    subtitle = "Predictions averaged over 2006–2024 and indicator species",
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


# Adjusted site predictions

# Upper

upper_newdata <- niagara_upper_ind %>%
  group_by(site_name) %>%
  summarise(
    region = first(region),
    year = max(recent_years),
    length_cm = median(length_cm, na.rm = TRUE),
    Species = first(Species),
    .groups = "drop"
  ) %>%
  mutate(
    site_name = factor(site_name, levels = levels(niagara_upper_ind$site_name)),
    region = factor(region, levels = levels(niagara_upper_ind$region)),
    Species = factor(Species, levels = levels(niagara_upper_ind$Species))
  )

upper_pred <- predict(m_upper_ind, upper_newdata, se.fit = TRUE)

upper_site_pred <- upper_newdata %>%
  mutate(
    log_fit = upper_pred$fit,
    log_se  = upper_pred$se.fit,
    ratio_fit = exp(log_fit),
    lower = exp(log_fit - 2 * log_se),
    upper = exp(log_fit + 2 * log_se),
    site_name = str_to_title(as.character(site_name))
  ) %>%
  left_join(
    niagara_upper_ind %>% count(site_name, name = "n") %>%
      mutate(site_name = str_to_title(as.character(site_name))),
    by = "site_name"
  ) %>%
  arrange(desc(ratio_fit))

# Lower

lower_newdata <- niagara_lower_ind %>%
  group_by(site_name) %>%
  summarise(
    region = first(region),
    year = max(recent_years),
    length_cm = median(length_cm, na.rm = TRUE),
    Species = first(Species),
    long = median(long, na.rm = TRUE),
    lat  = median(lat, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    site_name = factor(site_name, levels = levels(niagara_lower_ind$site_name)),
    region = factor(region, levels = levels(niagara_lower_ind$region)),
    Species = factor(Species, levels = levels(niagara_lower_ind$Species))
  )

lower_pred <- predict(m_lower_ind, lower_newdata, se.fit = TRUE)

lower_site_pred <- lower_newdata %>%
  mutate(
    log_fit = lower_pred$fit,
    log_se  = lower_pred$se.fit,
    ratio_fit = exp(log_fit),
    lower = exp(log_fit - 2 * log_se),
    upper = exp(log_fit + 2 * log_se),
    site_name = str_to_title(as.character(site_name))
  ) %>%
  left_join(
    niagara_lower_ind %>% count(site_name, name = "n") %>%
      mutate(site_name = str_to_title(as.character(site_name))),
    by = "site_name"
  ) %>%
  arrange(desc(ratio_fit))


# Overall reference line (mean predicted log_ratio)

overall_upper <- exp(mean(predict(m_upper_ind, type = "link")))
overall_lower <- exp(mean(predict(m_lower_ind, type = "link")))


# "Elevated" flag -> above consumption threshold (log_ratio = 1)
upper_site_pred <- upper_site_pred %>%
  mutate(elevated = lower > 1)

lower_site_pred <- lower_site_pred %>%
  mutate(elevated = lower > 1)


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




# Temporal trends ------------
library(mgcv)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(broom)

# Fit aoc-only temporal projection models


m_t3c_pcb_upper <- gam(
  log_conc ~
    year +
    s(length_cm, k = 6) +
    Species +
    s(site_name, bs = "re"),
  data = niagara_upper_dat,
  method = "REML",
  family = scat()
)

m_t3c_pcb_lower <- gam(
  log_conc ~
    year +
    s(length_cm, k = 6) +
    Species +
    s(site_name, bs = "re"),
  data = niagara_lower_dat,
  method = "REML",
  family = scat()
)

# Exctract decline rate and half-life
extract_t3c_rate <- function(mod, label) {
  sm <- summary(mod)
  coefs <- as.data.frame(sm$p.table)
  coefs$term <- rownames(coefs)
  
  yr <- coefs %>% filter(term == "year")
  if (nrow(yr) != 1) stop("Could not find parametric year term.")
  
  slope <- yr$Estimate
  slope_se <- yr$`Std. Error`
  k <- -slope
  
  tibble(
    river_section = label,
    slope = slope,
    slope_se = slope_se,
    k = k,
    half_life_years = ifelse(k > 0, log(2) / k, NA_real_),
    outcome = case_when(
      is.na(k) ~ "Unsupportive",
      k <= 0 ~ "Unsupportive",
      half_life_years > 10 ~ "Unsupportive",
      TRUE ~ "Supportive"
    )
  )
}

hl_tab <- bind_rows(
  extract_t3c_rate(m_t3c_pcb_upper, "Upper"),
  extract_t3c_rate(m_t3c_pcb_lower, "Lower")
)

hl_tab



## Make temporal trends plot -------

round5 <- function(x) round(x / 5) * 5

get_rep_lengths_quartiles <- function(dat, probs = c(0.25, 0.5, 0.75)) {
  qs <- quantile(dat$length_cm, probs = probs, na.rm = TRUE)
  tibble(
    role = c("Lower quartile", "Median", "Upper quartile")[seq_along(qs)],
    length_cm = round5(as.numeric(qs))
  )
}

rep_info_upper <- get_rep_lengths_quartiles(niagara_upper_dat)
rep_info_lower <- get_rep_lengths_quartiles(niagara_lower_dat)

make_observed_t3c_plot_pcb <- function(
    mod,
    dat,
    threshold = 105,
    label = "Upper River"
) {
  year_seq <- seq(min(dat$year, na.rm = TRUE), max(dat$year, na.rm = TRUE), by = 1)
  
  ref_species <- names(sort(table(dat$Species), decreasing = TRUE))[1]
  ref_site <- names(sort(table(dat$site_name), decreasing = TRUE))[1]
  
  newdata <- tibble(
    year = year_seq,
    length_cm = median(dat$length_cm, na.rm = TRUE),
    Species = ref_species,
    site_name = ref_site
  )
  
  pr <- predict(
    mod,
    newdata = newdata,
    type = "link",
    se.fit = TRUE,
    exclude = "s(site_name)"
  )
  
  plot_df <- newdata %>%
    mutate(
      fit = as.numeric(pr$fit),
      se  = as.numeric(pr$se.fit),
      conc = exp(fit),
      lwr = exp(fit - 1.96 * se),
      upr = exp(fit + 1.96 * se)
    )
  
  ggplot(plot_df, aes(x = year, y = conc)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.15) +
    geom_line(linewidth = 1) +
    geom_hline(yintercept = threshold, linetype = "dashed") +
    labs(
      x = "Year",
      y = "Predicted PCB concentration (ng/g)",
      title = paste("Observed temporal trend -", label)
    ) +
    theme_classic()
}

p_t3c_obs_upper <- make_observed_t3c_plot_pcb(
  m_t3c_pcb_upper,
  niagara_upper_dat,
  threshold = 105,
  label = "Upper River"
)

p_t3c_obs_lower <- make_observed_t3c_plot_pcb(
  m_t3c_pcb_lower,
  niagara_lower_dat,
  threshold = 105,
  label = "Lower River"
)


compute_years_to_threshold_pcb <- function(
    mod,
    hl_tab,
    dat,
    rep_lengths_cm,
    threshold = 105,
    label = "Upper"
) {
  year_anchor <- max(dat$year, na.rm = TRUE)
  
  ref_species <- names(sort(table(dat$Species), decreasing = TRUE))[1]
  ref_site    <- names(sort(table(dat$site_name), decreasing = TRUE))[1]
  
  k <- hl_tab$k[1]
  slope_se <- hl_tab$slope_se[1]
  log_target <- log(threshold)
  
  map_dfr(rep_lengths_cm, function(len_cm) {
    newdata <- tibble(
      year = year_anchor,
      length_cm = len_cm,
      Species = ref_species,
      site_name = ref_site
    )
    
    pr <- predict(
      mod,
      newdata = newdata,
      type = "link",
      se.fit = TRUE,
      exclude = "s(site_name)"
    )
    
    pred_log  <- as.numeric(pr$fit)
    se_log    <- as.numeric(pr$se.fit)
    pred_conc <- exp(pred_log)
    
    years_to_target <- case_when(
      pred_conc <= threshold ~ 0,
      is.finite(k) & k > 0   ~ (pred_log - log_target) / k,
      TRUE                   ~ NA_real_
    )
    
    se_t <- if (is.finite(years_to_target) && !is.na(years_to_target) && k > 0) {
      term1 <- (1 / k)^2 * se_log^2
      term2 <- (years_to_target / k)^2 * slope_se^2
      sqrt(term1 + term2)
    } else {
      NA_real_
    }
    
    tibble(
      river_section   = label,
      length_cm       = len_cm,
      predicted_conc  = pred_conc,
      target_conc     = threshold,
      years_to_target = years_to_target,
      years_lwr       = ifelse(is.na(se_t), NA_real_, years_to_target - 1.96 * se_t),
      years_upr       = ifelse(is.na(se_t), NA_real_, years_to_target + 1.96 * se_t),
      outcome = case_when(
        !is.na(years_to_target) && years_to_target == 0 ~ "Supportive",
        is.na(years_to_target) | k <= 0 ~ "Unsupportive",
        years_to_target > 10 ~ "Unsupportive",
        TRUE ~ "Supportive"
      )
    )
  })
}

t3c_upper_tbl <- compute_years_to_threshold_pcb(
  mod = m_t3c_pcb_upper,
  hl_tab = hl_tab %>% filter(river_section == "Upper"),
  dat = niagara_upper_dat,
  rep_lengths_cm = rep_info_upper$length_cm,
  threshold = 105,
  label = "Upper"
) %>%
  left_join(rep_info_upper, by = "length_cm") %>%
  relocate(role, .before = length_cm)

t3c_lower_tbl <- compute_years_to_threshold_pcb(
  mod = m_t3c_pcb_lower,
  hl_tab = hl_tab %>% filter(river_section == "Lower"),
  dat = niagara_lower_dat,
  rep_lengths_cm = rep_info_lower$length_cm,
  threshold = 105,
  label = "Lower"
) %>%
  left_join(rep_info_lower, by = "length_cm") %>%
  relocate(role, .before = length_cm)

t3c_sensitivity_tbl <- bind_rows(t3c_upper_tbl, t3c_lower_tbl)

t3c_sensitivity_tbl

t3c_summary_table <- t3c_sensitivity_tbl %>%
  mutate(
    `Predicted PCB (current year)` = sprintf("%.1f", predicted_conc),
    `Threshold (ng/g)` = sprintf("%.0f", target_conc),
    `Years to 105 ng/g` = if_else(
      is.na(years_to_target), "—", sprintf("%.1f", years_to_target)
    ),
    `95% CI` = case_when(
      is.na(years_lwr) | is.na(years_upr) ~ "—",
      TRUE ~ sprintf("%.1f–%.1f", years_lwr, years_upr)
    )
  ) %>%
  select(
    River = river_section,
    `Representative group` = role,
    `Length (cm)` = length_cm,
    `Predicted PCB (current year)`,
    `Threshold (ng/g)`,
    `Years to 105 ng/g`,
    `95% CI`,
    Outcome = outcome
  )

t3c_summary_table

saveRDS(t3c_summary_table, "Derived/NR/Tier3/t3c_summary_table.rds")



colour_vals <- c(
  "Lower quartile" = "#696969",
  "Median"         = "black",
  "Upper quartile" = "#ADADAD"
)

plot_t3c_future_multi_pcb <- function(
    mod,
    dat,
    rep_info,
    threshold = 105,
    cut_info = NULL,
    cut_role = "Upper quartile",
    label = "Upper River",
    horizon_years = 20
) {
  min_year <- min(dat$year, na.rm = TRUE)
  max_year <- max(dat$year, na.rm = TRUE)
  anchor_year <- max_year
  
  ref_species <- names(sort(table(dat$Species), decreasing = TRUE))[1]
  ref_site    <- names(sort(table(dat$site_name), decreasing = TRUE))[1]
  
  cut_year <- NA_real_
  if (!is.null(cut_info)) {
    ytt <- cut_info %>%
      filter(role == cut_role) %>%
      pull(years_to_target)
    
    if (length(ytt) == 1 && is.finite(ytt) && !is.na(ytt) && ytt > 0) {
      cut_year <- anchor_year + ytt
    }
  }
  
  end_year <- if (is.finite(cut_year)) ceiling(cut_year) else max_year + horizon_years
  proj_years <- seq(min_year, end_year, by = 1)
  
  pred_df <- map_dfr(seq_len(nrow(rep_info)), function(i) {
    len_cm <- rep_info$length_cm[i]
    role   <- rep_info$role[i]
    
    newdata <- tibble(
      year = proj_years,
      length_cm = len_cm,
      Species = ref_species,
      site_name = ref_site
    )
    
    pr <- predict(
      mod,
      newdata = newdata,
      type = "link",
      se.fit = TRUE,
      exclude = "s(site_name)"
    )
    
    newdata %>%
      mutate(
        role = role,
        fit = as.numeric(pr$fit),
        se = as.numeric(pr$se.fit),
        conc = exp(fit),
        lwr = exp(fit - 1.96 * se),
        upr = exp(fit + 1.96 * se),
        period = if_else(year <= max_year, "Observed", "Projected")
      )
  })
  
  ggplot(pred_df, aes(x = year, y = conc, colour = role)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = role), alpha = 0.12, colour = NA) +
    geom_line(aes(linetype = period), linewidth = 1) +
    geom_hline(yintercept = threshold, linetype = "dotted") +
    scale_colour_manual(values = colour_vals) +
    scale_x_continuous(breaks = seq(1975,2050, 5)) +
    labs(
      x = "Year",
      y = "Predicted PCB concentration (ng/g)",
      title = paste("Projected time to 105 ng/g PCB threshold -", label),
      colour = "Representative length",
      fill = "Representative length",
      linetype = ""
    ) +
    theme_classic()
}

p_t3c_proj_upper <- plot_t3c_future_multi_pcb(
  mod = m_t3c_pcb_upper,
  dat = niagara_upper_dat,
  rep_info = rep_info_upper,
  threshold = 105,
  cut_info = t3c_upper_tbl,
  cut_role = "Upper quartile",
  label = "Upper River"
)

p_t3c_proj_lower <- plot_t3c_future_multi_pcb(
  mod = m_t3c_pcb_lower,
  dat = niagara_lower_dat,
  rep_info = rep_info_lower,
  threshold = 105,
  cut_info = t3c_lower_tbl,
  cut_role = "Upper quartile",
  label = "Lower River"
)

p_t3c_proj_upper
p_t3c_proj_lower

ggsave("Derived/NR/Tier3/LR/lr_pcb_temp_plot.png", p_t3c_proj_lower, dpi = 300, height = 8, width = 12)
ggsave("Derived/NR/Tier3/UR/ur_pcb_temp_plot.png", p_t3c_proj_upper, dpi = 300, height = 8, width = 12)
