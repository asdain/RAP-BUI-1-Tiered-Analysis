# Tier 3 initial data exploration

source("Scripts/setup.R")

raw_data = read.csv("Data/Great Lakes Data to Ken 2024-12 PCB-Hg(Data).csv")

glimpse(raw_data)

recent_data = raw_data %>%
  filter(Sample.Year >= 2014)


## Loading shapefiles - AOC, Lake Ontario, SLR sections
aoc_path = "Tier3/Data/SLR_AOC"
ontario_path = "Tier3/Data/hydro_p_LakeOntario"
lsl_path = "Tier3/Data/lakestlawrence"

aoc_shp = st_read(aoc_path)
print(aoc_shp)
plot(st_geometry(aoc_shp), col = "lightblue", main = "SLR (Ontario)")

on_shp = st_read(ontario_path)
plot(st_geometry(on_shp), col = "lightblue")

target_sr = 4326 # Set spatial reference here; 4326 = WGS 84

lsl_shp = st_read(lsl_path) %>%
  st_transform(lsl_shp, crs = target_sr)
ti_shp = st_read("Tier3/Data/thousislands") %>% st_transform(ti_shp, crs = target_sr)
lsf_shp = st_read("Tier3/Data/lsf") %>% st_transform(lsf_shp, crs = target_sr)
cornwall_shp = st_read("Tier3/Data/cornwall") %>% st_transform(cornwall_shp, crs = target_sr)
brockville_shp = st_read("Tier3/Data/brockville") %>% st_transform(brockville_shp, crs = target_sr)

aoc_shp = st_transform(aoc_shp, crs = target_sr) %>% st_make_valid() %>% summarise()
on_shp = st_transform(on_shp, crs = target_sr) %>% st_make_valid() %>% summarise()


# Combining SLR sections and subtracting AOC
river_sections = rbind(lsl_shp, ti_shp, lsf_shp, cornwall_shp, brockville_shp)

river_union = river_sections %>%
  st_make_valid() %>%
  summarise()

slr_ref = st_difference(river_union, st_union(aoc_shp))

full_union = rbind(slr_ref, aoc_shp, on_shp) %>%
  st_make_valid() %>%
  summarise()

plot(st_geometry(slr_ref), col = "blue", extent = full_union)
plot(st_geometry(aoc_shp), col = "red", add = T)
plot(st_geometry(on_shp), col = "lightblue", alpha = 0.8, add = T)
# Filtering for mercury data
hg_data = recent_data %>%
  filter(Contaminant == "MERCURY")

# Creating references to data points in AOC vs Lake Ontario
hg_pts = hg_data %>%
  filter(!is.na(Latitude.Decimal) & !is.na(Longitude.Decimal)) %>%
  st_as_sf(coords = c("Longitude.Decimal", "Latitude.Decimal"), crs = 4326)

aoc_shp <- st_transform(aoc_shp, crs = st_crs(hg_pts))

aoc_df = hg_pts[st_intersects(hg_pts, aoc_shp, sparse = FALSE),]

slr_df = hg_pts[st_intersects(hg_pts, slr_ref, sparse = FALSE),]

on_df = hg_pts[st_intersects(hg_pts, on_shp, sparse = FALSE),]






plot(st_geometry(aoc_shp), col="lightblue")
plot(st_geometry(aoc_df), col = "darkred", add = TRUE)

st_geometry(hg_pts)
st_geometry(aoc_shp)

plot(st_geometry(slr_ref), col = "cyan")
plot(st_geometry(aoc_shp), col = "lightgreen", add = T)
plot(st_geometry(on_shp), col = "lightblue", alpha = 0.8, add = T)
plot(st_geometry(walleye_df), pch = 1,col = "darkred", lwd = 3, add = T)



# Alternatively, filtering ALL sites by Lake Ontario/SLR in the name
on_df_full = hg_data %>%
  filter(grepl("Lake Ontario", Locname.Fishbase)) 

slr_df_full = hg_data %>%
  filter(grepl("St. Lawrence River",Locname.Fishbase))

on_df_full$region = "Lake Ontario"
slr_df_full$region = "St. Lawrence River"

full_df =
  bind_rows(on_df_full, slr_df_full) %>%
  mutate(site = Locname.Fishbase) %>%
  filter(Specname %in% aoc_species) %>%
  mutate(region = as.factor(region))

full_df_walleye = full_df %>%
  filter(Specname == "Walleye")


ontario_names = full_df_walleye %>%
  filter(grepl("Lake Ontario", site)) %>%
  distinct(site) %>%
  arrange(site) %>%
  pull(site)

print(ontario_names)

slr_names = full_df_walleye %>%
  filter(grepl("St. Lawrence River", site)) %>%
  distinct(site) %>%
  arrange(site) %>%
  pull(site)
slr_names

site_order <- full_df_walleye %>%
  group_by(site) %>%
  summarise(median_hg = median(Value, na.rm = TRUE)) %>%
  arrange(median_hg) %>%
  pull(site)

full_df_walleye = full_df_walleye %>%
  mutate(site = factor(site, levels = site_order))


# Initial visualization - walleye Hg concentrations in AOC vs LO
aoc_df$region = "AOC"
on_df$region = "Lake Ontario"
slr_df$region = "St. Lawrence River"

aoc_species = unique(aoc_df$Specname)

all_df = bind_rows(slr_df, on_df, aoc_df) %>%
  mutate(site = Locname.Fishbase) %>%
  filter(Specname %in% aoc_species) %>%
  mutate(region = as.factor(region))



walleye_df <- all_df %>%
  filter(grepl("Walleye", Specname, ignore.case = TRUE))



site_order <- walleye_df %>%
  group_by(site) %>%
  summarise(median_hg = median(Value, na.rm = TRUE)) %>%
  arrange(median_hg) %>%
  pull(site)

walleye_df = walleye_df %>%
  mutate(site = factor(site, levels = site_order))


# Average Hg concentration in walleye, by zone
ggplot(walleye_df, aes(x = site, y = Value, fill = region)) +
  geom_boxplot(outlier.size = 0.8, alpha = 0.7) +
  scale_fill_manual(values = c("AOC" = "red", "Lake Ontario" = "grey", "St. Lawrence River" = "lightblue")) +
  labs(
    title = "Mercury in Walleye by Site",
    x = "Site",
    y = "Mercury (µg/g)"
  ) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(
      face = ifelse(levels(walleye_df$site) %in% unique(aoc_df$Locname.Fishbase), "bold", "plain"),
      color = ifelse(levels(walleye_df$site) %in% unique(aoc_df$Locname.Fishbase), "red", "black")
    ) 
  ) 



# Hg concentration of walleye by size
ggplot(walleye_df, aes(x = Length, y = Value, color = region)) +
  geom_point(alpha = 0.9) +
  geom_smooth(method = "gam", se = TRUE, linewidth = 1.2) +
  labs(
    title = "Contaminant Concentration vs. Fish Length",
    x = "Length (mm)",
    y = "Mercury (µg/g)"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("AOC" = "red", "St. Lawrence River" = "magenta", "Lake Ontario" = "cyan"))


# GAM
library(mgcv)
gam_fit <- gam(Value ~ s(Length, by = region, bs = "cs") + region, data = walleye_df)
summary(gam_fit)

# Virtual advisory for Hg in walleye



library(purrr)


# Power model: Hg = a * length^b => log(Hg) = log(a) + b * log(length)
fit_models <- walleye_df %>%
  filter(Value > 0, Length > 0) %>%
  group_by(region) %>%
  nest() %>%
  mutate(model = map(data, ~lm(log(Value) ~ log(Length), data = .x)))

# Create prediction intervals
length_grid <- tibble(Length = seq(0, 80, by = 5))

predictions <- fit_models %>%
  select(region, model) %>%
  mutate(length_grid = list(length_grid)) %>%
  unnest(length_grid) %>%
  mutate(
    predicted_hg = map2_dbl(model, Length, ~ exp(predict(.x, newdata = tibble(Length = .y))))
  )

assign_advisory <- function(hg, population = "General") {
  if (population == "Sensitive") {
    case_when(
      hg > 0.5       ~ 0,
      hg > 0.25      ~ 0,   # sensitive turns 1/2 into 0
      hg > 0.16      ~ 4,
      hg > 0.12      ~ 8,
      hg > 0.06      ~ 12,
      TRUE           ~ 32
    )
  } else {
    case_when(
      hg > 1.8       ~ 0,
      hg > 1.2       ~ 1,
      hg > 0.6       ~ 2,
      hg > 0.4       ~ 4,
      hg > 0.3       ~ 8,
      hg > 0.15      ~ 12,
      hg > 0.0       ~ 16,
      TRUE           ~ 32
    )
  }
}



predictions <- predictions %>%
  mutate(
    meals_sensitive = assign_advisory(predicted_hg, "Sensitive"),
    meals_general   = assign_advisory(predicted_hg, "General")
  )

library(ggplot2)

ggplot(predictions, aes(x = Length, y = predicted_hg, color = region)) +
  geom_point(data= walleye_df, aes(x = Length, y = Value, color = region), size = 1.5, alpha = 0.7) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = c(0.5, 0.25, 0.16, 0.06), linetype = "dashed", color = "black") +
  labs(
    title = "Virtual Mercury Advisories in Walleye",
    subtitle = "Predicted Hg concentration vs Length",
    x = "Length (cm)",
    y = "Hg concentration (µg/g)"
  ) +
  theme_minimal(base_size = 13) + annotate("text", x = 80, y = 0.52, label = "0 meals", hjust = 1, size = 3.5, color = "black") +
  scale_colour_manual(values = c("AOC" = "red", "St. Lawrence River" = "magenta", "Lake Ontario" = "cyan")) +
  annotate("text", x = 80, y = 0.26, label = "4 meals", hjust = 1, size = 3.5, color = "black") +
  annotate("text", x = 80, y = 0.17, label = "8 meals", hjust = 1, size = 3.5, color = "black") +
  annotate("text", x = 80, y = 0.065, label = "12 meals", hjust = 1, size = 3.5, color = "black")

summary(walleye_df$Value)
library(janitor)

predictions_binned <- predictions %>%
  mutate(
    length_bin = cut(Length,
                     breaks = c(seq(15, 75, by = 5), Inf),
                     labels = c(paste(seq(15, 70, by = 5), seq(20, 75, by = 5), sep = "-"), "75+"),
                     right = FALSE)
  ) %>%
  filter(!is.na(length_bin)) %>%
  group_by(region, length_bin) %>%
  summarise(
    meals_general = round(mean(meals_general, na.rm = TRUE)),
    meals_sensitive = round(mean(meals_sensitive, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  arrange(region, length_bin)

library(knitr)
kable(predictions_binned, caption = "Virtual Fish Consumption Advisory by Region and Length Bin")

library(reactable)

reactable(
  predictions_binned,
  striped = TRUE,
  bordered = TRUE,
  highlight = TRUE,
  defaultSorted = list(length_bin = "asc"),
  columns = list(
    region = colDef(name = "Region"),
    length_bin = colDef(name = "Length Bin"),
    meals_general = colDef(name = "Meals (General)"),
    meals_sensitive = colDef(name = "Meals (Sensitive)")
  ),
  theme = reactableTheme(
    headerStyle = list(background = "#f0f0f0", fontWeight = "bold"),
    cellPadding = "6px 12px"
  )
)

virtual_long <- predictions_binned %>%
  pivot_longer(cols = c(meals_general, meals_sensitive),
               names_to = "Population",
               names_prefix = "meals_",
               values_to = "Advisory") %>%
  mutate(
    Species = "Walleye",
    data_type = "Virtual",
    Population = case_when(
      Population == "general" ~ "General",
      Population == "sensitive" ~ "Sensitive"
    ),
    adv_cause = "Mercury"
  )


virtual_wide <- predictions_binned %>%
  pivot_longer(
    cols = c(meals_general, meals_sensitive),
    names_to = "Population",
    names_prefix = "meals_",
    values_to = "Advisory"
  ) %>%
  mutate(
    Population = case_when(
      Population == "general" ~ "General",
      Population == "sensitive" ~ "Sensitive"
    )
  ) %>%
  unite("Row_Label", region, Population, sep = " - ") %>%
  pivot_wider(
    names_from = length_bin,
    values_from = Advisory
  )

color_advisory_cell <- function(value) {
  if (is.na(value)) return("")
  
  if (value < 8) {
    color <- colorRampPalette(c("red", "red"))(9)[value + 1]
  } else {
    # 9+ values: 9–32 → index 1–24
    idx <- min(value - 7, 24)
    color <- colorRampPalette(c("forestgreen", "forestgreen"))(24)[idx]
  }
  
  list(background = color, color = "black", fontWeight = "bold")
}



reactable(
  virtual_wide,
  bordered = TRUE,
  highlight = TRUE,
  striped = TRUE,
  defaultColDef = colDef(
    style = function(value) color_advisory_cell(value),
    align = "center"
  ),
  columns = list(
    Row_Label = colDef(name = "Region & Population", sticky = "left", style = list(fontWeight = "bold"))
  )
)


# Plot all species
# Filter data by species with >10 samples in AOC
species_counts <- aoc_df %>%
  group_by(Specname) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n >= 10) %>%
  pull(Specname)

all_df_n = all_df %>%
  filter(Specname %in% species_counts)

ggplot(all_df_n, aes(x = Length, y = Value, color = region)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "gam", se = FALSE) +
  facet_wrap(~Specname, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Length vs. Mercury for AOC Species (≥10 AOC Samples)",
    x = "Length (mm)",
    y = "Mercury (µg/g)"
  )


# Plot using standardized sizes after power regression
model_data <- walleye_df %>%
  filter(Value > 0, Length > 0) %>%
  group_by(site) %>%
  nest() %>%
  mutate(model = map(data, ~ lm(log(Value) ~ log(Length), data = .x)))


# 45 cm walleye
standard_length <- 45

predictions_45 <- model_data %>%
  mutate(
    pred_hg = map_dbl(model, ~ exp(predict(.x, newdata = tibble(Length = standard_length))))
  ) %>%
  select(site, pred_hg) %>%
  left_join(distinct(walleye_df, site, region), by = "site")


# Plot of predicted concentration of 45 cm walleye
ggplot(predictions_45, aes(x = reorder(site, pred_hg), y = pred_hg, fill = region)) +
  geom_point(width = 0.6) +
  coord_flip() +
  scale_fill_manual(values = c("AOC" = "red", "Lake Ontario" = "grey", "St. Lawrence River" = "lightblue")) +
  labs(
    title = "Predicted Mercury in Walleye (Standardized at 45 cm)",
    subtitle = "Based on site-specific power regressions",
    x = "Site",
    y = "Predicted Hg (µg/g)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(
      face = ifelse(levels(factor(predictions_45$site)) %in% unique(aoc_df$Locname.Fishbase), "bold", "plain"),
      color = ifelse(levels(factor(predictions_45$site)) %in% unique(aoc_df$Locname.Fishbase), "red", "black")
    )
  )


# (Not very informative, IMO)


# Boxplot of walleye at 40-50cm only
# Filter walleye to a consistent size range
walleye_std <- walleye_df %>%
  filter(Length >= 40, Length <= 50)

# Boxplot of actual Hg values for that size class
ggplot(walleye_std, aes(x = site, y = Value, fill = region)) +
  geom_boxplot(outlier.size = 0.8, alpha = 0.7) +
  scale_fill_manual(values = c("AOC" = "red", "Lake Ontario" = "grey", "St. Lawrence River" = "lightblue")) +
  labs(
    title = "Mercury in Walleye (40–50 cm) by Site",
    subtitle = "Standardized size window to reduce bias from length differences",
    x = "Site",
    y = "Mercury (µg/g)"
  ) +
  coord_flip() +
  theme_minimal(base_size = 12)

# Good, but missing some data


# Using predictive model to standardize size class of samples

library(dplyr)
library(tidyr)
library(purrr)

# Only use sites with at least 5 fish
site_counts <- walleye_df %>%
  group_by(site) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n >= 5)

# Fit power model per site
site_models <- walleye_df %>%
  filter(Value > 0, Length > 0, site %in% site_counts$site) %>%
  group_by(site) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(log(Value) ~ log(Length), data = .x)),
    slope = map_dbl(model, ~ coef(.x)[2])
  ) %>%
  select(site, slope)

standard_length <- 45

# Merge slopes into main data and compute standardized Hg
walleye_std <- walleye_df %>%
  filter(Value > 0, Length > 0) %>%
  inner_join(site_models, by = "site") %>%
  mutate(
    Value_std = Value * (standard_length / Length)^slope
  )


# Compute site means to use for ordering
site_order <- walleye_std %>%
  group_by(site) %>%
  summarise(mean_std = mean(Value_std, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_std) %>%
  pull(site)

# Reorder site factor levels
walleye_std <- walleye_std %>%
  mutate(site = factor(site, levels = site_order))


ggplot(walleye_std, aes(x = site, y = Value_std, fill = region)) +
  geom_boxplot(outlier.size = 0.8, alpha = 0.7) +
  scale_fill_manual(values = c("AOC" = "red", "Lake Ontario" = "grey", "St. Lawrence River" = "lightblue")) +
  labs(
    title = "Size-Standardized Mercury in Walleye by Site (≥ 5 samples)",
    subtitle = "Using site-specific power regression (standardized to 45 cm)",
    x = "Site",
    y = "Standardized Hg (µg/g at 45 cm)"
  ) +
  coord_flip() +
  theme_minimal(base_size = 12)



# Time series

all_hg_data = raw_data %>%
  filter(Contaminant == "MERCURY")


# Creating references to data points in AOC vs Lake Ontario
all_hg_pts = all_hg_data %>%
  filter(!is.na(Latitude.Decimal) & !is.na(Longitude.Decimal)) %>%
  st_as_sf(coords = c("Longitude.Decimal", "Latitude.Decimal"), crs = 4326)

all_aoc_shp <- st_transform(aoc_shp, crs = st_crs(all_hg_pts))

aoc_df_all = all_hg_pts[st_intersects(all_hg_pts, aoc_shp, sparse = FALSE),]

slr_df_all = all_hg_pts[st_intersects(all_hg_pts, slr_ref, sparse = FALSE),]

on_df_all = all_hg_pts[st_intersects(all_hg_pts, on_shp, sparse = FALSE),]


aoc_df_all$region = "AOC"
on_df_all$region = "Lake Ontario"
slr_df_all$region = "St. Lawrence River"

aoc_species = unique(aoc_df$Specname)

all_df_alltime = bind_rows(slr_df_all, on_df_all, aoc_df_all) %>%
  mutate(site = Locname.Fishbase) %>%
  filter(Specname %in% aoc_species) %>%
  mutate(region = as.factor(region))



all_walleye_df <- all_df_alltime %>%
  filter(grepl("Walleye", Specname, ignore.case = TRUE))

library(dplyr)

# Define bins (e.g., 30–34, 35–39, ..., 75+)
all_walleye_df <- all_walleye_df %>%
  filter(Value > 0, Length > 0) %>%
  mutate(
    length_bin = cut(
      Length,
      breaks = c(seq(25, 75, by = 5), Inf),
      labels = c(paste(seq(25, 70, by = 5), seq(29, 74, by = 5), sep = "-"), "75+"),
      right = FALSE
    )
  )

top_bins <- all_walleye_df %>%
  count(length_bin, sort = TRUE) %>%
  slice_head(n = 3) %>%
  pull(length_bin)

walleye_top_bins <- all_walleye_df %>%
  filter(length_bin %in% top_bins)

walleye_top_bins <- walleye_top_bins %>%
  mutate(Sample.Year = as.numeric(Sample.Year))


library(ggplot2)

library(ggplot2)

plot_region_trends <- function(data, region_label) {
  ggplot(data, aes(x = Sample.Year, y = Value, color = length_bin, shape = length_bin)) +
    geom_point(alpha = 0.7, size = 2) +
    geom_smooth(method = "gam", se = TRUE, linewidth = 1) +
    labs(
      title = paste("Temporal Mercury Trends in Walleye -", region_label),
      subtitle = paste("Top 3 most sampled length bins (", paste(top_bins, collapse = ", "), ")"),
      x = "Sample Year",
      y = "Mercury (µg/g)",
      color = "Length Bin",
      shape = "Length Bin"
    ) +
    theme_minimal(base_size = 13)
}

aoc_plot <- plot_region_trends(filter(walleye_top_bins, region == "AOC"), "AOC")
slr_plot <- plot_region_trends(filter(walleye_top_bins, region == "St. Lawrence River"), "St. Lawrence River")
on_plot  <- plot_region_trends(filter(walleye_top_bins, region == "Lake Ontario"), "Lake Ontario")

# Print them
print(aoc_plot)
print(slr_plot)
print(on_plot)



