
# Load data
all_hg_data <- raw_data %>%
  filter(Contaminant == toupper(params$contaminant))

# Convert to spatial
all_hg_pts <- all_hg_data %>%
  filter(!is.na(Latitude.Decimal) & !is.na(Longitude.Decimal)) %>%
  st_as_sf(coords = c("Longitude.Decimal", "Latitude.Decimal"), crs = 4326)

# Reproject AOC shape to match
aoc_shp_proj <- st_transform(aoc_shp, st_crs(all_hg_pts))
slr_ref_proj <- st_transform(slr_ref, st_crs(all_hg_pts))
on_shp_proj  <- st_transform(on_shp,  st_crs(all_hg_pts))

# Spatially filter points by region
aoc_df_all <- all_hg_pts[st_intersects(all_hg_pts, aoc_shp_proj, sparse = FALSE),]
slr_df_all <- all_hg_pts[st_intersects(all_hg_pts, slr_ref_proj, sparse = FALSE),]
on_df_all  <- all_hg_pts[st_intersects(all_hg_pts, on_shp_proj,  sparse = FALSE),]

# Label regions
aoc_df_all$region <- "AOC"
slr_df_all$region <- "St. Lawrence River"
on_df_all$region  <- "Lake Ontario"

# Keep species observed in AOC
aoc_species <- unique(aoc_df$Specname)

# Merge and filter to species of interest
all_df_alltime <- bind_rows(aoc_df_all, slr_df_all, on_df_all) %>%
  mutate(site = Locname.Fishbase) %>%
  filter(Specname %in% aoc_species) %>%
  mutate(region = as.factor(region))

# Filter to species parameter
target_df <- all_df_alltime %>%
  filter(grepl(params$species, Specname, ignore.case = TRUE)) %>%
  filter(Value > 0, Length > 0) %>%
  mutate(
    length_bin = cut(
      Length,
      breaks = c(seq(25, 75, by = 5), Inf),
      labels = c(paste(seq(25, 70, by = 5), seq(29, 74, by = 5), sep = "-"), "75+"),
      right = FALSE
    ),
    Sample.Year = as.numeric(Sample.Year)
  )

# Identify top 3 most sampled bins
top_bins <- target_df %>%
  count(length_bin, sort = TRUE) %>%
  slice_head(n = 3) %>%
  pull(length_bin)

# Filter to those bins
target_top_bins <- target_df %>%
  filter(length_bin %in% top_bins)

# Plot function
plot_region_trends <- function(data, region_label) {
  ggplot(data, aes(x = Sample.Year, y = Value, color = length_bin, shape = length_bin)) +
    geom_point(alpha = 0.7, size = 2) +
    geom_smooth(method = "gam", se = TRUE, linewidth = 1) +
    labs(
      title = paste("Temporal Mercury Trends in", params$species, "-", region_label),
      subtitle = paste("Top 3 length bins:", paste(top_bins, collapse = ", ")),
      x = "Sample Year",
      y = paste(params$contaminant, "(µg/g)"),
      color = "Length Bin",
      shape = "Length Bin"
    ) +
    theme_minimal(base_size = 13)
}

# Generate plots
aoc_plot <- plot_region_trends(filter(target_top_bins, region == "AOC"), "AOC")
slr_plot <- plot_region_trends(filter(target_top_bins, region == "St. Lawrence River"), "St. Lawrence River")
on_plot  <- plot_region_trends(filter(target_top_bins, region == "Lake Ontario"), "Lake Ontario")
