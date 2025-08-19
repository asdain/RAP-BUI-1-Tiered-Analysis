# RUN LOCALLY (not in Shinylive)
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr)
})

source(here::here("Scripts","Setup.R"))          # builds cons_data, length_levels, etc.
source(here::here("R","tier_table_shared.R"))    # your helpers

# --- inputs you use for Tier 2
aoc_id          <- 45087425
reference_sites <- c("44007727",
                    '43557712',
                    '43497905',
                    '43117922',
                    '43397900',
                    '43517847',
                    '43527851',
                    '44397535',
                    '43557717',
                    '43527856')

# median rule (lower of two middles)
median_floor <- function(x) {
  x <- sort(x[!is.na(x)]); n <- length(x)
  if (n == 0) return(NA_integer_)
  if (n %% 2 == 1) x[(n + 1) %/% 2] else x[n / 2]
}

# 1) Filter to AOC + refs, keep only AOC combos
aoc_combos <- prep_aoc_combinations(cons_data, aoc_id, length_levels)
filtered   <- filter_advisory_data(cons_data, c(reference_sites, aoc_id), aoc_id, length_levels) %>%
  semi_join(aoc_combos, by = c("Species","Size","Population"))

# 2) Collapse to max advisory per Species/Pop/Size/Site
base_data <- summarise_max_advisory(filtered)

# 3) AOC wide
aoc_wide <- base_data %>%
  filter(site_type == "AOC") %>%
  pivot_wider(names_from = Size, values_from = advisory) %>%
  mutate(site_order = 1L)

# 4) Reference medians (your custom median) + n
ref_long <- filter(base_data, site_type == "Reference")

ref_medians_raw <- ref_long %>%
  group_by(Species, Population, Size) %>%
  summarise(Median = median_floor(advisory), .groups = "drop") %>%
  pivot_wider(names_from = Size, values_from = Median)

ref_n_raw <- ref_long %>%
  group_by(Species, Population, Size) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = Size, values_from = n)

size_cols_m <- intersect(length_levels, names(ref_medians_raw))
size_cols_n <- intersect(length_levels, names(ref_n_raw))

ref_medians <- ref_medians_raw %>%
  mutate(Site = "Reference Median", site_type = "Reference", site_order = 2L) %>%
  select(Species, Population, all_of(size_cols_m), Site, site_type, site_order)

ref_n <- ref_n_raw %>%
  mutate(Site = "n", site_type = "Reference", site_order = 3L) %>%
  select(Species, Population, all_of(size_cols_n), Site, site_type, site_order)

# 5) Raw reference rows for “details” panel (optional)
ref_rows <- ref_long %>% pivot_wider(names_from = Size, values_from = advisory)

# 6) Display table (add display labels & order)
display <- bind_rows(aoc_wide, ref_medians) %>%
  add_row_order_labels(length_levels)

# 7) JS lookups (precompute once)
comparison_lookup <- ref_medians %>%
  select(Species, Population, all_of(size_cols_m)) %>%
  pivot_longer(cols = all_of(size_cols_m), names_to = "Size", values_to = "Median") %>%
  mutate(id = paste(Species, Population, Size, sep = "||"))

n_lookup <- ref_n %>%
  select(Species, Population, all_of(size_cols_n)) %>%
  pivot_longer(cols = all_of(size_cols_n), names_to = "Size", values_to = "n") %>%
  mutate(id = paste(Species, Population, Size, sep = "||"))

bundle <- list(
  length_levels = length_levels,
  display       = display,                      # table to render
  ref_detail    = ref_rows,                     # for details panel
  medians       = setNames(as.list(comparison_lookup$Median), comparison_lookup$id),
  ns            = setNames(as.list(n_lookup$n), comparison_lookup$id)
)

dir.create("Web/tier2/data", recursive = TRUE, showWarnings = FALSE)
saveRDS(bundle, "Web/tier2/data/t2_bundle.rds", compress = "xz")
cat("✓ Wrote Web/tier2/data/t2_bundle.rds\n")

