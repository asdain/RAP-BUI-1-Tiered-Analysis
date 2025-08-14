#' Lookup waterbody names from group IDs
get_waterbody_names <- function(cons_data, site_ids) {
  cons_data %>%
    filter(waterbody_group %in% site_ids) %>%
    distinct(waterbody_group, guide_locname_eng) %>%
    arrange(waterbody_group) %>%
    pull(guide_locname_eng)
}

#' Lookup waterbody group IDs from names
get_waterbody_ids <- function(cons_data, site_names) {
  cons_data %>%
    filter(guide_locname_eng %in% site_names) %>%
    distinct(guide_locname_eng, waterbody_group) %>%
    arrange(guide_locname_eng) %>%
    pull(waterbody_group)
}
