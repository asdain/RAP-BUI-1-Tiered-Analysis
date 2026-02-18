# Parse "60-65" / "60–65" / "60 - 65 cm" -> c(60,65)
.parse_size_bounds <- function(lbl) {
  nums <- stringr::str_extract_all(lbl, "\\d+\\.?\\d*")[[1]]
  if (length(nums) < 2) return(c(NA_real_, NA_real_))
  as.numeric(nums[1:2])
}

# Choose the size bin that contains length_cm; if none contain it, choose closest by midpoint
length_to_size_bin <- function(length_cm, size_levels) {
  bnds <- purrr::map_dfr(size_levels, function(s) {
    bb <- .parse_size_bounds(s)
    tibble::tibble(Size = s, lo = bb[1], hi = bb[2], mid = mean(bb, na.rm = TRUE))
  })
  
  in_bin <- bnds %>% dplyr::filter(is.finite(lo), is.finite(hi), length_cm >= lo, length_cm < hi)
  if (nrow(in_bin) >= 1) return(in_bin$Size[1])
  
  # fallback: closest midpoint
  bnds <- bnds %>% dplyr::filter(is.finite(mid))
  if (nrow(bnds) == 0) return(NA_character_)
  bnds$Size[which.min(abs(bnds$mid - length_cm))]
}

get_t2_median_meals_for_length <- function(t2_prep_obj,
                                           species,
                                           length_cm,
                                           pop_value = "Sensitive") {
  
  size_levels <- t2_prep_obj$size_cols
  size_bin <- length_to_size_bin(length_cm, size_levels)
  
  if (is.na(size_bin)) return(NA_real_)
  
  key <- paste(species, pop_value, size_bin, sep = "||")
  val <- t2_prep_obj$medians_map[[key]]
  
  # ---- HARD GUARD ----
  if (is.null(val) || length(val) == 0 || is.na(val)) {
    return(NA_real_)
  }
  
  as.numeric(val)
}


# Round to nearest 5 cm (reuse your existing helper if already defined)
round5 <- function(x) round(x / 5) * 5



# Build a tibble of representative lengths (quartiles by default)
get_rep_lengths_quartiles <- function(
    t3c_data,
    probs = c(0.25, 0.5, 0.75),
    round_fun = round5
) {
  qs <- stats::quantile(t3c_data$Length, probs = probs, na.rm = TRUE)
  tibble::tibble(
    role      = c("Lower quartile", "Median", "Upper quartile")[seq_along(qs)],
    prob      = probs,
    length_cm = round_fun(as.numeric(qs))
  )
}

compute_years_to_threshold_lengths <- function(
    t3c_gam, hl_tab, thresholds_df,
    t1_meals,
    rep_info,            # <- pass rep_info tibble with role + length_cm + t2_meals
    t3c_data
) {
  year_anchor <- max(t3c_data$Sample.Year, na.rm = TRUE)
  
  get_target_conc <- function(meals_val) {
    if (is.na(meals_val) || !is.finite(meals_val)) return(NA_real_)
    out <- thresholds_df %>% dplyr::filter(.data$meals == meals_val) %>% dplyr::pull(.data$conc)
    if (length(out) != 1) return(NA_real_)
    out
  }
  
  if (nrow(hl_tab) != 1) stop("hl_tab should contain a single AOC row.")
  k        <- hl_tab$k[1]
  slope_se <- hl_tab$slope_se[1]
  
  purrr::map_dfr(seq_len(nrow(rep_info)), function(i) {
    len_cm  <- rep_info$length_cm[i]
    role    <- rep_info$role[i]
    t2_meals <- rep_info$t2_meals[i]
    
    target_conc_t1 <- get_target_conc(t1_meals)
    target_conc_t2 <- get_target_conc(t2_meals)
    
    target_conc <- dplyr::case_when(
      is.finite(target_conc_t1) & is.finite(target_conc_t2) ~ max(target_conc_t1, target_conc_t2),
      is.finite(target_conc_t1) ~ target_conc_t1,
      is.finite(target_conc_t2) ~ target_conc_t2,
      TRUE ~ NA_real_
    )
    
    newdata <- tibble::tibble(Sample.Year = year_anchor, Length = len_cm)
    pr <- predict(t3c_gam, newdata = newdata, type = "link", se.fit = TRUE)
    
    pred_log <- as.numeric(pr$fit)
    se_log   <- as.numeric(pr$se.fit)
    pred_c0  <- exp(pred_log)
    
    years_to_target <- dplyr::case_when(
      !is.finite(target_conc) ~ NA_real_,
      pred_c0 <= target_conc  ~ 0,
      is.finite(k) & k > 0    ~ (pred_log - log(target_conc)) / k,
      TRUE                    ~ NA_real_
    )
    
    se_t <- if (is.finite(years_to_target) && !is.na(years_to_target) && k > 0) {
      term1 <- (1 / k)^2 * (se_log^2)
      term2 <- (years_to_target / k)^2 * (slope_se^2)
      sqrt(term1 + term2)
    } else NA_real_
    
    tibble::tibble(
      role            = role,
      length_cm       = len_cm,
      t1_meals = t1_meals,
      t2_meals        = t2_meals,
      predicted_conc  = pred_c0,
      target_conc_t1  = target_conc_t1,
      target_conc_t2  = target_conc_t2,
      target_conc     = target_conc,
      years_to_target = years_to_target,
      years_lwr       = if (is.na(se_t)) NA_real_ else years_to_target - 1.96 * se_t,
      years_upr       = if (is.na(se_t)) NA_real_ else years_to_target + 1.96 * se_t
    )
  })
}


collapse_t2_meals <- function(df) {
  if (!"t2_meals" %in% names(df)) return(df)
  
  if (is.list(df$t2_meals)) {
    df %>%
      dplyr::mutate(
        t2_meals = purrr::map_dbl(t2_meals, ~{
          x <- suppressWarnings(as.numeric(.x))
          x <- x[is.finite(x)]
          if (length(x) == 0) NA_real_ else x[1]
        })
      )
  } else {
    df %>% dplyr::mutate(t2_meals = suppressWarnings(as.numeric(t2_meals)))
  }
}
