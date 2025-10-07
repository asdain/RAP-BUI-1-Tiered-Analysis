# Tier3/R/tier3_run.R
# Runs Tier 3 for one species given raw_data + params, caches outputs, and returns a structured list.
# Requires: dplyr, tidyr, stringr, sf, ggplot2, cowplot, maptiles, ggspatial, mgcv, emmeans, glue,
#           kableExtra, htmlwidgets, webshot2 (optional), reactable (optional)

tier3_run <- function(
    raw_data,
    params,
    save_artifacts = TRUE,
    chrome_path = "C:/Program Files/BraveSoftware/Brave-Browser/Application/brave.exe"
) {
  # ---- pkgs ----
  pkgs <- c("dplyr","tidyr","stringr","sf","ggplot2","cowplot","mgcv","emmeans","glue")
  invisible(lapply(pkgs, function(p) if (!requireNamespace(p, quietly = TRUE)) stop("Package required: ", p)))
  # optional
  has_maptiles  <- requireNamespace("maptiles",  quietly = TRUE)
  has_ggspatial <- requireNamespace("ggspatial", quietly = TRUE)
  has_reactable <- requireNamespace("reactable", quietly = TRUE)
  has_webshot2  <- requireNamespace("webshot2",  quietly = TRUE)
  has_htmlwidgets <- requireNamespace("htmlwidgets", quietly = TRUE)
  has_kableExtra <- requireNamespace("kableExtra", quietly = TRUE)
  
  # set chromote for PDF table snapshots if used
  if (has_webshot2) Sys.setenv(CHROMOTE_CHROME = chrome_path)
  
  # ========= helpers (self-contained) =========
  here_or <- function(...) {
    if (requireNamespace("here", quietly = TRUE)) here::here(...) else file.path(...)
  }
  
  ensure_dirs <- function() {
    dir.create(here_or("Derived","Tier3","t3_rds"), showWarnings = FALSE, recursive = TRUE)
    dir.create(here_or("Derived","Tier3","figs"),   showWarnings = FALSE, recursive = TRUE)
    dir.create(here_or("Derived","Tier3","widgets"),showWarnings = FALSE, recursive = TRUE)
  }
  
  make_key <- function(species, contaminant, combine_ref, ref_1, ref_2, aoc_name) {
    clean <- function(x) {
      x <- tolower(paste0(x, collapse = "_"))
      x <- gsub("[^a-z0-9]+", "_", x)
      gsub("^_|_$","",x)
    }
    paste(
      clean(species),
      clean(contaminant),
      if (combine_ref) "ref_combined" else paste("ref", clean(ref_1), clean(ref_2), sep = "_"),
      clean(aoc_name),
      sep = "__"
    )
  }
  
  paths_for <- function(key) {
    list(
      rds   = here_or("Derived","Tier3","t3_rds",  paste0(key,".rds")),
      figs  = here_or("Derived","Tier3","figs",    key),
      wigs  = here_or("Derived","Tier3","widgets", key)
    )
  }
  
  export_plot <- function(p, path_base, name, width=7, height=4.5, dpi=300) {
    if (!inherits(p, c("gg","ggplot","grob")) && !is.list(p)) return(invisible(NULL))
    dir.create(path_base, showWarnings = FALSE, recursive = TRUE)
    fn <- file.path(path_base, paste0(name,".png"))
    ggplot2::ggsave(fn, p, width = width, height = height, dpi = dpi, bg = "white")
    fn
  }
  
  widget_to_png <- function(widget, out_png, width=1200, height=800) {
    if (!has_webshot2 || !has_htmlwidgets) return(invisible(NULL))
    dir.create(dirname(out_png), showWarnings = FALSE, recursive = TRUE)
    html_tmp <- sub("\\.png$",".html", out_png)
    htmlwidgets::saveWidget(widget, html_tmp, selfcontained = TRUE)
    webshot2::webshot(html_tmp, out_png, vwidth = width, vheight = height)
    out_png
  }
  
  adjust_bbox_to_aspect <- function(bbox, aspect_ratio = 10/6, crs = 3857) {
    xmin <- bbox["xmin"]; xmax <- bbox["xmax"]; ymin <- bbox["ymin"]; ymax <- bbox["ymax"]
    width <- xmax - xmin; height <- ymax - ymin
    if (width <= 0 || height <= 0) return(sf::st_bbox(bbox, crs = sf::st_crs(crs)))
    current_ratio <- width/height
    cx <- (xmin+xmax)/2; cy <- (ymin+ymax)/2
    if (current_ratio > aspect_ratio) {
      new_h <- width/aspect_ratio
      sf::st_bbox(c(xmin=xmin, ymin=cy-new_h/2, xmax=xmax, ymax=cy+new_h/2), crs = sf::st_crs(crs))
    } else {
      new_w <- height*aspect_ratio
      sf::st_bbox(c(xmin=cx-new_w/2, ymin=ymin, xmax=cx+new_w/2, ymax=ymax), crs = sf::st_crs(crs))
    }
  }
  
  get_unit_label <- function(contaminant) {
    cu <- toupper(contaminant)
    if (cu == "MERCURY") "µg/g" else if (cu == "PCBs") "ng/g" else "units"
  }
  
  get_thresholds <- function(contaminant = "MERCURY", population = "Sensitive") {
    if (contaminant == "MERCURY") {
      if (population == "Sensitive") {
        tibble::tibble(meals = c(16,12,8,4,0), conc = c(0.06,0.12,0.16,0.25,0.50))
      } else {
        tibble::tibble(meals = c(32,16,12,8,4,2,0), conc = c(0.15,0.15,0.3,0.4,0.6,1.2,1.8))
      }
    } else if (contaminant == "PCBs") {
      tibble::tibble(meals = c(16,12,8,4,2,1,0), conc = c(26,53,70,105,211,422,844))
    } else {
      tibble::tibble(meals = numeric(), conc = numeric())
    }
  }
  
  # ------ Tier 3B helpers (inline) ------
  fit_gam <- function(gam_df, k_len = 10, method="REML", verbose=TRUE) {
    gam_df <- gam_df %>%
      dplyr::mutate(region = factor(region),
                    Waterbody.Code = factor(Waterbody.Code)) %>%
      droplevels()
    n_region <- nlevels(gam_df$region)
    n_site   <- nlevels(gam_df$Waterbody.Code)
    
    base <- glue::glue("log(Value) ~ s(Length, k = {k_len})")
    has_region <- n_region >= 2
    has_siteRE <- n_site   >= 2
    
    full_terms <- base
    if (has_region) full_terms <- glue::glue("log(Value) ~ s(Length, region, bs = 'fs', k = {k_len})")
    if (has_siteRE) full_terms <- paste0(full_terms, " + s(Waterbody.Code, bs = 're')")
    f_full <- as.formula(full_terms)
    
    null_terms <- base
    if (has_region) null_terms <- paste0(null_terms, " + region")
    if (has_siteRE) null_terms <- paste0(null_terms, " + s(Waterbody.Code, bs = 're')")
    f_null <- as.formula(null_terms)
    
    if (verbose) {
      message("Region levels: ", n_region, " | Site levels: ", n_site)
      message("NULL: ", deparse(f_null))
      message("FULL: ", deparse(f_full))
    }
    
    gam_null <- mgcv::gam(f_null, data = gam_df, method = method)
    gam_full <- mgcv::gam(f_full, data = gam_df, method = method)
    
    if (!has_region || identical(f_null, f_full)) {
      return(list(
        gam_null = gam_null, gam_full = gam_full,
        interaction_p = NA_real_,
        interaction_msg = if (!has_region) "Not tested (single region)." else "Not tested (identical models)."
      ))
    }
    
    atab <- anova(gam_null, gam_full, test = "Chisq")
    pcol <- suppressWarnings(as.numeric(atab$`Pr(>Chi)`))
    pval <- if (length(pcol) >= 2) pcol[2] else if (length(pcol) >= 1) pcol[length(pcol)] else NA_real_
    if (!is.finite(pval)) pval <- NA_real_
    
    list(gam_null = gam_null, gam_full = gam_full, interaction_p = pval,
         interaction_msg = "Tested (NULL: s(Length)+region+RE; FULL: + fs(Length,region)).")
  }
  
  generate_gam_prediction_table <- function(gam_model, recent_data, compare_to="Reference", alpha=0.05, back_transform=TRUE) {
    mf <- model.frame(gam_model)
    region_levels <- levels(mf$region)
    has_compare <- (length(region_levels) >= 2) && (compare_to %in% region_levels)
    aoc_name <- if ("AOC" %in% region_levels) "AOC" else setdiff(region_levels, compare_to)[1]
    
    qs <- recent_data %>%
      dplyr::group_by(region) %>%
      dplyr::summarise(lo = stats::quantile(Length,.01,na.rm=TRUE),
                       hi = stats::quantile(Length,.99,na.rm=TRUE), .groups="drop")
    lo5 <- ceiling(max(qs$lo,na.rm=TRUE)/5)*5
    hi5 <- floor(min(qs$hi,na.rm=TRUE)/5)*5
    if (!is.finite(lo5) || !is.finite(hi5) || lo5 > hi5) {
      lo5 <- ceiling(min(recent_data$Length,na.rm=TRUE)/5)*5
      hi5 <- floor(max(recent_data$Length,na.rm=TRUE)/5)*5
    }
    length_seq <- seq(lo5, hi5, by=5)
    if (length(length_seq)==0L) stop("No valid length sequence for predictions.")
    
    newdata_grid <- expand.grid(Length=length_seq, region=region_levels, KEEP.OUT.ATTRS=FALSE)
    newdata_grid$region <- factor(newdata_grid$region, levels = region_levels)
    if ("Waterbody.Code" %in% names(mf)) {
      if (is.factor(mf$Waterbody.Code)) {
        newdata_grid$Waterbody.Code <- factor(levels(mf$Waterbody.Code)[1], levels = levels(mf$Waterbody.Code))
      } else newdata_grid$Waterbody.Code <- mf$Waterbody.Code[1]
    }
    if ("Sample.Year" %in% names(mf)) {
      if (is.factor(mf$Sample.Year)) {
        newdata_grid$Sample.Year <- factor(levels(mf$Sample.Year)[1], levels = levels(mf$Sample.Year))
      } else newdata_grid$Sample.Year <- stats::median(recent_data$Sample.Year, na.rm = TRUE)
    }
    
    re_labels <- vapply(gam_model$smooth, function(s) if (!is.null(s$bs) && s$bs=="re") s$label else NA_character_, character(1))
    re_labels <- stats::na.omit(re_labels)
    
    Xp   <- predict(gam_model, newdata=newdata_grid, type="lpmatrix", exclude=re_labels)
    beta <- coef(gam_model)
    V    <- vcov(gam_model, unconditional = FALSE)
    
    fit   <- as.numeric(Xp %*% beta)
    se    <- sqrt(rowSums((Xp %*% V) * Xp))
    lower <- fit - 1.96*se
    upper <- fit + 1.96*se
    
    pred_df <- cbind(newdata_grid, fit=fit, se=se, lower=lower, upper=upper) |> as.data.frame()
    if (back_transform) {
      pred_df <- pred_df %>% dplyr::mutate(fit_bt = exp(fit), lower_bt = exp(lower), upper_bt = exp(upper))
    } else {
      pred_df <- pred_df %>% dplyr::mutate(fit_bt = fit, lower_bt = lower, upper_bt = upper)
    }
    
    sig_lengths <- integer(0)
    if (has_compare && !is.na(aoc_name) && (aoc_name %in% region_levels)) {
      row_idx <- function(r,L) which(pred_df$region==r & pred_df$Length==L)[1]
      pvals <- lapply(length_seq, function(L) {
        ia <- row_idx(aoc_name, L); ir <- row_idx(compare_to, L)
        if (is.na(ia) || is.na(ir)) return(data.frame(Length=L, p=NA_real_))
        Xa <- Xp[ia,,drop=FALSE]; Xr <- Xp[ir,,drop=FALSE]
        d  <- Xa - Xr
        est <- as.numeric(d %*% beta)
        seD <- sqrt(as.numeric(d %*% V %*% t(d)))
        p   <- if (seD > 0) 2*pnorm(-abs(est/seD)) else NA_real_
        data.frame(Length=L, p=p)
      }) %>% dplyr::bind_rows()
      sig_lengths <- pvals %>% dplyr::filter(is.finite(p), p < alpha) %>% dplyr::pull(Length)
    }
    
    pred_df <- pred_df %>%
      dplyr::mutate(sig_star = ifelse(region %in% aoc_name & Length %in% sig_lengths, "*",""),
                    length_label = paste0(Length," cm"),
                    pred_str = glue::glue("{round(fit_bt,2)} ({round(lower_bt,2)}–{round(upper_bt,2)}){sig_star}"))
    
    output_table <- pred_df %>%
      dplyr::select(region, length_label, pred_str) %>%
      tidyr::pivot_wider(names_from = length_label, values_from = pred_str) %>%
      dplyr::rename(Region = region)
    attr(output_table,"preds") <- pred_df
    output_table
  }
  
  lpmatrix_contrasts <- function(gam_model, length_seq, recent_data, aoc_name="AOC") {
    mf   <- model.frame(gam_model)
    regs <- levels(mf$region)
    refs <- setdiff(regs, aoc_name)
    stopifnot(length(refs) > 0)
    newgrid <- expand.grid(region=regs, Length=length_seq, KEEP.OUT.ATTRS=FALSE)
    newgrid$region <- factor(newgrid$region, levels = regs)
    if ("Waterbody.Code" %in% names(mf)) {
      newgrid$Waterbody.Code <- if (is.factor(mf$Waterbody.Code))
        factor(levels(mf$Waterbody.Code)[1], levels = levels(mf$Waterbody.Code))
      else mf$Waterbody.Code[1]
    }
    if ("Sample.Year" %in% names(mf)) {
      newgrid$Sample.Year <- if (is.factor(mf$Sample.Year))
        factor(levels(mf$Sample.Year)[1], levels = levels(mf$Sample.Year))
      else stats::median(recent_data$Sample.Year, na.rm = TRUE)
    }
    exclude_re <- vapply(gam_model$smooth, function(s) { bs <- tryCatch(s$bs, error=function(...) NULL); if (!is.null(bs) && identical(bs,"re")) s$label else NA_character_ }, character(1))
    exclude_re <- stats::na.omit(exclude_re)
    
    Xp   <- predict(gam_model, newdata=newgrid, type="lpmatrix", exclude=exclude_re, unconditional = FALSE)
    beta <- coef(gam_model); V <- vcov(gam_model, unconditional = FALSE)
    
    idx <- function(r,L) which(newgrid$region==r & newgrid$Length==L)[1]
    out <- lapply(length_seq, function(L) {
      ia <- idx(aoc_name,L)
      do.call(rbind, lapply(refs, function(ref) {
        ir <- idx(ref,L)
        d  <- Xp[ia,,drop=FALSE] - Xp[ir,,drop=FALSE]
        est <- as.numeric(d %*% beta)
        se  <- sqrt(as.numeric(d %*% V %*% t(d)))
        data.frame(Length=L, ref=ref, est=est, se=se, z=est/se, p=2*pnorm(-abs(est/se)))
      }))
    })
    dplyr::bind_rows(out)
  }
  
  round5 <- function(x) round(x/5)*5
  
  compute_years_to_threshold_AOC <- function(t3d_gam, hl_tab, thresholds_df, restrict_threshold, filtered_data, aoc_level="AOC") {
    region_levels <- levels(model.frame(t3d_gam)$region)
    median_len <- round5(stats::median(filtered_data$Length, na.rm = TRUE))
    year_anchor <- filtered_data %>%
      dplyr::filter(region == aoc_level) %>%
      dplyr::summarise(year_anchor = max(Sample.Year, na.rm = TRUE), .groups="drop") %>% dplyr::pull(year_anchor)
    target_conc <- thresholds_df %>% dplyr::filter(meals == restrict_threshold) %>% dplyr::pull(conc)
    if (length(target_conc) == 0) stop("restrict_threshold not in thresholds_df")
    
    newdata <- tibble::tibble(region = factor(aoc_level, levels = region_levels),
                              Sample.Year = year_anchor, Length = median_len)
    pred <- predict(t3d_gam, newdata = newdata, type = "link", se.fit = TRUE)
    pred_log <- as.numeric(pred$fit); se_log <- as.numeric(pred$se.fit); pred_c0 <- exp(pred_log)
    
    krow <- hl_tab %>% dplyr::filter(region == aoc_level)
    if (nrow(krow) != 1) stop("AOC row not found in half-life table.")
    k        <- krow$k[1]
    slope_se <- krow$slope_se[1]
    
    log_target <- log(target_conc)
    years_to_target <- dplyr::case_when(
      pred_c0 <= target_conc ~ 0,
      is.finite(k) & k > 0   ~ (pred_log - log_target)/k,
      TRUE                   ~ NA_real_
    )
    
    se_t <- if (is.finite(years_to_target) && !is.na(years_to_target) && k > 0) {
      term1 <- (1/k)^2 * (se_log^2)
      term2 <- (years_to_target/k)^2 * (slope_se^2)
      sqrt(term1 + term2)
    } else NA_real_
    lwr <- if (is.na(se_t)) NA_real_ else years_to_target - 1.96*se_t
    upr <- if (is.na(se_t)) NA_real_ else years_to_target + 1.96*se_t
    
    outcome <- dplyr::case_when(
      !is.na(years_to_target) && years_to_target == 0 ~ "Supportive",
      is.na(years_to_target) | k <= 0                 ~ "Unsupportive",
      years_to_target > 10                             ~ "Unsupportive",
      TRUE                                             ~ "Supportive"
    )
    
    results_tbl <- tibble::tibble(
      median_len_cm   = median_len,
      predicted_conc  = pred_c0,
      target_conc     = target_conc,
      years_to_target = years_to_target,
      years_lwr       = lwr,
      years_upr       = upr,
      outcome         = outcome
    )
    
    if (has_kableExtra) {
      kable_html <- results_tbl %>%
        dplyr::mutate(
          predicted_conc  = sprintf("%.2f", predicted_conc),
          target_conc     = sprintf("%.2f", target_conc),
          years_to_target = ifelse(is.na(years_to_target),"—",sprintf("%.1f", years_to_target)),
          ci_95 = dplyr::case_when(
            is.na(years_lwr) | is.na(years_upr) ~ "—",
            TRUE ~ sprintf("%.1f–%.1f", years_lwr, years_upr)
          )
        ) %>%
        dplyr::select(
          `Median Length (cm)` = median_len_cm,
          `Predicted Conc`     = predicted_conc,
          `Target Conc`        = target_conc,
          `Years to Threshold` = years_to_target,
          `95% CI`             = ci_95,
          `Outcome`            = outcome
        ) %>%
        knitr::kable(format = "html",
                     caption = glue::glue("Tier 3D (AOC): Time to reach an unrestrictive advisory of {restrict_threshold} meals/month for median-length fish")) %>%
        kableExtra::kable_styling(full_width = FALSE, position = "center") %>%
        as.character()
    } else {
      kable_html <- NULL
    }
    
    metric_text <- dplyr::case_when(
      is.na(years_to_target) ~ glue::glue("Trend does not indicate a decline; target not reachable under current trend."),
      years_to_target == 0   ~ glue::glue("Already unrestrictive for median-length ({median_len} cm) fish."),
      TRUE ~ {
        if (!is.na(lwr) && !is.na(upr)) {
          glue::glue("~{round(years_to_target,1)} years (95% CI {round(lwr,1)}–{round(upr,1)}) to reach {restrict_threshold} meals/month at median {median_len} cm.")
        } else {
          glue::glue("~{round(years_to_target,1)} years to reach {restrict_threshold} meals/month at median {median_len} cm.")
        }
      }
    )
    
    report_text <- dplyr::case_when(
      is.na(years_to_target) ~ glue::glue("For median-length ({median_len} cm) fish, the current trend does not indicate declining {tolower(params$contaminant)} concentrations; reaching ≥ {restrict_threshold} meals/month is not expected under the present trend."),
      years_to_target == 0 ~ glue::glue("For median-length ({median_len} cm) fish, predicted concentrations are already within the unrestrictive range (≥ {restrict_threshold} meals/month)."),
      TRUE ~ {
        if (!is.na(lwr) && !is.na(upr)) {
          glue::glue("For median-length ({median_len} cm) fish, ~{round(years_to_target,1)} years (95% CI {round(lwr,1)}–{round(upr,1)}) are predicted to reach ≥ {restrict_threshold} meals/month.")
        } else {
          glue::glue("For median-length ({median_len} cm) fish, ~{round(years_to_target,1)} years are predicted to reach ≥ {restrict_threshold} meals/month.")
        }
      }
    )
    
    tier3d_summary_tibble <- tibble::tibble(
      Tier = "Tier 3D",
      Description = "Half-life / Time to Unrestrictive Advisory (AOC)",
      Metric = metric_text,
      Outcome = outcome
    )
    
    list(
      results_tbl = results_tbl,
      results_kable = kable_html,
      metric_text = metric_text,
      report_text = report_text,
      tier3d_summary_tibble = tier3d_summary_tibble,
      outcome = outcome
    )
  }
  
  # ========= MAIN BODY =========
  ensure_dirs()
  key   <- make_key(params$species, params$contaminant, params$combine_ref, params$ref_1, params$ref_2, params$AOC_name)
  paths <- paths_for(key)
  
  # region labels/levels under combine_ref logic
  if (isTRUE(params$combine_ref)) {
    ref_1 <- "Reference"; ref_2 <- "none"
    region_levels <- c("AOC", ref_1)
    ref_label <- "Reference"
    ref_regions_to_match <- c(params$ref_1, params$ref_2)
  } else {
    ref_1 <- params$ref_1
    ref_2 <- params$ref_2
    region_levels <- c("AOC", ref_1, if (!is.null(ref_2) && ref_2 != "none") ref_2 else NULL)
    ref_label <- if (!is.null(ref_2) && ref_2 != "none") paste(ref_1, "and", ref_2) else ref_1
    ref_regions_to_match <- c(ref_1, if (!is.null(ref_2) && ref_2 != "none") ref_2 else NULL)
  }
  
  # contaminant labels/units
  contaminant <- params$contaminant
  contaminant_label <- if (toupper(contaminant) == "MERCURY") stringr::str_to_title(contaminant) else contaminant
  unit_label <- get_unit_label(contaminant)
  
  # shapefile guard
  use_aoc_shapefile <- !is.null(params$aoc_shapefile) && file.exists(params$aoc_shapefile)
  
  # ---------- Filter & region assignment ----------
  filtered_data <- raw_data %>%
    dplyr::filter(Specname == params$species, Contaminant == params$contaminant) %>%
    dplyr::mutate(region = NA_character_)
  
  has_coords <- !is.na(filtered_data$Longitude.Decimal) & !is.na(filtered_data$Latitude.Decimal)
  
  if (use_aoc_shapefile) {
    aoc_shp <- tryCatch(sf::st_read(params$aoc_shapefile, quiet = TRUE), error = function(e) NULL)
    if (!is.null(aoc_shp)) {
      aoc_shp <- sf::st_transform(aoc_shp, crs = params$target_sr)
      filtered_sf <- filtered_data[has_coords, ] %>%
        sf::st_as_sf(coords = c("Longitude.Decimal","Latitude.Decimal"), crs = params$target_sr) %>%
        sf::st_transform(crs = sf::st_crs(aoc_shp)) %>%
        sf::st_make_valid()
      aoc_match <- sf::st_intersects(filtered_sf, sf::st_make_valid(aoc_shp), sparse = FALSE)[,1]
      filtered_data$region[has_coords][aoc_match] <- "AOC"
    }
  }
  
  filtered_data <- filtered_data %>%
    dplyr::mutate(
      region = dplyr::case_when(
        region == "AOC" ~ "AOC",
        is.na(region) & stringr::str_detect(Locname.Fishbase, stringr::regex(stringr::str_c(params$add_AOC, collapse="|"), ignore_case = TRUE)) ~ "AOC",
        params$combine_ref & is.na(region) &
          (stringr::str_detect(Locname.Fishbase, stringr::fixed(params$ref_1, ignore_case = TRUE)) |
             stringr::str_detect(Locname.Fishbase, stringr::fixed(params$ref_2, ignore_case = TRUE))) ~ "Reference",
        !params$combine_ref & is.na(region) &
          stringr::str_detect(Locname.Fishbase, stringr::fixed(params$ref_1, ignore_case = TRUE)) ~ params$ref_1,
        !params$combine_ref & is.na(region) & !is.null(params$ref_2) & params$ref_2 != "none" &
          stringr::str_detect(Locname.Fishbase, stringr::fixed(params$ref_2, ignore_case = TRUE)) ~ params$ref_2,
        TRUE ~ region
      )
    ) %>%
    dplyr::filter(
      !stringr::str_detect(Locname.Fishbase, stringr::str_c(params$exclude_site, collapse="|")),
      !is.na(region)
    ) %>%
    dplyr::mutate(region = factor(region, levels = region_levels))
  
  full_data <- if (isTRUE(params$exclude_missing)) {
    filtered_data %>% dplyr::filter(!is.na(Longitude.Decimal), !is.na(Latitude.Decimal))
  } else filtered_data
  
  map_data <- filtered_data %>%
    dplyr::filter(Sample.Year >= 2014, !is.na(Longitude.Decimal), !is.na(Latitude.Decimal))
  
  missing_sites <- filtered_data %>%
    dplyr::filter(Sample.Year >= 2014, is.na(Longitude.Decimal) | is.na(Latitude.Decimal)) %>%
    dplyr::pull(Locname.Fishbase) %>% unique() %>% sort()
  
  recent_data <- filtered_data %>%
    dplyr::filter(Sample.Year >= 2014) %>%
    { if (isTRUE(params$exclude_missing)) dplyr::filter(., !is.na(Longitude.Decimal) & !is.na(Latitude.Decimal)) else . }
  
  # region colours (simple, stable)
  region_colours <- stats::setNames(
    c("red","cyan","magenta")[seq_along(region_levels)],
    region_levels
  )
  
  # ---------- Map (best-effort; falls back if maptiles/ggspatial missing) ----------
  map_plot <- NULL
  if (nrow(map_data) > 0) {
    sites_sf  <- sf::st_as_sf(map_data, coords = c("Longitude.Decimal","Latitude.Decimal"), crs = params$target_sr)
    sites_proj <- sf::st_transform(sites_sf, 3857)
    if (has_maptiles) {
      bbox_buffered <- sf::st_bbox(sites_proj)
      bbox_adjusted <- adjust_bbox_to_aspect(bbox_buffered, aspect_ratio = 10/6, crs = 3857)
      bbox_sfc <- sf::st_as_sfc(bbox_adjusted + c(-40000,-40000,40000,40000))
      basemap <- tryCatch(maptiles::get_tiles(bbox_sfc, provider = "Esri.WorldShadedRelief", crop = TRUE, zoom = 10), error=function(e) NULL)
      if (!is.null(basemap)) {
        bbox_vec <- sf::st_bbox(basemap)
        p_main <- ggplot2::ggplot() +
          maptiles::layer_spatial(basemap) +
          ggplot2::geom_sf(data = sites_proj, ggplot2::aes(color = region), size = 3.5, shape = 21, fill = "white", stroke = 1) +
          ggplot2::coord_sf(xlim = c(bbox_vec["xmin"], bbox_vec["xmax"]),
                            ylim = c(bbox_vec["ymin"], bbox_vec["ymax"]),
                            expand = FALSE, clip = "on", datum = NA) +
          ggplot2::labs(color = "Region") +
          ggplot2::theme_void(base_size = 12) +
          ggplot2::theme(
            legend.justification = c(1,0),
            legend.position = c(.98,.02),
            plot.margin = grid::unit(c(0,0,0,0), "pt")
          ) + ggplot2::scale_color_manual(values = region_colours)
        if (has_ggspatial) p_main <- p_main + ggspatial::annotation_scale(location = "bl", width_hint = 0.2)
        
        # Optional inset if AOC points exist
        aoc_sites <- sites_proj[sites_proj$region == "AOC", ]
        if (nrow(aoc_sites) > 0) {
          aoc_bbox <- sf::st_bbox(aoc_sites) + c(-10000,-10000,10000,10000)
          aoc_bbox_sfc <- sf::st_as_sfc(aoc_bbox)
          aoc_basemap <- tryCatch(maptiles::get_tiles(aoc_bbox_sfc, provider = "Esri.WorldShadedRelief", crop = TRUE, zoom = 12), error=function(e) NULL)
          if (!is.null(aoc_basemap)) {
            inset_map <- ggplot2::ggplot() +
              maptiles::layer_spatial(aoc_basemap) +
              ggplot2::geom_sf(data = sites_proj[sf::st_within(sites_proj, aoc_bbox_sfc, sparse = FALSE),],
                               ggplot2::aes(color = region), size = 2.5, shape = 21, fill = "white", stroke = 0.8) +
              ggplot2::coord_sf(xlim = c(aoc_bbox["xmin"], aoc_bbox["xmax"]),
                                ylim = c(aoc_bbox["ymin"], aoc_bbox["ymax"]), expand = FALSE) +
              ggplot2::scale_colour_manual(values = region_colours) +
              ggplot2::theme_void() +
              ggplot2::theme(panel.border = ggplot2::element_rect(color="black", fill=NA),
                             legend.position = "none")
            p_main <- p_main + ggplot2::geom_sf(data = sf::st_as_sfc(aoc_bbox), fill = NA, color = "red", linewidth = 0.5)
            map_plot <- cowplot::ggdraw(p_main) +
              cowplot::draw_plot(inset_map, x = -0.04, y = 0.6, width = 0.4, height = 0.35) +
              ggplot2::theme(plot.margin = grid::unit(c(0,0,0,0), "pt"))
          } else {
            map_plot <- p_main
          }
        } else {
          map_plot <- p_main
        }
      }
    }
    if (is.null(map_plot)) {
      # fallback: simple point plot (no basemap)
      map_plot <- ggplot2::ggplot(map_data, ggplot2::aes(x = Longitude.Decimal, y = Latitude.Decimal, color = region)) +
        ggplot2::geom_point(size=2) +
        ggplot2::scale_color_manual(values = region_colours) +
        ggplot2::theme_minimal() +
        ggplot2::labs(x="Longitude", y="Latitude", color="Region")
    }
  }
  
  # ---------- Reference site listing markdown ----------
  ref_sites_md <- tryCatch({
    recent_data %>%
      dplyr::distinct(region, site = Locname.Fishbase) %>%
      dplyr::arrange(region, site) %>%
      dplyr::group_by(region) %>%
      dplyr::reframe(markdown = paste0("#### ", unique(region), " {-}\n", paste0("- ", site, collapse = "\n")), .groups="drop") %>%
      dplyr::distinct(markdown) %>%
      dplyr::pull(markdown) %>%
      paste(collapse = "\n\n")
  }, error = function(e) "")
  
  # ---------- Tier 3A ----------
  virtual_advisory <- generate_virtual_advisories(
    df = recent_data, group_var = "region", contaminant = params$contaminant
  )
  pred_df <- virtual_advisory$predictions
  y_label <- if (toupper(contaminant) == "MERCURY") "Mercury Concentration (µg/g)"
  else if (toupper(contaminant) == "PCBs") "PCBs Concentration (ng/g)"
  else paste(contaminant_label, "(units)")
  
  thresholds_df <- get_thresholds(contaminant, population = "Sensitive")
  
  t3a_plot <- ggplot2::ggplot(pred_df, ggplot2::aes(x = Length, y = predicted, color = region)) +
    ggplot2::geom_point(data = recent_data, ggplot2::aes(x = Length, y = Value, color = region), size = 1.5, alpha = 0.7) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_hline(data = thresholds_df, ggplot2::aes(yintercept = conc), linetype = "dashed", color = "black") +
    ggplot2::geom_text(data = thresholds_df, ggplot2::aes(x = Inf, y = conc, label = paste0(meals, " meals/mo")),
                       hjust = 1.1, vjust = -0.2, color = "black", size = 3, inherit.aes = FALSE) +
    ggplot2::scale_colour_manual(values = region_colours) +
    ggplot2::labs(x = "Length (cm)", y = y_label, color = "Region") +
    ggplot2::theme_minimal(base_size = 13)
  
  # Model summary text
  model_summary <- virtual_advisory$model_info %>%
    dplyr::mutate(
      summary_text = purrr::pmap_chr(list(lm, correlated, pval, r2), function(mod, sig, pval, r2) {
        coef_vals <- coef(mod); a <- signif(exp(coef_vals[1]), 3); b <- round(coef_vals[2], 3)
        pval_fmt <- ifelse(pval < 0.001, "< 0.001", formatC(pval, format = "e", digits = 2))
        r2_fmt <- round(r2, 3)
        sig_text <- if (sig) "There is a statistically significant relationship between fish length and contaminant concentration."
        else "There is no significant relationship; bin-level means were used instead."
        model_text <- glue::glue("The power model used was: concentration = {a} × Length^{{{b}}} (R² = {r2_fmt}, p = {pval_fmt}).")
        paste0(sig_text, " ", model_text)
      })
    ) %>% dplyr::select(region, summary_text)
  
  # Tables and decisions
  predictions_binned <- virtual_advisory$predictions %>%
    dplyr::mutate(
      length_bin = cut(
        Length, breaks = c(seq(15,75,by=5), Inf),
        labels = c(paste(seq(15,70,by=5), seq(20,75,by=5), sep = "-"), "75+"),
        right = FALSE
      )
    ) %>%
    dplyr::filter(!is.na(length_bin)) %>%
    dplyr::group_by(region, length_bin) %>%
    dplyr::summarise(mean_conc = mean(predicted, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(
      meals_general   = assign_advisory(mean_conc, "General", params$contaminant),
      meals_sensitive = assign_advisory(mean_conc, "Sensitive", params$contaminant)
    )
  
  virtual_long <- predictions_binned %>%
    tidyr::pivot_longer(cols = c(meals_general, meals_sensitive),
                        names_to = "Population", names_prefix = "meals_", values_to = "Advisory") %>%
    dplyr::mutate(Population = dplyr::case_when(
      Population == "general" ~ "General",
      Population == "sensitive" ~ "Sensitive"
    )) %>%
    dplyr::filter(toupper(params$contaminant) == "MERCURY" | Population == "General")
  
  virtual_wide <- virtual_long %>%
    dplyr::select(Region = region, Population, length_bin, Advisory) %>%
    tidyr::pivot_wider(names_from = length_bin, values_from = Advisory)
  
  aoc_advisories <- virtual_long %>% dplyr::filter(region == "AOC") %>%
    dplyr::select(length_bin, Population, Advisory) %>% dplyr::rename(AOC_Advisory = Advisory)
  
  reference_advisories <- virtual_long %>% dplyr::filter(region != "AOC") %>%
    dplyr::group_by(length_bin, Population) %>%
    dplyr::summarise(Ref_Advisory = stats::median(Advisory, na.rm = TRUE), .groups="drop")
  
  tier3a_joined <- aoc_advisories %>%
    dplyr::left_join(reference_advisories, by = c("length_bin","Population")) %>%
    dplyr::mutate(
      restrict_pass = AOC_Advisory >= params$restrict_threshold,
      restrict_fail = !restrict_pass,
      more_restrictive_than_ref = AOC_Advisory < Ref_Advisory,
      final_decision = dplyr::case_when(
        restrict_pass ~ "Pass",
        restrict_fail & !more_restrictive_than_ref ~ "Pass",
        restrict_fail &  more_restrictive_than_ref ~ "Fail",
        TRUE ~ "Unknown"
      )
    )
  
  tier3a_summary <- tier3a_joined %>%
    dplyr::group_by(Population) %>%
    dplyr::summarise(
      n_bins = dplyr::n(),
      n_below_thresh = sum(restrict_fail),
      n_below_ref = sum(restrict_fail & more_restrictive_than_ref),
      n_pass = n_bins - n_below_ref,
      pct_pass = round(100 * n_pass / n_bins, 1),
      .groups = "drop"
    ) %>% dplyr::mutate(decision = ifelse(n_below_ref == 0, "Pass", "Fail"))
  
  tier3a_text <- tier3a_summary %>%
    dplyr::mutate(summary = glue::glue("**{Population}**: Of {n_bins} bins, {n_below_thresh} were below {params$restrict_threshold} meals/month; of these, {n_below_ref} were more restrictive than reference.")) %>%
    dplyr::pull(summary) %>% glue::glue_collapse(sep = " ")
  
  tier3a_overall <- if (any(tier3a_summary$decision == "Fail")) "Unsupportive" else "Supportive"
  
  tier3a_summary_tibble <- tibble::tibble(
    Tier = "Tier 3A",
    Description = "Virtual Advisory",
    Metric = glue::glue_collapse(glue::glue("{tier3a_summary$Population}: {tier3a_summary$n_below_ref}/{tier3a_summary$n_bins} bins restrictive vs ref"), sep = "; "),
    Outcome = tier3a_overall
  )
  
  # ---------- Tier 3B ----------
  gam_df <- recent_data %>%
    dplyr::mutate(Waterbody.Code = factor(Waterbody.Code), Sample.Year = factor(Sample.Year))
  
  res_gam <- fit_gam(gam_df, k_len = 10, method = "REML", verbose = TRUE)
  gam_model <- res_gam$gam_full
  gam_null  <- res_gam$gam_null
  interaction_p <- res_gam$interaction_p
  interaction_p_val <- if (!is.na(interaction_p)) signif(interaction_p, 3) else NA_character_
  
  # plot predictions (drop RE smooths)
  mf <- model.frame(gam_model)
  has_region_in_model <- "region" %in% names(mf)
  region_levels_model <- if (has_region_in_model) levels(mf$region) else NULL
  region_levels_plot <- if (!is.null(region_levels_model)) region_levels_model else sort(unique(as.character(recent_data$region)))
  
  len_seq_raw <- seq(min(recent_data$Length, na.rm=TRUE),
                     max(recent_data$Length, na.rm=TRUE),
                     length.out = 600)
  preds_plot <- tidyr::crossing(region = region_levels_plot, Length = len_seq_raw)
  if (has_region_in_model) preds_plot$region <- factor(preds_plot$region, levels = region_levels_model)
  
  if ("Waterbody.Code" %in% names(mf)) {
    preds_plot$Waterbody.Code <- if (is.factor(mf$Waterbody.Code))
      factor(levels(mf$Waterbody.Code)[1], levels = levels(mf$Waterbody.Code)) else mf$Waterbody.Code[1]
  }
  if ("Sample.Year" %in% names(mf)) {
    preds_plot$Sample.Year <- if (is.factor(mf$Sample.Year))
      factor(levels(mf$Sample.Year)[1], levels = levels(mf$Sample.Year)) else stats::median(as.numeric(as.character(recent_data$Sample.Year)), na.rm=TRUE)
  }
  exclude_re <- vapply(gam_model$smooth, function(s) { bs <- tryCatch(s$bs, error=function(...) NULL); if (!is.null(bs) && identical(bs,"re")) s$label else NA_character_ }, character(1))
  exclude_re <- stats::na.omit(exclude_re)
  pr <- predict(gam_model, newdata = preds_plot, type = "link", se.fit = TRUE, exclude = exclude_re, unconditional = FALSE)
  
  preds_plot <- preds_plot %>%
    dplyr::mutate(fit = as.numeric(pr$fit), se = as.numeric(pr$se.fit),
                  fit_bt = exp(fit), lower_bt = exp(fit - 1.96*se), upper_bt = exp(fit + 1.96*se))
  
  len_limits <- recent_data %>%
    dplyr::group_by(region) %>%
    dplyr::summarise(Lmin = stats::quantile(Length, .01, na.rm=TRUE),
                     Lmax = stats::quantile(Length, .99, na.rm=TRUE), .groups="drop") %>%
    dplyr::mutate(region = as.character(region))
  
  preds_plot <- preds_plot %>%
    dplyr::mutate(region = as.character(region)) %>%
    dplyr::inner_join(len_limits, by = "region") %>%
    dplyr::filter(Length >= Lmin, Length <= Lmax)
  
  gam_fit_plot <- ggplot2::ggplot(recent_data, ggplot2::aes(Length, Value, color = region)) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::geom_ribbon(data = preds_plot, ggplot2::aes(ymin = lower_bt, ymax = upper_bt, x = Length, fill = region),
                         alpha = 0.15, inherit.aes = FALSE) +
    ggplot2::geom_line(data = preds_plot, ggplot2::aes(y = fit_bt, x = Length, color = region), linewidth = 1.2) +
    ggplot2::labs(x = "Length (cm)", y = y_label) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::scale_colour_manual(values = region_colours, guide = "legend") +
    ggplot2::scale_fill_manual(values = region_colours, guide = "legend")
  
  # table + text
  if (isTRUE(interaction_p < 0.05)) {
    gam_table <- generate_gam_prediction_table(gam_model, recent_data, compare_to = "Reference", alpha = 0.05, back_transform = TRUE)
    length_seq <- sort(unique(attr(gam_table, "preds")$Length))
    
    t3b_summary_text <- glue::glue(
      "GAMs modeled {contaminant_label} vs length for {params$species} (AOC vs reference). ",
      "The relationship differed significantly (interaction p = {interaction_p_val}). ",
      "Predicted concentrations every 5 cm from {min(length_seq)}–{max(length_seq)} cm are shown; asterisks mark sizes where AOC differs (p < 0.05)."
    )
    
    contr <- lpmatrix_contrasts(gam_model, length_seq, recent_data, aoc_name = "AOC")
    decision_summary_3b <- contr %>% dplyr::mutate(higher_aoc = est > 0 & p < 0.05)
    total_bins <- dplyr::n_distinct(decision_summary_3b$Length)
    n_fail_b   <- decision_summary_3b %>%
      dplyr::group_by(Length) %>%
      dplyr::summarise(any_higher = any(higher_aoc, na.rm = TRUE), .groups="drop") %>%
      dplyr::summarise(n = sum(any_higher)) %>% dplyr::pull(n)
    n_pass_b <- total_bins - n_fail_b
    
    tier3b_decision_text <- glue::glue("Of {total_bins} lengths, {n_fail_b} had significantly higher predicted AOC concentrations (p < 0.05).")
    tier3b_overall <- if (n_fail_b > total_bins/2) "Unsupportive" else "Supportive"
    
    tier3b_metric_text <- glue::glue("Different smooths (p = {signif(interaction_p)}); {n_fail_b}/{total_bins} lengths with higher AOC concentration")
    
  } else {
    em_summary <- summary(emmeans::emmeans(gam_null, ~ region))
    em_aoc  <- em_summary %>% dplyr::filter(region == "AOC")
    em_refs <- em_summary %>% dplyr::filter(region != "AOC")
    ref_texts <- em_refs %>%
      dplyr::mutate(out = glue::glue("{region}: {round(emmean,2)} {unit_label} (95% CI: {round(lower.CL,2)}–{round(upper.CL,2)})")) %>%
      dplyr::pull(out) %>% paste(collapse = "; ")
    
    t3b_summary_text <- glue::glue(
      "GAMs modeled {contaminant_label} vs length for {params$species}. ",
      "No significant difference in shapes (interaction p = {interaction_p_val}). ",
      "Estimated marginal means (shared smooths): AOC {round(em_aoc$emmean,2)} {unit_label} (95% CI {round(em_aoc$lower.CL,2)}–{round(em_aoc$upper.CL,2)}); ",
      "References: {ref_texts}."
    )
    
    tier3b_decision_text <- if (em_aoc$emmean > max(em_refs$emmean)) {
      glue::glue("AOC marginal mean exceeds reference.")
    } else glue::glue("AOC marginal mean not higher than reference.")
    tier3b_overall <- if (em_aoc$emmean > max(em_refs$emmean)) "Unsupportive" else "Supportive"
    
    tier3b_metric_text <- glue::glue("No smooth difference (p = {round(interaction_p,3)}); AOC marginal mean {if (tier3b_overall=='Unsupportive') 'higher' else 'not higher'}")
    # fabricate a simple table-like tibble if needed
    gam_table <- tibble::as_tibble(em_summary)
  }
  
  tier3b_summary_tibble <- tibble::tibble(
    Tier = "Tier 3B",
    Description = "Contaminant Concentration vs Reference",
    Metric = tier3b_metric_text,
    Outcome = tier3b_overall
  )
  
  # ---------- Tier 3C ----------
  trend_results <- plot_temporal_trends(df = full_data, include_reference = FALSE, include_aoc = TRUE)
  
  interaction_fmt <- ifelse(
    is.na(trend_results$interaction_p), "NA",
    ifelse(trend_results$interaction_p < 0.001, "< 0.001", signif(trend_results$interaction_p, 3))
  )
  
  temporal_text <- {
    if (trend_results$interaction_sig) {
      bin_str <- glue::glue_collapse(trend_results$top_bins, sep = ", ", last = " and ")
      glue::glue(
        "Temporal {contaminant_label} ({unit_label}) trends for {params$species}, grouped by region and top 3 size bins ({bin_str} cm). ",
        "Lines represent linear fits (length × year interaction p = {interaction_fmt})."
      )
    } else {
      glue::glue(
        "Temporal {contaminant_label} ({unit_label}) trends for {params$species} by region. ",
        "Lines are linear fits to annual means (interaction p = {interaction_fmt})."
      )
    }
  }
  
  tier3c_result_text <- if (trend_results$interaction_sig) {
    tier3c_summary_bins <- trend_results$trend_bins %>%
      dplyr::filter(region == "AOC") %>%
      dplyr::count(trend, name = "n") %>%
      dplyr::mutate(trend = factor(trend, levels = c("Declining","Stable","Increasing"))) %>%
      dplyr::arrange(trend)
    format_trend_phrase <- function(n, trend) {
      verb <- ifelse(n == 1, "was", "were"); bin_word <- ifelse(n == 1, "bin", "bins")
      glue::glue("{n} {bin_word} {verb} {tolower(trend)}")
    }
    trend_sentences <- purrr::pmap_chr(list(n = tier3c_summary_bins$n, trend = tier3c_summary_bins$trend), format_trend_phrase) %>%
      glue::glue_collapse(sep = ", ", last = ", and ")
    glue::glue("In the AOC, {trend_sentences}.")
  } else {
    aoc_slope <- trend_results$model_stats %>% dplyr::filter(region == "AOC") %>% dplyr::pull(slope)
    direction <- dplyr::case_when(aoc_slope > 0 ~ "increasing", aoc_slope < 0 ~ "decreasing", TRUE ~ "stable")
    glue::glue("In the AOC, {contaminant_label} concentrations were {direction} over time.")
  }
  
  tier3c_overall <- if (trend_results$interaction_sig) {
    any_increasing <- trend_results$trend_bins %>%
      dplyr::filter(region == "AOC") %>%
      dplyr::pull(trend) %>% any(. == "Increasing")
    if (any_increasing) "Unsupportive" else "Supportive"
  } else {
    aoc_slope <- trend_results$model_stats %>% dplyr::filter(region == "AOC") %>% dplyr::pull(slope)
    if (aoc_slope > 0) "Unsupportive" else "Supportive"
  }
  
  tier3c_metric_text <- if (trend_results$interaction_sig) {
    trend_counts <- trend_results$trend_bins %>%
      dplyr::filter(region == "AOC") %>%
      dplyr::count(trend) %>%
      tidyr::complete(trend = c("Declining","Stable","Increasing"), fill = list(n=0))
    glue::glue("Length × year interaction (p = {interaction_fmt}); {trend_counts$n[trend_counts$trend=='Declining']} declining, {trend_counts$n[trend_counts$trend=='Stable']} stable, {trend_counts$n[trend_counts$trend=='Increasing']} increasing bins")
  } else {
    aoc_slope <- trend_results$model_stats %>% dplyr::filter(region == "AOC") %>% dplyr::pull(slope)
    direction <- dplyr::case_when(aoc_slope > 0 ~ "increasing", aoc_slope < 0 ~ "decreasing", TRUE ~ "stable")
    glue::glue("No length × year interaction (p = {interaction_fmt}); overall AOC trend: {direction}")
  }
  
  tier3c_summary_tibble <- tibble::tibble(
    Tier = "Tier 3C",
    Description = "Temporal Trends",
    Metric = tier3c_metric_text,
    Outcome = tier3c_overall
  )
  
  # ---------- Tier 3D ----------
  g_gamm <- mgcv::gamm(
    log(Value) ~ Sample.Year + s(Length, k = 6) + Sample.Year*region,
    random = list(Waterbody.Code = ~ 1),
    data = filtered_data
  )
  t3d_gam <- g_gamm$gam
  # assemble half-life table
  co <- coef(t3d_gam); vc <- vcov(t3d_gam)
  regs <- levels(filtered_data$region)
  lc_est <- function(coefs, vc, pick) {
    a <- numeric(length(coefs)); names(a) <- names(coefs); a[names(pick)] <- pick
    est <- sum(a * coefs); se  <- sqrt(as.numeric(t(a) %*% vc %*% a))
    list(est = est, se = se)
  }
  hl_tab <- purrr::map_dfr(regs, function(rg) {
    pick <- c(`Sample.Year` = 1)
    int_name <- paste0("Sample.Year:region", rg)
    if (int_name %in% names(co)) pick[int_name] <- 1
    est <- lc_est(co, vc, pick)
    slope <- est$est; slope_se <- est$se
    k <- -slope; ln2 <- log(2)
    t12 <- ifelse(k > 0, ln2 / k, NA_real_)
    t2  <- ifelse(k <= 0, ln2 / abs(k), NA_real_)
    dtds <- ln2 / (slope^2)
    t12_se <- ifelse(!is.na(t12), abs(dtds) * slope_se, NA_real_)
    t2_se  <- ifelse(!is.na(t2),  abs(dtds) * slope_se, NA_real_)
    tibble::tibble(
      region = rg,
      slope_log_per_year = slope,
      slope_se = slope_se,
      k = k,
      half_life_years = t12,
      half_life_lwr = ifelse(!is.na(t12), t12 - 1.96 * t12_se, NA_real_),
      half_life_upr = ifelse(!is.na(t12), t12 + 1.96 * t12_se, NA_real_),
      doubling_time_years = t2,
      doubling_lwr = ifelse(!is.na(t2), t2 - 1.96 * t2_se, NA_real_),
      doubling_upr = ifelse(!is.na(t2), t2 + 1.96 * t2_se, NA_real_)
    )
  })
  
  aoc_out <- compute_years_to_threshold_AOC(
    t3d_gam, hl_tab, thresholds_df, params$restrict_threshold, filtered_data, aoc_level = "AOC"
  )
  
  # ---------- Assemble result ----------
  res <- list(
    meta = list(
      species = params$species,
      contaminant = params$contaminant,
      aoc_name = params$AOC_name,
      region_levels = region_levels,
      restrict_threshold = params$restrict_threshold,
      unit_label = unit_label,
      params = params,
      session = utils::sessionInfo()
    ),
    t3a = list(
      plot = t3a_plot,
      table_long = virtual_long,
      table_wide = virtual_wide,
      text = tier3a_text,
      overall = tier3a_overall,
      summary_row = tier3a_summary_tibble,
      model_summary = model_summary
    ),
    t3b = list(
      fig = gam_fit_plot,
      table = gam_table,
      text_summary = t3b_summary_text,
      text_decision = tier3b_decision_text,
      overall = tier3b_overall,
      summary_row = tier3b_summary_tibble,
      interaction_p = interaction_p
    ),
    t3c = list(
      fig = trend_results$plot,
      temporal_text = temporal_text,
      result_text = tier3c_result_text,
      overall = tier3c_overall,
      summary_row = tier3c_summary_tibble
    ),
    t3d = list(
      hl_table = hl_tab,
      kable_html = aoc_out$results_kable,
      report_text = aoc_out$report_text,
      overall = aoc_out$outcome,
      summary_row = aoc_out$tier3d_summary_tibble
    ),
    ref = list(
      map = map_plot,
      sites_md = ref_sites_md,
      missing_sites = missing_sites
    ),
    summary = dplyr::bind_rows(
      tier3a_summary_tibble, tier3b_summary_tibble, tier3c_summary_tibble, aoc_out$tier3d_summary_tibble
    )
  )
  
  # ---------- Export artifacts ----------
  if (isTRUE(save_artifacts)) {
    export_plot(res$ref$map, paths$figs, "map", width = 7.5, height = 4.5)
    export_plot(res$t3a$plot, paths$figs, "t3a_virtual")
    export_plot(res$t3b$fig, paths$figs, "t3b_gam")
    export_plot(res$t3c$fig, paths$figs, "t3c_trends")
    
    # widget/table snapshots if present
    if (has_reactable && inherits(res$t3b$table, "reactable")) {
      widget_to_png(res$t3b$table, file.path(paths$wigs, "t3b_table.png"))
    }
    # if you later build a reactable for T3A:
    if (has_reactable && inherits(res$t3a$table_wide, "reactable")) {
      widget_to_png(res$t3a$table_wide, file.path(paths$wigs, "t3a_table.png"))
    }
    
    saveRDS(res, paths$rds)
  }
  
  return(res)
}
