# ---- Helpers --------------------------------------------------------------

#' Validate a GAM model (mgcv) and return a tidy summary tibble
#'
#' @param fit   mgcv::gam or mgcv::bam object
#' @param data  optional data used to fit (only needed if you also want to re-fit a null model)
#' @param acf_lag max lag for ACF/PACF checks (default 36)
#' @param lb_lags integer vector of lags for Ljung-Box tests (default c(1, 2, 3, 12))
#' @param use_pearson if TRUE use Pearson residuals for ACF/PACF (recommended)
#' @return tibble with per-model diagnostics + per-term tables (as list-cols)
validate_gam <- function(fit,
                         data = NULL,
                         acf_lag = 36,
                         lb_lags = c(1, 2, 3, 12),
                         use_pearson = TRUE) {
  stopifnot(inherits(fit, c("gam", "bam")))
  
  # Residuals
  rtype <- if (use_pearson) "pearson" else "deviance"
  res   <- residuals(fit, type = rtype)
  
  # ---- gam.check info (k-index, etc.)
  gcapture <- utils::capture.output(
    gchk <- mgcv::gam.check(fit, rep = 0, k.sample = 500),
    type = "output"
  )
  # gchk$k is a vector of k-index values (one per smooth) when available
  k_index <- tryCatch(as.numeric(gchk$k), error = function(e) NA_real_)
  k_pval  <- tryCatch(as.numeric(gchk$p), error = function(e) NA_real_)
  k_tbl <- tibble::tibble(
    smooth = names(k_index %||% rep(NA_character_, length(k_index))),
    k_index = k_index,
    k_pval  = k_pval,
    k_flag  = ifelse(is.finite(k_index) & k_index < 1, TRUE, FALSE)
  )
  
  # ---- Term tables
  smry <- summary(fit)
  s_tbl <- if (!is.null(smry$s.table)) {
    st <- as.data.frame(smry$s.table)
    st$term <- rownames(smry$s.table)
    tibble::as_tibble(st[, c("term", "edf", "Ref.df", "F", "p-value")])
  } else {
    tibble::tibble(term = character(), edf = numeric(), Ref.df = numeric(), F = numeric(), `p-value` = numeric())
  }
  
  p_tbl <- if (!is.null(smry$p.table)) {
    pt <- as.data.frame(smry$p.table)
    pt$term <- rownames(smry$p.table)
    tibble::as_tibble(pt[, c("term", "Estimate", "Std. Error", "t value", "Pr(>|t|)")])
  } else {
    tibble::tibble(term = character(), Estimate = numeric(), `Std. Error` = numeric(), `t value` = numeric(), `Pr(>|t|)` = numeric())
  }
  
  # ---- Concurvity (can be expensive on large models)
  conc <- tryCatch(mgcv::concurvity(fit, full = FALSE), error = function(e) NULL)
  conc_max <- if (!is.null(conc) && !is.null(conc$estimate)) {
    # take the max concurvity across terms, excluding self-entries
    suppressWarnings(max(conc$estimate[is.finite(conc$estimate)], na.rm = TRUE))
  } else NA_real_
  conc_flag <- is.finite(conc_max) && conc_max > 0.90
  
  # ---- ACF / PACF
  acf_obj  <- stats::acf(res, plot = FALSE, lag.max = acf_lag, na.action = na.pass)
  pacf_obj <- stats::pacf(res, plot = FALSE, lag.max = acf_lag, na.action = na.pass)
  acf_vals  <- as.numeric(acf_obj$acf)[-1]   # drop lag 0
  pacf_vals <- as.numeric(pacf_obj$acf)
  n <- sum(is.finite(res))
  ci <- 1.96 / sqrt(n)
  acf_flag  <- any(abs(acf_vals)  > ci, na.rm = TRUE)
  pacf_flag <- any(abs(pacf_vals) > ci, na.rm = TRUE)
  
  # ---- Ljung-Box tests at a few lags
  lb_tbl <- purrr::map_dfr(lb_lags, function(L) {
    p <- tryCatch(stats::Box.test(res, lag = L, type = "Ljung-Box")$p.value,
                  error = function(e) NA_real_)
    tibble::tibble(lag = L, p_value = p)
  })
  lb_flag <- any(lb_tbl$p_value < 0.05, na.rm = TRUE)
  
  # ---- Family / link / fit stats
  fam   <- fit$family$family
  link  <- fit$family$link
  r2    <- smry$r.sq %||% NA_real_
  r2_adj<- smry$dev.expl %||% NA_real_  # mgcv calls this "deviance explained"
  gcv   <- fit$gcv.ubre %||% NA_real_   # often NA for REML, but included for completeness
  scale <- fit$scale %||% NA_real_
  
  # ---- Assemble model-level tibble
  tibble::tibble(
    family = fam,
    link   = link,
    n      = n,
    r2     = r2,
    dev_expl = r2_adj,
    scale  = scale,
    gcv_ubre = gcv,
    acf_flag = acf_flag,
    pacf_flag = pacf_flag,
    lb_flag = lb_flag,
    concurvity_max = conc_max,
    concurvity_flag = conc_flag,
    k_table = list(k_tbl),
    smooth_table = list(s_tbl),
    param_table  = list(p_tbl),
    lb_table     = list(lb_tbl),
    gamcheck_text = list(paste(gcapture, collapse = "\n"))
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x
