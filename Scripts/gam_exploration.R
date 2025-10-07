# Model data, factorizing inputs
gam_df = recent_data %>%
  mutate(Waterbody.Code = factor(Waterbody.Code))


gam_df_ar <- gam_df %>%
  arrange(Waterbody.Code, Sample.Year) %>%
  group_by(Waterbody.Code) %>%
  mutate(AR_start = row_number() == 1) %>%
  ungroup()

# Fit GAMs
gam_model <- gam(Value ~ s(Length, by = region) + s(Waterbody.Code, bs = "re") + s(Sample.Year, k = 10) + region,
                 data = gam_df,
                 method = "REML")


gam_model_2 <- gam(Value ~ s(Length, region, bs = "fs", k = 10) + s(Waterbody.Code, bs = "re") + s(Sample.Year, k = 10),
                   data = gam_df,
                   method = "REML")

gam_model_tensor = gam(Value ~ te(Length, Sample.Year, by = region, k = c(10, 10)) + region + s(Waterbody.Code, bs = "re") + region,
                       data = gam_df,
                       method = "REML")

gam_rando <- gam(log(Value) ~ s(Length, region, bs = "fs", k = 10) + s(Waterbody.Code, bs = "re") + s(Sample.Year, bs = "re"),
                            data = gam_df,
                            method = "REML")
gam_rando1 <- gam(log(Value) ~ s(Length, region, bs = "fs", k = 10) + s(Waterbody.Code, bs = "re"),
                 data = gam_df,
                 method = "REML")

gam_fam = gam(Value ~ s(Length, region, bs = "fs", k = 10) + s(Waterbody.Code, bs = "re"), 
              family = gaussian(link = "log"),
              data = gam_df,
              method = "REML")

gam.check(gam_fam)
acf(resid(gam_fam, type = "pearson"), lag.max = 36, main = "ACF (Pearson)")

interaction_test <- anova(gam_rando, gam_rando1, test = "Chisq")
summary(interaction_test)

fit_lean <- gam(
  Value ~ 
    s(Length, region, bs = "fs", k = 8) +   # region-varying smooth of Length (no extra +region needed here)
    region:Sample.Year +                    # linear Year slope by region (parsimonious)
    s(Waterbody.Code, bs = "re"),           # random intercept for waterbody
  data   = gam_df,
  method = "REML"
)

gam.check(fit_lean)
acf(resid(fit_lean, type = "pearson"), lag.max = 36, main = "ACF (Pearson)")

# Null GAM to test for significance of interaction term
gam_null <- gam(Value ~ s(Length) + region,
                data = recent_data,
                method = "REML")

interaction_test <- anova(gam_null, gam_rando, test = "Chisq")
interaction_p <- interaction_test$`Pr(>Chi)`[2]
interaction_p_val <- signif(interaction_p, 3)

gam.check(gam_fam)

par(mar=c(3,3,3,0),mfrow=c(1,2))
acf(resid(gam_rando1), lag.max = 36, main = "ACF")
pacf(resid(gam_rando1), lag.max = 36, main = "pACF")

plot(gam_rando1)

summary(gam_rando1)




#1. Autocorrelation in residuals
#
#You checked ACF/PACF and/or Ljung–Box and saw significant correlation.
#That means residuals aren’t independent → the GAM is underestimating uncertainty (CIs too narrow, p-values too optimistic).
#
#Options:
#  
#  If data are time series (e.g. annual, seasonal fish samples):
#  
#  Use bam() with AR correction:

fit_ar <- bam(Value ~ s(Length, region, bs = "fs", k = 10) + 
                s(Waterbody.Code, bs = "re") +
                s(Sample.Year, k = 10),
              data = gam_df_ar,
              method = "fREML",
              rho = 0.3,         # start value, will need tuning
              AR.start = gam_df_ar$AR_start)  # logical vector for series starts

acf(residuals(fit_ar, type="pearson"), main="ACF (AR-corrected)")

#rho is the estimated lag-1 autocorrelation. You can fit once, inspect residuals, and re-estimate.

#AR.start tells the function where independent series begin (important if you have multiple sites).

gam.check(fit_ar)
acf(resid(fit_ar), lag.max = 36, main = "ACF")
pacf(resid(fit_ar), lag.max = 36, main = "pACF")

# Or use gamm() (wrapper around lme) with a correlation structure:
fit_gamm <- gamm(Value ~ s(Length, by = region) + 
                   s(Sample.Year, k = 10) + region,
                 random = list(Waterbody.Code = ~1),
                 correlation = corARMA(p = 1), # AR(1)
                 data = gam_df)

summary(fit_gamm)
gam.check(fit_gamm)

gam_valid = validate_gam(gam_model_2, data = gam_df)

gam.check(fit_ar)


knitr::kable(
  gam_valid %>%
    transmute(
      family, link, n, r2 = round(r2, 3),
      dev_expl = round(dev_expl, 3),
      acf_flag, pacf_flag, lb_flag,
      concurvity_max = round(concurvity_max, 3), concurvity_flag
    ),
  caption = "GAM validation summary"
)

# Expand the per-term tables when needed (e.g., in collapsible sections)
smooth_terms   <- gam_valid$smooth_table[[1]]
param_terms    <- gam_valid$param_table[[1]]
k_diagnostics  <- gam_valid$k_table[[1]]
lb_diagnostics <- gam_valid$lb_table[[1]]

# Example: flag text you can drop into the report
issue_msgs <- c(
  if (gam_valid$acf_flag)  "Residual ACF shows significant autocorrelation." else NULL,
  if (gam_valid$lb_flag)   "Ljung–Box test indicates autocorrelation at some lags." else NULL,
  if (any(k_diagnostics$k_flag, na.rm = TRUE)) "Some smooths suggest k may be too low (k-index < 1)." else NULL,
  if (gam_valid$concurvity_flag) "High concurvity detected (> 0.90). Consider simplifying terms." else NULL
)
if (length(issue_msgs) == 0) issue_msgs <- "No major validation flags detected."
cat(paste0("**Validation notes:** ", paste(issue_msgs, collapse = " ")))


concurvity(gam_model, full = T)
















# --- 1) minimal predictions (dense grid per region; REs dropped) ---
mf <- model.frame(gam_rando1)
exclude_re <- names(gam_rando1$smooth)[sapply(gam_rando1$smooth, function(s) s$bs == "re")]

preds_plot <- tidyr::crossing(
  region = levels(mf$region),
  Length = seq(min(recent_data$Length, na.rm = TRUE),
               max(recent_data$Length, na.rm = TRUE),
               length.out = 600)
) %>%
  mutate(
    Waterbody.Code = if ("Waterbody.Code" %in% names(mf))
      (if (is.factor(mf$Waterbody.Code)) levels(mf$Waterbody.Code)[1] else mf$Waterbody.Code[1]) else NA,
    
  ) %>%
  { pr <- predict(gam_rando1, newdata = ., type = "link", se.fit = TRUE,
                  exclude = exclude_re, unconditional = FALSE);
  mutate(., fit = as.numeric(pr$fit), se = as.numeric(pr$se.fit),
         fit_bt = exp(fit),
         lower_bt = exp(fit - 1.96*se),
         upper_bt = exp(fit + 1.96*se)) }

len_limits <- recent_data %>%
  group_by(region) %>%
  summarise(Lmin = quantile(Length, 0.02, na.rm=TRUE),
            Lmax = quantile(Length, 0.98, na.rm=TRUE), .groups="drop")

preds_plot <- preds_plot %>% inner_join(len_limits, by="region") %>%
  filter(Length >= Lmin, Length <= Lmax)

# --- 2) plot your actual model curve + CI in ggplot ---
ggplot(recent_data, aes(Length, Value, color = region)) +
  geom_point(alpha = 0.6) +
  geom_ribbon(data = preds_plot, aes(ymin = lower_bt, ymax = upper_bt, x = Length, fill = region),
              alpha = 0.15, inherit.aes = FALSE) +
  geom_line(data = preds_plot, aes(y = fit_bt, x = Length, color = region), linewidth = 1.2) +
  labs(title = "Contaminant vs Length (region-level GAM, REs excluded)",
       x = "Length (cm)", y = y_label) +
  theme_minimal(base_size = 14) +
  scale_colour_manual(values = region_colours) +
  scale_fill_manual(values = region_colours)
