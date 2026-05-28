# =====================================================================
# 04_analysis.R
# Main regression analysis:
#   Layer 1 - Headline pooled LPM/logit with over_obese x active interaction
#   Layer 2 - 2023-only robustness with CDC PA guideline variables
#   Layer 3 - Pandemic heterogeneity (2021 vs 2023)
#   Layer 4 - Race/ethnicity heterogeneity
# All with state + year FE, cluster-robust SEs at state level.
# =====================================================================

suppressMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(sandwich)
  library(lmtest)
  library(modelsummary)
  library(broom)
})

proj_dir <- "D:/Academic/2026/ECN 140/Project"
fig_dir  <- file.path(proj_dir, "figures")
out_dir  <- file.path(proj_dir, "output")
dir.create(out_dir, showWarnings = FALSE)

d <- readRDS(file.path(proj_dir, "clean/brfss_pooled.rds"))

# ----- Analysis sample: complete cases on key analytic vars -----
analysis <- d %>%
  filter(
    !is.na(diabetes), !is.na(over_obese), !is.na(active),
    !is.na(age), !is.na(female), !is.na(race), !is.na(educ),
    !is.na(income), !is.na(current_smoker)
  ) %>%
  mutate(
    # Factor versions for FE
    state_fe = factor(state_fips),
    year_fe  = factor(year),
    race     = factor(race, levels = c("White","Black","Hispanic","Asian",
                                        "AIAN","NHPI","Multi","Other")),
    educ     = factor(educ),
    income   = factor(income),
    bmi_cat  = factor(bmi_cat,
                      levels = c("Underweight","Normal weight","Overweight","Obese"))
  )

cat(sprintf("Analysis N = %d\n", nrow(analysis)))
cat("Year x state counts:\n")
print(table(analysis$year, useNA = "ifany"))

# Save the analysis sample so the paper code-appendix can load it directly
saveRDS(analysis, file.path(out_dir, "analysis_sample.rds"), compress = "xz")

# =====================================================================
# Layer 1: HEADLINE pooled LPM, building up controls + FE
# =====================================================================

cat("\n========== LAYER 1: pooled LPM ==========\n")

f1 <- diabetes ~ over_obese
f2 <- diabetes ~ over_obese + age + female + factor(race) +
                 factor(income) + factor(educ) + current_smoker
f3 <- diabetes ~ over_obese + age + female + factor(race) +
                 factor(income) + factor(educ) + current_smoker +
                 state_fe + year_fe
f4 <- diabetes ~ over_obese * active + age + female + factor(race) +
                 factor(income) + factor(educ) + current_smoker +
                 state_fe + year_fe   # HEADLINE INTERACTION

cat("Fitting M1..."); m1 <- lm(f1, data = analysis); cat(" done\n")
cat("Fitting M2..."); m2 <- lm(f2, data = analysis); cat(" done\n")
cat("Fitting M3..."); m3 <- lm(f3, data = analysis); cat(" done\n")
cat("Fitting M4 (headline interaction)..."); m4 <- lm(f4, data = analysis); cat(" done\n")

# ----- Cluster-robust SE at state level (Lecture 16) -----
clust_vcov <- function(model) {
  vcovCL(model, cluster = ~state_fips, type = "HC1")
}
v1 <- clust_vcov(m1); v2 <- clust_vcov(m2); v3 <- clust_vcov(m3); v4 <- clust_vcov(m4)

cat("\n--- M4 headline coefficients (cluster-robust SE at state) ---\n")
print(coeftest(m4, vcov = v4)[c("over_obese","active","over_obese:active"), ])

# ----- Headline LPM table -----
ms_main <- modelsummary(
  list(
    "(1) Baseline" = m1,
    "(2) +Controls" = m2,
    "(3) +State & Year FE" = m3,
    "(4) +Interaction" = m4
  ),
  vcov = list(v1, v2, v3, v4),
  stars = c('*'=0.10, '**'=0.05, '***'=0.01),
  coef_map = c(
    "over_obese"          = "Overweight or Obese",
    "active"              = "Active (any leisure-time PA)",
    "over_obese:active"   = "Overweight/Obese × Active",
    "age"                 = "Age",
    "female"              = "Female",
    "current_smoker"      = "Current smoker"
  ),
  gof_map = c("nobs","r.squared","adj.r.squared"),
  notes = list(
    "Linear probability models. Dependent variable: 1 if self-reported diabetes (DIABETE4=1), 0 if no diabetes (DIABETE4=3).",
    "Standard errors clustered at state level. Race, income, education entered as factors (coefficients omitted for space)."
  ),
  output = file.path(out_dir, "table1_headline_lpm.md")
)
ms_main_console <- modelsummary(
  list(
    "(1) Baseline" = m1,
    "(2) +Controls" = m2,
    "(3) +State & Year FE" = m3,
    "(4) +Interaction" = m4
  ),
  vcov = list(v1, v2, v3, v4),
  stars = c('*'=0.10, '**'=0.05, '***'=0.01),
  coef_map = c(
    "over_obese"          = "Overweight or Obese",
    "active"              = "Active (any leisure-time PA)",
    "over_obese:active"   = "Overweight/Obese × Active",
    "age"                 = "Age",
    "female"              = "Female",
    "current_smoker"      = "Current smoker"
  ),
  gof_map = c("nobs","r.squared","adj.r.squared"),
  output = "data.frame"
)
print(ms_main_console)

# =====================================================================
# Layer 1b: Logit version of the headline (Lecture 17)
# =====================================================================
cat("\n========== Logit version of headline ==========\n")
logit_h <- glm(f4, data = analysis, family = binomial("logit"))
cat("Logit done. Coef on key terms:\n")
key_terms <- c("over_obese","active","over_obese:active")
print(coef(summary(logit_h))[key_terms, ])

# Average marginal effects via numerical derivative around the means
# (Simpler: report predicted-probability differences for the four cells)
new_pp <- expand.grid(over_obese = c(0,1), active = c(0,1)) %>%
  mutate(
    age = mean(analysis$age, na.rm = TRUE),
    female = mean(analysis$female, na.rm = TRUE),
    current_smoker = mean(analysis$current_smoker, na.rm = TRUE),
    race = factor("White", levels = levels(analysis$race)),
    income = factor("5_50-100k", levels = levels(analysis$income)),
    educ = factor("3_Some college", levels = levels(analysis$educ)),
    state_fe = factor("6", levels = levels(analysis$state_fe)),   # CA
    year_fe = factor("2023", levels = levels(analysis$year_fe))
  )
new_pp$pred_lpm   <- predict(m4, newdata = new_pp)
new_pp$pred_logit <- predict(logit_h, newdata = new_pp, type = "response")
cat("\nPredicted P(diabetes) at sample mean / White / mid-income / CA / 2023:\n")
print(new_pp[, c("over_obese","active","pred_lpm","pred_logit")])

# =====================================================================
# Marginal-effects plot (Lecture 10): predicted P(diabetes) by BMI cat × active
# Using the LPM with bmi_cat (richer than binary over_obese)
# =====================================================================
cat("\n========== Marginal effects plot ==========\n")
f_bmicat <- diabetes ~ bmi_cat * active + age + female + factor(race) +
                       factor(income) + factor(educ) + current_smoker +
                       state_fe + year_fe
m_bmicat <- lm(f_bmicat, data = analysis)

mfx_grid <- expand.grid(
  bmi_cat = factor(c("Underweight","Normal weight","Overweight","Obese"),
                   levels = c("Underweight","Normal weight","Overweight","Obese")),
  active = c(0, 1)
) %>%
  mutate(
    age = mean(analysis$age, na.rm = TRUE),
    female = mean(analysis$female, na.rm = TRUE),
    current_smoker = mean(analysis$current_smoker, na.rm = TRUE),
    race = factor("White", levels = levels(analysis$race)),
    income = factor("5_50-100k", levels = levels(analysis$income)),
    educ = factor("3_Some college", levels = levels(analysis$educ)),
    state_fe = factor("6", levels = levels(analysis$state_fe)),
    year_fe = factor("2023", levels = levels(analysis$year_fe))
  )
pred <- predict(m_bmicat, newdata = mfx_grid, se.fit = TRUE,
                interval = "confidence", level = 0.95)
mfx_grid$fit <- pred$fit[,"fit"]
mfx_grid$lwr <- pred$fit[,"lwr"]
mfx_grid$upr <- pred$fit[,"upr"]
mfx_grid$active_lbl <- ifelse(mfx_grid$active == 1, "Active", "Inactive")

p_mfx <- ggplot(mfx_grid, aes(x = bmi_cat, y = fit, color = active_lbl,
                              group = active_lbl)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.12, linewidth = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("Active" = "#2c7fb8", "Inactive" = "#d95f0e")) +
  labs(x = "BMI category", y = "Predicted P(diabetes)",
       color = "",
       title = "LPM-predicted diabetes probability by BMI category × activity",
       subtitle = "Holding controls at reference / sample-mean values; 95% pointwise CI") +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave(file.path(fig_dir, "fig_marginal_effects.png"), p_mfx,
       width = 9, height = 5.5, dpi = 150)

# =====================================================================
# Layer 2: 2023-only robustness with PA guideline measures
# =====================================================================
cat("\n========== LAYER 2: 2023-only PA guideline ==========\n")
d23 <- analysis %>% filter(year == 2023, !is.na(meets_aerobic))

f_r1 <- diabetes ~ over_obese * meets_aerobic + age + female +
                   factor(race) + factor(income) + factor(educ) +
                   current_smoker + state_fe
m_r1 <- lm(f_r1, data = d23)
v_r1 <- vcovCL(m_r1, cluster = ~state_fips, type = "HC1")

# 4-category dose-response
d23b <- analysis %>% filter(year == 2023, !is.na(pa_recommend_cat)) %>%
  mutate(pa_rec = factor(pa_recommend_cat,
                         levels = c("Neither","StrengthOnly","AerobicOnly","Both")))
f_r2 <- diabetes ~ over_obese * pa_rec + age + female +
                   factor(race) + factor(income) + factor(educ) +
                   current_smoker + state_fe
m_r2 <- lm(f_r2, data = d23b)
v_r2 <- vcovCL(m_r2, cluster = ~state_fips, type = "HC1")

cat("--- 2023 with meets_aerobic (CDC 150-min guideline) ---\n")
print(coeftest(m_r1, vcov = v_r1)[c("over_obese","meets_aerobic","over_obese:meets_aerobic"), ])
cat("\n--- 2023 with 4-category recommendation ---\n")
print(coeftest(m_r2, vcov = v_r2)[grep("over_obese|pa_rec", rownames(coef(summary(m_r2)))), ])

# =====================================================================
# Layer 3: Pandemic heterogeneity - 2021 vs 2023 vs 2024 interaction
# =====================================================================
cat("\n========== LAYER 3: pandemic heterogeneity ==========\n")
analysis <- analysis %>% mutate(year_factor = factor(year))
f_h1 <- diabetes ~ over_obese * active * year_factor + age + female +
                   factor(race) + factor(income) + factor(educ) +
                   current_smoker + state_fe
m_h1 <- lm(f_h1, data = analysis)
v_h1 <- vcovCL(m_h1, cluster = ~state_fips, type = "HC1")

# Extract the three-way interaction coefficients
ttab <- coeftest(m_h1, vcov = v_h1)
cat("--- Three-way interactions: over_obese × active × year ---\n")
print(ttab[grep("over_obese:active:year", rownames(ttab)), ])

# F-test: H0: 3-way interaction coefficients are jointly zero
# Use linearHypothesis from car for a clean Wald test with clustered SE
if (!requireNamespace("car", quietly = TRUE))
  install.packages("car", repos = "https://cloud.r-project.org")
cat("\n--- Wald test: H0: over_obese:active:year_2023 = over_obese:active:year_2024 = 0 ---\n")
ftest <- car::linearHypothesis(
  m_h1,
  c("over_obese:active:year_factor2023 = 0",
    "over_obese:active:year_factor2024 = 0"),
  vcov. = v_h1
)
print(ftest)
m_h0 <- m_h1  # placeholder so saveRDS later doesn't error

# =====================================================================
# Layer 4: Race/ethnicity heterogeneity
# =====================================================================
cat("\n========== LAYER 4: race heterogeneity ==========\n")
# Limit to main racial categories with sufficient cells
analysis_race <- analysis %>%
  filter(race %in% c("White","Black","Hispanic","Asian")) %>%
  mutate(race = droplevels(race))

f_h2 <- diabetes ~ over_obese * active * race + age + female +
                   factor(income) + factor(educ) + current_smoker +
                   state_fe + year_fe
m_h2 <- lm(f_h2, data = analysis_race)
v_h2 <- vcovCL(m_h2, cluster = ~state_fips, type = "HC1")
ttab2 <- coeftest(m_h2, vcov = v_h2)
cat("--- Three-way interactions: over_obese × active × race ---\n")
print(ttab2[grep("over_obese:active:race", rownames(ttab2)), ])

# =====================================================================
# Layer 5: Pre-diabetes secondary outcome
# =====================================================================
cat("\n========== LAYER 5: pre-diabetes outcome ==========\n")
d_pre <- d %>%
  filter(!is.na(prediabetes), !is.na(over_obese), !is.na(active),
         !is.na(age), !is.na(female), !is.na(race), !is.na(educ),
         !is.na(income), !is.na(current_smoker)) %>%
  mutate(state_fe = factor(state_fips),
         year_fe  = factor(year),
         race = factor(race),
         educ = factor(educ),
         income = factor(income))
cat(sprintf("Pre-diabetes analysis N = %d\n", nrow(d_pre)))

f_pre <- prediabetes ~ over_obese * active + age + female + factor(race) +
                       factor(income) + factor(educ) + current_smoker +
                       state_fe + year_fe
m_pre <- lm(f_pre, data = d_pre)
v_pre <- vcovCL(m_pre, cluster = ~state_fips, type = "HC1")
cat("--- Pre-diabetes headline interaction ---\n")
print(coeftest(m_pre, vcov = v_pre)[c("over_obese","active","over_obese:active"), ])

# =====================================================================
# Robustness: continuous BMI + survey-weighted version
# =====================================================================
cat("\n========== Robustness: continuous BMI & survey-weighted ==========\n")
analysis_bmi <- analysis %>% filter(!is.na(bmi))

f_b1 <- diabetes ~ bmi * active + age + female + factor(race) +
                   factor(income) + factor(educ) + current_smoker +
                   state_fe + year_fe
m_bmi <- lm(f_b1, data = analysis_bmi)
v_bmi <- vcovCL(m_bmi, cluster = ~state_fips, type = "HC1")
cat("--- Continuous BMI × active ---\n")
print(coeftest(m_bmi, vcov = v_bmi)[c("bmi","active","bmi:active"), ])

# Survey-weighted LPM
m4_w <- lm(f4, data = analysis, weights = survey_weight)
v4_w <- vcovCL(m4_w, cluster = ~state_fips, type = "HC1")
cat("\n--- Survey-weighted LPM (headline interaction) ---\n")
print(coeftest(m4_w, vcov = v4_w)[c("over_obese","active","over_obese:active"), ])

# =====================================================================
# Master regression table for the paper
# =====================================================================
cat("\n========== Building paper regression tables ==========\n")
ms_full <- modelsummary(
  list(
    "(1) Baseline" = m1,
    "(2) +Controls" = m2,
    "(3) +State & Year FE" = m3,
    "(4) +Interaction (headline)" = m4,
    "(5) Logit" = logit_h,
    "(6) Weighted" = m4_w
  ),
  vcov = list(v1, v2, v3, v4, sandwich::vcovCL(logit_h, cluster=~state_fips, type="HC1"), v4_w),
  stars = c('*'=0.10, '**'=0.05, '***'=0.01),
  coef_map = c(
    "over_obese"          = "Overweight or Obese",
    "active"              = "Active",
    "over_obese:active"   = "Over/Obese × Active",
    "age"                 = "Age",
    "female"              = "Female",
    "current_smoker"      = "Current smoker"
  ),
  gof_map = c("nobs","r.squared","adj.r.squared"),
  output = file.path(out_dir, "table2_full_models.md")
)

ms_robust <- modelsummary(
  list(
    "Headline LPM" = m4,
    "Continuous BMI" = m_bmi,
    "BMI category" = m_bmicat,
    "Survey-weighted" = m4_w,
    "Pre-diabetes (Y=PD)" = m_pre,
    "2023 PA guideline" = m_r1
  ),
  vcov = list(v4, v_bmi, vcovCL(m_bmicat, cluster=~state_fips, type="HC1"),
              v4_w, v_pre, v_r1),
  stars = c('*'=0.10, '**'=0.05, '***'=0.01),
  coef_map = c(
    "over_obese"               = "Over/Obese",
    "active"                   = "Active",
    "over_obese:active"        = "Over/Obese × Active",
    "bmi"                      = "BMI",
    "bmi:active"               = "BMI × Active",
    "bmi_catOverweight"        = "Overweight",
    "bmi_catObese"             = "Obese",
    "bmi_catOverweight:active" = "Overweight × Active",
    "bmi_catObese:active"      = "Obese × Active",
    "meets_aerobic"            = "Meets 150-min guideline",
    "over_obese:meets_aerobic" = "Over/Obese × meets guideline"
  ),
  gof_map = c("nobs","r.squared"),
  output = file.path(out_dir, "table3_robustness.md")
)

# Save all fitted models for the Rmd-knit step
saveRDS(list(m1=m1, m2=m2, m3=m3, m4=m4,
             logit_h=logit_h, m4_w=m4_w,
             m_bmi=m_bmi, m_bmicat=m_bmicat,
             m_r1=m_r1, m_r2=m_r2,
             m_h1=m_h1, m_h0=m_h0, m_h2=m_h2,
             m_pre=m_pre),
        file.path(out_dir, "fitted_models.rds"), compress = "xz")

cat("\nAll outputs saved to:\n  ", out_dir, "\n", sep="")
cat("DONE.\n")
