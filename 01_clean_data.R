# =====================================================================
# 01_clean_data.R
# Read raw BRFSS 2021/2023/2024 XPT files, subset to needed variables,
# harmonize across years, build analysis variables, save clean RDS+CSV
# =====================================================================

suppressMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
})

proj_dir <- "D:/Academic/2026/ECN 140/Project"
out_dir  <- file.path(proj_dir, "clean")
dir.create(out_dir, showWarnings = FALSE)

# Files
xpt_files <- list(
  "2021" = file.path(proj_dir, "LLCP2021XPT/LLCP2021.XPT"),
  "2023" = file.path(proj_dir, "LLCP2023XPT/LLCP2023.XPT"),
  "2024" = file.path(proj_dir, "LLCP2024XPT/LLCP2024.XPT")
)

# Common columns we need in all years
common_cols <- c(
  # Survey design
  "_LLCPWT","_PSU","_STSTR",
  # Geo / time
  "_STATE","IMONTH","IYEAR","IDAY",
  # Demographics
  "_AGE80","_AGEG5YR","_SEX","_RACE","_HISPANC","_IMPRACE",
  # SES
  "INCOME3","EDUCA","_EDUCAG","_INCOMG1",
  # Smoking
  "_SMOKER3","_RFSMOK3",
  # BMI & weight status
  "_BMI5","_BMI5CAT","_RFBMI5",
  # Activity (common)
  "EXERANY2","_TOTINDA",
  # Outcomes
  "DIABETE4"
)
# Year-specific pre-diabetes name
pdiab_col <- c("2021" = "PREDIAB1", "2023" = "PREDIAB2", "2024" = "PREDIAB2")
# 2023-only richer PA variables
pa_cols_2023 <- c("_PAINDX3","_PAREC3","_PASTRNG","PAFREQ1_","PAFREQ2_")

# Build BMI category labels from _BMI5CAT
# 1 = Underweight, 2 = Normal, 3 = Overweight, 4 = Obese
bmi_cat_labels <- c("1" = "Underweight",
                    "2" = "Normal weight",
                    "3" = "Overweight",
                    "4" = "Obese")

# Recoder for diabetes
# DIABETE4 codes: 1=Yes diabetes, 2=Yes pregnancy only, 3=No,
#                 4=No pre-diabetes, 7=DK, 9=Refused
recode_diabetes <- function(x) {
  case_when(
    x == 1 ~ 1L,        # diabetes
    x == 3 ~ 0L,        # no diabetes
    TRUE   ~ NA_integer_  # exclude pregnancy-only, pre-diabetic, DK/Ref
  )
}

# Pre-diabetes coding:
# Outcome = 1 if pre-diabetic (DIABETE4==4 OR PREDIAB==1), 0 if clean no-diabetes (DIABETE4==3)
# Exclude actual diabetics (DIABETE4==1) and others
recode_prediabetes <- function(diab, pdiab) {
  case_when(
    diab == 1 ~ NA_integer_,                       # exclude established diabetics
    diab == 4 ~ 1L,                                # told pre-diabetes
    !is.na(pdiab) & pdiab == 1 ~ 1L,               # told pre-diabetes in module
    diab == 3 & (is.na(pdiab) | pdiab == 2) ~ 0L,  # clean no
    TRUE ~ NA_integer_
  )
}

# Overweight/obese: _RFBMI5 1=No (BMI<=25), 2=Yes (>25), 9=DK/Ref/Missing
recode_over_obese <- function(x) {
  case_when(
    x == 1 ~ 0L,
    x == 2 ~ 1L,
    TRUE   ~ NA_integer_
  )
}

# Active: _TOTINDA 1=Yes, 2=No, 9=DK
recode_active <- function(x) {
  case_when(
    x == 1 ~ 1L,
    x == 2 ~ 0L,
    TRUE   ~ NA_integer_
  )
}

# Sex: BRFSS _SEX 1=Male, 2=Female
recode_female <- function(x) {
  case_when(
    x == 1 ~ 0L,
    x == 2 ~ 1L,
    TRUE   ~ NA_integer_
  )
}

# Current smoker: _SMOKER3 1=Every day, 2=Some days, 3=Former, 4=Never, 9=DK
recode_current_smoker <- function(x) {
  case_when(
    x %in% c(1,2) ~ 1L,
    x %in% c(3,4) ~ 0L,
    TRUE          ~ NA_integer_
  )
}

# Race/ethnicity: _RACE 1=White-only, 2=Black, 3=AIAN, 4=Asian, 5=NHPI,
#                       6=Other, 7=Multi, 8=Hispanic, 9=DK/Ref
recode_race <- function(x) {
  case_when(
    x == 1 ~ "White",
    x == 2 ~ "Black",
    x == 3 ~ "AIAN",
    x == 4 ~ "Asian",
    x == 5 ~ "NHPI",
    x == 6 ~ "Other",
    x == 7 ~ "Multi",
    x == 8 ~ "Hispanic",
    TRUE   ~ NA_character_
  )
}

# Education: _EDUCAG 1=Did not graduate HS, 2=HS, 3=Some college, 4=College+, 9=DK
recode_educ <- function(x) {
  case_when(
    x == 1 ~ "1_Less than HS",
    x == 2 ~ "2_HS grad",
    x == 3 ~ "3_Some college",
    x == 4 ~ "4_College grad",
    TRUE   ~ NA_character_
  )
}

# Income: _INCOMG1 (2021+) collapses to 7 categories
# 1 = <$15k, 2 = 15-25k, 3 = 25-35k, 4 = 35-50k, 5 = 50-100k, 6 = 100-200k, 7 = 200k+, 9 = DK
recode_income <- function(x) {
  case_when(
    x == 1 ~ "1_<15k",
    x == 2 ~ "2_15-25k",
    x == 3 ~ "3_25-35k",
    x == 4 ~ "4_35-50k",
    x == 5 ~ "5_50-100k",
    x == 6 ~ "6_100-200k",
    x == 7 ~ "7_200k+",
    TRUE   ~ NA_character_
  )
}

# ----- Process each year -----
process_year <- function(yr) {
  cat(sprintf("\n=== Processing %s ===\n", yr))
  t0 <- Sys.time()
  pdc <- pdiab_col[[yr]]

  # Which columns to actually read
  cols <- c(common_cols, pdc)
  if (yr == "2023") cols <- c(cols, pa_cols_2023)

  cat(sprintf("Reading XPT (%d cols)...\n", length(cols)))
  df <- read_xpt(xpt_files[[yr]], col_select = all_of(cols))
  cat(sprintf("  Loaded %d rows in %.1fs\n", nrow(df), as.numeric(Sys.time()-t0)))

  # Rename pre-diabetes col to a common name
  df$PREDIAB <- df[[pdc]]

  # Build analysis variables
  cat("Building analysis variables...\n")
  d <- df %>%
    mutate(
      year             = as.integer(yr),
      state_fips       = as.integer(`_STATE`),
      survey_month     = as.integer(IMONTH),
      survey_weight    = as.numeric(`_LLCPWT`),
      psu              = as.numeric(`_PSU`),
      stratum          = as.numeric(`_STSTR`),
      age              = as.numeric(`_AGE80`),
      age_group        = as.integer(`_AGEG5YR`),
      female           = recode_female(`_SEX`),
      race             = recode_race(`_RACE`),
      educ             = recode_educ(`_EDUCAG`),
      income           = recode_income(`_INCOMG1`),
      current_smoker   = recode_current_smoker(`_SMOKER3`),
      bmi              = as.numeric(`_BMI5`) / 100,
      bmi_cat_code     = as.integer(`_BMI5CAT`),
      bmi_cat          = bmi_cat_labels[as.character(bmi_cat_code)],
      over_obese       = recode_over_obese(`_RFBMI5`),
      active           = recode_active(`_TOTINDA`),
      any_exercise     = recode_active(EXERANY2),
      diabetes         = recode_diabetes(DIABETE4),
      prediabetes      = recode_prediabetes(DIABETE4, PREDIAB)
    )

  # 2023-only PA module variables
  if (yr == "2023") {
    d <- d %>% mutate(
      # _PAINDX3 1=Meets aerobic guideline, 2=Does not meet, 9=DK/Ref
      meets_aerobic = case_when(
        `_PAINDX3` == 1 ~ 1L,
        `_PAINDX3` == 2 ~ 0L,
        TRUE ~ NA_integer_
      ),
      # _PAREC3 1=Both, 2=Aerobic only, 3=Strength only, 4=Neither, 9=DK
      pa_recommend_cat = case_when(
        `_PAREC3` == 1 ~ "Both",
        `_PAREC3` == 2 ~ "AerobicOnly",
        `_PAREC3` == 3 ~ "StrengthOnly",
        `_PAREC3` == 4 ~ "Neither",
        TRUE ~ NA_character_
      ),
      # _PASTRNG 1=Meets, 2=Does not, 9=DK
      meets_strength = case_when(
        `_PASTRNG` == 1 ~ 1L,
        `_PASTRNG` == 2 ~ 0L,
        TRUE ~ NA_integer_
      )
    )
  } else {
    d$meets_aerobic     <- NA_integer_
    d$pa_recommend_cat  <- NA_character_
    d$meets_strength    <- NA_integer_
  }

  # Select final columns in tidy order
  keep <- c("year","state_fips","survey_month","survey_weight","psu","stratum",
            "age","age_group","female","race","educ","income","current_smoker",
            "bmi","bmi_cat_code","bmi_cat","over_obese",
            "active","any_exercise",
            "meets_aerobic","pa_recommend_cat","meets_strength",
            "diabetes","prediabetes")
  d <- d[, keep]

  cat(sprintf("  Built %d rows × %d cols in %.1fs total\n",
              nrow(d), ncol(d), as.numeric(Sys.time()-t0)))

  # Save per-year files
  saveRDS(d, file.path(out_dir, sprintf("brfss_%s_clean.rds", yr)),
          compress = "xz")

  d
}

# ----- Run for all three years -----
all_years <- list()
for (yr in names(xpt_files)) {
  all_years[[yr]] <- process_year(yr)
}

# ----- Stack into pooled dataset -----
cat("\n=== Stacking pooled dataset ===\n")
pooled <- bind_rows(all_years)
cat(sprintf("Pooled rows: %d, cols: %d\n", nrow(pooled), ncol(pooled)))
cat(sprintf("Rows per year:\n"))
print(table(pooled$year, useNA = "ifany"))

# Save pooled
saveRDS(pooled, file.path(out_dir, "brfss_pooled.rds"), compress = "xz")
write.csv(pooled, file.path(out_dir, "brfss_pooled.csv"), row.names = FALSE)

cat("\nSaved:\n")
cat(sprintf("  %s\n", file.path(out_dir, "brfss_pooled.rds")))
cat(sprintf("  %s\n", file.path(out_dir, "brfss_pooled.csv")))

# Quick missingness summary on key vars
cat("\n=== Missingness on key analysis variables ===\n")
key_vars <- c("diabetes","prediabetes","over_obese","active","any_exercise",
              "bmi","age","female","race","educ","income","current_smoker",
              "meets_aerobic","pa_recommend_cat")
miss_tbl <- sapply(key_vars, function(v) {
  x <- pooled[[v]]
  c(n_obs = sum(!is.na(x)),
    n_miss = sum(is.na(x)),
    pct_miss = round(100*mean(is.na(x)), 2))
})
print(t(miss_tbl))

cat("\nDONE.\n")
