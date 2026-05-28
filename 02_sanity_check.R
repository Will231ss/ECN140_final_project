# =====================================================================
# 02_sanity_check.R
# Quick descriptive look — does the headline pattern even exist?
# =====================================================================

suppressMessages({
  library(dplyr)
})

d <- readRDS("D:/Academic/2026/ECN 140/Project/clean/brfss_pooled.rds")

cat("=== Pooled sample ===\n")
cat(sprintf("Total rows: %d\n", nrow(d)))
cat("Rows per year:\n"); print(table(d$year))

cat("\n=== Diabetes rate by year ===\n")
d %>% group_by(year) %>%
  summarise(n = sum(!is.na(diabetes)),
            diabetes_rate = mean(diabetes, na.rm = TRUE)) %>%
  print()

cat("\n=== Diabetes rate by BMI category ===\n")
d %>% filter(!is.na(diabetes), !is.na(bmi_cat)) %>%
  group_by(bmi_cat) %>%
  summarise(n = n(),
            diabetes_rate = mean(diabetes)) %>%
  arrange(match(bmi_cat, c("Underweight","Normal weight","Overweight","Obese"))) %>%
  print()

cat("\n=== HEADLINE: diabetes rate by over_obese x active ===\n")
d %>% filter(!is.na(diabetes), !is.na(over_obese), !is.na(active)) %>%
  group_by(over_obese, active) %>%
  summarise(n = n(),
            diabetes_rate = mean(diabetes),
            .groups = "drop") %>%
  mutate(over_obese = ifelse(over_obese == 1, "Over/Obese", "Normal/Under"),
         active     = ifelse(active == 1, "Active", "Inactive")) %>%
  print()

cat("\n=== Same but using the 2023-only meets_aerobic (CDC 150-min guideline) ===\n")
d %>% filter(year == 2023, !is.na(diabetes), !is.na(over_obese), !is.na(meets_aerobic)) %>%
  group_by(over_obese, meets_aerobic) %>%
  summarise(n = n(),
            diabetes_rate = mean(diabetes),
            .groups = "drop") %>%
  mutate(over_obese    = ifelse(over_obese == 1, "Over/Obese", "Normal/Under"),
         meets_aerobic = ifelse(meets_aerobic == 1, "Meets 150min", "Doesnt meet")) %>%
  print()

cat("\n=== 4-way recommendation category in 2023 ===\n")
d %>% filter(year == 2023, !is.na(diabetes), !is.na(over_obese), !is.na(pa_recommend_cat)) %>%
  group_by(over_obese, pa_recommend_cat) %>%
  summarise(n = n(),
            diabetes_rate = mean(diabetes),
            .groups = "drop") %>%
  mutate(over_obese = ifelse(over_obese == 1, "Over/Obese", "Normal/Under")) %>%
  print()

cat("\n=== Pandemic angle: obesity x activity interaction 2021 vs 2023 ===\n")
d %>% filter(year %in% c(2021, 2023),
             !is.na(diabetes), !is.na(over_obese), !is.na(active)) %>%
  group_by(year, over_obese, active) %>%
  summarise(n = n(),
            diabetes_rate = mean(diabetes),
            .groups = "drop") %>%
  print(n = 20)

cat("\n=== BMI distribution ===\n")
summary(d$bmi)
cat("\nBMI by year:\n")
d %>% filter(!is.na(bmi)) %>%
  group_by(year) %>%
  summarise(mean_bmi = mean(bmi),
            sd_bmi   = sd(bmi),
            pct_obese = mean(bmi >= 30)) %>%
  print()

cat("\n=== State coverage by year (should show KY/PA gap in 2023) ===\n")
states_by_year <- d %>% distinct(year, state_fips) %>%
  count(year, name = "n_states")
print(states_by_year)
# Identify which states are missing in 2023 (should be KY=21, PA=42)
states_2021 <- d %>% filter(year == 2021) %>% distinct(state_fips) %>% pull()
states_2023 <- d %>% filter(year == 2023) %>% distinct(state_fips) %>% pull()
states_2024 <- d %>% filter(year == 2024) %>% distinct(state_fips) %>% pull()
cat("\nStates in 2021 but not 2023:\n"); print(setdiff(states_2021, states_2023))
cat("States in 2024 but not 2023:\n"); print(setdiff(states_2024, states_2023))
