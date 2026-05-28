# =====================================================================
# 03_quality_audit.R
# Data quality audit: histograms of every analysis variable,
# missingness chart, sample-restriction waterfall, save figures.
# =====================================================================

suppressMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

proj_dir <- "D:/Academic/2026/ECN 140/Project"
fig_dir  <- file.path(proj_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE)

d <- readRDS(file.path(proj_dir, "clean/brfss_pooled.rds"))

# ---- 1. Sample-restriction waterfall ----
cat("=== Sample-restriction waterfall ===\n")
N0 <- nrow(d)
N1 <- sum(!is.na(d$diabetes))
N2 <- sum(!is.na(d$diabetes) & !is.na(d$over_obese))
N3 <- sum(!is.na(d$diabetes) & !is.na(d$over_obese) & !is.na(d$active))
N4 <- sum(!is.na(d$diabetes) & !is.na(d$over_obese) & !is.na(d$active) &
          !is.na(d$age) & !is.na(d$female) & !is.na(d$race) &
          !is.na(d$educ) & !is.na(d$income) & !is.na(d$current_smoker))

waterfall <- tibble::tibble(
  step = c("Raw pooled (2021+2023+2024)",
           "Non-missing diabetes outcome (drop preg/pre-diab/DK)",
           "+ Non-missing BMI / over_obese",
           "+ Non-missing physical-activity (_TOTINDA)",
           "+ Non-missing demographic & SES controls"),
  N = c(N0, N1, N2, N3, N4),
  pct_retained = round(100*c(N0,N1,N2,N3,N4)/N0, 1)
)
print(waterfall)
write.csv(waterfall, file.path(fig_dir, "sample_waterfall.csv"), row.names = FALSE)

# ---- 2. Missingness table per variable ----
key_vars <- c("diabetes","prediabetes","over_obese","bmi","active","any_exercise",
              "meets_aerobic","pa_recommend_cat","meets_strength",
              "age","female","race","educ","income","current_smoker",
              "state_fips","survey_weight")
miss_tbl <- lapply(key_vars, function(v) {
  x <- d[[v]]
  tibble::tibble(variable = v,
                 n_obs    = sum(!is.na(x)),
                 n_miss   = sum(is.na(x)),
                 pct_miss = round(100*mean(is.na(x)), 2))
}) %>% bind_rows()
print(miss_tbl)
write.csv(miss_tbl, file.path(fig_dir, "missingness_table.csv"), row.names = FALSE)

# ---- 3. Figure: BMI distribution by year ----
p_bmi <- d %>% filter(!is.na(bmi), bmi < 70) %>%
  ggplot(aes(x = bmi, fill = factor(year))) +
  geom_histogram(binwidth = 0.5, position = "identity", alpha = 0.45) +
  geom_vline(xintercept = c(18.5, 25, 30), linetype = "dashed", color = "grey30") +
  labs(x = "BMI", y = "Count", fill = "Year",
       title = "BMI distribution across years (BRFSS pooled)",
       subtitle = "Dashed lines: WHO BMI cutoffs (18.5, 25, 30)") +
  theme_minimal()
ggsave(file.path(fig_dir, "fig_bmi_dist.png"), p_bmi, width = 8, height = 5, dpi = 150)

# ---- 4. Figure: diabetes rate by BMI category x activity ----
plot_d <- d %>%
  filter(!is.na(diabetes), !is.na(bmi_cat), !is.na(active)) %>%
  mutate(bmi_cat = factor(bmi_cat,
                          levels = c("Underweight","Normal weight","Overweight","Obese")),
         active_lbl = ifelse(active == 1, "Active (any leisure-time PA)", "Inactive")) %>%
  group_by(bmi_cat, active_lbl) %>%
  summarise(diabetes_rate = mean(diabetes), n = n(), .groups = "drop")

p_headline <- ggplot(plot_d, aes(x = bmi_cat, y = diabetes_rate, fill = active_lbl)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", 100*diabetes_rate)),
            position = position_dodge(width = 0.7),
            vjust = -0.4, size = 3.2) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.32)) +
  scale_fill_manual(values = c("Active (any leisure-time PA)" = "#2c7fb8",
                                "Inactive" = "#d95f0e")) +
  labs(x = "BMI category", y = "Share reporting diabetes",
       fill = "",
       title = "Diabetes prevalence by BMI category and physical activity",
       subtitle = "BRFSS pooled 2021/2023/2024, unweighted") +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave(file.path(fig_dir, "fig_headline_bmicat_x_active.png"),
       p_headline, width = 8.5, height = 5, dpi = 150)

# ---- 5. Figure: 2023-only with CDC 4-category guideline ----
plot_2023 <- d %>%
  filter(year == 2023, !is.na(diabetes), !is.na(bmi_cat), !is.na(pa_recommend_cat)) %>%
  mutate(bmi_cat = factor(bmi_cat,
                          levels = c("Underweight","Normal weight","Overweight","Obese")),
         pa_lbl = factor(pa_recommend_cat,
                         levels = c("Neither","StrengthOnly","AerobicOnly","Both"),
                         labels = c("Neither","Strength only","Aerobic only","Both"))) %>%
  group_by(bmi_cat, pa_lbl) %>%
  summarise(diabetes_rate = mean(diabetes), n = n(), .groups = "drop")

p_dose <- ggplot(plot_2023, aes(x = bmi_cat, y = diabetes_rate, fill = pa_lbl)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "RdYlBu") +
  labs(x = "BMI category", y = "Share reporting diabetes",
       fill = "CDC physical-activity\nrecommendation met",
       title = "Dose-response: diabetes by BMI × CDC PA recommendation (2023)",
       subtitle = "Categories ordered worst→best (neither → both aerobic + strength)") +
  theme_minimal() +
  theme(legend.position = "right")
ggsave(file.path(fig_dir, "fig_2023_pa_recommendation.png"),
       p_dose, width = 9, height = 5, dpi = 150)

# ---- 6. Figure: pandemic angle — 2021 vs 2023 interaction stability ----
plot_pandemic <- d %>%
  filter(year %in% c(2021, 2023),
         !is.na(diabetes), !is.na(over_obese), !is.na(active)) %>%
  mutate(group = paste0(ifelse(over_obese == 1, "Over/Obese", "Normal/Under"),
                        " × ",
                        ifelse(active == 1, "Active", "Inactive"))) %>%
  group_by(year, group) %>%
  summarise(diabetes_rate = mean(diabetes), n = n(), .groups = "drop")

p_pandemic <- ggplot(plot_pandemic, aes(x = factor(year), y = diabetes_rate,
                                         color = group, group = group)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  geom_text(aes(label = sprintf("%.1f%%", 100*diabetes_rate)),
            hjust = -0.2, size = 3.2) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(expand = expansion(add = 0.4)) +
  labs(x = "Survey year", y = "Diabetes rate", color = "",
       title = "Obesity × activity interaction across COVID years",
       subtitle = "Stable protective gradient from 2021 (peak COVID) to 2023 (post-COVID)") +
  theme_minimal() +
  theme(legend.position = "right")
ggsave(file.path(fig_dir, "fig_pandemic_stability.png"),
       p_pandemic, width = 8.5, height = 5, dpi = 150)

# ---- 7. Figure: missingness bar chart ----
miss_plot <- miss_tbl %>%
  mutate(variable = forcats::fct_reorder(variable, pct_miss))
p_miss <- ggplot(miss_plot, aes(x = variable, y = pct_miss)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = sprintf("%.1f%%", pct_miss)), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(x = "", y = "% missing",
       title = "Missingness by variable (pooled 2021/2023/2024)",
       subtitle = "PA-module variables only present in 2023, hence ~72% across the pool") +
  theme_minimal() +
  ylim(0, max(miss_plot$pct_miss) * 1.15)
ggsave(file.path(fig_dir, "fig_missingness.png"),
       p_miss, width = 8, height = 6, dpi = 150)

cat("\nFigures written to:\n  ", fig_dir, "\n", sep="")
cat("\nDONE.\n")
