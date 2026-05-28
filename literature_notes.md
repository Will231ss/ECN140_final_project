# Literature notes for ECN 140 empirical project

Topic: Does physical activity attenuate the obesity → diabetes association?

## Key precedent paper (must cite up front)

**Abernethy D, Bennie J, Pavey T (2025).** "Joint Effects of Physical Activity and Body Mass Index on Prevalent Diabetes in a Nationally Representative Sample of 1.9 Million US Adults." *Journal of Diabetes Research*, 2025:7466757. DOI: 10.1155/jdr/7466757.
- Uses BRFSS 2011–2019, N = 1,913,732
- Method: **Poisson log-linear regression with robust SEs, reporting adjusted prevalence ratios**
- BMI: normal / overweight / Class I / II / III
- PA: highly active (≥300 min/wk MVPA), active (150–299), inactive (0.01–149), nonactive (0)
- **Headline finding:** "PA provided modest reductions in the prevalence of diabetes but did not attenuate the detrimental impact of overweight and increasing levels of obesity on diabetes prevalence."
- Direct quote: *"BMI appears to be a substantially larger predictor of diabetes compared to PA in a large population-level sample of US adults."*
- This is our direct precedent — we update with 2021/2023/2024, contrast LPM (additive) vs logit (multiplicative), and add the COVID stability check.

## Foundational MHO literature

**Bell JA, Kivimaki M, Hamer M (2014).** "Metabolically healthy obesity and risk of incident type 2 diabetes: a meta-analysis of prospective cohort studies." *Obesity Reviews*, 15(6): 504–515.
- Meta-analysis of prospective cohorts
- Pooled adjusted RR for incident T2D: **4.03** in metabolically-healthy obese vs **8.93** in metabolically-unhealthy obese (both vs. healthy normal weight)
- Establishes that ~1/3 of obese adults are "metabolically healthy" — relevant to the population-level question

**Kramer CK, Zinman B, Retnakaran R (2013).** "Are metabolically healthy overweight and obesity benign conditions?: A systematic review and meta-analysis." *Annals of Internal Medicine*, 159(11): 758–769.
- Systematic review concluding that even metabolically healthy people with obesity have increased CV and mortality risk
- Counter to the "fat but fit is fine" view — provides the tension our paper engages with

**Stenholm S, Head J, Kivimäki M et al (2017).** "Smoking, physical inactivity and obesity as predictors of healthy and disease-free life expectancy between ages 50 and 75: a multi-cohort study." *Int J Epidemiol*, 46(3): 911–919.
- Joint effects of BMI + smoking + inactivity on healthy life expectancy — supports the joint-effects framing.

## Physical-activity-and-BMI joint-effects meta-analysis

**Aune D, Norat T, Leitzmann M, et al. (2015).** "Physical activity and the risk of type 2 diabetes: a systematic review and dose-response meta-analysis." *Eur J Epidemiol*, 30(7): 529–542. (Also Smith et al.'s 9-cohort study referenced in NCBI PMC4666059.)
- Pooled 9 prospective cohorts
- Both PA and BMI independently predict T2D incidence
- Sets up the question of whether interaction is additive or multiplicative

## CDC physical-activity guideline references

**U.S. Department of Health and Human Services (2018).** *Physical Activity Guidelines for Americans, 2nd edition.* Washington, DC: HHS.
- Defines the 150-min/week moderate-intensity aerobic + 2×/week strength recommendation that `_PAINDX3` and `_PAREC3` operationalize in BRFSS 2023

## BRFSS validity references

**Pierannunzi C, Hu SS, Balluz L (2013).** "A systematic review of publications assessing reliability and validity of the Behavioral Risk Factor Surveillance System (BRFSS), 2004–2011." *BMC Med Res Methodol*, 13:49.
- Foundation for defending BRFSS data quality

**Li C, Balluz L, Ford ES, et al. (2012).** "A comparison of prevalence estimates for selected health indicators and chronic diseases or conditions from the Behavioral Risk Factor Surveillance System, the National Health Interview Survey, and the National Health and Nutrition Examination Survey, 2007–2008." *Prev Med*, 54(6): 381–387.
- BRFSS estimates align with NHIS and NHANES — counters self-report concerns

## How we position the contribution

Our paper has three angles relative to the literature:

1. **Replication with updated data.** Abernethy et al. ended at 2019; we add 2021, 2023, 2024 — the COVID-era and post-COVID years. This is the standard "extend the data" contribution.

2. **Methodological reconciliation.** Abernethy et al. used multiplicative prevalence ratios and concluded "no attenuation." Our LPM (additive) shows a large interaction; our logit (multiplicative) replicates their no-attenuation finding. The disagreement is *driven by the choice of model* — a Lecture 16 (LPM) vs Lecture 17 (logit) result. We show this directly and discuss policy implications: clinicians and policymakers care about *absolute* risk reductions (LPM); academic literature defaults to *relative* risks (logit/Poisson).

3. **Stability through COVID disruption.** A three-way interaction (obesity × activity × year) finds no significant change across 2021/2023/2024 — the protective gradient survived the pandemic. This is a novel descriptive result.

## In-text citation style

Suggested format: author–year, e.g. (Abernethy et al., 2025; Bell et al., 2014). Full bibliography at end.
