# ECN 140 Empirical Project — Reproducibility README

**Title:** Does Physical Activity Attenuate the Obesity-Diabetes Association?
Evidence from BRFSS 2021/2023/2024
**Authors:** Haoyu Yan and Chenyu Zhu

## Files

| File | Purpose |
|---|---|
| `ECN140_final_paper.Rmd` | The paper. Knits to PDF. |
| `ECN140_final_paper.pdf` | Pre-rendered PDF output. |
| `01_clean_data.R` | Reads raw BRFSS XPT files, harmonizes 2021/2023/2024, writes `clean/brfss_pooled.rds`. |
| `02_sanity_check.R` | Descriptive sanity checks on the cleaned panel. |
| `03_quality_audit.R` | Missingness audit, sample-waterfall, and figures saved to `figures/`. |
| `04_analysis.R` | All regressions (LPM, logit, weighted, robustness, heterogeneity), saves model tables and the analysis sample. |
| `clean/brfss_pooled.rds` | Cleaned pooled dataset (~30 MB). Built by `01_clean_data.R`. Required by the Rmd. |
| `output/analysis_sample.rds` | Complete-case analytic sample (~10 MB). Built by `04_analysis.R`. Required by the Rmd. |
| `figures/*.png` | All figures referenced in the paper. |
| `output/*.md` | Pre-rendered regression tables (cosmetic only — the Rmd refits in-document). |
| `literature_notes.md` | Notes on the literature search and how each cited paper is positioned. |

## Reproducing the paper end-to-end

### Fastest path (uses pre-built intermediate files)

```sh
Rscript -e "rmarkdown::render('ECN140_final_paper.Rmd')"
```

This loads `output/analysis_sample.rds` and `clean/brfss_pooled.rds`, refits all
models inline, and produces `ECN140_final_paper.pdf`. Wall time: ~1 minute.

### Full reproducibility from raw BRFSS files

If you want to start from the CDC raw data (≈ 1 GB), download the three SAS Transport
files from the BRFSS Annual Data page (https://www.cdc.gov/brfss/annual_data/) and
place them at:

```
LLCP2021XPT/LLCP2021.XPT
LLCP2023XPT/LLCP2023.XPT
LLCP2024XPT/LLCP2024.XPT
```

Then run the pipeline in order:

```sh
Rscript 01_clean_data.R       # ~30 sec
Rscript 02_sanity_check.R     # ~10 sec
Rscript 03_quality_audit.R    # ~30 sec
Rscript 04_analysis.R         # ~3 minutes for fitting; ~15 minutes if also writing fitted_models.rds
Rscript -e "rmarkdown::render('ECN140_final_paper.Rmd')"
```

Total wall time from raw XPT: ~5-10 minutes.

## R packages required

```r
install.packages(c(
  "haven","dplyr","tidyr","ggplot2","sandwich","lmtest",
  "modelsummary","kableExtra","broom","car","rmarkdown","tinytex","scales","forcats"
))
```

Tested under R 4.5.1 on Windows.

## How to verify the paper's numbers

Open `ECN140_final_paper.pdf`. The headline numbers in the abstract:

| Quantity | Value | Where to find in code |
|---|---|---|
| N (analysis sample) | 931,829 | `04_analysis.R`, after first filter |
| Over/Obese × Active (LPM, headline) | -0.069 (SE 0.003, t = -26.6) | `04_analysis.R` Model `m4` |
| Over/Obese × Active (logit, log-odds) | -0.007 (SE 0.018, p = 0.69) | `04_analysis.R` Model `logit_h` |
| Total active effect for over/obese (LPM) | -8.9 pp | Sum of `m4` coefficients: -0.020 + (-0.069) |
| Pandemic 3-way (2023 vs 2021) | -0.003 (p = 0.51) | `04_analysis.R` Model `m_h1` |

Re-knitting the Rmd reproduces all of these directly from the data.

## Data-quality issues we flagged (also in §2 of the paper)

1. Kentucky and Pennsylvania did not meet BRFSS minimum requirements in 2023 and are
   absent from that year's public-use file. The state fixed effects absorb this.
2. The detailed BRFSS Physical Activity Module variables (`_PAINDX3`, `_PAREC3`) were
   fielded only in 2023 across our three years. The pooled headline uses the binary
   `_TOTINDA`; the 2023-only robustness uses the richer guideline measures.
3. BMI and diabetes status are both self-reported. CDC's own validation work documents
   systematic underreporting of body weight in telephone interviews.

## Contact

For questions about reproduction, contact the authors via Canvas.
