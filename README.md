# Pharmaverse Exercises — Questions 1 to 3

## Repository Structure

```
├── data/
│   ├── ds_raw.rda                Raw disposition data from {pharmaverseraw}
│   └── sdtm_ct.csv              Study controlled terminology (CDISC codelists)
│
├── question_1_sdtm/
│   ├── 01_create_ds_domain.R    R script — DS domain using {sdtm.oak}
│   ├── ds.csv                   Output dataset (560 records × 12 variables)
│   └── 01_create_ds_domain.log  Log file (error-free)
│
├── question_2_adam/
│   ├── create_adsl.R            R script — ADSL dataset using {admiral}
│   ├── adsl.csv                 Output dataset (306 subjects × 43 variables)
│   └── create_adsl.log          Log file (error-free)
│
└── question_3_tlg/
    ├── 01_create_ae_summary_table.R    R script — TEAE summary table using {gtsummary}
    ├── 02_create_visualizations.R      R script — AE plots using {ggplot2}
    ├── ae_summary_table.html           Summary table (FDA Table 10 format)
    ├── ae_severity_by_treatment.png    Plot 1: severity by treatment arm
    ├── top10_ae_forest_plot.png        Plot 2: top 10 AEs with 95% CIs
    ├── 01_create_ae_summary_table.log  Log file (error-free)
    └── 02_create_visualizations.log    Log file (error-free)
```

## Required R Packages

| Question | Packages |
|----------|----------|
| Q1 — SDTM | `sdtm.oak`, `pharmaverseraw`, `dplyr` |
| Q2 — ADaM | `admiral`, `pharmaversesdtm`, `dplyr`, `lubridate`, `stringr` |
| Q3 — TLG  | `gtsummary`, `gt`, `ggplot2`, `pharmaverseadam`, `dplyr` |

## How to Run

1. Install the required R packages listed above.
2. Set the working directory to the root of this repository.
3. Run each script:

```r
Rscript question_1_sdtm/01_create_ds_domain.R
Rscript question_2_adam/create_adsl.R
Rscript question_3_tlg/01_create_ae_summary_table.R
Rscript question_3_tlg/02_create_visualizations.R
```

Each script produces its output files in the same folder and prints a summary to the console.
