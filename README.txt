Pharmaverse Exercises - Questions 1 to 3
=========================================

Repository structure
--------------------

data/
  ds_raw.rda          Raw disposition data from {pharmaverseraw}
  sdtm_ct.csv         Study controlled terminology (CDISC codelists + study-specific mappings)

question_1_sdtm/
  01_create_ds_domain.R    R script to create the SDTM DS (Disposition) domain using {sdtm.oak}
  ds.csv                   Output DS domain dataset (560 records, 12 variables)
  01_create_ds_domain.log  Log file confirming error-free execution

question_2_adam/
  create_adsl.R       R script to create the ADaM ADSL (Subject Level) dataset using {admiral}
  adsl.csv            Output ADSL dataset (306 subjects, 43 variables)
  create_adsl.log     Log file confirming error-free execution

question_3_tlg/
  01_create_ae_summary_table.R    R script to create a TEAE summary table using {gtsummary}
  02_create_visualizations.R      R script to create AE visualizations using {ggplot2}
  ae_summary_table.html           Output summary table (FDA Table 10 format, by SOC and preferred term)
  ae_severity_by_treatment.png    Plot 1: stacked bar chart of AE severity by treatment arm
  top10_ae_forest_plot.png        Plot 2: top 10 most frequent AEs with 95% Clopper-Pearson CIs
  01_create_ae_summary_table.log  Log file confirming error-free execution
  02_create_visualizations.log    Log file confirming error-free execution


Required R packages
-------------------

Question 1: sdtm.oak, pharmaverseraw, dplyr
Question 2: admiral, pharmaversesdtm, dplyr, lubridate, stringr
Question 3: gtsummary, gt, ggplot2, pharmaverseadam, dplyr


How to run
----------

1. Install the required R packages listed above.
2. Set the working directory to the root of this repository.
3. Run each script with: Rscript <path_to_script>

   Rscript question_1_sdtm/01_create_ds_domain.R
   Rscript question_2_adam/create_adsl.R
   Rscript question_3_tlg/01_create_ae_summary_table.R
   Rscript question_3_tlg/02_create_visualizations.R

Each script produces its output files in the same folder and prints a summary to the console.
