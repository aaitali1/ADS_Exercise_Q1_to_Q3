# =============================================================================
# Name:  ADSL (Subject Level Analysis Dataset)
# Label: R program to create the ADaM ADSL dataset using {admiral}
#
# Input datasets:
#   pharmaversesdtm::dm   - Demographics
#   pharmaversesdtm::ex   - Exposure
#   pharmaversesdtm::ds   - Disposition
#   pharmaversesdtm::ae   - Adverse Events
#   pharmaversesdtm::vs   - Vital Signs
#
# Reference:
#   Pharmaverse Examples - ADSL: https://pharmaverse.github.io/examples/adam/adsl.html
#   {admiral} ADSL vignette: https://pharmaverse.github.io/admiral/cran-release/articles/adsl.html
#
# Derived variables (per question specifications):
#   AGEGR9, AGEGR9N  - Age grouping (<18, 18-50, >50)
#   TRTSDTM, TRTSTMF - Treatment start date-time (with imputation)
#   ITTFL            - Intent-to-Treat flag (Y if ARM is not missing)
#   LSTAVLDT         - Last known alive date
# =============================================================================

library(admiral)
library(dplyr, warn.conflicts = FALSE)
library(pharmaversesdtm)
library(lubridate)
library(stringr)

# ---- Read in Data ----
# As per the question: "Start by assigning pharmaversesdtm::dm to an adsl object
# as explained in the ADSL article."

dm <- pharmaversesdtm::dm
ex <- pharmaversesdtm::ex
ds <- pharmaversesdtm::ds
ae <- pharmaversesdtm::ae
vs <- pharmaversesdtm::vs

# Convert blank strings to NA for consistent handling
dm <- convert_blanks_to_na(dm)
ex <- convert_blanks_to_na(ex)
ds <- convert_blanks_to_na(ds)
ae <- convert_blanks_to_na(ae)
vs <- convert_blanks_to_na(vs)

# Use DM as the basis for ADSL
adsl <- dm %>%
  select(-DOMAIN)

# ---- Derive Treatment Variables (TRT01P, TRT01A) ----
adsl <- adsl %>%
  mutate(
    TRT01P = ARM,
    TRT01A = ACTARM
  )

# ---- Derive/Impute Treatment Start Date-Time (TRTSDTM, TRTSTMF) ----
# Per spec: Set to datetime of patient's first exposure observation Start Date/Time
# of Treatment [EX.EXSTDTC] converted to numeric datetime when sorted in date/time order.
# Derivation only includes observations where the patient received a valid dose
# (EXDOSE > 0 or (EXDOSE == 0 and EXTRT contains 'PLACEBO')) and datepart of EXSTDTC
# is complete.
# If time is missing, impute completely missing time with 00:00:00,
# partially missing time with 00 for missing components.
# If only seconds are missing then do not populate the imputation flag (TRTSTMF).

ex_ext <- ex %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST",
    highest_imputation = "h",
    time_imputation = "first"
  ) %>%
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    highest_imputation = "h",
    time_imputation = "last"
  )

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
      (EXDOSE == 0 &
        str_detect(EXTRT, "PLACEBO"))) & !is.na(EXSTDTM),
    new_vars = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order = exprs(EXSTDTM, EXSEQ),
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID)
  )

# Derive treatment end date-time
adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
      (EXDOSE == 0 &
        str_detect(EXTRT, "PLACEBO"))) & !is.na(EXENDTM),
    new_vars = exprs(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    order = exprs(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = exprs(STUDYID, USUBJID)
  )

# Derive date variables from datetime
adsl <- adsl %>%
  derive_vars_dtm_to_dt(source_vars = exprs(TRTSDTM, TRTEDTM))

# Derive treatment duration
adsl <- adsl %>%
  derive_var_trtdurd()

# ---- Derive Disposition Variables ----

# Convert DS dates
ds_ext <- derive_vars_dt(
  ds,
  dtc = DSSTDTC,
  new_vars_prefix = "DSST"
)

# End of Study Date (EOSDT)
adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ds_ext,
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(EOSDT = DSSTDT),
    filter_add = DSCAT == "DISPOSITION EVENT" & DSDECOD != "SCREEN FAILURE"
  )

# End of Study Status (EOSSTT)
format_eosstt <- function(x) {
  case_when(
    x %in% c("COMPLETED") ~ "COMPLETED",
    x %in% c("SCREEN FAILURE") ~ NA_character_,
    TRUE ~ "DISCONTINUED"
  )
}

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ds,
    by_vars = exprs(STUDYID, USUBJID),
    filter_add = DSCAT == "DISPOSITION EVENT",
    new_vars = exprs(EOSSTT = format_eosstt(DSDECOD)),
    missing_values = exprs(EOSSTT = "ONGOING")
  )

# Disposition Reason (DCSREAS)
adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ds,
    by_vars = exprs(USUBJID),
    new_vars = exprs(DCSREAS = DSDECOD, DCSREASP = DSTERM),
    filter_add = DSCAT == "DISPOSITION EVENT" &
      !(DSDECOD %in% c("SCREEN FAILURE", "COMPLETED", NA))
  )

# Randomization Date (RANDDT)
adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ds_ext,
    filter_add = DSDECOD == "RANDOMIZED",
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(RANDDT = DSSTDT)
  )

# ---- Derive Birth Date and Analysis Age ----

adsl <- adsl %>%
  derive_vars_dt(
    new_vars_prefix = "BRTH",
    dtc = BRTHDTC
  ) %>%
  derive_vars_aage(
    start_date = BRTHDT,
    end_date = RANDDT
  )

# ---- Derive AGEGR9 and AGEGR9N ----
# Per spec:
#   AGEGR9: Age grouping into categories: "<18", "18 - 50", ">50"
#   AGEGR9N: Numeric groupings: 1, 2, 3

adsl <- adsl %>%
  mutate(
    AGEGR9 = case_when(
      AGE < 18 ~ "<18",
      AGE >= 18 & AGE <= 50 ~ "18 - 50",
      AGE > 50 ~ ">50",
      TRUE ~ NA_character_
    ),
    AGEGR9N = case_when(
      AGE < 18 ~ 1L,
      AGE >= 18 & AGE <= 50 ~ 2L,
      AGE > 50 ~ 3L,
      TRUE ~ NA_integer_
    )
  )

# ---- Derive Death Variables ----

adsl <- adsl %>%
  derive_vars_dt(
    new_vars_prefix = "DTH",
    dtc = DTHDTC
  )

# Cause of Death
adsl <- adsl %>%
  derive_vars_extreme_event(
    by_vars = exprs(STUDYID, USUBJID),
    events = list(
      event(
        dataset_name = "ae",
        condition = AEOUT == "FATAL",
        set_values_to = exprs(DTHCAUS = AEDECOD, DTHDOM = "AE", DTHSEQ = AESEQ)
      ),
      event(
        dataset_name = "ds",
        condition = DSDECOD == "DEATH" & grepl("DEATH DUE TO", DSTERM),
        set_values_to = exprs(DTHCAUS = DSTERM, DTHDOM = "DS", DTHSEQ = DSSEQ)
      )
    ),
    source_datasets = list(ae = ae, ds = ds),
    tmp_event_nr_var = event_nr,
    order = exprs(event_nr),
    mode = "first",
    new_vars = exprs(DTHCAUS, DTHDOM, DTHSEQ)
  )

# Duration relative to death
adsl <- adsl %>%
  derive_vars_duration(
    new_var = DTHADY,
    start_date = TRTSDT,
    end_date = DTHDT
  )

# ---- Derive ITTFL (Intent-to-Treat Flag) ----
# Per spec: Set to "Y" if [DM.ARM] not equal to missing, Else set to "N"

adsl <- adsl %>%
  mutate(
    ITTFL = if_else(!is.na(ARM), "Y", "N")
  )

# ---- Derive LSTAVLDT (Last Known Alive Date) ----
# Per spec: Set to the last date patient has documented clinical data to show
# him/her alive, using:
#   (1) Last complete date of vital assessment with a valid test result
#       (VSSTRESN and VSSTRESC not both missing) and datepart of VSDTC not missing
#   (2) Last complete onset date of AEs (datepart of AESTDTC)
#   (3) Last complete disposition date (datepart of DSSTDTC)
#   (4) Last date of treatment administration where patient received a valid dose
#       (datepart of ADSL.TRTEDTM -> TRTEDT)
# Set to max of all above.

# Derive LSTALVDT from each source separately, then take the max
# This avoids duplicate issues with derive_vars_extreme_event

# (1) VS: last complete date with valid test result
vs_last <- vs %>%
  filter(!is.na(VSDTC) & !(is.na(VSSTRESN) & is.na(VSSTRESC))) %>%
  mutate(LSTALVDT_VS = convert_dtc_to_dt(VSDTC, highest_imputation = "M")) %>%
  group_by(STUDYID, USUBJID) %>%
  summarise(LSTALVDT_VS = max(LSTALVDT_VS, na.rm = TRUE), .groups = "drop")

# (2) AE: last complete onset date
ae_last <- ae %>%
  filter(!is.na(AESTDTC)) %>%
  mutate(LSTALVDT_AE = convert_dtc_to_dt(AESTDTC, highest_imputation = "M")) %>%
  group_by(STUDYID, USUBJID) %>%
  summarise(LSTALVDT_AE = max(LSTALVDT_AE, na.rm = TRUE), .groups = "drop")

# (3) DS: last complete disposition date
ds_last <- ds %>%
  filter(!is.na(DSSTDTC)) %>%
  mutate(LSTALVDT_DS = convert_dtc_to_dt(DSSTDTC, highest_imputation = "M")) %>%
  group_by(STUDYID, USUBJID) %>%
  summarise(LSTALVDT_DS = max(LSTALVDT_DS, na.rm = TRUE), .groups = "drop")

# Merge all sources and take the max
adsl <- adsl %>%
  left_join(vs_last, by = c("STUDYID", "USUBJID")) %>%
  left_join(ae_last, by = c("STUDYID", "USUBJID")) %>%
  left_join(ds_last, by = c("STUDYID", "USUBJID")) %>%
  mutate(
    # (4) TRTEDT is already in adsl
    LSTAVLDT = pmax(LSTALVDT_VS, LSTALVDT_AE, LSTALVDT_DS, TRTEDT, na.rm = TRUE)
  ) %>%
  select(-LSTALVDT_VS, -LSTALVDT_AE, -LSTALVDT_DS)

# ---- Select final ADSL variables ----
# Include standard ADSL variables plus the specifically requested ones

adsl_final <- adsl %>%
  select(
    # Identifiers
    STUDYID, USUBJID, SUBJID, SITEID,
    # Demographics
    AGE, AGEU, AGEGR9, AGEGR9N, SEX, RACE, ETHNIC,
    # Treatment
    ARM, ARMCD, ACTARM, ACTARMCD, TRT01P, TRT01A,
    # Treatment dates
    TRTSDTM, TRTSTMF, TRTEDTM, TRTETMF, TRTSDT, TRTEDT, TRTDURD,
    # Disposition
    EOSDT, EOSSTT, DCSREAS, DCSREASP, RANDDT,
    # Birth and death
    BRTHDT, DTHDT, DTHCAUS, DTHDOM, DTHSEQ, DTHADY, DTHFL,
    # Flags
    ITTFL,
    # Last known alive
    LSTAVLDT,
    # Reference dates
    RFSTDTC, RFENDTC, RFXSTDTC, RFXENDTC,
    # Country
    COUNTRY
  ) %>%
  arrange(STUDYID, USUBJID)

# ---- Output summary ----
cat("=== ADSL Summary ===\n")
cat("Dimensions:", nrow(adsl_final), "rows x", ncol(adsl_final), "columns\n\n")

cat("=== Variable structure ===\n")
str(adsl_final)

cat("\n=== AGEGR9 frequency ===\n")
print(table(adsl_final$AGEGR9, useNA = "ifany"))

cat("\n=== AGEGR9N frequency ===\n")
print(table(adsl_final$AGEGR9N, useNA = "ifany"))

cat("\n=== ITTFL frequency ===\n")
print(table(adsl_final$ITTFL, useNA = "ifany"))

cat("\n=== TRTSTMF frequency ===\n")
print(table(adsl_final$TRTSTMF, useNA = "ifany"))

cat("\n=== TRTSDTM sample (first 10) ===\n")
print(head(adsl_final[, c("USUBJID", "TRTSDTM", "TRTSTMF")], 10))

cat("\n=== LSTAVLDT sample (first 10) ===\n")
print(head(adsl_final[, c("USUBJID", "LSTAVLDT")], 10))

cat("\n=== LSTAVLDT summary ===\n")
print(summary(adsl_final$LSTAVLDT))

# ---- Save output ----
dir.create("question_2_adam", showWarnings = FALSE)
write.csv(adsl_final, "./question_2_adam/adsl.csv", row.names = FALSE)
cat("\n=== ADSL saved to question_2_adam/adsl.csv ===\n")
