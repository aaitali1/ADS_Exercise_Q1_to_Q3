# =============================================================================
# Name:  DS Domain
# Label: R program to create the SDTM DS (Disposition) domain using {sdtm.oak}
#
# Input raw data:  pharmaverseraw::ds_raw
# Study controlled terminology: data/sdtm_ct.csv
# DM domain: pharmaversesdtm::dm (for study day derivation)
#
# Reference: CDISC SDTMIG v3.4 - DS domain (Events class)
# Hint: This example follows the same pattern as the AE example in the
#       Pharmaverse Examples (https://pharmaverse.github.io/examples/sdtm/ae.html)
#
# Expected output variables:
#   STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD, DSCAT,
#   VISITNUM, VISIT, DSDTC, DSSTDTC, DSSTDY
# =============================================================================

library(sdtm.oak)
library(pharmaverseraw)
library(pharmaversesdtm)
library(dplyr)

# ---- Read in Study Controlled Terminology ----
# The study_ct file contains codelist mappings including DS-specific entries
# for DSDECOD (C66727), VISIT, and VISITNUM.
study_ct <- read.csv("./data/sdtm_ct.csv")

# ---- Read in raw data & create oak_id_vars ----
# oak_id_vars are internal tracking variables used by sdtm.oak to maintain
# traceability between raw source records and SDTM output records.
ds_raw <- pharmaverseraw::ds_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  )

# Read in the DM domain (needed for derive_study_day)
dm <- pharmaversesdtm::dm

# ---- Map Topic Variable: DSDECOD ----
# DSDECOD (Standardized Disposition Term) is the topic variable for the DS domain.
# It is mapped from IT.DSDECOD using controlled terminology codelist C66727.
# Records where DSDECOD cannot be mapped (NA) are filtered out.
ds <-
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "IT.DSDECOD",
    tgt_var = "DSDECOD",
    ct_spec = study_ct,
    ct_clst = "C66727"
  ) %>%
  dplyr::filter(!is.na(.data$DSDECOD))

# ---- Map Rest of the Variables ----

# Map DSTERM (Reported Term for the Disposition Event)
# DSTERM is the verbatim term as collected - no controlled terminology.
ds <- ds %>%
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "IT.DSTERM",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  )

# Map DSDTC (Date/Time of Collection)
# Mapped from DSDTCOL (collection date, format MM-DD-YYYY) and
# DSTMCOL (collection time, format HH:MM).
ds <- ds %>%
  assign_datetime(
    raw_dat = ds_raw,
    raw_var = c("DSDTCOL", "DSTMCOL"),
    tgt_var = "DSDTC",
    raw_fmt = c(list(c("m-d-y")), list(c("H:M")))
  )

# Map DSSTDTC (Start Date/Time of Disposition Event)
# Mapped from IT.DSSTDAT (format MM-DD-YYYY).
ds <- ds %>%
  assign_datetime(
    raw_dat = ds_raw,
    raw_var = c("IT.DSSTDAT"),
    tgt_var = "DSSTDTC",
    raw_fmt = c(list(c("m-d-y"))),
    id_vars = oak_id_vars()
  )

# Map VISIT from INSTANCE using controlled terminology
ds <- ds %>%
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "INSTANCE",
    tgt_var = "VISIT",
    ct_spec = study_ct,
    ct_clst = "VISIT",
    id_vars = oak_id_vars()
  )

# Map VISITNUM from INSTANCE using controlled terminology
ds <- ds %>%
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "INSTANCE",
    tgt_var = "VISITNUM",
    ct_spec = study_ct,
    ct_clst = "VISITNUM",
    id_vars = oak_id_vars()
  )

# ---- Create SDTM derived variables ----
# Domain-level variables applicable to all records.
ds <- ds %>%
  dplyr::mutate(
    STUDYID = "CDISCPILOT01",
    DOMAIN  = "DS",
    USUBJID = paste0("01-", patient_number),
    # Derive DSCAT: RANDOMIZED is a protocol milestone, rest are disposition events
    DSCAT = dplyr::case_when(
      DSDECOD %in% c("RANDOMIZED") ~ "PROTOCOL MILESTONE",
      TRUE ~ "DISPOSITION EVENT"
    )
  )

# Derive DSSEQ (Sequence Number)
ds <- ds %>%
  dplyr::arrange(USUBJID, DSSTDTC) %>%
  derive_seq(
    tgt_var = "DSSEQ",
    rec_vars = c("USUBJID", "DSDECOD")
  )

# Derive DSSTDY (Study Day of Start of Disposition Event)
# Uses the DM domain RFSTDTC (Reference Start Date) as the reference date.
ds <- ds %>%
  derive_study_day(
    sdtm_in = .,
    dm_domain = dm,
    tgdt = "DSSTDTC",
    refdt = "RFSTDTC",
    study_day_var = "DSSTDY"
  )

# ---- Select and order final SDTM DS variables ----
ds_final <- ds %>%
  dplyr::select(
    STUDYID,
    DOMAIN,
    USUBJID,
    DSSEQ,
    DSTERM,
    DSDECOD,
    DSCAT,
    VISITNUM,
    VISIT,
    DSDTC,
    DSSTDTC,
    DSSTDY
  ) %>%
  dplyr::arrange(USUBJID, DSSEQ)

# ---- Output summary ----
cat("=== DS Domain Summary ===\n")
cat("Dimensions:", nrow(ds_final), "rows x", ncol(ds_final), "columns\n\n")

cat("=== Variable structure ===\n")
str(ds_final)

cat("\n=== First 20 rows ===\n")
print(as.data.frame(head(ds_final, 20)), row.names = FALSE)

cat("\n=== DSDECOD frequency ===\n")
print(table(ds_final$DSDECOD, useNA = "ifany"))

cat("\n=== DSCAT frequency ===\n")
print(table(ds_final$DSCAT, useNA = "ifany"))

cat("\n=== DSSTDY summary ===\n")
print(summary(ds_final$DSSTDY))

# ---- Save output ----
dir.create("question_1_sdtm", showWarnings = FALSE)
write.csv(ds_final, "./question_1_sdtm/ds.csv", row.names = FALSE)
cat("\n=== DS domain saved to question_1_sdtm/ds.csv ===\n")
