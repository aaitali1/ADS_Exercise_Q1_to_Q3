# =============================================================================
# Name:  AE Summary Table
# Label: Create a summary table of treatment-emergent adverse events (TEAEs)
#        using {gtsummary}, following FDA Table 10 pattern.
#
# Input datasets:
#   pharmaverseadam::adae - ADAE dataset
#   pharmaverseadam::adsl - ADSL dataset
#
# Output: ae_summary_table.html
#
# Table specifications:
#   - Treatment-emergent AEs only (TRTEMFL == "Y")
#   - Rows: AESOC (Primary System Organ Class) and AETERM
#   - Columns: Treatment groups (ACTARM)
#   - Cell values: Count (n) and percentage (%)
#   - Include total column with all subjects
#   - Sort by descending frequency
#
# Reference: FDA Table 10, Pharmaverse Examples TLG section
# =============================================================================

library(pharmaverseadam)
library(gtsummary)
library(gt)
library(dplyr)

# ---- Read in Data ----
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

# ---- Pre-processing ----

# Filter to treatment-emergent AEs and safety population
adae_teae <- adae %>%
  filter(TRTEMFL == "Y", SAFFL == "Y")

# Get safety population counts per arm (denominators)
adsl_saf <- adsl %>%
  filter(SAFFL == "Y")

n_per_arm <- adsl_saf %>%
  count(ACTARM) %>%
  arrange(ACTARM)

n_total <- nrow(adsl_saf)

cat("=== Safety population per arm ===\n")
print(n_per_arm)
cat("Total:", n_total, "\n\n")

# ---- Build the TEAE summary table ----
# Strategy: Create a subject-level dataset with one row per subject per SOC/TERM
# combination, then use gtsummary to tabulate.

# Step 1: Get unique subjects with any TEAE per arm (for the header row)
subj_any_teae <- adae_teae %>%
  distinct(USUBJID, ACTARM) %>%
  mutate(AESOC = "Treatment Emergent AEs", AETERM = NA_character_)

# Step 2: Get unique subjects per SOC per arm
subj_per_soc <- adae_teae %>%
  distinct(USUBJID, ACTARM, AESOC)

# Step 3: Get unique subjects per SOC/TERM per arm
subj_per_term <- adae_teae %>%
  distinct(USUBJID, ACTARM, AESOC, AETERM)

# Step 4: Calculate frequencies for each level

# Function to calculate n (%) for each arm
calc_freq <- function(data, group_vars, denom_df) {
  data %>%
    group_by(across(all_of(c(group_vars, "ACTARM")))) %>%
    summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
    left_join(
      denom_df %>% rename(N = n),
      by = "ACTARM"
    ) %>%
    mutate(
      pct = round(n / N * 100, 0),
      value = paste0(n, " (", pct, "%)")
    )
}

# Any TEAE row
any_teae <- adae_teae %>%
  distinct(USUBJID, ACTARM) %>%
  group_by(ACTARM) %>%
  summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
  left_join(n_per_arm %>% rename(N = n), by = "ACTARM") %>%
  mutate(
    pct = round(n / N * 100, 0),
    value = paste0(n, " (", pct, "%)"),
    AESOC = "Treatment Emergent AEs",
    AETERM = "",
    level = 0
  )

# Also compute total column for any TEAE
any_teae_total <- adae_teae %>%
  summarise(n = n_distinct(USUBJID)) %>%
  mutate(
    N = n_total,
    pct = round(n / N * 100, 0),
    value = paste0(n, " (", pct, "%)"),
    ACTARM = "Total",
    AESOC = "Treatment Emergent AEs",
    AETERM = "",
    level = 0
  )

# SOC level
soc_freq <- calc_freq(subj_per_soc, "AESOC", n_per_arm) %>%
  mutate(AETERM = "", level = 1)

soc_freq_total <- subj_per_soc %>%
  group_by(AESOC) %>%
  summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(
    N = n_total, pct = round(n / N * 100, 0),
    value = paste0(n, " (", pct, "%)"),
    ACTARM = "Total", AETERM = "", level = 1
  )

# TERM level
term_freq <- calc_freq(subj_per_term, c("AESOC", "AETERM"), n_per_arm) %>%
  mutate(level = 2)

term_freq_total <- subj_per_term %>%
  group_by(AESOC, AETERM) %>%
  summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(
    N = n_total, pct = round(n / N * 100, 0),
    value = paste0(n, " (", pct, "%)"),
    ACTARM = "Total", level = 2
  )

# Combine all
all_freq <- bind_rows(
  any_teae, any_teae_total,
  soc_freq, soc_freq_total,
  term_freq, term_freq_total
)

# ---- Pivot to wide format ----
# Order arms
arm_order <- c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose", "Total")
arm_labels <- c(
  "Placebo" = paste0("Placebo\nN = ", n_per_arm$n[n_per_arm$ACTARM == "Placebo"]),
  "Xanomeline High Dose" = paste0("Xanomeline\nHigh Dose\nN = ", n_per_arm$n[n_per_arm$ACTARM == "Xanomeline High Dose"]),
  "Xanomeline Low Dose" = paste0("Xanomeline\nLow Dose\nN = ", n_per_arm$n[n_per_arm$ACTARM == "Xanomeline Low Dose"]),
  "Total" = paste0("Total\nN = ", n_total)
)

wide_df <- all_freq %>%
  select(AESOC, AETERM, ACTARM, value, level, n) %>%
  tidyr::pivot_wider(
    id_cols = c(AESOC, AETERM, level),
    names_from = ACTARM,
    values_from = c(value, n),
    values_fill = list(value = "0 (0%)", n = 0)
  )

# Sort by descending total frequency within each SOC
# First sort SOCs by total frequency, then terms within SOC
soc_order <- wide_df %>%
  filter(level == 1) %>%
  arrange(desc(n_Total)) %>%
  pull(AESOC)

wide_df <- wide_df %>%
  mutate(
    soc_rank = match(AESOC, c("Treatment Emergent AEs", soc_order)),
    soc_rank = ifelse(is.na(soc_rank), 999, soc_rank)
  ) %>%
  arrange(soc_rank, level, desc(n_Total))

# ---- Create gt table ----
# Build the display label
wide_df <- wide_df %>%
  mutate(
    label = case_when(
      level == 0 ~ AESOC,
      level == 1 ~ AESOC,
      level == 2 ~ paste0("    ", AETERM)
    )
  )

gt_table <- wide_df %>%
  select(
    label,
    `value_Placebo`,
    `value_Xanomeline High Dose`,
    `value_Xanomeline Low Dose`,
    `value_Total`
  ) %>%
  gt() %>%
  cols_label(
    label = md("**Primary System Organ Class**<br>**Reported Term for the Adverse Event**"),
    value_Placebo = md(paste0("**Placebo**<br>N = ", n_per_arm$n[n_per_arm$ACTARM == "Placebo"])),
    `value_Xanomeline High Dose` = md(paste0("**Xanomeline<br>High Dose**<br>N = ", n_per_arm$n[n_per_arm$ACTARM == "Xanomeline High Dose"])),
    `value_Xanomeline Low Dose` = md(paste0("**Xanomeline<br>Low Dose**<br>N = ", n_per_arm$n[n_per_arm$ACTARM == "Xanomeline Low Dose"])),
    value_Total = md(paste0("**Total**<br>N = ", n_total))
  ) %>%
  cols_align(align = "center", columns = -label) %>%
  cols_align(align = "left", columns = label) %>%
  tab_header(
    title = "Table 10: Summary of Treatment-Emergent Adverse Events by SOC and Preferred Term",
    subtitle = "Safety Population"
  ) %>%
  tab_options(
    table.font.size = px(11),
    heading.title.font.size = px(14),
    heading.subtitle.font.size = px(12),
    column_labels.font.size = px(11)
  )

# ---- Save output ----
dir.create("question_3_tlg", showWarnings = FALSE)
gtsave(gt_table, filename = "question_3_tlg/ae_summary_table.html")

cat("=== AE Summary Table saved to question_3_tlg/ae_summary_table.html ===\n")
cat("=== Table dimensions:", nrow(wide_df), "rows ===\n")
cat("=== Number of SOCs:", length(soc_order), "===\n")
cat("=== Number of unique AETERMs:", n_distinct(adae_teae$AETERM), "===\n")
