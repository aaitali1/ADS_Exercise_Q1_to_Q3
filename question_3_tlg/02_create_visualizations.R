# =============================================================================
# Name:  AE Visualizations
# Label: Create adverse events visualizations using {ggplot2}
#
# Input datasets:
#   pharmaverseadam::adae - ADAE dataset
#   pharmaverseadam::adsl - ADSL dataset
#
# Outputs:
#   Plot 1: ae_severity_by_treatment.png
#           AE severity distribution by treatment (stacked bar chart)
#   Plot 2: top10_ae_forest_plot.png
#           Top 10 most frequent AEs with 95% Clopper-Pearson CIs
#
# Reference: Pharmaverse Examples TLG section, FDA TLG Catalogue
# =============================================================================

library(pharmaverseadam)
library(ggplot2)
library(dplyr)

# ---- Read in Data ----
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

# Filter to TEAEs and safety population
adae_teae <- adae %>%
  filter(TRTEMFL == "Y", SAFFL == "Y")

adsl_saf <- adsl %>%
  filter(SAFFL == "Y")

n_total <- nrow(adsl_saf)

# =============================================================================
# Plot 1: AE Severity Distribution by Treatment (Stacked Bar Chart)
# =============================================================================

# Count AEs by treatment arm and severity
ae_sev <- adae_teae %>%
  mutate(
    AESEV = factor(AESEV, levels = c("SEVERE", "MODERATE", "MILD")),
    ACTARM = factor(ACTARM, levels = c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose"))
  ) %>%
  count(ACTARM, AESEV, .drop = FALSE)

p1 <- ggplot(ae_sev, aes(x = ACTARM, y = n, fill = AESEV)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c("MILD" = "#F8766D", "MODERATE" = "#00BA38", "SEVERE" = "#619CFF"),
    name = "Severity/Intensity"
  ) +
  labs(
    title = "AE severity distribution by treatment",
    x = "Treatment Arm",
    y = "Count of AEs"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 10),
    legend.position = "right"
  )

dir.create("question_3_tlg", showWarnings = FALSE)
ggsave(
  "question_3_tlg/ae_severity_by_treatment.png",
  plot = p1,
  width = 8,
  height = 6,
  dpi = 300
)
cat("=== Plot 1 saved: question_3_tlg/ae_severity_by_treatment.png ===\n")

# =============================================================================
# Plot 2: Top 10 Most Frequent AEs with 95% Clopper-Pearson CIs
# =============================================================================

# Count unique subjects per AETERM (across all arms)
ae_top <- adae_teae %>%
  group_by(AETERM) %>%
  summarise(n_subj = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(desc(n_subj)) %>%
  slice_head(n = 10)

# Calculate proportions and 95% Clopper-Pearson CIs
ae_top <- ae_top %>%
  mutate(
    pct = n_subj / n_total * 100,
    # Clopper-Pearson exact binomial CI
    ci_lower = qbeta(0.025, n_subj, n_total - n_subj + 1) * 100,
    ci_upper = qbeta(0.975, n_subj + 1, n_total - n_subj) * 100,
    AETERM = factor(AETERM, levels = rev(AETERM))
  )

p2 <- ggplot(ae_top, aes(x = pct, y = AETERM)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper), width = 0.2, orientation = "y") +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    breaks = seq(0, 35, 5)
  ) +
  labs(
    title = "Top 10 Most Frequent Adverse Events",
    subtitle = paste0("n = ", n_total, " subjects; 95% Clopper-Pearson CIs"),
    x = "Percentage of Patients (%)",
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.major.y = element_blank()
  )

ggsave(
  "question_3_tlg/top10_ae_forest_plot.png",
  plot = p2,
  width = 9,
  height = 6,
  dpi = 300
)
cat("=== Plot 2 saved: question_3_tlg/top10_ae_forest_plot.png ===\n")
