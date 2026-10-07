# ==============================================================================
# Question 3 - Task 1: Adverse Events Summary Table
# File: question_3_tlg/01_create_ae_summary_table.R
# ==============================================================================

library(pharmaverseadam)
library(gtsummary)
library(dplyr)
library(gt)

# 1. Caricamento dati
data("adae", package = "pharmaverseadam")

dir.create("question_3_tlg", showWarnings = FALSE)

# 2. Filtraggio per Treatment-Emergent Adverse Events (TEAE)
adae_teae <- adae %>%
  filter(TRTEMFL == "Y") %>%
  mutate(
    ACTARM = factor(ACTARM),
    AESOC = as.character(AESOC),
    AETERM = as.character(AETERM)
  )

# 3. Creazione tabella riassuntiva senza errori di merge
ae_summary_table <- adae_teae %>%
  select(ACTARM, AESOC, AETERM) %>%
  tbl_summary(
    by = ACTARM,
    include = c(AESOC, AETERM),
    label = list(
      AESOC ~ "Primary System Organ Class",
      AETERM ~ "Reported Term for the Adverse Event"
    ),
    missing = "no"
  ) %>%
  add_overall(last = TRUE, col_label = "**Total**") %>%
  modify_caption("**Summary of Treatment-Emergent Adverse Events (TEAEs) by SOC and Preferred Term**")

# 4. Converti in gt e salva in HTML
ae_summary_gt <- as_gt(ae_summary_table)
gtsave(ae_summary_gt, filename = "question3/ae_summary_table.html")

print("Tabella creata con successo in question_3_tlg/ae_summary_table.html")

