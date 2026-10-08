# ==============================================================================
# Question 3 - Task 1: Adverse Events Summary Table
# File: question_3_tlg/01_create_ae_summary_table.R
# ==============================================================================

#LOG 


log_file <- "question_3_tlg/ae_summary_table_log.txt"

zz <- file(log_file, open = "wt")

sink(zz)
sink(zz, type = "message")

cat("Program Started:", Sys.time(), "\n\n")

sessionInfo()


library(pharmaverseadam)
library(gtsummary)
library(dplyr)
library(gt)

#Upload data
data("adae", package = "pharmaverseadam")


#filter Treatment-Emergent Adverse Event
adae_teae <- adae %>%
  filter(TRTEMFL == "Y") %>%
  mutate(
    ACTARM = factor(ACTARM),
    AESOC = as.character(AESOC),
    AETERM = as.character(AETERM)
  )

# create summary table
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

# save table
ae_summary_gt <- as_gt(ae_summary_table)
gtsave(ae_summary_gt, filename = "question3/ae_summary_table.html")


cat("\nProgram Finished:", Sys.time(), "\n")

sink(type = "message")
sink()

close(zz)
