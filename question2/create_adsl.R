#Question 2: Create ADSL domain: Expected Result
#ADSL domain with the following variables: 
#AGEGR9, AGEGR9N,TRTSDTM, TRTSTMF,ITTFL,LSTAVLDT,TRTSDTM, TRTSTMF

#LOG 


log_file <- "question2/adsl_log.txt"

zz <- file(log_file, open = "wt")

sink(zz)
sink(zz, type = "message")

cat("Program Started:", Sys.time(), "\n\n")

sessionInfo()

# call the necessary library 
library(sdtm.oak)
library(pharmaverseraw)
library(pharmaversesdtm)

library(dplyr)
library(tidyverse)

library(metacore)
library(metatools)
library(pharmaversesdtm)
library(admiral)
library(xportr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

# Read in input SDTM data
dm <- pharmaversesdtm::dm
ds <- pharmaversesdtm::ds
ex <- pharmaversesdtm::ex
ae <- pharmaversesdtm::ae
vs <- pharmaversesdtm::vs
suppdm <- pharmaversesdtm::suppdm

# ==============================================================================
# Question 2: ADAM ADSL Dataset Creation
# File: question_2_adam/create_adsl.R
# ==============================================================================

library(admiral)
library(pharmaversesdtm)
library(dplyr)
library(lubridate)
library(stringr)

# ------------------------------------------------------------------------------
# 1. Caricamento dei dataset SDTM di input
# ------------------------------------------------------------------------------
data("dm", package = "pharmaversesdtm")
data("ds", package = "pharmaversesdtm")
data("ex", package = "pharmaversesdtm")
data("ae", package = "pharmaversesdtm")
data("vs", package = "pharmaversesdtm")

# Conversione dei vuoti ("") in NA per coerenza
dm <- convert_blanks_to_na(dm)
ds <- convert_blanks_to_na(ds)
ex <- convert_blanks_to_na(ex)
ae <- convert_blanks_to_na(ae)
vs <- convert_blanks_to_na(vs)

# ------------------------------------------------------------------------------
# 2. Inizializzazione di ADSL da DM
# ------------------------------------------------------------------------------
adsl <- dm %>%
  select(STUDYID, USUBJID, SUBJID, RFSTDTC, RFENDTC, ACTARM, ARM, AGE, AGEU, SEX, RACE, ETHNIC)

# ------------------------------------------------------------------------------
# 3. Derivazione AGEGR9 e AGEGR9N
# ------------------------------------------------------------------------------
adsl <- adsl %>%
  mutate(
    AGEGR9 = case_when(
      AGE < 18 ~ "<18",
      AGE >= 18 & AGE <= 50 ~ "18-50",
      AGE > 50 ~ ">50",
      TRUE ~ NA_character_
    ),
    AGEGR9N = case_when(
      AGEGR9 == "<18" ~ 1,
      AGEGR9 == "18-50" ~ 2,
      AGEGR9 == ">50" ~ 3,
      TRUE ~ NA_real_
    )
  )

# ------------------------------------------------------------------------------
# 4. Derivazione ITTFL
# ------------------------------------------------------------------------------
adsl <- adsl %>%
  mutate(
    ITTFL = if_else(!is.na(ARM) & str_trim(ARM) != "", "Y", "N")
  )

# ------------------------------------------------------------------------------
# 5. Derivazione TRTSDTM, TRTSTMF e TRTEDTM (da EX)
# ------------------------------------------------------------------------------
# Filtraggio dosi valide da EX: EXDOSE > 0 oppure (EXDOSE == 0 e EXTRT contiene 'PLACEBO')
ex_valid <- ex %>%
  filter(
    (EXDOSE > 0 | (EXDOSE == 0 & str_detect(toupper(EXTRT), "PLACEBO"))) &
      !is.na(EXSTDTC)
  )

# Derivazione TRTSDTM e TRTSTMF (Primo trattamento)
adsl <- derive_vars_merged(
  dataset = adsl,
  dataset_add = ex_valid,
  by_vars = exprs(STUDYID, USUBJID),
  order = exprs(EXSTDTC, EXSEQ),
  mode = "first",
  new_vars = exprs(EXSTDTC_FIRST = EXSTDTC)
) %>%
  derive_vars_dtm(
    dtc = EXSTDTC_FIRST,
    new_vars_prefix = "TRTS",
    time_imputation = "00:00:00",
    highest_imputation = "h",
    flag_imputation = "auto"
  ) %>%
  select(-EXSTDTC_FIRST)

# Derivazione TRTEDTM (Ultimo trattamento valido)
adsl <- derive_vars_merged(
  dataset = adsl,
  dataset_add = ex_valid,
  by_vars = exprs(STUDYID, USUBJID),
  order = exprs(EXENDTC, EXSEQ),
  mode = "last",
  new_vars = exprs(EXENDTC_LAST = EXENDTC)
) %>%
  derive_vars_dtm(
    dtc = EXENDTC_LAST,
    new_vars_prefix = "TRTE",
    time_imputation = "23:59:59",
    highest_imputation = "h",
    flag_imputation = "none"
  ) %>%
  select(-EXENDTC_LAST)

# ------------------------------------------------------------------------------
# 6. Derivazione LSTAVLDT (Last Known Alive Date)
# ------------------------------------------------------------------------------

# (1) Ultima data di Vital Signs completa
vs_dates <- vs %>%
  filter((!is.na(VSSTRESN) | !is.na(VSSTRESC)) & !is.na(VSDTC)) %>%
  derive_vars_dt(dtc = VSDTC, new_vars_prefix = "VS") %>%
  filter(!is.na(VSDT)) %>%
  group_by(STUDYID, USUBJID) %>%
  summarise(LSTDT_VS = max(VSDT, na.rm = TRUE), .groups = "drop")

# (2) Ultima data di inizio Adverse Events completa
ae_dates <- ae %>%
  filter(!is.na(AESTDTC)) %>%
  derive_vars_dt(dtc = AESTDTC, new_vars_prefix = "AE") %>%
  filter(!is.na(AEDT)) %>%
  group_by(STUDYID, USUBJID) %>%
  summarise(LSTDT_AE = max(AEDT, na.rm = TRUE), .groups = "drop")

# (3) Ultima data di Disposition completa
ds_dates <- ds %>%
  filter(!is.na(DSSTDTC)) %>%
  derive_vars_dt(dtc = DSSTDTC, new_vars_prefix = "DS") %>%
  filter(!is.na(DSDT)) %>%
  group_by(STUDYID, USUBJID) %>%
  summarise(LSTDT_DS = max(DSDT, na.rm = TRUE), .groups = "drop")

# Unione delle date e calcolo del massimo (inclusa la parte data di TRTEDTM)
adsl <- adsl %>%
  left_join(vs_dates, by = c("STUDYID", "USUBJID")) %>%
  left_join(ae_dates, by = c("STUDYID", "USUBJID")) %>%
  left_join(ds_dates, by = c("STUDYID", "USUBJID")) %>%
  mutate(
    TRTEDT = date(TRTEDTM),
    LSTAVLDT = pmax(LSTDT_VS, LSTDT_AE, LSTDT_DS, TRTEDT, na.rm = TRUE)
  ) %>%
  # Pulizia variabili temporanee d'appoggio
  select(-LSTDT_VS, -LSTDT_AE, -LSTDT_DS, -TRTEDT)

write.csv(
  ds,
  file = "question2/adsl.csv",
  row.names = FALSE,
  na = ""
)

cat("\nProgram Finished:", Sys.time(), "\n")

sink(type = "message")
sink()

close(zz)
