# ==============================================================================
# Question 2: ADAM ADSL Dataset Creation
# File: question_2_adam/create_adsl.R
# ==============================================================================
#AGEGR9, AGEGR9N,TRTSDTM, TRTSTMF,ITTFL,LSTAVLDT,TRTSDTM, TRTSTMF

#create LOG FILE 


log_file <- "question_2_adam/adsl_log.txt"

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



# convert blanck value in NA
dm <- convert_blanks_to_na(dm)
ds <- convert_blanks_to_na(ds)
ex <- convert_blanks_to_na(ex)
ae <- convert_blanks_to_na(ae)
vs <- convert_blanks_to_na(vs)

#select variables from sdtm.dm
adsl <- dm %>%
  select(STUDYID, USUBJID, SUBJID, RFSTDTC, RFENDTC, ACTARM, ARM, AGE, AGEU, SEX, RACE, ETHNIC)

# Derive AGEGR9 e AGEGR9N

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


# Derive ITTFL

adsl <- adsl %>%
  mutate(
    ITTFL = if_else(!is.na(ARM) & str_trim(ARM) != "", "Y", "N")
  )


#  Derive TRTSDTM, TRTSTMF e TRTEDTM (da EX)


ex_valid <- ex %>%
  filter(
    (EXDOSE > 0 | (EXDOSE == 0 & str_detect(toupper(EXTRT), "PLACEBO"))) &
      !is.na(EXSTDTC)
  )

# Derive TRTSDTM e TRTSTMF 
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

# Derive TRTEDTM 
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


# Derive LSTAVLDT (Last Known Alive Date)

#last date from vital sign
vs_dates <- vs %>%
  filter((!is.na(VSSTRESN) | !is.na(VSSTRESC)) & !is.na(VSDTC)) %>%
  derive_vars_dt(dtc = VSDTC, new_vars_prefix = "VS") %>%
  filter(!is.na(VSDT)) %>%
  group_by(STUDYID, USUBJID) %>%
  summarise(LSTDT_VS = max(VSDT, na.rm = TRUE), .groups = "drop")

# last date from Adverse Events
ae_dates <- ae %>%
  filter(!is.na(AESTDTC)) %>%
  derive_vars_dt(dtc = AESTDTC, new_vars_prefix = "AE") %>%
  filter(!is.na(AEDT)) %>%
  group_by(STUDYID, USUBJID) %>%
  summarise(LSTDT_AE = max(AEDT, na.rm = TRUE), .groups = "drop")

# last date from disposition
ds_dates <- ds %>%
  filter(!is.na(DSSTDTC)) %>%
  derive_vars_dt(dtc = DSSTDTC, new_vars_prefix = "DS") %>%
  filter(!is.na(DSDT)) %>%
  group_by(STUDYID, USUBJID) %>%
  summarise(LSTDT_DS = max(DSDT, na.rm = TRUE), .groups = "drop")

# calculate the maximum date
adsl <- adsl %>%
  left_join(vs_dates, by = c("STUDYID", "USUBJID")) %>%
  left_join(ae_dates, by = c("STUDYID", "USUBJID")) %>%
  left_join(ds_dates, by = c("STUDYID", "USUBJID")) %>%
  mutate(
    TRTEDT = date(TRTEDTM),
    LSTAVLDT = pmax(LSTDT_VS, LSTDT_AE, LSTDT_DS, TRTEDT, na.rm = TRUE)
  ) %>%

  select(-LSTDT_VS, -LSTDT_AE, -LSTDT_DS, -TRTEDT)

write.csv(
  ds,
  file = "question_2_adam/adsl.csv",
  row.names = FALSE,
  na = ""
)

cat("\nProgram Finished:", Sys.time(), "\n")

sink(type = "message")
sink()

close(zz)
