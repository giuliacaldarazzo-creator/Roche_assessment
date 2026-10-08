# ==============================================================================
# Question 1: SDTM DS Dataset Creation
# File: question_1_sdtm/create_ds_domain.R
# ==============================================================================
#STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD, DSCAT, VISITNUM, VISIT, 
#DSDTC,DSSTDTC, DSSTDY

#LOG 


log_file <- "question_1_sdtm/ds_log.txt"

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


#use study_ct file-stored into question 1 folder

study_ct <- read_csv("/cloud/project/question_1_sdtm/sdtm_ct.csv")

#import the necessary raw data
dm <- pharmaversesdtm::dm
ds_raw <- pharmaverseraw::ds_raw

#create oak Id variable
ds_raw <- ds_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  )

#map dsterm variable from IT.DSTERM

ds <-
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "IT.DSTERM",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  )

#MAP ALL THE OTHERS VARAIBLES
ds<-ds%>%
  
  mutate(
    
    STUDYID = ds_raw$STUDY,
    DOMAIN = "DS",
    USUBJID = paste0("01-", ds_raw$PATNUM),
    
    VISIT = ds_raw$INSTANCE,
    
    DSTERM = case_when(
      
      !is.na(ds_raw$OTHERSP) &
        trimws(ds_raw$OTHERSP) != ""
      ~ toupper(ds_raw$OTHERSP),
      
      TRUE
      ~ toupper(ds_raw$IT.DSTERM)
      
    ),
    
    DSDECOD = case_when(
      
      !is.na(ds_raw$OTHERSP) &
        trimws(ds_raw$OTHERSP) != ""
      ~ toupper(ds_raw$OTHERSP),
      
      TRUE
      ~ toupper(ds_raw$IT.DSDECOD)
      
    )
    
  ) %>%
  
  mutate(
    
    DSCAT = case_when(
      
      !is.na(ds_raw$OTHERSP) &
        trimws(ds_raw$OTHERSP) != ""
      ~ "OTHER EVENT",
      
      DSDECOD == "RANDOMIZED"
      ~ "PROTOCOL MILESTONE",
      
      TRUE
      ~ "DISPOSITION EVENT"
      
    )
    
  )

ds <- ds %>%
  
  mutate(
    
    DSDTC = case_when(
      
      !is.na(ds_raw$DSDTCOL) &
        !is.na(ds_raw$DSTMCOL) &
        trimws(ds_raw$DSTMCOL) != "" ~
        
        paste(
          format(mdy(ds_raw$DSDTCOL), "%Y-%m-%d"),
          trimws(ds_raw$DSTMCOL),
          sep = "T"
        ),
      
      !is.na(ds_raw$DSDTCOL) ~
        
        format(
          mdy(ds_raw$DSDTCOL),
          "%Y-%m-%d"
        ),
      
      TRUE ~ NA_character_
      
    )
  )
ds <- ds %>%
  mutate(
    
    DSSTDTC = case_when(
      
      DSDECOD == "DEATH" &
        !is.na(ds_raw$DEATHDT) ~
        
        format(
          lubridate::mdy(ds_raw$DEATHDT),
          "%Y-%m-%d"
        ),
      
      !is.na(ds_raw$IT.DSSTDAT) ~
        
        format(
          lubridate::mdy(ds_raw$IT.DSSTDAT),
          "%Y-%m-%d"
        ),
      
      TRUE ~ NA_character_
      
    
    )
    
  )


ds <- ds %>%
  
  mutate(
    
    VISITNUM = case_when(
      
      VISIT== "Screening 1" ~ 1,
      
      VISIT == "Baseline" ~ 2,
      
      VISIT == "Week 2" ~ 2,
      
      VISIT == "Week 4" ~ 4,
      
      VISIT == "Week 6" ~ 6,
      
      VISIT == "Week 8" ~ 8,
      
      VISIT == "Week 12" ~ 12,
      
      VISIT == "Week 16" ~ 16,
      
      VISIT == "Week 20" ~ 20,
      
      VISIT == "Week 24" ~ 24,
      
      (VISIT) == "Week 26" ~ 26,
      
      (VISIT) == "Retrieval" ~ 900,
      
      (VISIT) == "Ambul Ecg Removal" ~ 901,
      
      (VISIT) == "Unscheduled 1.1" ~ 1.1,
      
      (VISIT) == "Unscheduled 4.1" ~ 4.1,
      
      (VISIT) == "Unscheduled 5.1" ~ 5.1,
      
      (VISIT) == "Unscheduled 6.1" ~ 6.1,
      
      (VISIT) == "Unscheduled 8.2" ~ 8.2,
      
      (VISIT) == "Unscheduled 13.1" ~ 13.1,
      
      TRUE ~ NA_real_
      
    
      
    )
    
  )
ds <- ds %>%
  
  derive_seq(
    tgt_var = "DSSEQ",
    rec_vars = c(
      "USUBJID",
      "DSSTDTC",
      "DSTERM"
    )
  ) %>%
  
  derive_study_day(
    sdtm_in = .,
    dm_domain = dm,
    tgdt = "DSSTDTC",
    refdt = "RFXSTDTC",
    study_day_var = "DSSTDY"
  )
ds <- ds %>%
  
  select(
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
  )


write.csv(
  ds,
  file = "question_1_sdtm/ds.csv",
  row.names = FALSE,
  na = ""
)

cat("\nProgram Finished:", Sys.time(), "\n")

sink(type = "message")
sink()

close(zz)
