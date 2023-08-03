#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# create_share_bowel_cohort.R
# Calum Purdie
# Feb 2023
# Script 1 of 1
# Data linkage
# Written/run on R Studio Server
# R version 3.6.1
# This script links the SHARE cohort with the bowel screening database
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### 1 Housekeeping ----

# Loading packages

library(dplyr)
library(janitor)
library(readr)
library(lubridate)
library(tidylog)
library(haven)
library(sjlabelled)
library(phsmethods)
library(here)

# Define todays date

todays_date <- strftime(today(), "%Y%m%d")

# Set filepaths

bowel_path <- paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening/Data/", 
                     "Programme/2022-11/")

share_path <- paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening/Projects/", 
                     "20230202-SHARE-Cohort/Temp/")

source(here::here("Code","00_housekeeping.R"))



### 2 Data Extraction ----

# Read in SHARE cohort
# Rename CHI to chinum for easy matching to bowel

share <- read_csv(paste0(share_path, "share_cohort_20230104.csv")) |> 
  rename(chinum = CHI)

# Check CHI validity

share |> 
  mutate(is_chi_valid = chi_check(chinum)) |> 
  count(is_chi_valid)

# All CHIs are valid

# Read in bowel screening database
# Filter for latest two year period and select columns

bowel_extract <- read_rds(paste0(bowel_path, "combined_extract_all.rds")) |> 
  filter(between(invdate, as.Date(date_first), as.Date(date_last))) |> 
  select(chinum, invdate, hbres, sex, screres, screresdate, err, haemoglobin, 
         fit_test, hbident, sex, screresdat, dateprecol, colperf, datecolperf, 
         colreason, colcomp, barenctc, barectalt, barctdat, cancer, icd_10, 
         tnm_t, tnm_n, tnm_m, dukes, polyp, adenoma, adenno, adensize, polypca, 
         polypect, complicp, mort)

# Join share and bowel_extract

share_bowel_cohort <- share |> 
  left_join(bowel_extract)

# Check if all CHIs have matched

share_bowel_cohort |> 
  count(is.na(screres))

# 125,946 (42%) of CHIs in share are not in bowel screening database
# This is probably ok due to ages/not being part of programme etc.

# View CHIs in share that are not in bowel screening database

share_bowel_cohort |> 
  filter(is.na(screres)) |> 
  View()

# Check err column

share_bowel_cohort |> 
  count(err)

# # View any rows with err == 1
# 
# share_bowel_cohort |> 
#   filter(err == 1) |> 
#   View()



### 3 Output ----

# Save output as csv as this was format of original file

write_csv(share_bowel_cohort, 
          paste0(here("Output/CONFI-HIC-SHARE-cohort/"), todays_date, 
                      "_share_bowel_cohort.csv"), na = "")


