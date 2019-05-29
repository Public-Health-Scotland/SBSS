##########################################################
# 5_MI_FOBT_FIT_comparison.R
# Thomas Godfrey
# 28/05/2019
# Script 5
# Management Information:
# COmparison of KPIs between FOBT and qFIT data.
# Written/run on R Studio server: and so uses //PHI_conf/
# R version 3.5.1 
# This script creates Confidence Interval data for funnel plots in Excel
# Transcribed from SPSS script "FIT comparison KPIs.sps"
# At: 
#\\stats\CancerGroup1\Topics\BowelScreening\Publications\SBoSP-Statistics\20190205\Syntax
##########################################################

### Purpose of this script

### This script has two aims:

### Key outputs:

### Notes: 
# Create Screening History file.
# Get relevant records for the KPI report.
# Create variables required.
# Calculate uptake and positivity.


### Step 1: Housekeeping

## Load packages
library(dplyr)
library(readr)
library(haven)
library(janitor)
library(magrittr)
library(lubridate)
library(invgamma)
library(here)
library(tidyr)
library("rlang", lib.loc="~/R/x86_64-redhat-linux-gnu-library/3.2")

## Define functions


## Set filepaths and import reporting period dates from Script 0
# Define location of combined_extract_all and analsysis_dataset
source(here::here("code", "0_housekeeping.R"))


### Step 2: Import data
#Keep this code and delete below when 'here' works
sbsp_analysis_db <- read_sav(analysis_db_path) %>%
  clean_names()

analysis_db <- read_sav(paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening/",
                               "TPP/KPIs/Code + DB/TPP/data/analysis_dataset.rds"))


### STEP 3: Create Screening History file.

# Use Select statement to keep only required variables: chinum screres date_round.
# Sort cases by chi-number
# Spread individual patients screening results onto single rows by screening round
screening_history_byCHI <- sbspdb_combined_extract_all %>% 
  select('chinum', 'screres', 'date_round') %>%
  arrange(chinum) %>%
  group_by(chinum) %>%
  spread(key = 'date_round',value = 'screres') %>%
  ungroup()



### Step 4: Get relevant records for the KPI report.
#Open the most recent combined_extract_all file.
sbsp_slimdb <- sbspdb_combined_extract_all %>%
  filter(invdate >= as.Date(date_first) & invdate <= as.Date(date_last)) %>%
  filter(optin == 0) %>%
  filter(hbr14 %in% 1:14) %>%
  filter(screres %in% c(1:18,21,22,24))
dim(sbsp_slimdb)
names(sbsp_slimdb)


#Select report time period and comparison time period
test_comp_db <- sbsp_slimdb %>% 
  mutate(
    fobt_flag = ifelse((invdate >= as.Date("2016-11-20") & invdate <= as.Date("2017-04-30")),1,0),
    fit_flag  = ifelse((invdate >= as.Date("2017-11-20") & invdate <= as.Date("2018-04-30")),1,0)) %>%
  filter(fobt_flag  == 1 | fit_flag == 1)

#Create variables required.

