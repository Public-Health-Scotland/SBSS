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
install.packages('dplyr')
library(readr)
library(haven)
library(janitor)
library(magrittr)
library(dplyr)
library(tidyr)
library(lubridate)
library(invgamma)

## Define functions


## Set filepaths and import reporting period dates from Script 0
# Define location of combined_extract_all and analsysis_dataset
#source(here::here("code", "0_housekeeping.R"))



### Step 2: Import data
#Keep this code and delete below when 'here' works
#raw_db <- read_sav(sbsdb_path)

############################################################################
## Temp
# currently use direct import since 'here' not working. Replace later.
sbsdb_path  <- read_sav(paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening/Data/",
                               "Programme/2018-11/combined_extract_all.zsav"))


analysis_db <- readRDS(paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening/",
                              "TPP/KPIs/Code + DB/TPP/data/analysis_dataset.rds"))
# Directly define dates but delete later.
date_first <- "2016-05-01"
date_last  <- "2018-04-30"
############################################################################

### STEP 3: Create Screening History file.

# Use Select statement to keep only required variables: chinum screres date_round.
slim_db1 <- clean_names(sbsdb_path) %>% 
  select('chinum', 'screres', 'date_round')
dim(slim_db1)
names(slim_db1)


# Sort cases by chi-number
slim_db1 <- arrange(slim_db1, chinum)

# Spread individual patients screening results onto single rows by screening round
screening_history_byCHI <- slim_db1 %>% group_by(chinum) %>% spread(key = 'date_round',value = 'screres')


### Step 4: Get relevant records for the KPI report.
sbsdb_path1 <- clean_names(sbsdb_path)

slim_db2 <- filter(sbsdb_path1,invdate >= as.Date(date_first) & invdate <= as.Date(date_last)) %>%
  filter(optin == 0) %>%
  filter(hbr14 %in% 1:14)

?as.Date

dim(slim_db2)
head(slim_db2)



