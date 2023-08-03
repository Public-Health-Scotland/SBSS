#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tayside_complications.R
# Gavin Clark
# Feb 2022
# Data preparation for export
# Written/run on R Studio Server
# R version 3.6.1
# This script pulls out the CHIs of those with a complication arising
# from colonoscopy for Tayside to check
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### Step 0: Housekeeping ----

library(readr)
library(dplyr)
library(tidyr)
library(readxl)
library(here)
library(haven)
library(ggplot2)
library(janitor)
library(tidylog)

## Set filepaths and extract dates with script 0
#rm(list = ls())
source(here::here("Code","00_housekeeping.R"))
# wd <- paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening",
#              "/Publications/SBoSP-Statistics/20230221")
# source(paste0(wd, "/Code/00_housekeeping.R"))



### Step 1: Analysis ----

# Bring in analysis database from script 1_2_5
analysis_db <- read_rds(analysis_db_path) %>%
  filter(optin == 0 &
           hbr19 %in% c(1:14) &
           between(invdate, as.Date(date_first), as.Date(date_last))
  )

dim(analysis_db)

# Filter for Tayside data where col_complic_n is 1

tayside_comp <- analysis_db %>% 
  filter(hbr19 == 13, col_complic_n == 1) %>%
  select(chinum, complicp)

# Save output

write_csv(tayside_comp, "Temp/Tayside_complications.csv")



