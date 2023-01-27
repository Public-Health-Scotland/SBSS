#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 12_cancer_registry_extract.R
# Thomas Godfrey, Gavin Clark and Calum Purdie
# Oct 2022

# Written/run on R Studio Server
# R version 3.6.1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## IR information
# Screen detected cancers for the registry 
# Customer: Karen Smith, Cancer Data Services.
# Customer request: recurring IR Cancer Registry Update
# e.g. IR2018-00347
#      IR2018-01242
# Colonoscopies performed during latest available period


### Step 0: Housekeeping ----

# Loading packages

library(dplyr)
library(haven)
library(sjlabelled)
library(janitor)
library(stringr)
library(ggplot2)
library(tidyr)
library(readr)
library(openxlsx)
library(here)
library(lubridate)
library(zoo)
library(tidylog)

# Install/open the PHSmethods package
# install.packages("remotes")
# library(remotes)
# remotes::install_github("Public-Health-Scotland/phsmethods", upgrade = "never")
library(phsmethods)

rm(list = ls())

# Define current data extract folder name

extract_folder <- "2022-11"

# Define cancer registry extract folder for last extract

last_scr_folder <- "20220805"
new_scr_folder <- "20230221"

last_scr_month <- "May2022"
new_scr_month <- "Nov2022"

# Define filepaths

pub_folder <- paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening/", 
                     "Publications/SBoSP-Statistics/")

sbs_db_path <- paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening/Data/",
                      "Programme/", extract_folder, 
                      "/combined_extract_all.rds")

# Define working directory as no project for this publication

setwd(paste0(pub_folder, new_scr_folder))

# Extract dates with script 0

source(here::here("Code","00_housekeeping.R"))
wd <- paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening",
             "/Publications/SBoSP-Statistics/20230221")
# source(paste0(wd, "/Code/00_housekeeping.R"))


### Step 1: Get most recent bowel screening file ----

# Read in latest bowel screening file
# Remove SPSS formatting and labels as not required for this work

sbs_db <- read_rds(sbs_db_path) %>% 
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels() %>% 
  arrange(datecolperf)

dim(sbs_db)

#  8,518,163 records, Nov 2018
#  8,988,877 records, May 2019
# 10,908,604 records, Nov 2021
# 11,405,091 records, May 2022
# 11,874,205 records, Nov 2022



### Step 2: Prepare current extract data ----

# Extract screen-detected cancers for the most recent two-year period cancer 
# diagnoses (reduce this to most recent 6 months if required)
# In the SPSS, we used to select the period in question based on date 
# colonoscopy performed, but this can't be right - not everyone who has a 
# cancer has a datecolperf, so we 'lost' (filtered out) relevant cases

cancer_reg_data <- sbs_db %>%
                   filter(between(invdate, as_date(date_first), 
                                  as_date(date_last)))

dim(cancer_reg_data)

# 1,482,751 cases, two-year period, May 2022
# 1,583,195 cases, two-year period, Nov 2022

names(cancer_reg_data)

# Create a variable for invitation month as a yearmon type
# Select relevant columns

cancer_reg_data <- cancer_reg_data %>%
                    mutate(invitation_month = as.yearmon(invdate)) %>%
                    select(invitation_month, chinum, patfname, patsname, dob, 
                           sex, patpcode, hbres, hbident, colperf, datecolperf, 
                           colcomp, barenctc, barectalt, barctdat, cancer, 
                           icd_10, tnm_t, tnm_n, tnm_m, dukes, polypca)

# Count cancer column to see number of people diagnosed

cancer_reg_data %>% 
  group_by(cancer) %>% 
  count()

# Filter to take people diagnosed with cancer and arrange by chinum

cancer_reg_data <- cancer_reg_data %>%
                    filter(cancer %in% "01") %>%
                    arrange(chinum)

# 1,087 cases, two-year period, May 2022
# 1,198 cases, two-year period, Nov 2022

# Just for information

cancer_reg_data %>% 
  group_by(cancer, polypca) %>% 
  count() %>% 
  adorn_totals()

# 306 cancers (64 out of all cancers are polyp cancers) - Nov 2018
# 361 cancers (75 out of all cancers are polyp cancers) - May 2019
# 370 cancers (66 out of all cancers are polyp cancers) - Nov 2019

# Two-year period:
# 1,019 cancers (187 out of all cancers are polyp cancers) - Nov 2021
# 1,087 cancers (213 out of all cancers are polyp cancers) - May 2022
# 1,198 cancers (231 out of all cancers are polyp cancers) - Nov 2022

head(cancer_reg_data)

# Arrange data by invitation_month and chinum

cancer_reg_data <- cancer_reg_data %>%
                   arrange(invitation_month, chinum)

# Remove sbs_db from environment to save space

rm(sbs_db)



### Step 3: Compare with last extract ----

# Read in last cancer registry extract
# Need to add leading zeros to some CHIs which have dropped from this csv
# Also convert sex to characterand invitation_month to yearmon for comparison
# Convert some columns to dates
# Add a leading zero for other columns where required, keeping NAs as NA and 
# storing them as characters

# The new output has been saved as a .xlsx file, so these steps hopefully
# won't be required next time

last_scr_extract <- read_csv(paste0(pub_folder, last_scr_folder, "/Output/", 
                                    "CancerReg_Extract_", last_scr_month, 
                                    ".csv")) %>% 
  mutate(chinum = chi_pad(as.character(chinum)), 
         sex = as.character(sex),
         invitation_month = as.yearmon(invitation_month, "%b-%y")) %>% 
  mutate(across(c(dob, datecolperf, barctdat), ~ dmy(.))) %>% 
  mutate(across(c(colperf, colcomp, barenctc, barectalt, cancer, dukes, 
                  polypca), ~ case_when(is.na(.) ~ NA_character_, 
                                        !is.na(.) ~ str_pad(., 2, "left", "0"))
                )
         ) %>% 
  select(-c(record_type)) # added as it was reading as an extra column for comparison
  

# Take a subset of all cancers in current extract that were not in previous one

diff_from_last_extract <- dplyr::setdiff(cancer_reg_data, last_scr_extract)

# Take a list of all chi numbers in the last extract

last_scr_extract_chi <- last_scr_extract %>% 
  pull(chinum)

# This will be a mix of new cancers and older ones which have changed slightly
# e.g. their dukes code has changed
# Create a flag to identify these as new or updated
# Arrange data by invitation_month and chinum

diff_from_last_extract <- diff_from_last_extract %>% 
  mutate(record_type = case_when(chinum %in% last_scr_extract_chi ~ "updated", 
                                 !(chinum %in% last_scr_extract_chi) ~ "new"
                                 )) %>% 
 arrange(invitation_month, chinum)
 # arrange(chinum)  

# Check all rows have been assigned a record_type value
# Majority should be new records

diff_from_last_extract %>% count(record_type)

# Most updated from Nov 2022 are due to issue with invitation month

# Save output

write.xlsx(diff_from_last_extract, 
          here::here(paste0("Output/", "CancerReg_Extract_", new_scr_month, 
                            ".xlsx")))