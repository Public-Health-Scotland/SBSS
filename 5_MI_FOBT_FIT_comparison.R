##########################################################
# 5_MI_FOBT_FIT_comparison.R
# Thomas Godfrey
# 29/05/2019
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
####################################
#Test
KPI_rate <- function(gp, num, den, kpi_name) {
  
  # KPI for all persons
  test_comp_db %>%
    group_by(test_type, !!sym(gp)) %>%
    summarise(p = sum(!!sym(num)) / sum(!!sym(den)) * 100) %>%
    ungroup() %>%
    mutate(group_label = kpi_name)
}

result_test <- KPI_fraction('test_type','positive_n','uptake_n',as.character('uptake_by_test'))
# it works!!
###########################


## Set filepaths and import reporting period dates from Script 0
# Define location of combined_extract_all and analsysis_dataset
source(here::here("code", "0_housekeeping.R"))

### Step 2: Import data
#Keep this code and delete below when 'here' works
sbsp_analysis_db <- readRDS(analysis_db_path) %>%
  clean_names()
#analysis_db <- readRDS(paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening/",
#                           "TPP/KPIs/Code + DB/TPP/data/analysis_dataset.rds"))


### Step 3: Get relevant records for the KPI report.
# Open the analysis database.
# Select test comparisosn dates.
# Ensure: no optins and only valid health boards.
# Select only participating individuals.

sbsp_slimdb <- sbsp_analysis_db %>%
  filter(invdate >= as.Date(date_first) & invdate <= as.Date(date_last)) %>%
  filter(optin == 0) %>%
  filter(hbr14 %in% 1:14) %>%
  filter(screres %in% c(1:18,21,22,24))
dim(sbsp_slimdb)
names(sbsp_slimdb)


#Select report time period and comparison time period
test_comp_db <- sbsp_slimdb %>% 
  mutate(
    fobt_flag = ifelse((invdate >= as.Date("2016-11-20") & invdate <= as.Date("2017-04-20")),1,0),
    fit_flag  = ifelse((invdate >= as.Date("2017-11-20") & invdate <= as.Date("2018-04-30")),1,0)) %>%
  filter(fobt_flag  == 1 | fit_flag == 1)

test_comp_db$test_type <- 0
test_comp_db <- test_comp_db %>% 
  mutate(test_type = ifelse((fobt_flag == 1),1,ifelse(fit_flag == 1,2,0)))


names(test_comp_db)
dim(test_comp_db)
# 823,203 rows here.
# 849,287 in the SPSS syntax.

#################################################
#Calculate uptake

#Uptake all
uptake_all <- KPI_rate('test_type','uptake_n','invite_n','uptake_overall')
#uptake_all <-  test_comp_db %>%
#              group_by(test_type) %>%
#              summarise(p = sum(uptake_n) / sum(invite_n) * 100) %>%
#              ungroup()

#By ParticipationHistory.
uptake_hist <- KPI_rate('uptake_history','uptake_n','invite_n','uptake_hist')
#uptake_hist <-  test_comp_db %>%
#                group_by(test_type,uptake_history) %>%
#                summarise(p = sum(uptake_n) / sum(invite_n) * 100) %>%
#                ungroup()

#By sex.
uptake_sex <- KPI_rate('sex','uptake_n','invite_n','uptake_sex')
#uptake_sex <-  test_comp_db %>%
#                group_by(test_type,sex) %>%
#                summarise(p = sum(uptake_n) / sum(invite_n) * 100) %>%
#                ungroup()

#By agegroup.
uptake_age <- KPI_rate('age_group','uptake_n','invite_n','uptake_agegroup')
#uptake_age <-  test_comp_db %>%
#                group_by(test_type,age_group) %>%
#                summarise(p = sum(uptake_n) / sum(invite_n) * 100) %>%
#                ungroup()

#By SIMD2016.
uptake_simd <- test_comp_db %>%
  group_by(test_type, simd2016) %>%
  summarise(p = sum(uptake_n) / sum(invite_n) * 100) %>%
  ungroup() %>%
  filter(!is.na(simd2016)) %>%
  mutate(group_label = 'uptake_agegroup')

test_comp_uptake <- bind_rows(
  uptake_all,
  uptake_hist,
  uptake_sex,
  uptake_age,
  uptake_simd) %>% 
  select(group_label, test_type,sex,age_group,
         simd2016,uptake_history,p)

# Save output file
write_excel_csv(test_comp_uptake, 
                path = paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening/TPP/KPIs/Code + DB/TPP/data/test_comp_uptake.csv"))
#saveRDS(test_comp_uptake, file = paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening/",
#                                "TPP/KPIs/Code + DB/TPP/data/test_comp_uptake.rds"))


#################################################

#Calculate Positivity.
#Test script to see if we need to condition on colperf - doesn't look like it.
#test_comp_db <- test_comp_db %>% 
#                  mutate(t_positive_n = ifelse((colperf == '01'),positive_n,0))

#positivity all
positivity_all <- KPI_rate('test_type','positive_n','uptake_n','pos_overall')
#positivity_all <-  test_comp_db %>%
#                    group_by(test_type) %>%
#                    summarise(p = sum(positive_n) / sum(uptake_n) * 100) %>%
#                    ungroup()


#By ParticipationHistory.
positivity_hist <- KPI_rate('uptake_history','positive_n','uptake_n','pos_hist')
#positivity_hist <- test_comp_db %>%
#                    group_by(test_type,uptake_history) %>%
#                    summarise(p = sum(positive_n) / sum(uptake_n) * 100) %>%
#                    ungroup()

#By sex.
positivity_sex <- KPI_rate('sex','positive_n','uptake_n','pos_sex')
#positivity_sex <-  test_comp_db %>%
#                    group_by(test_type,sex) %>%
#                    summarise(p = sum(positive_n) / sum(uptake_n) * 100) %>%
#                    ungroup()

#By agegroup.
positivity_age <- KPI_rate('age_group','positive_n','uptake_n','pos_age')
#positivity_age <-  test_comp_db %>%
#                    group_by(test_type,age_group) %>%
#                    summarise(p = sum(positive_n) / sum(uptake_n) * 100) %>%
#                    ungroup()


#By SIMD2016.
positivity_simd <- test_comp_db %>%
  group_by(test_type, simd2016) %>%
  summarise(p = sum(positive_n) / sum(uptake_n) * 100) %>%
  ungroup() %>%
  filter(!is.na(simd2016)) %>%
  mutate(group_label = 'pos_agegroup')

test_comp_positivity <- bind_rows(
  positivity_all,
  positivity_hist,
  positivity_sex,
  positivity_age,
  positivity_simd) %>%
  select(group_label,test_type,sex,age_group,
         simd2016,uptake_history,p)

# Save output file
write_excel_csv(test_comp_positivity, 
                path = paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening/TPP/KPIs/Code + DB/TPP/data/test_comp_positivity.csv"))
#saveRDS(test_comp_uptake, file = paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening/",
#                                "TPP/KPIs/Code + DB/TPP/data/test_comp_uptake.rds"))


###################################################
# Calculate confidence intervals for PPV stats



