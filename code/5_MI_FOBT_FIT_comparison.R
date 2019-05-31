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
# GC TO DO - check the screening history calculation
install.packages("DescTools")
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
# library("rlang", lib.loc="~/R/x86_64-redhat-linux-gnu-library/3.2")

## Define functions
####################################
#Test
KPI_rate <- function(num, den, kpi_name) {
  
  # KPI for all persons
  KPI_all <- test_comp_db %>%
    group_by(test_type) %>%
    summarise(
      regs_n = sum(!!sym(num)),
      regs_d = sum(!!sym(den)),
      p = sum(!!sym(num)) / sum(!!sym(den))) %>%
    ungroup() %>%
    mutate(group_label = kpi_name)
  
  # KPI for uptake history
  KPI_hist <- test_comp_db %>%
    group_by(test_type, uptake_history) %>%
    summarise(
      regs_n = sum(!!sym(num)),
      regs_d = sum(!!sym(den)),
      p = sum(!!sym(num)) / sum(!!sym(den))) %>%
    ungroup() %>%
    mutate(group_label = kpi_name)
  
  # KPI for sex
  KPI_sex <- test_comp_db %>%
    group_by(test_type, sex) %>%
    summarise(
      regs_n = sum(!!sym(num)),
      regs_d = sum(!!sym(den)),
      p = sum(!!sym(num)) / sum(!!sym(den))) %>%
    ungroup() %>%
    mutate(group_label = kpi_name)
  
  # KPI for age
  KPI_age <- test_comp_db %>%
    group_by(test_type, age_group) %>%
    summarise(
      regs_n = sum(!!sym(num)),
      regs_d = sum(!!sym(den)),
      p = sum(!!sym(num)) / sum(!!sym(den))) %>%
    ungroup() %>%
    mutate(group_label = kpi_name)
  
  #By SIMD2016.
  KPI_simd <- test_comp_db %>%
    group_by(test_type, simd2016) %>%
    summarise(
      regs_n = sum(!!sym(num)),
      regs_d = sum(!!sym(den)),
      p = sum(!!sym(num)) / sum(!!sym(den))) %>%
    ungroup() %>%
    filter(!is.na(simd2016)) %>%
    mutate(group_label = kpi_name)
  
  test_comp_KPI <- bind_rows(
    KPI_all,
    KPI_hist,
    KPI_sex,
    KPI_age,
    KPI_simd) %>% 
    select(group_label, test_type, uptake_history, sex,age_group,
           simd2016, regs_n, regs_d, p) %>%
    # Calculate upper and lower 95% confidence intervals
    mutate(
      lower95CI = 
        ((p + qchisq((1-0.05),df=1)/(2*regs_d) - qnorm(1-(0.05/2),mean=0,sd=1)
          *sqrt((p*(1-p)+qchisq((1-0.05),df=1)/(4*regs_d))/regs_d))) / 
        (1+qchisq((1-0.05),df=1)/regs_d),
      upper95CI = ((p + qchisq((1-0.05),df=1)/(2*regs_d) + qnorm(1-(0.05/2),
                                                                 mean=0,sd=1)
                    *sqrt((p*(1-p)+qchisq((1-0.05),df=1)/(4*regs_d))/regs_d))) / 
        (1+qchisq((1-0.05),df=1)/regs_d)   )
}

###########################

## Set filepaths and import reporting period dates from Script 0
# Define location of combined_extract_all and analsysis_dataset
source(here::here("code", "0_housekeeping.R"))

### Step 2: Import data
#Keep this code and delete below when 'here' works
sbsp_analysis_db <- readRDS(analysis_db_path)

### Step 3: Get relevant records for the KPI report.
# Open the analysis database.
# Select test comparisosn dates.
# Ensure: no optins and only valid health boards.
# Select only participating individuals.

sbsp_slimdb <- sbsp_analysis_db %>%
  filter(optin == 0 &
           hbr14 %in% 1:14 &
           screres %in% c(1:18,21,22,24)) %>%
  mutate(
    canc_col_n = cancer_n * col_perf_n,
    adenoma_col_n = adenoma_n * col_perf_n,
    hr_adenoma_col_n = hr_adenoma_n * col_perf_n
  )
dim(sbsp_slimdb)
names(sbsp_slimdb)

#Select report time period and comparison time period
test_comp_db <- sbsp_slimdb %>% 
  mutate(
    fobt_flag = ifelse((invdate >= as.Date("2016-11-20") & 
                          invdate <= as.Date("2017-04-30")),1,0),
    fit_flag  = ifelse((invdate >= as.Date("2017-11-20") & 
                          invdate <= as.Date("2018-04-30")),1,0)) %>%
  filter(fobt_flag  == 1 | fit_flag == 1)

test_comp_db$test_type <- 0
test_comp_db <- test_comp_db %>% 
  mutate(test_type = ifelse((fobt_flag == 1),1,ifelse(fit_flag == 1,2,0))) %>%
  # GC TO DO - Need to think about whether it is still appropriate to exclude 
  # those with no simd
  filter(simd2016 %in% 1:5)

#################################################
#Calculate test comparison for each KPI 

#Uptake all
test_comp_uptake <- KPI_rate('uptake_n','invite_n','Uptake')
#Positivity
test_comp_positivity <- KPI_rate('positive_n','uptake_n','Positivity')

# Cancer PPV
# GC have run with cancer_n as a check and it matches what was done previously, 
# however canc_col_n should be used as this is only cases of cancer where a 
# colonoscopy has been performed
test_comp_cancer_ppv <- KPI_rate('canc_col_n','col_perf_n','Cancer PPV')

#Adenoma PPV - same comment applies as with cancer
test_comp_adenoma_ppv <- KPI_rate('adenoma_n','col_perf_n','Adenoma PPV')

###################################################
# Calculate stats by haemoglobin concentration

hbg <- test_comp_db %>%
  mutate(hbg20 =
           # Want to round down to nearest 20 for purposes of test threshold
           # min part isn't working for some reason, though the parts work
           # separately
           min(
             floor(haemoglobin/20) * 20,
             200, na.rm = FALSE
           )
  )



