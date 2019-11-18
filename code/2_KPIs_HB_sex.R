##########################################################
# 2_KPIs_HB_sex.R
# Gavin Clark
# 28/06/2019
# Script 2 of ?
# Data preparation for export
# Written/run on R Studio Server
# R version 3.2.3
# This script creates the 2-year percentage and demography tables for KPI report
# Transcribed from scripts at:
# \\stats\CancerGroup1\Topics\BowelScreening\Publications\SBoSP-Statistics\
# 20190205\Syntax\1_KPIs_N18.sps for percentage tables and 
# \\stats\CancerGroup1\Topics\BowelScreening\Publications\SBoSP-Statistics\
# 20190205\Syntax\5_Trend_uptake_N18 WIP_unsmoothed_FOBT-FIT_comparison.sps
##########################################################

### Step 1 - housekeeping
library(dplyr)
library(tidyr)
library(readxl)
library(here)
library(haven)
library(janitor)
library(tidylog)

#   set filepaths and extract dates with script 0
source(here::here("code", "0_housekeeping.R"))

# Define functions
# Calculate ratio KPIs by health board and sex

KPI_fraction <- function(num, den, kpi_no) {
  # By sex
  KPI_sex <- analysis_db %>%
    group_by(sex, hbr14) %>%
    summarise(p = sum(!!sym(num)) / sum(!!sym(den)) * 100) %>%
    ungroup()
  
  
  # For all sexes
  KPI_all <- analysis_db %>%
    group_by(hbr14) %>%
    summarise(p = sum(!!sym(num)) / sum(!!sym(den)) * 100) %>%
    mutate(sex = 3) %>%
    ungroup()
  
  # Scotland sexes
  KPI_Scot_sex <- analysis_db %>%
    group_by(sex) %>%
    summarise(p = sum(!!sym(num)) / sum(!!sym(den)) * 100) %>%
    mutate(hbr14 = 15) %>%
    ungroup()
  
  # Scotland total
  KPI_Scot_all <- analysis_db %>%
    group_by() %>%
    summarise(p = sum(!!sym(num)) / sum(!!sym(den)) * 100) %>%
    mutate(hbr14 = 15, sex = 3) %>%
    ungroup()
  
  # Combine all of the above
  KPI_all <-
    bind_rows(KPI_sex, KPI_all, KPI_Scot_sex, KPI_Scot_all) %>%
    mutate(KPI = kpi_no) %>%
    arrange(hbr14, sex) %>%
    spread(hbr14, p)
  
}


# Proportion function

KPI_proportion <-
  function(filter_column, filter_value, by, count_var, kpi_no) {
    # By sex
    KPI_sex <- analysis_db %>%
      filter(!!sym(filter_column) != filter_value) %>%
      group_by(sex, hbr14,!!sym(by)) %>%
      summarise(n = sum(!!sym(count_var))) %>%
      mutate(p = n / sum(n) ) %>%
      ungroup()
    
    # For all sexes
    KPI_all <- analysis_db %>%
      filter(!!sym(filter_column) != filter_value) %>%
      group_by(hbr14,!!sym(by)) %>%
      summarise(n = sum(!!sym(count_var)))  %>%
      mutate(sex = 3,
             p = n / sum(n) ) %>%
      ungroup()
    
    # Scotland sexes
    KPI_Scot_sex <- analysis_db %>%
      filter(!!sym(filter_column) != filter_value) %>%
      group_by(sex,!!sym(by)) %>%
      summarise(n = sum(!!sym(count_var))) %>%
      mutate(hbr14 = 15,
             p = n / sum(n) ) %>%
      ungroup()
    
    # Scotland total
    KPI_Scot_all <- analysis_db %>%
      filter(!!sym(filter_column) != filter_value) %>%
      group_by(!!sym(by)) %>%
      summarise(n = sum(!!sym(count_var)) ) %>%
      mutate(hbr14 = 15, 
             sex = 3,
             p = n / sum(n) ) %>%
      ungroup()
    
    # Combine all of the above
    KPI <- bind_rows(KPI_sex, KPI_all, KPI_Scot_sex, KPI_Scot_all) %>%
      group_by(!!sym(by), sex, hbr14) %>%
      mutate(KPI = kpi_no,
             p = p * 100) %>%
      select(-n) %>%
      spread(hbr14, p) %>% 
      arrange(!!sym(by)) %>%
      ungroup()
  }

### Step 2 - dataset preparation

# Bring in analysis database from script 1
analysis_db <-
  readRDS(analysis_db_path)

# Filter on dates
analysis_db <- filter(analysis_db,
                      between(invdate,
                              as.Date(date_first),
                              as.Date(date_last)) &
                        optin == 0 &
                        hbr14 %in% 1:14)
# 1,844,815 - same as SPSS - Nov18 upload
# 1,866,332 - May19 upload 


# Next step in SPSS is flexi-sig removal, this has been done in R script 1

### Step 3 -  Summarise variables into appropriate KPIs

# Arguments to KPI_fraction:
KPI_1 <- KPI_fraction("uptake_n", "invite_n", 1)
KPI_3 <- KPI_fraction("positive_n", "uptake_n", 3)
KPI_5 <- KPI_fraction("col_perf_n", "positive_n", 5)
KPI_6 <- KPI_fraction("col_complete_n", "col_perf_n", 6)
KPI_7 <- KPI_fraction("col_complic_n", "col_perf_n", 7)
KPI_8 <- KPI_fraction("cancer_n", "uptake_n", 8)
KPI_17 <- KPI_fraction("polyp_cancer_n", "uptake_n", 17)
KPI_18 <- KPI_fraction("polyp_cancer_n", "cancer_n", 18)
KPI_19 <- KPI_fraction("adenoma_n", "uptake_n", 19)
KPI_20 <- KPI_fraction("hr_adenoma_n", "uptake_n", 20)
KPI_21 <- KPI_fraction("canc_col_n", "col_perf_n", 21)
KPI_22 <- KPI_fraction("adenoma_col_n", "col_perf_n", 22)
KPI_23 <- KPI_fraction("hr_adenoma_col_n", "col_perf_n", 23)
KPI_24 <- KPI_fraction("canc_hr_n", "col_perf_n", 24)
KPI_25 <- KPI_fraction("all_neoplasia_n", "col_perf_n", 25)

# KPI 2 is uptake by SIMD 2016, doesn't fit into KPI_fraction function

# By sex
KPI_sex <- analysis_db %>%
  group_by(sex, hbr14, simd2016) %>%
  summarise(p = sum(uptake_n) / sum(invite_n) * 100) %>%
  ungroup()

# For all sexes
KPI_all <- analysis_db %>%
  group_by(hbr14, simd2016) %>%
  summarise(p = sum(uptake_n) / sum(invite_n) * 100) %>%
  mutate(sex = 3) %>%
  ungroup()

# Scotland sexes
KPI_Scot_sex <- analysis_db %>%
  group_by(sex, simd2016) %>%
  summarise(p = sum(uptake_n) / sum(invite_n) * 100) %>%
  mutate(hbr14 = 15) %>%
  ungroup()

# Scotland total
KPI_Scot_all <- analysis_db %>%
  group_by(simd2016) %>%
  summarise(p = sum(uptake_n) / sum(invite_n) * 100) %>%
  mutate(hbr14 = 15, sex = 3) %>%
  ungroup()

# Combine all of the above
KPI_2 <- bind_rows(KPI_sex, KPI_all, KPI_Scot_sex, KPI_Scot_all) %>%
  mutate(KPI = 2) %>%
  arrange(hbr14, sex) %>%
  spread(hbr14, p) %>%
  filter(!is.na(simd2016)) %>%
  arrange(sex, desc(simd2016))

# Arguments to KPI_proportion
## GC - dropping KPI 16 as it is just 1 minus KPI 15
KPI_4 <-
  KPI_proportion("waiting_time",
                 "No colonoscopy",
                 "waiting_time",
                 "col_perf_n",
                 4) %>% arrange(sex)
KPI_9_15 <- KPI_proportion("dukes_der", "", "dukes_der", "cancer_n", 9) %>%
  mutate(KPI = c(9,9,9,10,10,10,11,11,11,12,12,12,13,13,13,14,14,14))
# GC TO DO, automate this all so that it can be merged with skeleton/
# have the correct number of rows
KPI_26_28 <- KPI_proportion("icd", "", "icd", "cancer_n", 26) %>%
  filter(icd %in% c("C18","C19","C20")) %>%
  mutate( KPI = c(26,26,26,27,27,27,28,28,28))

# Pull together basic, 2-year KPI data
KPI_data <- bind_rows(
  KPI_1,
  KPI_2,
  KPI_3,
  KPI_4,
  KPI_5,
  KPI_6,
  KPI_7,
  KPI_8,
  KPI_9_15,
  KPI_17,
  KPI_18,
  KPI_19,
  KPI_20,
  KPI_21,
  KPI_22,
  KPI_23,
  KPI_24,
  KPI_25,
  KPI_26_28
)

skeleton <- read_sav(here("Temp", "skeleton1_(v02)_data_SUMTABS.sav")) %>%
  clean_names() %>%
  filter(kpi %in% c(1:28), !kpi %in% c(15:16), index1 == 3) %>%
  mutate(waiting_time =
           case_when(
             time_ref == 1 ~ "0 to 4 weeks",
             time_ref == 2 ~ "4 to 8 weeks",
             time_ref == 3 ~ ">8 weeks")) %>%
  select(kpi, sex, simd2016, waiting_time)

KPI_data_full <- left_join(skeleton, KPI_data, 
                           by = c("kpi" = "KPI", 
                                  "sex", 
                                  "simd2016" = "simd2016",
                                  "waiting_time"))

# TO DO - Complications with no colonoscopies performed.Add to QA process in SPSS
# TO DO - Script 1 - cancers detected with no colonoscopy performed, 
# add to QA in SPSS
# TO DO - Not checking for Dukes staging currently, find a way to do this vs.
# spss

# Demography information for appendices
## GC - label HB

sex_dem <- analysis_db %>%
  select(hbr14, sex, invite_n) %>%
  group_by(hbr14, sex) %>%
  summarise(invite_n = sum(invite_n)) %>%
  spread(sex, invite_n) %>%
  ungroup()

age_dem <- analysis_db %>%
  select(hbr14, age_group, invite_n) %>%
  group_by(hbr14, age_group) %>%
  summarise(invite_n = sum(invite_n)) %>%
  spread(age_group, invite_n) %>%
  ungroup()

## TO DO - need to figure out how we will order and output by health board, may
## be for the excel output script(s) or may be for here
simd_dem <- analysis_db %>%
  select(hbr14, simd2016, invite_n) %>%
  filter(!is.na(simd2016)) %>%
  group_by(hbr14, simd2016) %>%
  summarise(invite_n = sum(invite_n)) %>%
  spread(simd2016, invite_n) %>%
  ungroup()

# Possible to do for future - number of cancers with no ICD10 included 
# (tends to be zero)

### Step 4 - save files
# Unsure of best way to save these files, as current approach requires
# combination of:
# - server R (because file is large)
# - stats network (to load/save PII files)
# - local drive (to upload to github)

saveRDS(KPI_data, file = here("Temp", "KPI_data.rds"))

saveRDS(sex_dem, file = here("Temp", "sex_dem.rds"))

saveRDS(age_dem, file = here("Temp", "age_dem.rds"))

saveRDS(simd_dem, file = here("Temp", "simd_dem.rds"))

