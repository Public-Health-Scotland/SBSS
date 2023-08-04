#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 10_optins_uptake.R
# Eibhlin O'Sullivan
# Sep 2022
# Script 10 of 13
# Data preparation for export
# Written/run on R Studio Server
# R version 3.6.1
# This script creates the 2-year percentage and demography tables for opt-ins 
# uptake report
# This script is adapted from 2_KPIs_HB_sex v2.R taken from this folder
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
library(phsverse)
library(glue)
library(openxlsx)
library(tidylog)

## Set filepaths and extract dates with script 0
#rm(list = ls())
source(here::here("Code","00_housekeeping.R"))
# wd <- paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening",
#              "/Publications/SBoSP-Statistics/20230221")
# source(paste0(wd, "/Code/00_housekeeping.R"))
## Define functions

# Calculate % fraction KPIs by health board and sex 

KPI_fraction <- function(num, den, kpi_no) {
  # By sex
  KPI_sex <- analysis_db %>%
    group_by(sex, hbr19) %>%
    summarise(p = sum(!!sym(num)) / sum(!!sym(den)) * 100) %>%
    ungroup()
  
  # For all sexes
  KPI_all <- analysis_db %>%
    group_by(hbr19) %>%
    summarise(p = sum(!!sym(num)) / sum(!!sym(den)) * 100) %>%
    mutate(sex = 3) %>%
    ungroup()
  
  # Scotland sexes
  KPI_Scot_sex <- analysis_db %>%
    group_by(sex) %>%
    summarise(p = sum(!!sym(num)) / sum(!!sym(den)) * 100) %>%
    mutate(hbr19 = 15) %>%
    ungroup()
  
  # Scotland total
  KPI_Scot_all <- analysis_db %>%
    group_by() %>%
    summarise(p = sum(!!sym(num)) / sum(!!sym(den)) * 100) %>%
    mutate(hbr19 = 15, sex = 3) %>%
    ungroup()
  
  # Combine all of the above
  
  KPI_all <- bind_rows(KPI_sex, KPI_all, KPI_Scot_sex, KPI_Scot_all) %>%
    mutate(KPI = kpi_no) %>%
    arrange(hbr19, sex) %>%
    pivot_wider(names_from = hbr19, values_from = p)
  
}


# Proportion function

KPI_proportion <- function(filter_column, filter_value, by, count_var, kpi_no) {
  # By sex
  KPI_sex <- analysis_db %>%
    filter(!!sym(filter_column) != filter_value) %>%
    group_by(sex, hbr19,!!sym(by)) %>%
    summarise(n = sum(!!sym(count_var))) %>%
    mutate(p = n / sum(n) ) %>%
    ungroup()
  
  # For all sexes
  KPI_all <- analysis_db %>%
    filter(!!sym(filter_column) != filter_value) %>%
    group_by(hbr19,!!sym(by)) %>%
    summarise(n = sum(!!sym(count_var)))  %>%
    mutate(sex = 3,
           p = n / sum(n) ) %>%
    ungroup()
  
  # Scotland sexes
  KPI_Scot_sex <- analysis_db %>%
    filter(!!sym(filter_column) != filter_value) %>%
    group_by(sex,!!sym(by)) %>%
    summarise(n = sum(!!sym(count_var))) %>%
    mutate(hbr19 = 15,
           p = n / sum(n) ) %>%
    ungroup()
  
  # Scotland total
  KPI_Scot_all <- analysis_db %>%
    filter(!!sym(filter_column) != filter_value) %>%
    group_by(!!sym(by)) %>%
    summarise(n = sum(!!sym(count_var)) ) %>%
    mutate(hbr19 = 15, 
           sex = 3,
           p = n / sum(n) ) %>%
    ungroup()
  
  # Combine all of the above
  KPI <- bind_rows(KPI_sex, KPI_all, KPI_Scot_sex, KPI_Scot_all) %>%
    group_by(!!sym(by), sex, hbr19) %>%
    mutate(KPI = kpi_no,
           p = p * 100) %>%
    select(-n) %>%
    #pivot_wider(names_from = hbr19, values_from = p) # not sure why this doesn't work
    #                                                 # perhaps needs to id_cols specified
    spread(hbr19, p) %>%
    arrange(!!sym(by)) %>%
    ungroup()
}

# Define function for creating KPI output

create_kpi_output <- function(KPI_n){
  
  KPI_data_out %>% 
    filter(KPI == KPI_n) %>% 
    select(-c(KPI, simd2020))
  
}

# Define todays date for output

todays_date <- strftime(lubridate::today(), "%Y-%m-%d")



### Step 1: Prepare dataset ----

# Bring in analysis database from script 1_2_5
analysis_db <- read_rds(analysis_db_path) %>%
  filter(optin == 1 &
           hbr19 %in% c(1:14) &
           between(invdate, as.Date(date_first), as.Date(date_last))
  )
dim(analysis_db)

# Number of opt-ins based on above extract
#  7,355 - May 19 upload
#  7,636 - Nov 20 upload
#  6,541 - May 21 upload
#  7,477 - Nov 21 upload
#  8,518 - May 22 upload
#  9,781 - Nov 22 upload
# 11,933 - May 23 upload

# Check overview of invitation dates
analysis_db %>%
  ggplot(aes(x = invdate)) +
  geom_histogram(binwidth = 1) +
  xlab("Invitation date") + ylab("Number of invitations per day") +
  scale_x_date(date_breaks = "months", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Screening results per day looks sensible
# Big jumps in July 2022 and October 2022

# Check all records are optins

analysis_db %>% count(optin)



### Step 2: Calculate KPIs ----

# 2.1 Calculate percentage KPIs using KPI_fraction function ----

KPI_1  <- KPI_fraction("uptake_n",        "invite_n",    1)
KPI_3  <- KPI_fraction("positive_n",      "uptake_n",    3)
KPI_5  <- KPI_fraction("col_perf_n",      "positive_n",  5)
KPI_6  <- KPI_fraction("col_complete_n",  "col_perf_n",  6)
KPI_7  <- KPI_fraction("col_complic_n",   "col_perf_n",  7)
KPI_8  <- KPI_fraction("cancer_n",        "uptake_n",    8)
KPI_19 <- KPI_fraction("adenoma_n",       "uptake_n",   19)
KPI_21 <- KPI_fraction("canc_col_n",      "col_perf_n", 21)
KPI_22 <- KPI_fraction("adenoma_col_n",   "col_perf_n", 22)
KPI_23 <- KPI_fraction("hr_adenoma_col_n","col_perf_n", 23)

## 2.2 Calculate number of cancers recorded ----

cancers <- analysis_db %>%
  summarise(col_perf_n = sum(col_perf_n),
            canc_col_n = sum(canc_col_n)
  )
cancers


## 2.3 Calculate KPI 2 using bespoke code ----

# KPI 2 is uptake by SIMD 2020, 
# doesn't fit into KPI_fraction function

# By sex
KPI_sex <- analysis_db %>%
  group_by(sex, hbr19, simd2020) %>%
  summarise(p = sum(uptake_n) / sum(invite_n) * 100) %>%
  ungroup()

# For all sexes
KPI_all <- analysis_db %>%
  group_by(hbr19, simd2020) %>%
  summarise(p = sum(uptake_n) / sum(invite_n) * 100) %>%
  mutate(sex = 3) %>%
  ungroup()

# Scotland sexes
KPI_Scot_sex <- analysis_db %>%
  group_by(sex, simd2020) %>%
  summarise(p = sum(uptake_n) / sum(invite_n) * 100) %>%
  mutate(hbr19 = 15) %>%
  ungroup()

# Scotland total
KPI_Scot_all <- analysis_db %>%
  group_by(simd2020) %>%
  summarise(p = sum(uptake_n) / sum(invite_n) * 100) %>%
  mutate(hbr19 = 15, sex = 3) %>%
  ungroup()

# Combine all of the above
KPI_2 <- bind_rows(KPI_sex, KPI_all, KPI_Scot_sex, KPI_Scot_all) %>%
  mutate(KPI = 2) %>%
  arrange(hbr19, sex) %>%
  pivot_wider(names_from = hbr19, values_from = p) %>%
  filter(!is.na(simd2020)) %>%
  arrange(sex, desc(simd2020))



### 2.4 Combine KPIs ----

# Pull together basic, 2-year KPI data

KPI_data <- bind_rows(KPI_1,
                      KPI_2,
                      KPI_3,
                      KPI_5,
                      KPI_6,
                      KPI_7,
                      KPI_8,
                      KPI_19,
                      KPI_21,
                      KPI_22,
                      KPI_23
)

View(KPI_data)
KPI_data




skeleton <- read_sav(here::here("Temp", "skeleton1_(v02)_data_SUMTABS.sav")) %>%
  clean_names() %>%
  filter(kpi %in% c(1:3, 5:8, 19, 21:23) &
           index1 == 3) %>%
  mutate(simd2020 = simd2016) %>%
  select(kpi, index1, sex, simd2020)

KPI_data_full <- left_join(skeleton, KPI_data, 
                           by = c("kpi" = "KPI", 
                                  "sex", 
                                  "simd2020" = "simd2020"))

library(labelled)

KPI_data_full <- KPI_data_full %>%
  select(kpi, index1, sex, simd2020, '1':'15') %>%
  arrange(kpi, sex, desc(simd2020)) %>%
  mutate(index1 = to_character(index1),
         sex = to_character(sex),
         simd2020 = to_character(simd2020))

View(KPI_data_full)
dim(KPI_data_full)
# 45 rows



### 2.5 Demographics ----

# Demography information for appendices
## GC - label HB

sex_dem <- analysis_db %>%
  select(hbr19, sex, invite_n) %>%
  group_by(hbr19, sex) %>%
  summarise(invite_n = sum(invite_n)) %>%
  pivot_wider(names_from = sex, values_from = invite_n) %>%
  ungroup()

age_dem <- analysis_db %>%
  select(hbr19, age_group, invite_n) %>%
  group_by(hbr19, age_group) %>%
  summarise(invite_n = sum(invite_n)) %>%
  pivot_wider(names_from = age_group, values_from = invite_n) %>%
  ungroup()

## TO DO - need to figure out how we will order and output by health board, may
## be for the excel output script(s) or may be for here

simd_dem <- analysis_db %>%
  select(hbr19, simd2020, invite_n) %>%
  filter(!is.na(simd2020)) %>%
  group_by(hbr19, simd2020) %>%
  summarise(invite_n = sum(invite_n)) %>%
  pivot_wider(names_from = simd2020, values_from = invite_n) %>%
  ungroup()



### Step 3: Save files ----

# Unsure of best way to save these files, as current approach requires
# combination of:
# - server R (because file is large)
# - stats network (to load/save PII files)
# - local drive (to upload to github)


# MT 6-1-21 - relative filepaths incorrect, absolute paths used - fix needed
# KH 3/9/21 - switched file paths as absolute not working

write_rds(KPI_data_full, here::here("Temp", "opt_ins_data.rds"), 
          compress = 'gz')

# Data_check
KPI_data_full <- read_rds(here::here('Temp', 'opt_ins_data.rds')) %>%
  clean_names()



### Step 4: Create Individual Report Tables for Excel Output ----

# Create KPI table outputs

# Update headers on KPI sheet output

KPI_data_out <-  KPI_data %>% 
  rename('Ayrshire and Arran' = '1',
         'Borders' = '2',
         'Dumfries and Galloway' = '3',
         'Fife' = '4',
         'Forth Valley' = '5',
         'Grampian' = '6',
         'Greater Glasgow and Clyde' = '7',
         'Highland' = '8',
         'Lanarkshire' = '9',
         'Lothian' = '10',
         'Orkney' = '11',
         'Shetland' = '12',
         'Tayside' = '13',
         'Western Isles' = '14',
         'Scotland' = '15') %>% 
  mutate(sex = case_when(sex == 1 ~ "Males",
                         sex == 2 ~ "Females",
                         sex == 3 ~ "All persons")) %>% 
  select(1:18)



# Create individual KPI outputs

KPI_1_out <- create_kpi_output(1)

KPI_2_out <- KPI_data_out %>% 
  filter(KPI == 2) %>% 
  select(-KPI) %>% 
  relocate(simd2020, .after = "sex")

KPI_3_out <- create_kpi_output(3)

KPI_5_out <- create_kpi_output(5)

KPI_6_out <- create_kpi_output(6)

KPI_7_out <- create_kpi_output(7)

KPI_8_out <- create_kpi_output(8)

KPI_19_out <- create_kpi_output(19)

KPI_21_out <- create_kpi_output(21)

KPI_22_out <- create_kpi_output(22)

KPI_23_out <- create_kpi_output(23)


# Create table for sex demographics

# Rename sex columns and add total
# Convert table to report orientation
# Rename hbr19 columns
# Total Scotland figures

sex_dem_opt_in_out <- sex_dem %>% 
  rename('Males' = '1',
         'Females' = '2') %>% 
  mutate(`All Persons` = Males + Females) %>%
  pivot_longer(cols = c('Males','Females','All Persons'),
               names_to = 'sex',
               values_to = 'pct') %>% 
  pivot_wider(names_from = hbr19, values_from = pct) %>%
  rename('Ayrshire and Arran' = '1',
         'Borders' = '2',
         'Dumfries and Galloway' = '3',
         'Fife' = '4',
         'Forth Valley' = '5',
         'Grampian' = '6',
         'Greater Glasgow and Clyde' = '7',
         'Highland' = '8',
         'Lanarkshire' = '9',
         'Lothian' = '10',
         'Orkney' = '11',
         'Shetland' = '12',
         'Tayside' = '13',
         'Western Isles' = '14') %>%
  mutate(Scotland = select(., `Ayrshire and Arran`:`Western Isles`) %>% 
           rowSums(na.rm = TRUE))



### Step 5 - Write Data to Excel ----

# Create workbook

wb <- createWorkbook()

# Define a header style for workbook

# hs <- createStyle(fontColour = "#ffffff", fgFill = "#0078D4",
#                   halign = "center", valign = "center", 
#                   textDecoration = "bold", border = "TopBottomLeftRight")

addWorksheet(wb, sheetName = "Opt_In_Numbers")

writeData(wb, paste0("Opt-In Numbers"), sheet = "Opt_In_Numbers", startRow = 1, 
          startCol = 1)
writeData(wb, sex_dem_opt_in_out, sheet = "Opt_In_Numbers", startRow = 3, 
          startCol = 1)

addWorksheet(wb, sheetName = "KPI_tables")

# KPI 1
writeData(wb, paste0("KPI 1"), sheet = "KPI_tables", startRow = 1, startCol = 1)
writeData(wb, KPI_1_out, sheet = "KPI_tables", startRow = 3, startCol = 1)

# KPI 2
writeData(wb, paste0("KPI 2"), sheet = "KPI_tables", startRow = 8, startCol = 1)
writeData(wb, KPI_2_out, sheet = "KPI_tables", startRow = 10, startCol = 1)

# KPI 3
writeData(wb, paste0("KPI 3"), sheet = "KPI_tables", startRow = 28, 
          startCol = 1)
writeData(wb, KPI_3_out, sheet = "KPI_tables", startRow = 30, startCol = 1)

# KPI 5
writeData(wb, paste0("KPI 5"), sheet = "KPI_tables", startRow = 35, 
          startCol = 1)
writeData(wb, KPI_5_out, sheet = "KPI_tables", startRow = 37, startCol = 1)

# KPI 6
writeData(wb, paste0("KPI 6"), sheet = "KPI_tables", startRow = 42, 
          startCol = 1)
writeData(wb, KPI_6_out, sheet = "KPI_tables", startRow = 44, startCol = 1)

# KPI 7
writeData(wb, paste0("KPI 7"), sheet = "KPI_tables", startRow = 49, 
          startCol = 1)
writeData(wb, KPI_7_out, sheet = "KPI_tables", startRow = 51, startCol = 1)

# KPI 8
writeData(wb, paste0("KPI 8"), sheet = "KPI_tables", startRow = 56, 
          startCol = 1)
writeData(wb, KPI_8_out, sheet = "KPI_tables", startRow = 58, startCol = 1)

# KPI 19
writeData(wb, paste0("KPI 19"), sheet = "KPI_tables", startRow = 63, 
          startCol = 1)
writeData(wb, KPI_19_out, sheet = "KPI_tables", startRow = 65, startCol = 1)

# KPI 21
writeData(wb, paste0("KPI 21"), sheet = "KPI_tables", startRow = 70, 
          startCol = 1)
writeData(wb, KPI_21_out, sheet = "KPI_tables", startRow = 72, startCol = 1)

# KPI 22
writeData(wb, paste0("KPI 22"), sheet = "KPI_tables", startRow = 77, 
          startCol = 1)
writeData(wb, KPI_22_out, sheet = "KPI_tables", startRow = 79, startCol = 1)

# KPI 23
writeData(wb, paste0("KPI 23"), sheet = "KPI_tables", startRow = 84, 
          startCol = 1)
writeData(wb, KPI_23_out, sheet = "KPI_tables", startRow = 86, startCol = 1)

# Save workbook

saveWorkbook(wb, here("Temp/opt_ins_data_1.xlsx"))
             