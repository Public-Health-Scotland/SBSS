#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 06_write_to_excel.R
# Calum Purdie
# Aug 2023
# Script 6 of 16
# Written/run on Posit Workbench
# R version 4.1.2
# This script writes the outputs from scripts 2:5 to excel
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### 1 Housekeeping ----

# Load packages
library(readr)
library(haven)
library(here)
library(tidyr)
library(dplyr)
library(openxlsx2) # note this is openxlsx2 not openxlsx
library(lubridate)
library(tidylog)

## Set filepaths and extract dates with script 0
rm(list = ls())
source(here::here("Code","00_housekeeping.R"))
wd <- paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening",
             "/Publications/SBoSP-Statistics/20230804")
# source(paste0(wd, "/Code/00_housekeeping.R"))

# Define report date

date_yyyymmdd <- stringr::str_extract(here::here(), "([^/]+$)")

report_date <- as_date(date_yyyymmdd)
cover_month <- gsub("_", " ", report_month)



### 2 Write to Excel ----

# GC COMMENT - 27/08/2021
# Once data are written out to the KPI pack spreadsheet, in the event that 
# calculations are not updating in that spreadsheet, 
# press Ctrl + Alt + Shift + F9 to refresh and recalculate everything

# Load KPI report worksheet
# This is a template file that CP created and saved in the Temp folder
# Can update this for each publication going forward

template <- wb_load(paste0(here("Temp/Bowel-Screening-KPI-Report-Template.xlsx")))

# Read in KPI data

KPI_data <- readRDS(here("Temp/KPI_data.rds"))

# Check column names

names(KPI_data) 

# Select columns and set any NAs to 0 for numeric columns

KPI_data <- KPI_data %>%
  select(kpi, index1, sex, simd2020, waiting_time, '1':'15') |> 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))

# Add data into workbook

write_data(template, "data", KPI_data, startRow = 11, startCol = 16, 
           colNames = FALSE, na.strings = "")

# Write in time-series data

ts_data <- readRDS(paste0(here("Temp/ts_data.rds"))) |> 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))

write_data(template, "TSData", ts_data, startRow = 7, startCol = 1, 
           colNames = FALSE, na.strings = "")


# Write in funnel-plot data

funnel_data <- readRDS(paste0(here("Temp/Funnel-data_Confidence-limits.rds"))) |> 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))

write_data(template, "funnel_Limits&Scot", funnel_data, startRow = 11, 
           startCol = 3, colNames = FALSE, na.strings = "")


# Write in funnel-plot denominator/x-axis data

funnel_uptake_data <- as.data.frame(t(select(
  readRDS(paste0(here("Temp/Funnel-data_HB-denominators.rds"))), uptake_n))) |> 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))

funnel_colperf_data <- as.data.frame(t(select(
  readRDS(paste0(here("Temp/Funnel-data_HB-denominators.rds"))), 
  col_perf_n))) |> 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))

write_data(template, "funnel_X-axis", funnel_uptake_data, startRow = 11, 
           startCol = 3, colNames = FALSE, na.strings = "")

write_data(template, "funnel_X-axis", funnel_colperf_data, startRow = 24, 
           startCol = 3, colNames = FALSE, na.strings = "")


# Write in SII/RII data

sii_rii_data <- readRDS(paste0(here("Temp/rii_sii_data.rds"))) |> 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))

write_data(template, "SII_RII_data", sii_rii_data, startRow = 7, startCol = 1, 
           colNames = FALSE, na.strings = "")


# Write in cancer time_series data

cancer_ts <- readRDS(paste0(here("Temp/cancer_ts.rds"))) %>%
  select(-dukes_der) |> 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))

write_data(template, "KPI_15", cancer_ts, startRow = 62, startCol = 3, 
           colNames = FALSE, na.strings = "")


# Write in sex demography data

sex_dem <- readRDS(paste0(here("Temp/sex_dem.rds"))) %>%
  select(-hbr19) |> 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))

write_data(template, "A1-Population by sex and HB", sex_dem, startRow = 5, 
           startCol = 3, colNames = FALSE, na.strings = "")


# Write in age demography data

age_dem <- readRDS(paste0(wd, "/Temp/age_dem.rds")) %>%
  select(-hbr19) |> 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))

write_data(template, "A2-Population by age and HB", age_dem, startRow = 5, 
           startCol = 3, colNames = FALSE, na.strings = "")


# Write in SIMD demography data

simd_dem <- readRDS(paste0(here("Temp/simd_dem.rds"))) |> 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))

# Need to include the health board names as part of this one, quick fix here

hblist <- data.frame(hbr19 = c(1:15), 
                     hb = c('Ayrshire and Arran',
                            'Borders',
                            'Dumfries and Galloway',
                            'Fife',
                            'Forth Valley',
                            'Grampian',
                            'Greater Glasgow and Clyde',
                            'Highland',
                            'Lanarkshire',
                            'Lothian',
                            'Orkney',
                            'Shetland',
                            'Tayside',
                            'Western Isles',
                            'Scotland'))

simd_dem <- left_join(hblist, simd_dem, by = "hbr19") %>%
  select(-hbr19)

write_data(template, "A3-Popn. by deprivation and HB", simd_dem, startRow = 5, 
           startCol = 2, colNames = FALSE, na.strings = "")


# Need to update Excel sheet for SIMD so that HBs are in correct
# order
# GC TO DO - automate this


# Write in Figure 19

f19 <- readRDS(paste0(here("Temp/figure_19.rds"))) %>%
  select(-c(sex, icd, KPI)) |> 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))

write_data(template, "figure19", f19, startRow = 9, 
           startCol = 3, colNames = FALSE, na.strings = "")


### 3 Save All to Workbook ----

wb_save(template, paste0(here::here("Output/"), report_date, 
                         "-Bowel-Screening-KPI-Report.xlsx"), 
        overwrite = T)

# Note - when you open this excel file, a popup box will appear saying 
# "We found a problem with some content in ..."
# Just click yes and the file should open correctly with no issues
# Manually resave the file once opened correctly
# A further popup box about repairs might appear once you've opened the 
# spreadsheet - I think you can safely ignore this

# Next steps - further automate this to update all dates within spreadsheet

# Note - check that cells have updated in Excel, press ctrl + shift + alt + F9
# to ensure this, currently need to change some cells from text to numeric too
