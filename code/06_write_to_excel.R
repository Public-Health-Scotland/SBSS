#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 06_write_to_excel.R
# Gavin Clark
# Aug 2022
# Script 6 of 13
# Data extraction/preparation
# Written/run on R Studio Server
# R version 3.2.3
# This script writes the outputs from scripts 2:5 to excel
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Improvements to make:
# TG - change the final formatting to select the vars required,
#      rather than exclude those not required. This will be more robust.


### Step 0: Housekeeping ----

##  Packages
library(readr)
library(haven)
library(here)
library(tidyr)
library(dplyr)
library(XLConnect)
library(tidylog)

## Set filepaths and extract dates with script 0
rm(list = ls())
source(here::here("Code","00_housekeeping.R"))
wd <- paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening",
             "/Publications/SBoSP-Statistics/20230221")
# source(paste0(wd, "/Code/00_housekeeping.R"))

# Define report date

date_yyyymmdd <- stringr::str_extract(here::here(), "([^/]+$)")

report_date <- as_date(date_yyyymmdd)

### Step 1: Write to Excel ----
# GC - need to be careful with this as there is no undo function, it overwrites
# the existing spreadsheet

## GC COMMENT - 27/08/2021
# Once data are written out to the KPI pack spreadsheet, in the event that 
# calculations are not updating in that spreadsheet, 
# press Ctrl + Alt + Shift + F9 to refresh and recalculate everything


## 1.1 Load KPI report worksheet ----
# wb <- loadWorkbook(paste0(here::here("Output/"), report_date, 
#                               "-Bowel-Screening-KPI-Report.xlsx"))
wb <- loadWorkbook(paste0(wd, "/Output/2023-01-23-Bowel-Screening-KPI-Report.xlsx"))
setStyleAction(wb, XLC$"STYLE_ACTION.NONE")


## 1.2 Write in 2-year data from script 2 ----
KPI_data <- readRDS(paste0(wd, "/Temp/KPI_data.rds"))
#KPI_data <- readRDS(here::here("Temp", "KPI_data.rds")) 

names(KPI_data) 
KPI_data <- KPI_data %>%
            select(kpi, index1, sex, simd2020, waiting_time, '1':'15')



writeWorksheet(wb, KPI_data,"data",startRow = 11, startCol = 16, header = FALSE)


## 1.3 Write in Time-series data ----
# ts_data <- readRDS(here::here("Temp", "ts_data_wide.rds"))
#ts_data <- readRDS(here::here("Temp", "ts_data.rds"))
ts_data <- readRDS(paste0(wd, "/Temp/ts_data.rds"))

writeWorksheet(wb, ts_data,"TSData",startRow = 7, startCol = 1, header = FALSE)


## 1.4 Write in Funnel-plot data ----
#funnel_data <- readRDS(here::here("Temp", "Funnel-data_Confidence-limits.rds"))
funnel_data <- readRDS(paste0(wd, "/Temp/Funnel-data_Confidence-limits.rds"))

writeWorksheet(wb, funnel_data,"funnel_Limits&Scot",startRow = 11, startCol = 3, 
               header = FALSE)


## 1.5 Write in Funnel-plot denominator/x-axis data ----
funnel_uptake_data <- as.data.frame(t(select(
  #readRDS(here::here("Temp", "Funnel-data_HB-denominators.rds")),
  readRDS(paste0(wd, "/Temp/Funnel-data_HB-denominators.rds")),
  uptake_n)))

funnel_colperf_data <- as.data.frame(t(select(
  #readRDS(here::here("Temp", "Funnel-data_HB-denominators.rds")),
  readRDS(paste0(wd, "/Temp/Funnel-data_HB-denominators.rds")),
  col_perf_n)))

writeWorksheet(wb, funnel_uptake_data,"funnel_X-axis", startRow = 11, 
               startCol = 3, header = FALSE)

writeWorksheet(wb, funnel_colperf_data,"funnel_X-axis", startRow = 24, 
               startCol = 3, header = FALSE)


## 1.6 Write in SII/RII data ----
#sii_rii_data <- readRDS(here::here("Temp", "rii_sii_data.rds"))
sii_rii_data <- readRDS(paste0(wd, "/Temp/rii_sii_data.rds"))

writeWorksheet(wb, sii_rii_data,"SII_RII_data", startRow = 7, startCol = 1, 
               header = FALSE)


## 1.7 Write in Cancer time_series data ----
#cancer_ts <- readRDS(here::here("Temp", "cancer_ts.rds")) %>%
cancer_ts <- readRDS(paste0(wd, "/Temp/cancer_ts.rds")) %>%
             select(-dukes_der)

writeWorksheet(wb, cancer_ts,"KPI_15",startRow = 62, startCol = 3, 
               header = FALSE)


## 1.8 Write in Sex demography data ----
#sex_dem <- readRDS(here::here("Temp", "sex_dem.rds")) %>%
sex_dem <- readRDS(paste0(wd, "/Temp/sex_dem.rds")) %>%
  select(-hbr19)

writeWorksheet(wb, sex_dem,"A1-Population by sex and HB",
               startRow = 5, startCol = 3, header = FALSE)


## 1.9 Write in Age demography data ----
#age_dem <- readRDS(here::here("Temp", "age_dem.rds")) %>%
age_dem <- readRDS(paste0(wd, "/Temp/age_dem.rds")) %>%
            select(-hbr19)

writeWorksheet(wb, age_dem,"A2-Population by age and HB",
               startRow = 5, startCol = 3, header = FALSE)


## 2.0 Write in SIMD demography data ----
#simd_dem <- readRDS(here::here("Temp", "simd_dem.rds")) #%>%
simd_dem <- readRDS(paste0(wd, "/Temp/simd_dem.rds")) #%>%

# Need to include the health board names as part of this one, quick fix here
## GC TO DO - move to script 2

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

writeWorksheet(wb, simd_dem,"A3-Popn. by deprivation and HB",
               startRow = 5, startCol = 2, header = FALSE)
# Need to update Excel sheet for SIMD so that HBs are in correct
# order
# GC TO DO - automate this



### Step 2: Save all to workbook ----
setForceFormulaRecalculation(wb,"*",TRUE)
# saveWorkbook(wb, paste0(here::here("Output/"), report_date, 
#                     "-Bowel-Screening-KPI-Report.xlsx"))

saveWorkbook(wb, paste0(here::here("Output/"),"2023-01-23-Bowel-Screening-KPI-Report.xlsx"))

# saveWorkbook(wb, paste0(wd, "/Output/", report_date, 
#                         "-Bowel-Screening-KPI-Report.xlsx"))

# Note - check that cells have updated in Excel, press ctrl + shift + alt + F9
# to ensure this, currently need to change some cells from text to numeric too

# GC TO DO - update Nov 2018 etc. in the hidden tabs, may be best for this to be
# manual so that some visual checking is carried out
