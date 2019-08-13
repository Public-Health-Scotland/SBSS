##########################################################
# 6_write_to_excel.R
# Gavin Clark
# 01/07/2019
# Data extraction/preparation
# Written/run on R Studio Server
# R version 3.2.3
# This script writes the outputs from scripts 2:5 to excel
##########################################################

### 1 - Housekeeping ----

#   loading packages
library(here)
library(tidyr)
library(dplyr)
library(XLConnect)

#   set filepaths and extract dates with script 0
source(here::here("code", "0_housekeeping.R"))

### 2 - Write to excel ----
# GC - need to be careful with this as there is no undo function, it overwrites
# the existing spreadsheet

## KPI report
# 2-year data from script 2
KPI_data <- readRDS(here("Temp", "KPI_data.rds")) %>%
  select(-sex, -KPI, -waiting_time, -simd2016, -icd, -dukes_der)

wb <- loadWorkbook(here("Output", "2019-08-06-Bowel-Screening-KPI-Report.xlsx"))
setStyleAction(wb, XLC$"STYLE_ACTION.NONE")

writeWorksheet(wb, KPI_data,"data",startRow = 11, startCol = 20, header = FALSE)

## Time-series data
ts_data <- readRDS(here("Temp", "ts_data.rds")) 

writeWorksheet(wb, ts_data,"TSData",startRow = 2, startCol = 1, header = FALSE)


## Funnel-plot data
funnel_data <- readRDS(here("Temp", "Funnel-data_Confidence-limits.rds"))

writeWorksheet(wb, funnel_data,"funnel_Limits&Scot",startRow = 11, startCol = 3, 
               header = FALSE)

## funnel denominator data
funnel_uptake_data <- as.data.frame(t(select(
  readRDS(here("Temp", "Funnel-data_HB-denominators.rds")),
  uptake_n
)))

funnel_colperf_data <- as.data.frame(t(select(
  readRDS(here("Temp", "Funnel-data_HB-denominators.rds")),
  col_perf_n
)))

writeWorksheet(wb, funnel_uptake_data,"funnel_X-axis", startRow = 11, startCol = 3, 
               header = FALSE)

writeWorksheet(wb, funnel_colperf_data,"funnel_X-axis", startRow = 24, startCol = 3, 
               header = FALSE)

## SII/RII data
sii_rii_data <- readRDS(here("Temp", "rii_sii_data.rds"))

writeWorksheet(wb, sii_rii_data,"SII_RII_data",startRow = 2, startCol = 1, 
               header = FALSE)

# cancer time_series data
cancer_ts <- readRDS(here("Temp", "cancer_ts.rds")) %>%
  select(-dukes_der)

writeWorksheet(wb, cancer_ts,"KPI_15",startRow = 62, startCol = 3, 
               header = FALSE)

# sex demography
sex_dem <- readRDS(here("Temp", "sex_dem.rds")) %>%
  select(-hbr14)

writeWorksheet(wb, sex_dem,"A1-Population by sex and HB",
               startRow = 5, startCol = 3, header = FALSE) %>%
  select(-hbr14)

# age demography
age_dem <- readRDS(here("Temp", "age_dem.rds")) %>%
  select(-hbr14)

writeWorksheet(wb, age_dem,"A2-Population by age and HB",
               startRow = 5, startCol = 3, header = FALSE)

# simd demography
simd_dem <- readRDS(here("Temp", "simd_dem.rds")) %>%
  mutate(hbr14 = as.numeric(hbr14))

# Need to include the health board names as part of this one, quick fix here
## GC TO DO - move to script 2

hblist <- data.frame(hbr14 = c(1:15), 
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

simd_dem <- left_join(hblist, simd_dem, by = "hbr14") %>%
  select(-hbr14)

writeWorksheet(wb, simd_dem,"A3-Popn. by deprivation and HB",
               startRow = 5, startCol = 2, header = FALSE)
# Need to update excel sheet for SIMD so that HBs are in correct
# order
# GC TO DO - automate this

saveWorkbook(wb)

## FIT/FOBT report

wb <- loadWorkbook(here("Output/CONFI-FIT-FOBT-comparison",
                        "FIT-FOBT-Comparison-Nov-17 to Oct-18.xlsx"))
setStyleAction(wb, XLC$"STYLE_ACTION.NONE")

# Uptake
test_comp_uptake <- readRDS(here("Temp", "test_comp_uptake.rds")) %>%
  select(-group_label)

writeWorksheet(wb, test_comp_uptake,"UptakeData", 
               startRow = 2, startCol = 1, header = FALSE)

# Positivity
test_comp_positivity <- readRDS(here("Temp", "test_comp_positivity.rds")) %>%
  select(-group_label)

writeWorksheet(wb, test_comp_positivity,"PositivityData", 
               startRow = 2, startCol = 1, header = FALSE)

# Cancer PPV
test_comp_cancer_ppv <- readRDS(here("Temp", "test_comp_cancer_ppv.rds")) %>%
  select(-group_label)

writeWorksheet(wb, test_comp_cancer_ppv,"CancerPPVColPerf", 
               startRow = 2, startCol = 1, header = FALSE)

# Adenoma PPV
test_comp_adenoma_ppv <- readRDS(here("Temp", "test_comp_adenoma_ppv.rds")) %>%
  select(-group_label)

writeWorksheet(wb, test_comp_adenoma_ppv,"AdenomaPPVColPerf", 
               startRow = 2, startCol = 1, header = FALSE)

# Hbg data
hbg <- readRDS(here("Temp", "hbg.rds")) %>%
  slice(-1)

writeWorksheet(wb, hbg,"Comparison - volumes", 
               startRow = 50, startCol = 3, header = FALSE)

# HR adenoma data
# Get into format 
hr_adenoma_ppv_p <- readRDS(here("Temp", 
                                         "test_comp_hr_adenoma_ppv.rds")) %>%
  slice(1:2) %>%
  select(test_type, p) %>%
  spread(test_type, p) %>%
  rename(
    pFOBT = 1,
    pFIT = 2
    )

hr_adenoma_ppv_lower <- readRDS(here("Temp", 
                                 "test_comp_hr_adenoma_ppv.rds")) %>%
  slice(1:2) %>%
  select(test_type, lower95CI) %>%
  spread(test_type, lower95CI) %>%
  rename(
    LowerCIFOBT = 1,
    LowerCIFIT = 2
  )

hr_adenoma_ppv_upper <- readRDS(here("Temp", 
                                 "test_comp_hr_adenoma_ppv.rds")) %>%
  slice(1:2) %>%
  select(test_type, upper95CI) %>%
  spread(test_type, upper95CI)%>%
  rename(
    UpperCIFOBT = 1,
    UpperCIFIT = 2
  )

hr_adenoma_ppv <- bind_cols(hr_adenoma_ppv_p, hr_adenoma_ppv_lower, 
                            hr_adenoma_ppv_upper) %>%
  mutate(
    lower_diff_fobt = pFOBT - LowerCIFOBT,
    lower_diff_fit = pFIT - LowerCIFIT,
    upper_diff_fobt = UpperCIFOBT - pFOBT,
    upper_diff_fit = UpperCIFIT - pFIT
  ) %>%
  select(-(LowerCIFOBT:UpperCIFIT))

writeWorksheet(wb, hr_adenoma_ppv,"Summary", 
               startRow = 74, startCol = 3, header = FALSE)

saveWorkbook(wb)

# Note - check that cells have updated in excel, press ctrl + shift + alt + F9
# to ensure this, currently need to change some cells from text to numeric too

# GC TO DO - update Nov 2018 etc. in the hidden tabs, may be best for this to be
# manual so that some visual checking is carried out
