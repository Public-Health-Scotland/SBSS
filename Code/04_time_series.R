#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 04_time_series.R
# Gavin Clark
# Aug 2022
# Script 4 of 13
# Data preparation and summary for export
# Written/run on R Studio Server
# R version 3.6.1
# This script creates the 2-year time-series tables for the KPI report
# Transcribed from scripts at:
# \\stats\CancerGroup1\Topics\BowelScreening\Publications\SBoSP-Statistics\
# 20190205\Syntax\5_Trend_uptake_N18 WIP.sps
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Step 0: Housekeeping ----
library(readr)
library(dplyr)
library(tidyr)
library(haven)
library(janitor)
library(here)
library(tidylog)

# set filepaths and extract dates with script 0
rm(list = ls())
source(here::here("Code","00_housekeeping.R"))
wd <- paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening",
             "/Publications/SBoSP-Statistics/20230804")
# source(paste0(wd, "/Code/00_housekeeping.R"))


# Create function for time series for desired kpi by male, female, all
ts_function <- function(denominator, numerator, KPI_no) {
  
  kpi_ts <- analysis_db %>%
    mutate(
      # Denominator by year
      denom_07_09 = year_07_09 * !! sym(denominator),
      denom_08_10 = year_08_10 * !! sym(denominator),
      denom_09_11 = year_09_11 * !! sym(denominator),
      denom_10_12 = year_10_12 * !! sym(denominator),
      denom_11_13 = year_11_13 * !! sym(denominator),
      denom_12_14 = year_12_14 * !! sym(denominator),
      denom_13_15 = year_13_15 * !! sym(denominator),
      denom_14_16 = year_14_16 * !! sym(denominator),
      denom_15_17 = year_15_17 * !! sym(denominator),
      denom_16_18 = year_16_18 * !! sym(denominator),
      denom_17_19 = year_17_19 * !! sym(denominator),
      denom_18_20 = year_18_20 * !! sym(denominator),
      denom_19_21 = year_19_21 * !! sym(denominator),
      denom_20_22 = year_20_22 * !! sym(denominator), 
      # Numerator by year
      num_07_09 = year_07_09 * !! sym(numerator),
      num_08_10 = year_08_10 * !! sym(numerator),
      num_09_11 = year_09_11 * !! sym(numerator),
      num_10_12 = year_10_12 * !! sym(numerator),
      num_11_13 = year_11_13 * !! sym(numerator),
      num_12_14 = year_12_14 * !! sym(numerator),
      num_13_15 = year_13_15 * !! sym(numerator),
      num_14_16 = year_14_16 * !! sym(numerator),
      num_15_17 = year_15_17 * !! sym(numerator),
      num_16_18 = year_16_18 * !! sym(numerator),
      num_17_19 = year_17_19 * !! sym(numerator),
      num_18_20 = year_18_20 * !! sym(numerator),
      num_19_21 = year_19_21 * !! sym(numerator),
      num_20_22 = year_20_22 * !! sym(numerator)
      ) %>%
    select(sex, denom_07_09:num_20_22) 

  # Males only
  male_ts <- kpi_ts %>%
    filter(sex == 1) %>%
    select(-sex) %>%
    pivot_longer(cols = everything(),               # or c('denom_07_09':'num_19_21')
                 names_to = 'year', values_to = 'n') %>%
    group_by(year) %>%
    summarise(n = sum(n)) %>%
    mutate(KPI = KPI_no,
           sex = "Males") %>%
    ungroup() %>% 
    separate(year, c("metric","year")) %>%
    # Create 2-year period level in formay YYYY/YY
    mutate(year2 = as.character(ifelse(as.numeric(year) + 2 == 9,
                                       "09", as.numeric(year) + 2)),
           report_yr = as.character(paste0("20", year,"/", year2))) %>%
    pivot_wider(names_from = 'metric', values_from = 'n') %>%
    mutate(KPI_rate = num/denom*100) %>%
    select(KPI, sex, report_yr, KPI_rate) %>%
    pivot_wider(id_cols = c('KPI', 'sex'), 
                names_from = 'report_yr', values_from = 'KPI_rate')
  
  
  # Females only
  female_ts <- kpi_ts %>%
    filter(sex == 2) %>%
    select(-sex) %>%
    pivot_longer(cols = everything(),               # or c('denom_07_09':'num_19_21')
                 names_to = 'year', values_to = 'n') %>%
    group_by(year) %>%
    summarise(n = sum(n)) %>%
    mutate(KPI = KPI_no,
           sex = "Females") %>%
    ungroup() %>% 
    separate(year, c("metric","year")) %>%
    # Create 2-year period level in formay YYYY/YY
    mutate(year2 = as.character(ifelse(as.numeric(year) + 2 == 9,
                                       "09", as.numeric(year) + 2)),
           report_yr = as.character(paste0("20", year,"/", year2))) %>%
    pivot_wider(names_from = 'metric', values_from = 'n') %>%
    mutate(KPI_rate = num/denom*100) %>%
    select(KPI, sex, report_yr, KPI_rate) %>%
    pivot_wider(id_cols = c('KPI', 'sex'), 
                names_from = 'report_yr', values_from = 'KPI_rate')
  
  
  # All persons
  all_ts <- kpi_ts %>%
    select(-sex) %>%
    pivot_longer(cols = everything(),               # or c('denom_07_09':'num_19_21')
                 names_to = 'year', values_to = 'n') %>%
    group_by(year) %>%
    summarise(n = sum(n)) %>%
    mutate(KPI = KPI_no,
           sex = "Persons") %>%
    ungroup() %>% 
    separate(year, c("metric","year")) %>%
    # Create 2-year period level in formay YYYY/YY
    mutate(year2 = as.character(ifelse(as.numeric(year) + 2 == 9,
                                       "09", as.numeric(year) + 2)),
           report_yr = as.character(paste0("20", year,"/", year2))) %>%
    pivot_wider(names_from = 'metric', values_from = 'n') %>%
    mutate(KPI_rate = num/denom*100) %>%
    select(KPI, sex, report_yr, KPI_rate) %>%
    pivot_wider(id_cols = c('KPI', 'sex'), 
                names_from = 'report_yr', values_from = 'KPI_rate')
  
  
  output <- bind_rows(male_ts, female_ts, all_ts)
  
}


### Step 1: Load and prepare data ----

# Bring in analysis database from script 1
analysis_db <- read_rds(analysis_db_path) %>%
               filter(hbr19 %in% 1:14 & 
                      optin == 0 )
  
names(analysis_db)



### Create year definition
# Not a great way of doing this, struggling to think of a better way currently
# Problem is creating a rolling 2 year period, shifted on by a year at a time
# This means that the same record needs to end up in 2 periods e.g. a test in
# May 2009 should be in 2008/10 and 2009/11

analysis_db <- analysis_db %>%
  ## May-April report period - YOU MAY NEED TO EXTEND
  # mutate(
     # year_07_09 = ifelse(invdate >= as.Date("2007-05-01") &
     #                       invdate <= as.Date("2009-04-30"),
     #                     1, 0),
     # year_08_10 = ifelse(invdate >= as.Date("2008-05-01") &
     #                       invdate <= as.Date("2010-04-30"),
     #                     1, 0),
     # year_09_11 = ifelse(invdate >= as.Date("2009-05-01") &
     #                       invdate <= as.Date("2011-04-30"),
     #                     1, 0),
     # year_10_12 = ifelse(invdate >= as.Date("2010-05-01") &
     #                       invdate <= as.Date("2012-04-30"),
     #                     1, 0),
     # year_11_13 = ifelse(invdate >= as.Date("2011-05-01") &
     #                       invdate <= as.Date("2013-04-30"),
     #                     1, 0),
     # year_12_14 = ifelse(invdate >= as.Date("2012-05-01") &
     #                       invdate <= as.Date("2014-04-30"),
     #                     1, 0),
     # year_13_15 = ifelse(invdate >= as.Date("2013-05-01") &
     #                       invdate <= as.Date("2015-04-30"),
     #                     1, 0),
     # year_14_16 = ifelse(invdate >= as.Date("2014-05-01") &
     #                       invdate <= as.Date("2016-04-30"),
     #                     1, 0),
     # year_15_17 = ifelse(invdate >= as.Date("2015-05-01") &
     #                       invdate <= as.Date("2017-04-30"),
     #                     1, 0),
     # year_16_18 = ifelse(invdate >= as.Date("2016-05-01") &
     #                       invdate <= as.Date("2018-04-30"),
     #                     1, 0),
     # year_17_19 = ifelse(invdate >= as.Date("2017-05-01") &
     #                       invdate <= as.Date("2019-04-30"),
     #                     1, 0),
     # year_18_20 = ifelse(invdate >= as.Date("2018-05-01") &
     #                     invdate <= as.Date("2020-04-30"),
     #                     1, 0),
     # year_19_21 = ifelse(invdate >= as.Date("2019-05-01") &
     #                       invdate <= as.Date("2021-04-30"),
     #                     1, 0),
     # year_20_22 = ifelse(invdate >= as.Date("2020-05-01") &
     #                       invdate <= as.Date("2022-04-30"),
     #                     1, 0),
     # year_21_23 = ifelse(invdate >= as.Date("2021-05-01") &
     #                       invdate <= as.Date("2023-04-30"),
     #                     1, 0)
  ## Nov-Oct report period - YOU MAY NEED TO EXTEND
   mutate(
     year_07_09 = ifelse(invdate >= as.Date("2007-11-01") &
                           invdate <= as.Date("2009-10-31"),
                         1, 0),
     year_08_10 = ifelse(invdate >= as.Date("2008-11-01") &
                           invdate <= as.Date("2010-10-31"),
                         1, 0),
     year_09_11 = ifelse(invdate >= as.Date("2009-11-01") &
                           invdate <= as.Date("2011-10-31"),
                         1, 0),
     year_10_12 = ifelse(invdate >= as.Date("2010-11-01") &
                           invdate <= as.Date("2012-10-31"),
                         1, 0),
     year_11_13 = ifelse(invdate >= as.Date("2011-11-01") &
                           invdate <= as.Date("2013-10-31"),
                         1, 0),
     year_12_14 = ifelse(invdate >= as.Date("2012-11-01") &
                           invdate <= as.Date("2014-10-31"),
                         1, 0),
     year_13_15 = ifelse(invdate >= as.Date("2013-11-01") &
                           invdate <= as.Date("2015-10-31"),
                         1, 0),
     year_14_16 = ifelse(invdate >= as.Date("2014-11-01") &
                           invdate <= as.Date("2016-10-31"),
                         1, 0),
     year_15_17 = ifelse(invdate >= as.Date("2015-11-01") &
                           invdate <= as.Date("2017-10-31"),
                         1, 0),
     year_16_18 = ifelse(invdate >= as.Date("2016-11-01") &
                           invdate <= as.Date("2018-10-31"),
                         1, 0),
     year_17_19 = ifelse(invdate >= as.Date("2017-11-01") &
                           invdate <= as.Date("2019-10-31"),
                         1, 0),
     year_18_20 = ifelse(invdate >= as.Date("2018-11-01") &
                           invdate <= as.Date("2020-10-31"),
                         1, 0),
     year_19_21 = ifelse(invdate >= as.Date("2019-11-01") &
                           invdate <= as.Date("2021-10-31"),
                         1, 0),
     year_19_21 = ifelse(invdate >= as.Date("2019-11-01") &
                           invdate <= as.Date("2021-10-31"),
                         1, 0), 
     year_20_22 = ifelse(invdate >= as.Date("2020-11-01") &
                           invdate <= as.Date("2022-10-31"),
                         1, 0)
) 

  
## Create cancer definitions for PPV measures
# Now produced in script 1
#analysis_db <- analysis_db %>%
#                mutate(canc_col_n = cancer_n * col_perf_n,
#                       adenoma_col_n = adenoma_n * col_perf_n
#                       ) 

glimpse(analysis_db)

# GC TO DO - automate so that date basis changes depending on whether
# report period is May-April or Nov-Oct

### Step 2:  Calculate time-series KPIs using function ----

uptake_ts      <- ts_function("invite_n",   "uptake_n",       1)
gc()
positivity_ts  <- ts_function("uptake_n",   "positive_n",     3)
gc()
cancer_ts      <- ts_function("uptake_n",   "cancer_n",       8)
gc()
adenoma_ts     <- ts_function("uptake_n",   "adenoma_n",     19)
gc()
cancer_ppv_ts  <- ts_function("col_perf_n", "canc_col_n",    21)
gc()
adenoma_ppv_ts <- ts_function("col_perf_n", "adenoma_col_n", 22)
gc()

ts_data <- bind_rows(uptake_ts, positivity_ts, cancer_ts, adenoma_ts, 
                     cancer_ppv_ts, adenoma_ppv_ts)

ts_data %>% View()
# Matches output from previous publication

#SII/RII calculation moved to separate script


# Calculate cancer staging time-series
cancer_ts <- analysis_db %>% 
             filter(cancer_n == 1) %>%
# mutate and select functions below
  select(dukes_der, year_07_09:year_20_22) %>%
  group_by(dukes_der) %>%
  summarise(
    year_07_09 = sum(year_07_09),
    year_08_10 = sum(year_08_10),    
    year_09_11 = sum(year_09_11),
    year_10_12 = sum(year_10_12),
    year_11_13 = sum(year_11_13),
    year_12_14 = sum(year_12_14),
    year_13_15 = sum(year_13_15),
    year_14_16 = sum(year_14_16),
    year_15_17 = sum(year_15_17),
    year_16_18 = sum(year_16_18),
    year_17_19 = sum(year_17_19),
    year_18_20 = sum(year_18_20),
    year_19_21 = sum(year_19_21),
    year_20_22 = sum(year_20_22)) %>%
  ungroup() %>%
  mutate(
    prop_07_09 = year_07_09/sum(year_07_09)*100,
    prop_08_10 = year_08_10/sum(year_08_10)*100,
    prop_09_11 = year_09_11/sum(year_09_11)*100,
    prop_10_12 = year_10_12/sum(year_10_12)*100,
    prop_11_13 = year_11_13/sum(year_11_13)*100,
    prop_12_14 = year_12_14/sum(year_12_14)*100,
    prop_13_15 = year_13_15/sum(year_13_15)*100,
    prop_14_16 = year_14_16/sum(year_14_16)*100,
    prop_15_17 = year_15_17/sum(year_15_17)*100,
    prop_16_18 = year_16_18/sum(year_16_18)*100,
    prop_17_19 = year_17_19/sum(year_17_19)*100,
    prop_18_20 = year_18_20/sum(year_18_20)*100,
    prop_19_21 = year_19_21/sum(year_19_21)*100,
    prop_20_22 = year_20_22/sum(year_20_22)*100) %>%
  select(dukes_der, prop_07_09:prop_20_22)


#### Step 3: Save final outputs ----
# write_rds(ts_data, here::here("Temp","ts_data.rds"),
#           compress = 'gz')
#saveRDS(ts_data, here::here("Temp","ts_data.rds"))
write_rds(ts_data, paste0(wd, "/Temp/ts_data.rds"))


# write_csv(cancer_ts, here::here("Temp", "cancer_ts.csv"))
# 
# write_rds(cancer_ts, here::here("Temp", "cancer_ts.rds"),
#           compress = 'gz')
#saveRDS(cancer_ts, here::here("Temp", "cancer_ts.rds"))
write_rds(cancer_ts, paste0(wd, "/Temp/cancer_ts.rds"))


