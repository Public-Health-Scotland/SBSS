##########################################################
# 4_time_series.R
# Gavin Clark
# 29/05/2019
# Script 4 of ?
# Data preparation and summary for export
# Written/run on R Studio Server
# R version 3.2.3
# This script creates the 2-year time-series tables for the KPI report
# Transcribed from scripts at:
# \\stats\CancerGroup1\Topics\BowelScreening\Publications\SBoSP-Statistics\
# 20190205\Syntax\5_Trend_uptake_N18 WIP.sps
##########################################################

### Step 1 - housekeeping
library(dplyr)
library(tidyr)

# set filepaths and extract dates with script 0
source(here::here("code", "0_housekeeping.R"))


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
      num_16_18 = year_16_18 * !! sym(numerator)) %>%
    select(sex, denom_07_09:num_16_18) 
  
  # Males only
  male_ts <- kpi_ts %>%
    filter(sex == 1) %>%
    gather(key = "year", value = "n", -sex) %>%
    group_by(year) %>%
    summarise(n = sum(n)) %>%
    mutate(KPI = KPI_no,
           sex = "Males") %>%
    ungroup() %>% 
    separate(year, c("metric","year")) %>%
    # Create 2-year period level in formay YYYY/YY
    mutate(
      year2 = as.character(
        ifelse(as.numeric(year) + 2 == 9,
               "09",
               as.numeric(year) + 2
        )),
      report_yr = as.character(paste0("20", year,"/", year2))) %>%
    spread(key = metric, value = n) %>% 
    mutate(KPI_rate = num/denom*100) %>%
    select(KPI, report_yr, sex, KPI_rate)
  
  
  # Females only
  female_ts <- kpi_ts %>%
    filter(sex == 2) %>%
    gather(key = "year", value = "n", -sex) %>%
    group_by(year) %>%
    summarise(n = sum(n)) %>%
    mutate(KPI = KPI_no,
           sex = "Females") %>%
    ungroup() %>% 
    separate(year, c("metric","year")) %>%
    # Create 2-year period level in formay YYYY/YY
    mutate(
      year2 = as.character(
        ifelse(as.numeric(year) + 2 == 9,
               "09",
               as.numeric(year) + 2
        )),
      report_yr = as.character(paste0("20", year,"/", year2))) %>%
    spread(key = metric, value = n) %>% 
    mutate(KPI_rate = num/denom*100) %>%
    select(KPI, report_yr, sex, KPI_rate)
  
  # All persons
  all_ts <- kpi_ts %>%
    gather(key = "year", value = "n", -sex) %>%
    group_by(year) %>%
    summarise(n = sum(n)) %>%
    mutate(KPI = KPI_no,
           sex = "Persons") %>%
    ungroup() %>% 
    separate(year, c("metric","year")) %>%
    # Create 2-year period level in formay YYYY/YY
    mutate(
      year2 = as.character(
        ifelse(as.numeric(year) + 2 == 9,
               "09",
               as.numeric(year) + 2
        )),
      report_yr = as.character(paste0("20", year,"/", year2))) %>%
    spread(key = metric, value = n) %>% 
    mutate(KPI_rate = num/denom*100) %>%
    select(KPI, report_yr, sex, KPI_rate)
  
  output <- bind_rows(male_ts, female_ts, all_ts)
}

# Step 2 - prep
# Bring in analysis database from script 1
analysis_db <-
  readRDS(analysis_db_path) %>%
  filter(optin == 0 &
           hbr14 %in% 1:14) %>%
  
  # Create year definition
  # Not a great way of doing this, struggling to think of a better way currently
  # Problem is creating a rolling 2 year period, shifted on by a year at a time
  # This means that the same record needs to end up in 2 periods e.g. a test in
  # May 2009 should be in 2008/10 and 2009/11
  mutate(
    year_07_09 = ifelse(invdate >= as.Date("2007-05-01") &
                          invdate <= as.Date("2009-04-30"),
                        1, 0),
    year_08_10 = ifelse(invdate >= as.Date("2008-05-01") &
                          invdate <= as.Date("2010-04-30"),
                        1, 0),
    year_09_11 = ifelse(invdate >= as.Date("2009-05-01") &
                          invdate <= as.Date("2011-04-30"),
                        1, 0),
    year_10_12 = ifelse(invdate >= as.Date("2010-05-01") &
                          invdate <= as.Date("2012-04-30"),
                        1, 0),
    year_11_13 = ifelse(invdate >= as.Date("2011-05-01") &
                          invdate <= as.Date("2013-04-30"),
                        1, 0),
    year_12_14 = ifelse(invdate >= as.Date("2012-05-01") &
                          invdate <= as.Date("2014-04-30"),
                        1, 0),
    year_13_15 = ifelse(invdate >= as.Date("2013-05-01") &
                          invdate <= as.Date("2015-04-30"),
                        1, 0),
    year_14_16 = ifelse(invdate >= as.Date("2014-05-01") &
                          invdate <= as.Date("2016-04-30"),
                        1, 0),
    year_15_17 = ifelse(invdate >= as.Date("2015-05-01") &
                          invdate <= as.Date("2017-04-30"),
                        1, 0),
    year_16_18 = ifelse(invdate >= as.Date("2016-05-01") &
                          invdate <= as.Date("2018-04-30"),
                        1, 0),
    # Create cancer definitions for PPV measures
    canc_col_n = cancer_n * col_perf_n,
    adenoma_col_n = adenoma_n * col_perf_n
  ) 

# GC TO DO - automate so that date basis changes depending on whether
# report period is May-April or Nov-Oct

# Calculate time-series KPIs using function
uptake_ts <- ts_function("invite_n", "uptake_n", 1)
positivity_ts <- ts_function("uptake_n", "positive_n", 3)
cancer_ts <- ts_function("uptake_n", "cancer_n", 8)
adenoma_ts <- ts_function("uptake_n", "adenoma_n", 19)
cancer_ppv_ts <- ts_function("col_perf_n", "canc_col_n", 21)
adenoma_ppv_ts <- ts_function("col_perf_n", "adenoma_col_n", 22)

ts_data <- bind_rows(uptake_ts, positivity_ts, cancer_ts, adenoma_ts, 
                     cancer_ppv_ts, adenoma_ppv_ts)
# Matches output from previous publication

#SII/RII calculation moved to separate script

# Save final output
saveRDS(ts_data, paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening/",
                        "TPP/KPIs/Code + DB/TPP/data/ts_data.rds"))




