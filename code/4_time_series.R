##########################################################
# 4_time_series.R
# Gavin Clark
# 26/04/2019
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

# Step 2 - prep
# Bring in analysis database from script 1
analysis_db <-
  readRDS(analysis_db_path) %>%
  filter(optin == 0 &
           hbr14 %in% 1:14)

# Create year definition
# Not a great way of doing this, struggling to think of a better way currently
# GC next - create dataset with just required variables and transpose year_ variables
  analysis_db <- analysis_db %>%
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
                        1, 0)
  ) 

# Calculate invites in each time period
  uptake_ts <- analysis_db %>%
    mutate(
      # Invite number
      invite_07_09 = year_07_09 * invite_n,
      invite_08_10 = year_08_10 * invite_n,
      invite_09_11 = year_09_11 * invite_n,
      invite_10_12 = year_10_12 * invite_n,
      invite_11_13 = year_11_13 * invite_n,
      invite_12_14 = year_12_14 * invite_n,
      invite_13_15 = year_13_15 * invite_n,
      invite_14_16 = year_14_16 * invite_n,
      invite_15_17 = year_15_17 * invite_n,
      invite_16_18 = year_16_18 * invite_n,
      # Uptake number
      uptake_07_09 = year_07_09 * uptake_n,
      uptake_08_10 = year_08_10 * uptake_n,
      uptake_09_11 = year_09_11 * uptake_n,
      uptake_10_12 = year_10_12 * uptake_n,
      uptake_11_13 = year_11_13 * uptake_n,
      uptake_12_14 = year_12_14 * uptake_n,
      uptake_13_15 = year_13_15 * uptake_n,
      uptake_14_16 = year_14_16 * uptake_n,
      uptake_15_17 = year_15_17 * uptake_n,
      uptake_16_18 = year_16_18 * uptake_n) %>%
    select(sex, invite_07_09:uptake_16_18) 
  
  male_ts <- uptake_ts %>%
    filter(sex == 1) %>%
    gather(key = "year", value = "n", -sex) %>%
    group_by(year) %>%
    summarise(n = sum(n)) %>%
    mutate(KPI = 1,
           sex = "Males") %>%
    ungroup() %>% 
    separate(year, c("metric","year")) %>%
    mutate(
      year2 = as.character(
        ifelse(as.numeric(year) + 2 == 9,
                "09",
                as.numeric(year) + 2
                )),
      report_yr = as.character(paste0("20", year,"/", year2))) %>%
    spread(key = metric, value = n) %>% 
    mutate(KPI_rate = uptake/invite) %>%
    select(KPI, report_yr, sex, KPI_rate)
  
  female_ts <- uptake_ts %>%
    filter(sex == 2) %>%
    gather(key = "year", value = "n", -sex) %>%
    group_by(year) %>%
    summarise(n = sum(n)) %>%
    mutate(KPI = 1,
           sex = "Females") %>%
    ungroup() %>% 
    separate(year, c("metric","year")) %>%
    mutate(
      year2 = as.character(
        ifelse(as.numeric(year) + 2 == 9,
               "09",
               as.numeric(year) + 2
        )),
      report_yr = as.character(paste0("20", year,"/", year2))) %>%
    spread(key = metric, value = n) %>% 
    mutate(KPI_rate = uptake/invite) %>%
    select(KPI, report_yr, sex, KPI_rate)
  
  all_ts <- uptake_ts %>%
    gather(key = "year", value = "n", -sex) %>%
    group_by(year) %>%
    summarise(n = sum(n)) %>%
    mutate(KPI = 1,
           sex = "Persons") %>%
    ungroup() %>% 
    separate(year, c("metric","year")) %>%
    mutate(
      year2 = as.character(
        ifelse(as.numeric(year) + 2 == 9,
               "09",
               as.numeric(year) + 2
        )),
      report_yr = as.character(paste0("20", year,"/", year2))) %>%
    spread(key = metric, value = n) %>% 
    mutate(KPI_rate = uptake/invite) %>%
    select(KPI, report_yr, sex, KPI_rate)
  
## GC HERE - next
  # check outputs vs. SPSS
  # create function
  # check outputs for all KPIs
  # SII/RII


# KPI 2 - SII/RII
simd <- analysis_db %>%
  group_by(simd2016, age_group) %>%
  summarise(invite_n = sum(invite_n),
            uptake_n = sum(uptake_n)) %>%
  mutate(uptake_p = uptake_n/invite_n)

rii(data = analysis_db, 
    health = uptake_n, 
    population = invite_n, 
    ses = simd2016, 
    age = age_group)

rii(data = health_data, 
    health = bad, 
    population = pop, 
    ses = quintile, 
    age = age)
ethnicity == "all")
# KPI 3 - Positivity
# KPI 8 - Cancer detection
# KPI 9-15 - Dukes staging
# KPI 19 - Adenoma detection
# KPI 21 - Cancer PPV
# KPI 22 - Adenoma PPV