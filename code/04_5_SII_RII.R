#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 04_5_SII_RII.R
# Gavin Clark
# Aug 2022
# Script 4.5 of 13
# Data preparation and summary for export
# Written/run on R Studio Server
# R version 3.2.3
# This script creates the SII and RII outputs for publication
# Transcribed from syntaxes 8, 9 and 10 of the SPSS process
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Step 0: Housekeeping ----
library(dplyr)
library(tidyr)
library(readr)
library(janitor)
library(lubridate)
library(invgamma)
library(purrr)
library(haven)
library(here)
library(tidylog)


# set filepaths and extract dates with script 0
rm(list = ls())
source(here::here("Code","00_housekeeping.R"))
wd <- paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening",
             "/Publications/SBoSP-Statistics/20230221")
# source(paste0(wd, "/Code/00_housekeeping.R"))



### Step 1: Import data ----

SIMD_data <- read_rds(analysis_db_path) %>%
              filter(hbr19 %in% 1:14 &
                     optin == 0) 

names(SIMD_data)

## Prepare data
SIMD_data <- SIMD_data %>%
  # Calculate correct SIMD for time period
  # See guidance
  # https://www.isdscotland.org/Products-and-Services/GPD-Support/Deprivation/
  mutate( ##!! Should the new time periods not match the beginning of that year? KH
    simd = case_when(
      invdate >= as.Date("2007-05-01") & invdate <= as.Date("2009-12-31") ~ as.numeric(simd2009),
      invdate >= as.Date("2010-01-01") & invdate <= as.Date("2013-12-31") ~ as.numeric(simd2012),
      invdate >= as.Date("2014-01-01") & invdate <= as.Date("2016-12-31") ~ as.numeric(simd2016),
      invdate >= as.Date("2017-01-01") ~ as.numeric(simd2020)),
    ### Variables for participation year
    ## May-April report period
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
     year_17_19 = ifelse(invdate >= as.Date("2017-05-01") &
                           invdate <= as.Date("2019-04-30"),
                         1, 0),
     year_18_20 = ifelse(invdate >= as.Date("2018-05-01") &
                           invdate <= as.Date("2020-04-30"),
                         1, 0),
     year_19_21 = ifelse(invdate >= as.Date("2019-05-01") &
                           invdate <= as.Date("2021-04-30"),
                         1, 0),
      year_20_22 = ifelse(invdate >= as.Date("2020-05-01") &
                          invdate <= as.Date("2022-04-30"),
                          1, 0),
    ## Nov-Oct report period                    
    #  year_07_09 = ifelse(invdate >= as.Date("2007-11-01") &
    #                        invdate <= as.Date("2009-10-31"),
    #                      1, 0),
    #  year_08_10 = ifelse(invdate >= as.Date("2008-11-01") &
    #                        invdate <= as.Date("2010-10-31"),
    #                      1, 0),
    #  year_09_11 = ifelse(invdate >= as.Date("2009-11-01") &
    #                        invdate <= as.Date("2011-10-31"),
    #                      1, 0),
    #  year_10_12 = ifelse(invdate >= as.Date("2010-11-01") &
    #                        invdate <= as.Date("2012-10-31"),
    #                      1, 0),
    #  year_11_13 = ifelse(invdate >= as.Date("2011-11-01") &
    #                        invdate <= as.Date("2013-10-31"),
    #                      1, 0),
    #  year_12_14 = ifelse(invdate >= as.Date("2012-11-01") &
    #                        invdate <= as.Date("2014-10-31"),
    #                      1, 0),
    #  year_13_15 = ifelse(invdate >= as.Date("2013-11-01") &
    #                        invdate <= as.Date("2015-10-31"),
    #                      1, 0),
    # year_14_16 = ifelse(invdate >= as.Date("2014-11-01") &
    #                        invdate <= as.Date("2016-10-31"),
    #                      1, 0),
    #  year_15_17 = ifelse(invdate >= as.Date("2015-11-01") &
    #                        invdate <= as.Date("2017-10-31"),
    #                      1, 0),
    #  year_16_18 = ifelse(invdate >= as.Date("2016-11-01") &
    #                        invdate <= as.Date("2018-10-31"),
    #                      1, 0),
    #  year_17_19 = ifelse(invdate >= as.Date("2017-11-01") &
    #                        invdate <= as.Date("2019-10-31"),
    #                      1, 0),
    #  year_18_20 = ifelse(invdate >= as.Date("2018-11-01") &
    #                        invdate <= as.Date("2020-10-31"),
    #                      1, 0),
    # year_19_21 = ifelse(invdate >= as.Date("2019-11-01") &
    #                       invdate <= as.Date("2021-10-31"),
    #                     1, 0),
    # Denominator by year
    denom_07_09 = year_07_09 * invite_n,
    denom_08_10 = year_08_10 * invite_n,
    denom_09_11 = year_09_11 * invite_n,
    denom_10_12 = year_10_12 * invite_n,
    denom_11_13 = year_11_13 * invite_n,
    denom_12_14 = year_12_14 * invite_n,
    denom_13_15 = year_13_15 * invite_n,
    denom_14_16 = year_14_16 * invite_n,
    denom_15_17 = year_15_17 * invite_n,
    denom_16_18 = year_16_18 * invite_n,
    denom_17_19 = year_17_19 * invite_n,
    denom_18_20 = year_18_20 * invite_n,
    denom_19_21 = year_19_21 * invite_n,
    denom_20_22 = year_20_22 * invite_n,
    # numerator by year
    num_07_09 = year_07_09 * uptake_n,
    num_08_10 = year_08_10 * uptake_n,
    num_09_11 = year_09_11 * uptake_n,
    num_10_12 = year_10_12 * uptake_n,
    num_11_13 = year_11_13 * uptake_n,
    num_12_14 = year_12_14 * uptake_n,
    num_13_15 = year_13_15 * uptake_n,
    num_14_16 = year_14_16 * uptake_n,
    num_15_17 = year_15_17 * uptake_n,
    num_16_18 = year_16_18 * uptake_n,
    num_17_19 = year_17_19 * uptake_n,
    num_18_20 = year_18_20 * uptake_n,
    num_19_21 = year_19_21 * uptake_n,
    num_20_22 = year_20_22 * uptake_n) %>%
  select(sex, simd, denom_07_09:num_20_22)


### Step 2: Create complex function ----
# Create function for each sex and simd combination by year
uptake_simd <- function(simd_num, simd_val, sex_num, sex_char) {
  
  simd_1_male <- SIMD_data %>%
    filter(simd %in% simd_num & 
           sex %in% sex_num) %>%
    # change to pivot_longer
    gather(key = "year", value = "n", -sex, -simd) %>%
    group_by(year) %>%
    summarise(n = sum(n)) %>%
    mutate(sex = sex_char) %>%
    ungroup() %>% 
    separate(year, c("metric","year")) %>%
    mutate(
      year2 = as.character(
        ifelse(as.numeric(year) + 2 == 9,
               "09",
               as.numeric(year) + 2
        )),
      report_yr = as.character(paste0("20", year,"/", year2))) %>%
    # change to pivot_wider
    spread(key = metric, value = n) %>% 
    mutate(KPI_rate = num/denom*100) %>%
    select(report_yr, sex, num, denom, KPI_rate) %>%
    mutate(simd = simd_val)
}



### Step 3: Calculate indices and organise tables ----
# Create uptake tables for each sex, simd combination
# GC TO DO - could probably do this more simply with cleverer use of grouping

## Males
simd_1_male <- uptake_simd(1, "1", 1, "Male")
simd_2_male <- uptake_simd(2, "2", 1, "Male")
simd_3_male <- uptake_simd(3, "3", 1, "Male")
simd_4_male <- uptake_simd(4, "4", 1, "Male")
simd_5_male <- uptake_simd(5, "5", 1, "Male")

# simd_total_male <- uptake_simd(c(1:5), "Total", 1, "Male")
simd_all_male <- bind_rows(simd_1_male, simd_2_male, simd_3_male, 
                           simd_4_male, simd_5_male)

## Females
simd_1_female <- uptake_simd(1, "1", 2, "Female")
simd_2_female <- uptake_simd(2, "2", 2, "Female")
simd_3_female <- uptake_simd(3, "3", 2, "Female")
simd_4_female <- uptake_simd(4, "4", 2, "Female")
simd_5_female <- uptake_simd(5, "5", 2, "Female")

# simd_total_female <- uptake_simd(c(1:5), "Total", 2, "Female")
simd_all_female <- bind_rows(simd_1_female, simd_2_female, simd_3_female, 
                             simd_4_female, simd_5_female)

## Total
simd_1_total <- uptake_simd(1, "1", c(1:2), "All Persons")
simd_2_total <- uptake_simd(2, "2", c(1:2), "All Persons")
simd_3_total <- uptake_simd(3, "3", c(1:2), "All Persons")
simd_4_total <- uptake_simd(4, "4", c(1:2), "All Persons")
simd_5_total <- uptake_simd(5, "5", c(1:2), "All Persons")
# simd_total_total <- uptake_simd(c(1:5), "Total", c(1:2), "All Persons")

simd_all_total <- bind_rows(simd_1_total, simd_2_total, simd_3_total, 
                            simd_4_total, simd_5_total)

## Combine all simd into one table

simd_all <- bind_rows(simd_all_male, simd_all_female, simd_all_total)

rm(simd_1_male, simd_2_male, simd_3_male, simd_4_male, simd_5_male,
   simd_1_female, simd_2_female, simd_3_female, simd_4_female, simd_5_female,
   simd_1_total, simd_2_total, simd_3_total, simd_4_total, simd_5_total)


# Step 4: SII/RII calculation for uptake ----
# Copied SII method from https://github.com/ScotPHO/indicator-production

# Create file for modelling
data_depr_sii <- simd_all %>%
  group_by(report_yr, sex) %>%
  # Calculate overall uptake rate and total population
  mutate(overall_rate = sum(num)/sum(denom),
         total_pop = sum(denom)) %>%
  # calculate the total population for each area (without SIMD)
  mutate(proportion_pop = denom/total_pop) %>% 
  arrange(report_yr, sex) %>% 
  # This first part is to adjust rate and denominator with the population weights
  # cumulative proportion population for each area
  mutate(cumulative_pro = cumsum(proportion_pop),  
         relative_rank = case_when(
           simd == "1" ~ 0.5*proportion_pop,
           simd != "1" ~ lag(cumulative_pro) + 0.5*proportion_pop),
         # square root of the proportion of the population in each SIMD
         sqr_proportion_pop = sqrt(proportion_pop), 
         relrank_sqr_proppop = relative_rank * sqr_proportion_pop,
         # rate based on population weights
         rate_sqr_proppop = KPI_rate * sqr_proportion_pop) %>% 
  arrange(sex, report_yr, simd) 

# Creating one column called data with all the variables not in the grouping
# Calculating linear regression for all the groups, then formatting the results
sii_model <- data_depr_sii %>% 
  nest() %>%
  mutate(model = map(data, ~lm(rate_sqr_proppop ~ 0 + sqr_proportion_pop 
                               + relrank_sqr_proppop, 
                               data = .)),
         #extracting sii from model, a bit fiddly but it works
         sii = as.numeric(map(map(model, "coefficients"), 
                              "relrank_sqr_proppop"))) %>%
  select(report_yr, sex, sii)

# Create table with overall uptake for calculating RII
rii_sii <- simd_all %>%
  group_by(report_yr, sex) %>%
  summarise(overall_rate = sum(num)/sum(denom)*100) %>%
  left_join(sii_model, c("report_yr", "sex")) %>%
  # Having reviewed the SPSS code, RII was previously calulated using the
  # simple mean of uptake for deciles 1:5. I believe the population-weighted
  # mean is more appropriate so will generate slightly different answers
  # to what was done previously
  mutate(rii = sii/overall_rate) %>%
  arrange(desc(sex), report_yr) %>%
  # change to pivot_longer
  gather("measure", "value", -report_yr, -sex, -overall_rate) %>%
  select(measure, sex, report_yr, value) %>%
  ungroup()

head(rii_sii)

rii_sii <- rii_sii %>%
           pivot_wider(id_cols = c('measure','sex'),
                       names_from = 'report_yr', values_from = 'value')

rii_sii
  
  
### Step 5: Save file with RII/SII ----

# write_rds(rii_sii, here::here("Temp", "rii_sii_data.rds"),
#           compress = 'gz')

#saveRDS(rii_sii, here::here("Temp", "rii_sii_data.rds"))
write_rds(rii_sii, paste0(wd, "/Temp/rii_sii_data.rds"))

