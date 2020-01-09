##########################################################
# 4_5_SII_RII.R
# Gavin Clark
# 29/05/2019
# Script 4.5 of ?
# Data preparation and summary for export
# Written/run on R Studio Server
# R version 3.2.3
# This script creates the SII and RII outputs for publication
# Transcribed from syntaxes 8, 9 and 10 of the SPSS process
##########################################################

### Step 1 - housekeeping
library(dplyr)
library(tidyr)
library(purrr)

# set filepaths and extract dates with script 0
source(here::here("code", "0_housekeeping.R"))


SIMD_data <- readRDS(analysis_db_path) %>%
  filter(optin == 0 &
           hbr14 %in% 1:14) %>%
  # Calculate correct SIMD for time period
  mutate(
    simd = case_when(
      invdate >= as.Date("2007-05-01") & invdate <= as.Date("2009-12-31") 
      ~ as.numeric(simd2009),
      invdate >= as.Date("2010-01-01") & invdate <= as.Date("2013-12-31") 
      ~ as.numeric(simd2012),
      invdate >= as.Date("2014-01-01")  ~ as.numeric(simd2016)
    ),
    # variables for participation year
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
    num_17_19 = year_17_19 * uptake_n) %>%
  select(sex, simd, denom_07_09:num_17_19)

# Create function for each sex and simd combination by year
uptake_simd <- function(simd_num, simd_val, sex_num, sex_char) {
  
  simd_1_male <-   SIMD_data %>%
    filter(simd %in% simd_num & 
             sex %in% sex_num) %>%
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
    spread(key = metric, value = n) %>% 
    mutate(KPI_rate = num/denom*100) %>%
    select(report_yr, sex, num, denom, KPI_rate) %>%
    mutate(simd = simd_val)
}

# Create uptake tables for each sex, simd combination
# GC TO DO - could probably do this more simply with cleverer use of grouping

# Males
simd_1_male <- uptake_simd(1, "1", 1, "Male")
simd_2_male <- uptake_simd(2, "2", 1, "Male")
simd_3_male <- uptake_simd(3, "3", 1, "Male")
simd_4_male <- uptake_simd(4, "4", 1, "Male")
simd_5_male <- uptake_simd(5, "5", 1, "Male")
# simd_total_male <- uptake_simd(c(1:5), "Total", 1, "Male")
simd_all_male <- bind_rows(simd_1_male, simd_2_male, simd_3_male, simd_4_male,
                           simd_5_male)
# Females
simd_1_female <- uptake_simd(1, "1", 2, "Female")
simd_2_female <- uptake_simd(2, "2", 2, "Female")
simd_3_female <- uptake_simd(3, "3", 2, "Female")
simd_4_female <- uptake_simd(4, "4", 2, "Female")
simd_5_female <- uptake_simd(5, "5", 2, "Female")
# simd_total_female <- uptake_simd(c(1:5), "Total", 2, "Female")
simd_all_female <- bind_rows(simd_1_female, simd_2_female, simd_3_female, 
                             simd_4_female, simd_5_female)

# Total
simd_1_total <- uptake_simd(1, "1", c(1:2), "All Persons")
simd_2_total <- uptake_simd(2, "2", c(1:2), "All Persons")
simd_3_total <- uptake_simd(3, "3", c(1:2), "All Persons")
simd_4_total <- uptake_simd(4, "4", c(1:2), "All Persons")
simd_5_total <- uptake_simd(5, "5", c(1:2), "All Persons")
# simd_total_total <- uptake_simd(c(1:5), "Total", c(1:2), "All Persons")

simd_all_total <- bind_rows(simd_1_total, simd_2_total, simd_3_total, 
                            simd_4_total, simd_5_total)

# Combine all simd into one table

simd_all <- bind_rows(simd_all_male, simd_all_female, simd_all_total)


# SII/RII calculation for uptake
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
         rate_sqr_proppop = sqr_proportion_pop * KPI_rate) %>% 
  arrange(sex, report_yr, simd) 

# Creating one column called data with all the variables not in the grouping
# Calculating linear regression for all the groups, then formatting the results
sii_model <- data_depr_sii %>% 
  nest() %>%
  mutate(model = map(data, ~lm(rate_sqr_proppop ~ sqr_proportion_pop 
                               + relrank_sqr_proppop + 0, 
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
  gather("measure", "value", -report_yr, -sex, -overall_rate) %>%
  select(measure, sex, report_yr, value) %>%
  ungroup()

# Save file with RII/SII
saveRDS(rii_sii, file = here::here("Temp", "rii_sii_data.rds"))

