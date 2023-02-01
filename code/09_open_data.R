#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 09_0pen_data.R
# Thomas Godfrey
# Feb 2022
# Script 9 of 13
# Data preparation for export
# Written/run on R Studio Server
# R version 3.6.1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


##!! This script has not been updated with hbr19 & SIMD2020


### Step 0: Housekeeping ----

library(dplyr)
library(magrittr)
library(tidyr)
library(readxl)
library(here)
library(haven)
library(janitor)
library(lubridate)
library(tidylog)
library(labelled)
library(readr)
library(WriteXLS)


#   set filepaths and extract dates with script 0
source(here::here("Code","00_housekeeping.R"))
# wd <- paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening",
#              "/Publications/SBoSP-Statistics/20230221")
# source(paste0(wd, "/Code/00_housekeeping.R"))


### Step 1: Get data ----

# Bring in the analysis database created in script 1.
# temp_analysis_db_path <- (paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening/",
#                         "Publications/SBoSP-Statistics/20200204/Github2/Temp/",
#                           "analysis_dataset.rds"))
#analysis_db <- readRDS(temp_analysis_db_path)

analysis_db <- readRDS(analysis_db_path)


### Step 2: Filter and clean data ----
### 
## Filter on dates to the focal period of the time series:
# Remember to change the last date to end of most recent publication period
analysis_db <- analysis_db %>%
  filter(optin == 0 &
         hbr19 %in% 1:14 &
         invdate >= as.Date('2008-01-01') & invdate <= as.Date('2019-04-30')
)

# Select the data required and reformat postcode for matching
slim_db <- analysis_db %>%
  mutate(year = year(invdate)) %>%
  select(year, sex, age, age_group, patpcode, invdate, screres, 
         hbres19, hbr19, simd2009, simd2012, simd2016, 
         colperf, colcomp, cancer, adenoma,
         invite_n, uptake_n, positive_n, negative_n,
         col_perf_n, col_complete_n, cancer_n, adenoma_n,
         canc_col_n, adenoma_col_n, hr_adenoma_col_n) %>%
    mutate(postcode = gsub(" ", "", patpcode))
# 9,294,488

# Reformat sex to be text (Male/Female) rather than numbers.
slim_db$sex <- to_character(slim_db$sex, levels = "labels")
is.character(slim_db$sex)

# Reformat age_group to be text rather than numbers.
slim_db$age_group <- to_character(slim_db$age_group, levels = "labels")
is.character(slim_db$age_group)
# Consider whether to rename 70+ to 70-74 or 70-75
slim_db <- slim_db %>%
  mutate(age_group = if_else(age_group == "70+","70-74",slim_db$age_group))


### Step 3: Get and match on Health Board Area 2019 codes from lookup ----
# Originally I got the 'Health Board Area 2019 Lookup.csv' from here
# /conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Codes and Names/"
# and matched on the 2019 codes to hbr14. But this can't be right, 
# since the counts would still be HB2014 while the codes would be HB2019.
# Since hbres14 and alphabetical hbr2014 are created by matching patient records onto
# the postcode directory using 'pc7', see
# \\nssstats01\CancerGroup1\Topics\BowelScreening\Data\Programme\2019-11\Syntax\2Roll_out_BOSS_N19.syntax
# we can just do that here but for HB2019.
# Note: deprivation codes are matched on to 'pc7' seperately in the SPSS syntax, so can just use them as is.
# 
hb2019_code_lookup_db <- readRDS(paste0("/conf/linkage/output/lookups/Unicode/Geography/",
                                                "Scottish Postcode Directory/Scottish_Postcode_Directory_2019_2.rds")) %>%
                            select("pc7", "HB2019") %>%
                            mutate(postcode = gsub(" ", "", pc7)) %>%
                            select("postcode", "HB2019") %>%
                            clean_names()

glimpse(hb2019_code_lookup_db)

## Join these togther
slim_db <- left_join(slim_db, hb2019_code_lookup_db, by = "postcode") %>%
                filter(!is.na(hb2019))
# matched rows 9,284,476; removed 10,012 rows



#########
# Originn
#hb_code_lookup_db <- read_csv(paste0("/conf/linkage/output/lookups/Unicode/Geography/",
#                                    "Scottish Postcode Directory/Codes and Names/",
#                                    "Health Board Area 2019 Lookup.csv"))

# Careful: requires that sorting of names alphabetically worked correctly.
#hb_code_lookup_db <- hb_code_lookup_db %>% arrange(HealthBoardArea2019Name)
#hb_code_lookup_db <- cbind(hb_code_lookup_db,  hbr14 = c(1:14))
#hb_code_lookup_db

## Reformat data so they are the same type for matching
# Make sure both are tibbles
#hb_code_lookup_db <- as_tibble(hb_code_lookup_db)
#slim_db <- as_tibble(slim_db)
# Make both of them character format
#hb_code_lookup_db$hbr14 <- as.character(hb_code_lookup_db$hbr14)
#slim_db$hbr14 <- as.character(slim_db$hbr14)
# Match on HB codes
#slim_db <- left_join(slim_db, hb_code_lookup_db, by = "hbr14")
# matched rows 9,294,488


## Create a unified deprivation variable from all of the SIMD variables.
# Is this the right place to filter those without a deprivation score? 
# If we do it here the counts for HBs and demography datasets should align
# with each other, tho not neccessarily with our published stats.
slim_db$simd2009 <- as.character(slim_db$simd2009)
slim_db$simd2012 <- as.character(slim_db$simd2012)
slim_db$simd2016 <- as.character(slim_db$simd2016)

slim_db <- slim_db %>%
            mutate(DeprivationQuintile =  case_when(
              year %in% c(2008,2009) ~ simd2009,
              year %in% c(2010,2011,2012,2013) ~ simd2012,
              year %in% c(2014,2015,2016,2017,2018,2019) ~ simd2016,
              TRUE ~ 'other')) 

slim_db %>% group_by(DeprivationQuintile) %>% count()
# There are no 'others', but there are NAs.

slim_db <- slim_db %>%
            filter(!is.na(DeprivationQuintile))
# removed 16,731 rows (<1%), 9,267,745 rows remaining

slim_db %>% group_by(DeprivationQuintile) %>% count()
slim_db %>% group_by(sex) %>% count()
slim_db %>% group_by(age_group) %>% count()
  

#### Part 1: Provide a time series of data counts/rates in the two-year period format ----
# using the dates of the latest publication to choose the focal dates May-April or Nov-Oct
# This requires functions to be created (as per the previous time series script)

# Create a two_year database with each persons presence in each two year period flagged as 1 or 0
two_year_db <- slim_db %>%
  filter(invdate >= as.Date('2010-05-01') & invdate <= as.Date('2019-04-30')) %>%

# Create two-year period definitions
mutate(
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
                      1, 0)
  )


# 1a)  First, count up data by Health Board 
# to create a time series of BowelScreeningCaseNumbersByHealthBoard in two-year periods

# We need to do this first for Heath Boards and then for Scotland so this is a long section.

# First, define a function to calculate numerator, denominator and rates by Health Board
# TG TO DO - learn about tidy evaluation and how to include multiple varables in function, select, and group_by
# probably requires seperate function arguments for select and group_by
# https://dplyr.tidyverse.org/articles/programming.html
# https://adv-r.hadley.nz/metaprogramming.html
# https://tidyeval.tidyverse.org/


ts_hb_ratefunction <- function(denominator, numerator, by) {
  
  ts_object <- two_year_db %>%
    mutate(
      # Denominator by year
      denom_10_12 = year_10_12 * !! sym(denominator),
      denom_11_13 = year_11_13 * !! sym(denominator),
      denom_12_14 = year_12_14 * !! sym(denominator),
      denom_13_15 = year_13_15 * !! sym(denominator),
      denom_14_16 = year_14_16 * !! sym(denominator),
      denom_15_17 = year_15_17 * !! sym(denominator),
      denom_16_18 = year_16_18 * !! sym(denominator),
      denom_17_19 = year_17_19 * !! sym(denominator),
      # Numerator by year
      num_10_12 = year_10_12 * !! sym(numerator),
      num_11_13 = year_11_13 * !! sym(numerator),
      num_12_14 = year_12_14 * !! sym(numerator),
      num_13_15 = year_13_15 * !! sym(numerator),
      num_14_16 = year_14_16 * !! sym(numerator),
      num_15_17 = year_15_17 * !! sym(numerator),
      num_16_18 = year_16_18 * !! sym(numerator),
      num_17_19 = year_17_19 * !! sym(numerator)) %>%
    select(!!sym(by), denom_10_12:num_17_19) 
  
  ts_output <- ts_object %>%
    pivot_longer(cols = c(denom_10_12:num_17_19), names_to = "period", values_to = "n") %>%
    group_by(!!sym(by), period) %>%
    summarise(n = sum(n)) %>%
    ungroup() %>% 
    separate(period, c("metric","year")) %>%
    # Create 2-year period level in formay YYYY/YY
    mutate(
      year2 = as.character(as.numeric(year) + 2),
      report_yr = as.character(paste0("20", year,"/", year2))) %>%
    pivot_wider(names_from = metric, values_from = n) %>% 
    mutate(KPI_rate = (num/denom)*100) %>%
    select(!!sym(by), report_yr, denom, num, KPI_rate)
  
  output <- ts_output
}

# Then define a function to count a single variable by Health Board
ts_hb_countfunction <- function(variable_to_count, by) {
  
  ts_object <- two_year_db %>%
    mutate(
      count_10_12 = year_10_12 * !! sym(variable_to_count),
      count_11_13 = year_11_13 * !! sym(variable_to_count),
      count_12_14 = year_12_14 * !! sym(variable_to_count),
      count_13_15 = year_13_15 * !! sym(variable_to_count),
      count_14_16 = year_14_16 * !! sym(variable_to_count),
      count_15_17 = year_15_17 * !! sym(variable_to_count),
      count_16_18 = year_16_18 * !! sym(variable_to_count),
      count_17_19 = year_17_19 * !! sym(variable_to_count)) %>%
      select(!!sym(by), count_10_12:count_17_19)
   
  ts_output <- ts_object %>%
    pivot_longer(cols = starts_with("count_"), names_to = "period", values_to = "n") %>%
    group_by(!!sym(by), period) %>%
    summarise(n = sum(n)) %>%
    ungroup() %>% 
    separate(period, c("metric","year")) %>%
    # Create 2-year period level in formay YYYY/YY
    mutate(
      year2 = as.character(as.numeric(year) + 2),
      report_yr = as.character(paste0("20", year,"/", year2))) %>% 
    select(!!sym(by), report_yr, n) %>%
    ungroup()
  
  output <- ts_output
}

### Count data into variables and then merge these together
#Invited
#Screened
#UptakePercentage
#TestPositive
#Positivity
#ColonoscopiesPerformed
#ColonoscopiesCompleted

# Calculate invited, screened, uptake 
invited_screened_uptake <- ts_hb_ratefunction('invite_n','uptake_n','hb2019') %>%
    rename(Invited = denom,
          Screened = num,
          UptakePercentage = KPI_rate) %>%
    select(hb2019, report_yr, Invited, Screened, UptakePercentage)


postive_positivity <- ts_hb_ratefunction('uptake_n','positive_n','hb2019') %>%
    rename(Screened = denom,
        TestPositive = num,
        Positivity = KPI_rate) %>%
    select(hb2019, report_yr, TestPositive, Positivity)


colperf <- ts_hb_countfunction('col_perf_n','hb2019') %>%
              rename(ColonoscopiesPerformed = n) %>%
              select(hb2019, report_yr, ColonoscopiesPerformed)


colcomp <- ts_hb_countfunction('col_complete_n', 'hb2019') %>%
              rename(ColonoscopiesCompleted = n) %>%
              select(hb2019, report_yr, ColonoscopiesCompleted)

hb_ts_data <- left_join(invited_screened_uptake, postive_positivity, by = c('hb2019', 'report_yr')) %>%
                left_join(., colperf, by = c('hb2019', 'report_yr')) %>%
                left_join(., colcomp, by = c('hb2019', 'report_yr'))

hb_ts_data <- hb_ts_data %>%
  rename(Period = report_yr,
         HBR = hb2019) %>%
         mutate(HBRQF = '') %>%
  select(Period, HBR, HBRQF, Invited, Screened, UptakePercentage, 
         TestPositive, Positivity, ColonoscopiesPerformed, ColonoscopiesCompleted)

glimpse(hb_ts_data)



##  Then calculate the numbers for Scotland and add them in as a derived variable.
# These functions are the same as above but with by/hbr14 removed.
ts_scot_ratefunction <- function(denominator, numerator) {
  
  ts_object <- two_year_db %>%
    mutate(
      # Denominator by year
      denom_10_12 = year_10_12 * !! sym(denominator),
      denom_11_13 = year_11_13 * !! sym(denominator),
      denom_12_14 = year_12_14 * !! sym(denominator),
      denom_13_15 = year_13_15 * !! sym(denominator),
      denom_14_16 = year_14_16 * !! sym(denominator),
      denom_15_17 = year_15_17 * !! sym(denominator),
      denom_16_18 = year_16_18 * !! sym(denominator),
      denom_17_19 = year_17_19 * !! sym(denominator),
      # Numerator by year
      num_10_12 = year_10_12 * !! sym(numerator),
      num_11_13 = year_11_13 * !! sym(numerator),
      num_12_14 = year_12_14 * !! sym(numerator),
      num_13_15 = year_13_15 * !! sym(numerator),
      num_14_16 = year_14_16 * !! sym(numerator),
      num_15_17 = year_15_17 * !! sym(numerator),
      num_16_18 = year_16_18 * !! sym(numerator),
      num_17_19 = year_17_19 * !! sym(numerator)) %>%
    select(denom_10_12:num_17_19) 
  
  ts_output <- ts_object %>%
    pivot_longer(cols = c(denom_10_12:num_17_19), names_to = "period", values_to = "n") %>%
    group_by(period) %>%
    summarise(n = sum(n)) %>%
    ungroup() %>% 
    separate(period, c("metric","year")) %>%
    # Create 2-year period level in formay YYYY/YY
    mutate(
      year2 = as.character(as.numeric(year) + 2),
      report_yr = as.character(paste0("20", year,"/", year2))) %>%
    pivot_wider(names_from = metric, values_from = n) %>% 
    mutate(KPI_rate = (num/denom)*100) %>%
    select(report_yr, denom, num, KPI_rate)
  
  output <- ts_output
}

# Then define a function to count a single variable by Health Board
ts_scot_countfunction <- function(variable_to_count) {
  
  ts_object <- two_year_db %>%
    mutate(
      count_10_12 = year_10_12 * !! sym(variable_to_count),
      count_11_13 = year_11_13 * !! sym(variable_to_count),
      count_12_14 = year_12_14 * !! sym(variable_to_count),
      count_13_15 = year_13_15 * !! sym(variable_to_count),
      count_14_16 = year_14_16 * !! sym(variable_to_count),
      count_15_17 = year_15_17 * !! sym(variable_to_count),
      count_16_18 = year_16_18 * !! sym(variable_to_count),
      count_17_19 = year_17_19 * !! sym(variable_to_count)) %>%
    select(count_10_12:count_17_19)
  
  ts_output <- ts_object %>%
    pivot_longer(cols = starts_with("count_"), names_to = "period", values_to = "n") %>%
    group_by(period) %>%
    summarise(n = sum(n)) %>%
    ungroup() %>% 
    separate(period, c("metric","year")) %>%
    # Create 2-year period level in formay YYYY/YY
    mutate(
      year2 = as.character(as.numeric(year) + 2),
      report_yr = as.character(paste0("20", year,"/", year2))) %>% 
    select(report_yr, n) %>%
    ungroup()
  
  output <- ts_output
}

### Count data into variables and then merge these together
#Invited
#Screened
#UptakePercentage
#TestPositive
#Positivity
#ColonoscopiesPerformed
#ColonoscopiesCompleted

# Calculate invited, screened, uptake 
invited_screened_uptake <- ts_scot_ratefunction('invite_n','uptake_n') %>%
  rename(Invited = denom,
         Screened = num,
         UptakePercentage = KPI_rate) %>%
  select(report_yr, Invited, Screened, UptakePercentage)


postive_positivity <- ts_scot_ratefunction('uptake_n','positive_n') %>%
  rename(Screened = denom,
         TestPositive = num,
         Positivity = KPI_rate) %>%
  select(report_yr, TestPositive, Positivity)


colperf <- ts_scot_countfunction('col_perf_n') %>%
  rename(ColonoscopiesPerformed = n) %>%
  select(report_yr, ColonoscopiesPerformed)


colcomp <- ts_scot_countfunction('col_complete_n') %>%
  rename(ColonoscopiesCompleted = n) %>%
  select(report_yr, ColonoscopiesCompleted)

scot_ts_data <- left_join(invited_screened_uptake, postive_positivity, by = 'report_yr') %>%
  left_join(., colperf, by = 'report_yr') %>%
  left_join(., colcomp, by = 'report_yr')


# Reformat for mergeing and display - add Scotland Country code.
scot_ts_data <- scot_ts_data %>%
  rename(Period = report_yr) %>%
  mutate(HBR = 'S92000003',
         HBRQF = 'd') %>%
  select(Period, HBR, HBRQF, 
         Invited, Screened, UptakePercentage, 
         TestPositive, Positivity,
         ColonoscopiesPerformed, ColonoscopiesCompleted)

scot_ts_data$HBR <- as.character(scot_ts_data$HBR)
names(scot_ts_data)

HB_Scotland_two_year_ts_for_opendata <- full_join(hb_ts_data, scot_ts_data) %>% 
  arrange(Period, HBR, HBRQF)

HB_Scotland_two_year_ts_for_opendata <- HB_Scotland_two_year_ts_for_opendata %>%
                                        mutate(UptakePercentage = round(UptakePercentage, digits=3),
                                               Positivity = round(Positivity, digits=3))

glimpse(HB_Scotland_two_year_ts_for_opendata)


# Save the data out as a csv
write.csv(HB_Scotland_two_year_ts_for_opendata, here::here('Temp', "Bowel_opendata_2_years_by_healthboard.csv"))


i got t here
# 1b) Second, count up data by demography 
# to create a timer series of BowelScreeningCaseNumbersByDemography in two-year periods

# First, define a function to calculate numerator, denominator and rates by sex, age_group and deprivation
# TG TO DO - As above
ts_dem_ratefunction <- function(denominator, numerator) {
  
  ts_object <- two_year_db %>%
    mutate(
      # Denominator by year
      denom_10_12 = year_10_12 * !! sym(denominator),
      denom_11_13 = year_11_13 * !! sym(denominator),
      denom_12_14 = year_12_14 * !! sym(denominator),
      denom_13_15 = year_13_15 * !! sym(denominator),
      denom_14_16 = year_14_16 * !! sym(denominator),
      denom_15_17 = year_15_17 * !! sym(denominator),
      denom_16_18 = year_16_18 * !! sym(denominator),
      denom_17_19 = year_17_19 * !! sym(denominator),
      # Numerator by year
      num_10_12 = year_10_12 * !! sym(numerator),
      num_11_13 = year_11_13 * !! sym(numerator),
      num_12_14 = year_12_14 * !! sym(numerator),
      num_13_15 = year_13_15 * !! sym(numerator),
      num_14_16 = year_14_16 * !! sym(numerator),
      num_15_17 = year_15_17 * !! sym(numerator),
      num_16_18 = year_16_18 * !! sym(numerator),
      num_17_19 = year_17_19 * !! sym(numerator)) %>%
    select(sex, age_group, DeprivationQuintile, denom_10_12:num_17_19) 
  
  ts_output <- ts_object %>%
    pivot_longer(cols = c(denom_10_12:num_17_19), names_to = "period", values_to = "n") %>%
    group_by(sex, age_group, DeprivationQuintile, period) %>%
    summarise(n = sum(n)) %>%
    ungroup() %>% 
    separate(period, c("metric","year")) %>%
    # Create 2-year period level in formay YYYY/YY
    mutate(
      year2 = as.character(as.numeric(year) + 2),
      report_yr = as.character(paste0("20", year,"/", year2))) %>%
    pivot_wider(names_from = metric, values_from = n) %>% 
    mutate(KPI_rate = (num/denom)*100) %>%
    select(sex, age_group, DeprivationQuintile, report_yr, denom, num, KPI_rate)
  
  output <- ts_output
}

# Then define a function to count a single variable by Health Board
ts_dem_countfunction <- function(variable_to_count) {
  
  ts_object <- two_year_db %>%
    mutate(
      count_10_12 = year_10_12 * !! sym(variable_to_count),
      count_11_13 = year_11_13 * !! sym(variable_to_count),
      count_12_14 = year_12_14 * !! sym(variable_to_count),
      count_13_15 = year_13_15 * !! sym(variable_to_count),
      count_14_16 = year_14_16 * !! sym(variable_to_count),
      count_15_17 = year_15_17 * !! sym(variable_to_count),
      count_16_18 = year_16_18 * !! sym(variable_to_count),
      count_17_19 = year_17_19 * !! sym(variable_to_count)) %>%
    select(sex, age_group, DeprivationQuintile, count_10_12:count_17_19)
  
  ts_output <- ts_object %>%
    pivot_longer(cols = starts_with("count_"), names_to = "period", values_to = "n") %>%
    group_by(sex, age_group, DeprivationQuintile, period) %>%
    summarise(n = sum(n)) %>%
    ungroup() %>% 
    separate(period, c("metric","year")) %>%
    # Create 2-year period level in formay YYYY/YY
    mutate(
      year2 = as.character(as.numeric(year) + 2),
      report_yr = as.character(paste0("20", year,"/", year2))) %>% 
    select(sex, age_group, DeprivationQuintile, report_yr, n) %>%
    ungroup()
  
  output <- ts_output
}

### Count
#Invited
#Screened
#UptakePercentage
#TestPositive
#Positivity
#ColonoscopiesPerformed
#ColonoscopiesCompleted


# Calculate invited, screened, uptake 
invited_screened_uptake <- ts_dem_ratefunction('invite_n','uptake_n') %>%
  rename(Invited = denom,
         Screened = num,
         UptakePercentage = KPI_rate) %>%
  select(report_yr, sex, age_group, DeprivationQuintile, Invited, Screened, UptakePercentage)

# Calculate no. of postive tests and positivity
postive_positivity <- ts_dem_ratefunction('uptake_n','positive_n') %>%
  rename(Screened = denom,
         TestPositive = num,
         Positivity = KPI_rate) %>%
  select(report_yr, sex, age_group, DeprivationQuintile, TestPositive, Positivity)

# Calculate no. of colonoscopies performed 
colperf <- ts_dem_countfunction('col_perf_n') %>%
  rename(ColonoscopiesPerformed = n) %>%
  select(report_yr, sex, age_group, DeprivationQuintile, ColonoscopiesPerformed)

# Calculate no. of colonoscopies completed
colcomp <- ts_dem_countfunction('col_complete_n') %>%
  rename(ColonoscopiesCompleted = n) %>%
  select(report_yr, sex, age_group, DeprivationQuintile, ColonoscopiesCompleted)

# Join all these files together
dem_ts_data <- left_join(invited_screened_uptake, postive_positivity, 
                        by = c('report_yr','sex','age_group','DeprivationQuintile')) %>%
  left_join(., colperf, by = c('report_yr','sex','age_group','DeprivationQuintile')) %>%
  left_join(., colcomp, by = c('report_yr','sex','age_group','DeprivationQuintile'))
dem_ts_data 


##### Reformat the data and merge on the scotland code
dem_ts_data <- dem_ts_data %>%
                  rename(Period = report_yr,
                         Sex = sex,
                         AgeGroup = age_group) %>%
                  mutate(Country = 'S92000003') %>%
    select(Country, Period, Sex, AgeGroup,DeprivationQuintile, Invited, Screened, UptakePercentage, 
         TestPositive, Positivity, ColonoscopiesPerformed, ColonoscopiesCompleted)

glimpse(dem_ts_data)


dem_ts_data <- dem_ts_data %>%
                mutate(UptakePercentage = round(UptakePercentage, digits = 3),
                       Positivity = round(Positivity, digits = 3))
  
# Save the data out as a csv
write.csv(dem_ts_data, here::here('Temp', "Bowel_opendata_2_years_by_demography.csv"))



#### Part 2 - Annual year-by-year time series - no functions required ----

glimpse(slim_db)

# Filter to focal time period for annual data series.
slim_db <- slim_db %>%
  filter(year > 2009 & year < 2019 )
# 2a) Bowel screening case numbes by Health Board
# NHS Board 2014 only (not 2006 or 2019)

# Count up the numbers and calculate rates by year and Health Board 2019
numbers_by_hb <- slim_db %>%
  group_by(year, hb2019) %>%
  summarise(Invited = sum(invite_n),
            Screened = sum(uptake_n),
            UptakePercentage = (Screened/Invited)*100,
            TestPositive = sum(positive_n),
            Positivity = (TestPositive/Screened)*100,
            ColonoscopiesPerformed = sum(col_perf_n),
            ColonoscopiesCompleted = sum(col_complete_n)) %>%
  ungroup()

# Reformat for display
numbers_by_hb <- numbers_by_hb %>%
          rename(Year = year, 
                 HBR = hb2019) %>%
          mutate(HBRQF = '') %>%
          select(Year, HBR, HBRQF, 
                 Invited, Screened, UptakePercentage, 
                 TestPositive, Positivity,
                 ColonoscopiesPerformed, ColonoscopiesCompleted)  

numbers_by_hb$HBR <- as.character(numbers_by_hb$HBR)
names(numbers_by_hb)
  

## Then calculate the numbers for Scotland and add them in as a derived variable.

# Count up the numbers and calculate rates by year and Health Board 2019
numbers_for_scotland <- slim_db %>%
  group_by(year) %>%
  summarise(Invited = sum(invite_n),
            Screened = sum(uptake_n),
            UptakePercentage = (Screened/Invited)*100,
            TestPositive = sum(positive_n),
            Positivity = (TestPositive/Screened)*100,
            ColonoscopiesPerformed = sum(col_perf_n),
            ColonoscopiesCompleted = sum(col_complete_n)) %>%
  ungroup()

# Reformat for display
numbers_for_scotland <- numbers_for_scotland %>%
  rename(Year = year) %>%
  mutate(HBR = 'S92000003',
         HBRQF = 'd') %>%
  select(Year, HBR, HBRQF, 
         Invited, Screened, UptakePercentage, 
         TestPositive, Positivity,
         ColonoscopiesPerformed, ColonoscopiesCompleted)

numbers_for_scotland$HBR <- as.character(numbers_for_scotland$HBR)
names(numbers_for_scotland)

HB_Scotland_numbers_for_opendata <- full_join(numbers_by_hb, numbers_for_scotland) %>% 
                                    arrange(Year,HBR,HBRQF)
                            

HB_Scotland_numbers_for_opendata <- HB_Scotland_numbers_for_opendata %>%
                                      mutate(UptakePercentage = round(UptakePercentage, digits = 3),
                                      Positivity = round(Positivity, digits = 3))

# Save final outputs
write.csv(HB_Scotland_numbers_for_opendata , here::here('Temp',"Bowel_opendata_annual_by_healthboard.csv"))


# 2b) Bowel screening case numbers by demography


# Count up the numbers and calculate rates by sex, age_group and deprivation
numbers_by_dem <- slim_db %>%
  group_by(year, sex, age_group, DeprivationQuintile) %>%
  summarise(Invited = sum(invite_n),
            Screened = sum(uptake_n),
            UptakePercentage = (Screened/Invited)*100,
            TestPositive = sum(positive_n),
            Positivity = (TestPositive/Screened)*100,
            ColonoscopiesPerformed = sum(col_perf_n),
            ColonoscopiesCompleted = sum(col_complete_n)) %>%
  ungroup() %>%
  filter(!is.na(DeprivationQuintile))


# Reformat for display
numbers_by_dem <- numbers_by_dem %>% arrange(year,sex,age_group,DeprivationQuintile) %>%
                    rename(Year = year, Sex = sex, AgeGroup = age_group)  %>%
                    mutate(Country = 'S92000003') %>%
  select(Year, Country, Sex, AgeGroup,DeprivationQuintile,
         Invited, Screened, UptakePercentage, 
         TestPositive, Positivity,
         ColonoscopiesPerformed, ColonoscopiesCompleted)

numbers_by_dem <- numbers_by_dem  %>%
                    mutate(UptakePercentage = round(UptakePercentage, digits = 3),
                           Positivity = round(Positivity, digits = 3))

# Save final outputs
write.csv(numbers_by_dem, here::here('Temp',"Bowel_opendata_annual_by_demography.csv"))


