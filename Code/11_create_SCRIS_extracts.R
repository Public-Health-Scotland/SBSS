#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 11_create_SCRIS_extracts.R
# Calum Purdie
# Nov 2022
# Script 11 of 13
# Data extraction/preparation
# Written/run on R Studio Server
# R version 3.6.1
# This script creates three output files for use in the SCRIS dashboard
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### 1 Housekeeping ----

# Define working directory - only needs run if you're not in a project

# setwd(paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening/Publications/", 
#              "SBoSP-Statistics/20220805"))

# Set filepaths

output_path <- paste0("/PHI_conf/CancerGroup1/Topics/CancerStatistics/Projects", 
                      "/20170601-SCRIS/Dashboard")

# loading packages

library(dplyr)
library(haven)
library(sjlabelled)
library(janitor)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)
library(tidylog)
library(here)

# Define todays date

todays_date <- strftime(today(), "%Y%m%d")

# Define current data extract folder name

extract_folder <- "2023-05"

sbsdb_path <- paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening/Data/",
                      "Programme/", extract_folder, 
                     "/combined_extract_all.rds")

# Define functions

summarise_grouped_vars <- function(df, ...){
  
  output <- df %>% 
    group_by(...) %>% 
    summarise(across(c(invite_n, uptake_n, positive_n), ~ sum(.))) %>% 
    ungroup()
  
}



### 2 Extract Data ----

# Read in latest Scottish Postcode Directory
# Get distinct health board codes and values and rename columns

spd <- read_rds(paste0("/conf/linkage/output/lookups/Unicode/Geography/", 
                       "Scottish Postcode Directory/", 
                       "Scottish_Postcode_Directory_2023_1.rds")) %>% 
  distinct(hb2019, hb2019name) %>% 
  rename(hbcode = hb2019, hbr19 = hb2019name)

# Read in FS_combined_extract file
# Select people who had FS performed, because we can not be sure about the 
# outcome for the rest of participants due to incomplete FS data 
# (e.g. no Grampian data at all) .
# Clean names and remove SPSS formatting

fs_combined_extract <- read_rds(paste0("/PHI_conf/CancerGroup1/Topics/", 
                                       "BowelScreening/Projects/", 
                                       "20110101-Flexi-Sig/Data/4June_2016/", 
                                       "FS_combined_extract.rds")) %>% #, 
                                #col_select = c(CHINUM, FSPERF, STUDYARM, 
                                #               FSRESULT))# %>% 
  select(CHINUM, FSPERF, STUDYARM,FSRESULT) %>% 
  filter(FSRESULT == "01") %>% 
  clean_names() %>% 
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels()

# Read in latest bowel extract

bowel_data <- read_rds(sbsdb_path) %>% 
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels() %>% 
  arrange(datecolperf)

# Take data which has a hbr19 value between 1 and 14 and is not an optin
# Define year, sex and hbr19 variables

bowel_data <- bowel_data %>% 
  filter(hbr19 %in% c(1:14) & optin == 0) %>% 
  mutate(year = year(invdate), 
         sex = as.character(sex), 
         sex = case_when(sex == "1" ~ "Males", 
                         sex == "2" ~ "Females"), 
         hbr19 = case_when(hbr19 == 1 ~ "NHS Ayrshire and Arran", 
                           hbr19 == 2 ~ "NHS Borders", 
                           hbr19 == 3 ~ "NHS Dumfries and Galloway", 
                           hbr19 == 4 ~ "NHS Fife", 
                           hbr19 == 5 ~ "NHS Forth Valley", 
                           hbr19 == 6 ~ "NHS Grampian", 
                           hbr19 == 7 ~ "NHS Greater Glasgow and Clyde", 
                           hbr19 == 8 ~ "NHS Highland", 
                           hbr19 == 9 ~ "NHS Lanarkshire", 
                           hbr19 == 10 ~ "NHS Lothian", 
                           hbr19 == 11 ~ "NHS Orkney", 
                           hbr19 == 12 ~ "NHS Shetland", 
                           hbr19 == 13 ~ "NHS Tayside", 
                           hbr19 == 14 ~ "NHS Western Isles"))

# Define networks based on hbr19 values and a simd value based on year
# Define invite, uptake, positive and negative
# Recode simd to add in most/least deprived quintile
# Select required columns to reduce dataset size

bowel_data <- bowel_data %>% 
  mutate(network = case_when(hbr19 %in% c("NHS Grampian", "NHS Highland", 
                                          "NHS Orkney", "NHS Shetland", 
                                          "NHS Tayside", 
                                          "NHS Western Isles") ~ "NCA", 
                             hbr19 %in% c("NHS Borders", 
                                          "NHS Dumfries and Galloway", "NHS Fife", 
                                          "NHS Lothian") ~ "SCAN", 
                             hbr19 %in% c("NHS Ayrshire and Arran", 
                                          "NHS Forth Valley", 
                                          "NHS Greater Glasgow and Clyde", 
                                          "NHS Lanarkshire") ~ "WOSCAN"), 
         simd = case_when(year %in% c(2007:2009) ~ simd2009, 
                          year %in% c(2010:2013) ~ simd2012,
                          year %in% c(2014:2016) ~ simd2016, 
                          year >= 2017 ~ simd2020), 
         age_group = case_when(age_group == 1 ~ "50-54", 
                               age_group == 2 ~ "55-59", 
                               age_group == 3 ~ "60-64", 
                               age_group == 4 ~ "65-69", 
                               age_group == 5 ~ "70-75"), 
         invite_n = if_else(screres %in% c(1:18, 21:24), 1, 0),
         uptake_n = if_else(screres %in% c(1, 3, 4, 5, 6, 7, 8, 21, 22), 1, 0),
         positive_n = if_else(screres %in% c(3, 5, 6, 8, 22), 1, 0),
         negative_n = if_else(screres %in% c(1, 4, 7, 21), 1, 0)) %>% 
  mutate(simd = case_when(simd == 1 ~ "1 most deprived quintile", 
                          simd == 5 ~ "5 least deprived quintile", 
                          TRUE ~ as.character(simd))) %>% 
  select(chinum, invdate, patsname, patfname, sex, screres, hbr19, simd, 
         age_group, year, network, invite_n, uptake_n, positive_n, negative_n)

# Filter to remove small numbers of rows where chinum is infs_combined_extract
# with an invdate between 2014-06-01 and 2015-12-31 and where sceres value is
# not in specified range

bowel_data <- bowel_data %>% 
  filter(!(chinum %in% fs_combined_extract$chinum & 
             (invdate >= "2014-06-01" & invdate <= "2015-12-31") &
             !(screres %in% c(1, 3, 4, 5, 6, 7, 8))
          )
        )

# Remove fs_combined_extract as no longer required

rm(fs_combined_extract)



### 3 Level 1 ----

# Select columns required for level one

level_one_data <- bowel_data %>% 
  select(year, sex, hbr19, network, screres, invite_n, uptake_n, positive_n)

# Group data by year, sex, hbr19, network
# Summarise number of invites, uptake and positives by group
# This creates base counts to use for analysis

level_one_counts <- summarise_grouped_vars(level_one_data, 
                                           year, sex, hbr19, network)

# Group level_one_counts by year, hbr19, network
# Summarise number of invites, uptake and positives by group
# Define sex as "All persons" for each row

level_one_sex_counts <- summarise_grouped_vars(level_one_counts, 
                                               year, hbr19, network) %>% 
  mutate(sex = "All persons")

# Bind level_one_counts onto level_one_sex_counts
# This gives us counts for men, women and all

level_one_sex_counts <- level_one_sex_counts %>% 
  bind_rows(level_one_counts)

# Group level_one_sex_counts by year, sex, network
# Summarise number of invites, uptake and positives by group
# Define hbr19 as network for each row
# This gives network level counts

level_one_network_counts <- summarise_grouped_vars(level_one_sex_counts, 
                                                   year, sex, network) %>% 
  mutate(hbr19 = network)

# Group level_one_sex_counts by year, sex
# Summarise number of invites, uptake and positives by group
# Define hbr19 as Scotland and network as 0 for each row
# This gives Scotland level counts

level_one_scotland_counts <- summarise_grouped_vars(level_one_sex_counts, 
                                                    year, sex) %>% 
  mutate(hbr19 = "Scotland", 
         network = "0")

# Bind level_one_sex_counts, level_one_network_counts and
# level_one_scotland_counts
# Calculate Uptake and Positivity and pivot data into wider format
# Calculate KPI_denominator and KPI_numerator for Uptake and Positivity
# Select required columns and created Scotland variable set as Scotland for all

level_one <- bind_rows(level_one_sex_counts, 
                       level_one_network_counts, 
                       level_one_scotland_counts) %>% 
  mutate(Uptake = uptake_n * 100 / invite_n,
         Positivity = positive_n * 100 / uptake_n) %>%
  pivot_longer(cols = c(Uptake, Positivity), names_to = "indicator", 
               values_to = "KPI_rate") %>% 
  mutate(KPI_denominator = case_when(indicator == "Uptake" ~ invite_n, 
                                     indicator == "Positivity" ~ uptake_n), 
         KPI_numerator = case_when(indicator == "Uptake" ~ uptake_n, 
                                   indicator == "Positivity" ~ positive_n)) %>% 
  select(year, sex, hbr19, KPI_denominator, KPI_numerator, KPI_rate, indicator, 
         network) %>% 
  mutate(Scotland = "Scotland")

# Filter for Scotland rows and select columns
# Rename columns to Scotland output where required

level_one_scotland_output <- level_one %>% 
  filter(hbr19 == "Scotland") %>% 
  select(year, sex, KPI_denominator, KPI_numerator, KPI_rate, indicator) %>% 
  rename_with(~ str_c("Scotland_", .), contains("KPI"))

# Filter for network rows and select columns
# Rename columns to network output where required

level_one_network_output <- level_one %>% 
  filter(hbr19 == network) %>% 
  select(year, sex, KPI_denominator, KPI_numerator, KPI_rate, indicator, 
         network) %>% 
  rename_with(~ str_c("network_", .), contains("KPI"))

# Join level_one_scotland_output and level_one_network_output to level_one
# Set any blanks in numeric columns to 0
# Join on SPD and define Hb_Map as hbr19 without NHS prefix
# Also update hbcode to use hbr19 to fill in any blanks
# This means we have the S08 codes for HBs and the network name for networks

level_one_output <- level_one %>% 
  left_join(level_one_scotland_output) %>% 
  left_join(level_one_network_output) %>% 
  left_join(spd) %>% 
  mutate(hbmap = gsub("NHS ", "", hbr19), 
         hbcode = coalesce(hbcode, hbr19))

# Remove objects that are no longer required

rm(level_one_data, level_one_counts, level_one_sex_counts, 
   level_one_network_counts, level_one_scotland_counts, level_one, 
   level_one_scotland_output, level_one_network_output)



### 4 Level 2 ----

# Select columns required for level two

level_two_data <- bowel_data %>% 
  select(year, sex, hbr19, network, screres, simd, age_group, invite_n, 
         uptake_n, positive_n)

# Group data by year, sex, hbr19, network, age_group, simd
# Summarise number of invites, uptake and positives by group
# This creates base counts to use for analysis

level_two_counts <- summarise_grouped_vars(level_two_data, 
                                           year, sex, hbr19, network, age_group, 
                                           simd)

# Group level_two_counts by year, hbr19, network, age_group, simd
# Summarise number of invites, uptake and positives by group
# Define sex as "All persons" for each row

level_two_sex_counts <- summarise_grouped_vars(level_two_counts, 
                                               year, hbr19, network, age_group, 
                                               simd) %>% 
  mutate(sex = "All persons")

# Bind level_two_counts onto level_two_sex_counts
# This gives us counts for men, women and all

level_two_sex_counts <- level_two_sex_counts %>% 
  bind_rows(level_two_counts)

# Group level_two_sex_counts by year, sex, network, age_group, simd
# Summarise number of invites, uptake and positives by group
# Define hbr19 as network for each row
# This gives network level counts

level_two_network_counts <- summarise_grouped_vars(level_two_sex_counts, 
                                                   year, sex, network, 
                                                   age_group, simd) %>% 
  mutate(hbr19 = network)

# Group level_two_sex_counts by year, sex, age_group, simd
# Summarise number of invites, uptake and positives by group
# Define hbr19 as Scotland and network as 0 for each row
# This gives Scotland level counts

level_two_scotland_counts <- summarise_grouped_vars(level_two_sex_counts, 
                                                    year, sex, age_group, 
                                                    simd) %>% 
  mutate(hbr19 = "Scotland", 
         network = "0")

# Bind level_two_sex_counts, level_two_network_counts and
# level_two_scotland_counts

level_two <- bind_rows(level_two_sex_counts, 
                       level_two_scotland_counts, 
                       level_two_network_counts)

# Group level_two by year, sex, hbr19, network, simd
# Summarise number of invites, uptake and positives by group
# Define age_group as All ages
# This gives age_group level counts

level_two_age_counts <- summarise_grouped_vars(level_two, 
                                               year, sex, hbr19, network, 
                                               simd) %>% 
  mutate(age_group = "All ages")

# Bind level_two and level_two_age_counts

level_two <- level_two %>% 
  bind_rows(level_two_age_counts)

# Group level_two by year, sex, hbr19, network, age_group
# Summarise number of invites, uptake and positives by group
# Define simd as All SIMD quintiles combined
# This gives simd level counts

level_two_simd_counts <- summarise_grouped_vars(level_two, 
                                                year, sex, hbr19, network, 
                                                age_group) %>% 
  mutate(simd = "All SIMD quintiles combined")

# Bind level_two and level_two_simd_counts

level_two <- level_two %>% 
  bind_rows(level_two_simd_counts)

# Calculate Uptake and Positivity and pivot data into wider format
# Remove any rows with blank simd
# Calculate regs_D and regs_N for Uptake and Positivity
# Select required columns and created Scotland variable set as Scotland for all

level_two <- level_two %>% 
  mutate(Uptake = uptake_n * 100 / invite_n,
         Positivity = positive_n * 100 / uptake_n) %>%
  pivot_longer(cols = c(Uptake, Positivity), names_to = "indicator", 
               values_to = "KPI_rate") %>% 
  filter(!is.na(simd)) %>% 
  mutate(KPI_denominator = case_when(indicator == "Uptake" ~ invite_n, 
                                     indicator == "Positivity" ~ uptake_n), 
         KPI_numerator = case_when(indicator == "Uptake" ~ uptake_n, 
                                   indicator == "Positivity" ~ positive_n)) %>% 
  select(year, sex, hbr19, simd, age_group, KPI_denominator, KPI_numerator, 
         KPI_rate, indicator, network)

# Filter for Scotland rows and select columns
# Rename columns to Scotland output where required

level_two_scotland_output <- level_two %>% 
  filter(hbr19 == "Scotland") %>% 
  select(year, sex, simd, age_group, KPI_denominator, KPI_numerator, KPI_rate, 
         indicator) %>% 
  rename_with(~ str_c("Scotland_", .), contains("KPI"))

# Filter for network rows and select columns
# Recode network to three characters and pivot data into wider format
# New names should be of format like regs_D_NOS

level_two_network_output <- level_two %>% 
  filter(hbr19 == network) %>% 
  select(year, sex, simd, age_group, KPI_denominator, KPI_numerator, KPI_rate, 
         indicator, network) %>% 
  mutate(network = case_when(network == "NCA" ~ "NOS", 
                             network == "SCAN" ~ "SOS", 
                             network == "WOSCAN" ~ "WOS")) %>% 
  pivot_wider(names_from = network, names_glue = "{network}_{.value}", 
              values_from = c(KPI_denominator, KPI_numerator, KPI_rate))

# Join level_two_scotland_output and level_two_network_output to level_two
# Set any blanks in numeric columns to 0
# Join on SPD and update HBCODE to use hbr19 to fill in any blanks
# This means we have the S08 codes for HBs and the network name for networks
# Select columns

level_two_output <- level_two %>% 
  left_join(level_two_scotland_output) %>% 
  left_join(level_two_network_output) %>% 
  left_join(spd) %>% 
  mutate(hbcode = coalesce(hbcode, hbr19)) %>% 
  select(year, sex, hbr19, simd, age_group, KPI_denominator, KPI_numerator, 
         KPI_rate, starts_with("Scotland"), starts_with("NOS"), 
         starts_with("SOS"), starts_with("WOS"), indicator, hbcode)

# Remove objects that are no longer required

rm(level_two_data, level_two_counts, level_two_sex_counts, level_two,
   level_two_network_counts, level_two_network_output, 
   level_two_scotland_counts, level_two_scotland_output, 
   level_two_age_counts, level_two_simd_counts)



### 5 Level 3 ----

# Define Uptake and Positivity based on screres value and redefine sex
# Join on SPD and update hbcode to use hbr19 to fill in any blanks
# This means we have the S08 codes for HBs and the network name for networks
# Select columns

level_three_output <- bowel_data %>% 
  mutate(uptake = case_when(uptake_n == 1 ~ "Responder", 
                            uptake_n == 0 ~ "Non-responder"), 
         positivity = case_when(positive_n == 1 ~ "Positive", 
                                negative_n == 1 ~ "Negative", 
                                TRUE ~ "Non-responder"), 
         sex = case_when(sex == "Males" ~ "Male", 
                         sex == "Females" ~ "Female", 
                         TRUE ~ sex)) %>% 
  left_join(spd) %>% 
  mutate(hbcode = coalesce(hbcode, hbcode)) %>% 
  select(chinum, invdate, year, patsname, patfname, sex, hbr19, network, 
         hbcode, simd, age_group, uptake, positivity)



### 6 Output ----

# Save output

write_csv(level_one_output,
          paste0(output_path, "/Level 1/SCRIS_Level_1_Bowel Screening/Data/",
                 "SCRIS_Level_1_Bowel_Screening_", todays_date, ".csv"))

write_csv(level_two_output,
          paste0(output_path, "/Level 2/SCRIS_Level_2_Bowel Screening/Output/",
                 "SCRIS_Level_2_Bowel_Screening_", todays_date, ".csv"))

write_csv(level_three_output,
          paste0(output_path, "/Level 3/SCRIS_Level_3_Bowel screening/Data/",
                 "SCRIS_Level_3_Bowel_Screening.csv"))
