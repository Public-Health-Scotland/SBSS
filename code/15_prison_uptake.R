#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 15_prison_uptake
# Calum Purdie
# 03/11/2021
# Data extraction/preparation
# Written/run on R Studio Server
# R version 3.6.1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### 1 Housekeeping ----

library(dplyr)
library(janitor)
library(tidyr)
library(lubridate)
library(haven)
library(sjlabelled)
library(glue)
library(openxlsx)
library(tidylog)

# Define filepaths

screening_path <- glue("/PHI_conf/CancerGroup1/Topics/BowelScreening")
current_pub <- glue("{screening_path}/Data/Programme/2022-11")
output_folder <- glue("{screening_path}/Publications/SBoSP-Statistics/", 
                      "20230221")
template_path <- glue("{output_folder}/Output/CONFI-prison-uptake/", 
                      "2023-02-21-Bowel-Screening-Prisons-Uptake.xlsx")

# Set start and end dates

start <- as_date("2020-05-01")
end <- as_date("2022-04-30")



### 2 Data Extraction ----

# Read in combined extract and remove formatting
# Filter for people invited between start and end dates
# Select columns and arrange data by patpcode
# Recode sex column

extract <- read_rds(glue("{current_pub}/combined_extract_all.rds")) %>% 
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels() %>% 
  filter(invdate >= start & invdate <= end) %>% 
  arrange(patpcode) %>% 
  select(chinum, invdate, sex, screres, patpcode) %>% 
  mutate(sex = case_when(sex == 1 ~ "Males", 
                         sex == 2 ~ "Females"))

gc()

# Read in prison postcode file and clean names
# Rename prison_pcode to patpcode
# Arrange data by patpcode

prison_pc <- read_sav(glue("{screening_path}/Publications/SBoSP-Statistics/", 
                           "20180206/Analysis/Prison_Pcode.sav")) %>% 
  clean_names() %>% 
  rename(patpcode = prison_pcode) %>% 
  arrange(patpcode)

# Read in prison postcode all file and clean names
# This file contains prison and prison health centre postcodes in one column
# Arrange data by patpcode

prison_pc_all <- read_sav(glue("{screening_path}/Publications/", 
                               "SBoSP-Statistics/20180206/Analysis/", 
                               "Prison_Pcode_All.sav")) %>% 
  clean_names() %>% 
  arrange(patpcode)



### 3 Join Data ----

# Join extract and prison_pc to identify how many patients match up to prisons
# Join by patpcode to get information for prisons

prison_matches <- extract %>% 
  inner_join(prison_pc) %>% 
  nrow()

# Join extract and prison_pc to identify how many patients match up to prison 
# health centres
# Remove patpcode from prison_pc before joining and then join by prison_hc_pcode
# to get information for prisons

prison_hc_matches <- extract %>% 
  inner_join(prison_pc %>% select(-patpcode), 
             by = c("patpcode" = "prison_hc_pcode")) %>% 
  nrow()

# Get all prison related records by joining extract and prison_pc_all
# Number of rows should match the total for prison_matches and prison_hc_matches
# Exclude HMP Aberdeen - CP: need to check why as can't remember

prison_data <- extract %>% 
  inner_join(prison_pc_all) %>% 
  filter(prison_name != "HMP Aberdeen")

# Deduplicate data to see if anyone has more than one screening
# Select chinum and invdate and arrange data
# Count chinum and filter to select any chinum that appears more than once
# Group by chinum and calculate the time difference between the previous
# invdate value 

duplicates <- prison_data %>% 
  select(chinum, invdate) %>% 
  arrange(chinum) %>% 
  add_count(chinum) %>% 
  filter(n > 1) %>% 
  group_by(chinum) %>% 
  mutate(time_diff = time_length(lag(invdate) %--% invdate, "years"))

# Feb 2023 - no duplicates
# Use the below code to exclude multiple screenings if required

# prison_data <- prison_data %>%
#   filter(!(chinum == "INSERT CHI NUMBER" & invdate == "INSERT DATE"))



### 4 Calculate Uptake ----

# Check frequencies for each column

prison_data %>% count(sex)
prison_data %>% count(prison_name)
prison_data %>% count(screres)

# Check frequency for sex and prison
# Format into crosstab

prison_data %>% count(sex, prison_name) %>% spread(sex, n, fill = 0)

# Select out columns required for output
# Create variables for invited and participated, with 1 and 0 values to 
# indicate whether the column is true or false for each row
# Group data by sex, prison and prison_name 
# Sum the total number invited and participated

prison_uptake <- prison_data %>% 
  select(sex, prison_name, screres) %>% 
  mutate(invited = if_else(screres %in% c(1:18, 21, 22, 24), 1, 0), 
         participated = if_else(screres %in% c(1, 3:8, 21, 22), 1, 0)) %>% 
  group_by(sex, prison_name) %>% 
  summarise(n_invited = sum(invited), 
            n_participated = sum(participated)) %>% 
  ungroup()

# Calculate uptake by prison for all sex
# Group data by prison_name 
# Modify totals for each group by adding a total row for all "Persons"

prison_uptake <- prison_uptake %>% 
  group_by(prison_name) %>% 
  group_modify(~ .x %>% adorn_totals("row", name = "Persons")) %>% 
  ungroup()

# Calculate uptake by sex for all prisons
# Group data by sex
# Modify totals for each group by adding a total row for Scotland
# Arrange data by prison_name

prison_uptake <- prison_uptake %>% 
  group_by(sex) %>% 
  group_modify(~ .x %>% adorn_totals("row", name = "Scotland")) %>% 
  ungroup() %>% 
  arrange(prison_name)

# Calculate the uptake_rate for each row
# Pivot data to longer format, setting column names to "Uptake" variable and 
# values to "n" variable
# Pivot data to wider format, using names from "prison_name" and values from "n"
# Capitalise sex and recode Uptake
# Set Sex as factor then arrange data

output_format <- prison_uptake %>% 
  mutate(uptake_rate = 100 * n_participated / n_invited) %>% 
  pivot_longer(cols = c(n_participated, n_invited, uptake_rate), 
               names_to = "Uptake", values_to = "n") %>% 
  pivot_wider(names_from = "prison_name", values_from = "n", values_fill = 0) %>% 
  rename(Sex = sex) %>% 
  mutate(Uptake = case_when(Uptake == "n_participated" ~ "Screened", 
                            Uptake == "n_invited" ~ "Invited", 
                            Uptake == "uptake_rate" ~ "Uptake Rate"), 
         Sex = factor(Sex, levels = c("Males", "Females", "Persons"))) %>% 
  arrange(Sex)

# Subset data for each output format

invites <- output_format %>% 
  filter(Uptake == "Invited") %>% 
  select(-Uptake)

screened <- output_format %>% 
  filter(Uptake == "Screened") %>% 
  select(-Uptake)

uptake_rate <- output_format %>% 
  filter(Uptake == "Uptake Rate") %>% 
  select(-Uptake)



### 5 Output ----

# Save output as rds

saveRDS(output_format, glue("{output_folder}/Temp/prison_data_kpi_1.rds"))


### 5.1 Excel Output ----

# Load bowel template

bowel_template <- loadWorkbook(template_path)

# Add invites to workbook

writeData(bowel_template, sheet = "Prisons Uptake", invites, 
          startCol = 2, startRow = 16, colNames = F)

# Add screened to workbook

writeData(bowel_template, sheet = "Prisons Uptake", screened, 
          startCol = 2, startRow = 21, colNames = F)

# Add uptake_rate to workbook

writeData(bowel_template, sheet = "Prisons Uptake", uptake_rate, 
          startCol = 2, startRow = 26, colNames = F)

# Save workbook

saveWorkbook(bowel_template, template_path, overwrite = TRUE)
