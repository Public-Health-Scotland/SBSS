
##########################################################
# Analysis_dataset_.R
# Gavin Clark
# 28/06/2019
# Data extraction/preparation
# Written/run on R Studio Server
# R version 3.2.3
# This script creates a dataset for summary in later scripts
# This is not a step carried out in the SPSS flow, but should save time 
# over the course of KPI production
# Approximate run time
##########################################################

### 1 - Housekeeping ----

#   loading packages
library(dplyr)
library(haven)
library(janitor)
library(lubridate)
library(ggplot2)
library(tidyr)
library(reshape2)
library(tidylog)

#   set filepaths and extract dates with script 0
source(here::here("code", "0_housekeeping.R"))


### Step 1 - Extract database, drop unnecessary columns and save as R file
## Bringing in optins version, as could produce opt-in report from same file

raw_db <- read_sav(sbsdb_path)

slim_db <- raw_db %>% select(
  -(PATSNAME:HBRES),
  -FIT_THRESH,
  -ERR,
  -(final_date_round:transf06),
  -transf14,
  -(workaround:HBIDENT),
  -PRECOLAS,
  -FURTHASS,
  -(BARENCTC:BARCTDAT),
  -POLYP,
  -POLYPECT,
  -(MORT:diff)
) %>%
  clean_names()

## glimpse() shows that date fields are being correctly read in
## 8,988,877 records, same as SPSS


### Remove flexi. sig. study participants
flexi_sig <- read_sav(paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening/",
                             "Projects/20110101-Flexi-Sig/Data/4June_2016/",
                             "FS_combined_extract.zsav")) %>%
  mutate(chinum = as.numeric(CHINUM)) %>%
  select(chinum, FSPERF, STUDYARM, FSRESULT) %>%
  filter(FSRESULT == '01')

## Merge on to bs database
slim_db <- left_join(slim_db, flexi_sig, by = "chinum") %>%
  mutate(remove = ifelse(FSRESULT == "01" & 
                           (invdate >= as.Date("2014-06-01") & invdate <= as.Date("2015-12-31")) &
                           !screres %in% c(1,3,4,5,6,7,8), 1, 0)) %>%
  filter(remove == 0|is.na(remove)) %>%
  select(-(FSPERF:remove))
# 27 removed, 8,988,850 remain


# Remove tables that are no longer needed
rm(list = c("flexi_sig", "raw_db"))



### Step 2 - Create flags for KPI numerator/denominator counts
slim_db <- slim_db %>%
  mutate(
    ## Create variables for the main KPIs
    invite_n = ifelse(screres %in% c(1:18,21:24), 1, 0),
    uptake_n = ifelse(screres %in% c(1,3,4,5,6,7,8,21,22), 1, 0),
    positive_n = ifelse(screres %in% c(3,5,6,8,22), 1, 0),
    negative_n = ifelse(screres %in% c(1,4,7,21), 1, 0),
    col_perf_n = ifelse(colperf == "01", 1, 0),
    col_complete_n = ifelse(colcomp == "01", 1, 0),
    col_complic_n = ifelse(complicp %in% 
                             c("01A","01B","01C","02","03","04","05","06","98") &
                             colperf == "01",
                           1, 0),
    cancer_n = ifelse(cancer == "01", 1, 0),
    polyp_cancer_n = ifelse(polypca == "01", 1, 0),
    adenoma_n = ifelse(adenoma == "01" & cancer == "00", 1, 0),
    ## Create adenoma risk stratification variables
    # Low risk
    lr_adenoma_n = ifelse(
      cancer == "00" &
        adenoma == "01" &
        adenno %in% c("01","02") &
        adensize %in% c("01","02","03","04","05","06","07","08","09"),
      1, 0),
    # Intermediate risk
    ir_adenoma_n = ifelse(
      # If adenoma is most serious diagnosis
      (cancer == "00" & adenoma == "01"), 
      # And there are 3 or 4 adenomas, none bigger than 9mm
      ifelse(
        (adenno %in% c("03","04") &
           adensize %in% c("01","02","03","04","05","06","07","08","09")), 
        1,
        # Or if there are 1 or 2 adenomas with one larger than 9mm
        ifelse(
          (adenno %in% c("01","02") &
             !(adensize %in% c("00","01","02","03","04","05","06","07","08","09",
                               "99","X", ""))),
          1, 0)),
      0),
    # High risk
    hr_adenoma_n = ifelse(
      cancer == "00" &
        adenoma == "01",
      ifelse(
        !(adenno %in% c("00","01","02","03","04","99","X","")),
        1, ifelse(
          adenno %in% c("03","04") &
            !(adensize %in% c("00","01","02","03","04","05","06","07","08","09",
                              "99","X", "")), 1, 0)), 
      0), 
    
    ## Create additional variables for numerators of KPIs 21, 22, 24 and 25,
    # conditioning on whether a colonoscopy was performed or not. 
    canc_col_n = cancer_n * col_perf_n,
    adenoma_col_n = adenoma_n * col_perf_n,
    hr_adenoma_col_n = hr_adenoma_n * col_perf_n,
    canc_hr_n = cancer_n * col_perf_n + hr_adenoma_n * col_perf_n,
    all_neoplasia_n = cancer_n * col_perf_n + adenoma_n * col_perf_n,
    icd = substr(icd10, 1, 3),
    
    ## Create additional variables for health board reports, script 7.
    # Colonoscopy complication excluding "other"
    col_complic_o_n = if_else(complicp == 98, 0, col_complic_n),
    # Death due to colonoscopy complications
    col_death_n = if_else(complicp == 6, 1, 0),
    # Unclassified risk adenomas
    uncl_adenoma_n = if_else(adenoma_n == 1 &
                               (lr_adenoma_n + ir_adenoma_n + hr_adenoma_n) == 0,
                             1, 0),
    # All adenomas, including those where cancer is diagnosed
    all_adenoma_n = if_else(adenoma == "01", 1, 0)
  )


# Sense-check - are percentages in line with expectation?
check <- slim_db %>%
  summarise(
    uptake_p = sum(uptake_n)/sum(invite_n),
    positive_p = sum(positive_n)/sum(uptake_n),
    col_perf_p = sum(col_perf_n)/sum(positive_n),
    col_complete_p = sum(col_complete_n)/sum(col_perf_n),
    col_complic_p = sum(col_complic_n)/sum(col_perf_n),
    cancer_ppv_approx = sum(cancer_n)/sum(col_perf_n),
    cancer_ppv_true = sum(canc_col_n)/sum(col_perf_n),
    adenoma_ppv_approx = sum(adenoma_n)/sum(col_perf_n),
    adenoma_ppv_true = sum(adenoma_col_n)/sum(col_perf_n),
    hr_adenoma_ppv_approx = sum(hr_adenoma_n)/sum(col_perf_n),
    hr_adenoma_ppv_true = sum(hr_adenoma_col_n)/sum(col_perf_n),
    hr_adenoma_cancer_ppv_true = sum(canc_hr_n)/sum(col_perf_n),
    all_neoplasia_ppv_true = sum(all_neoplasia_n)/sum(col_perf_n),
  )
View(check)


### Step 3 - Create 'by' variables

## Waiting times
slim_db <- slim_db %>%
  mutate(date_diff = 
           ifelse(col_perf_n == 1,
                  difftime(datecolperf, screresdat, units = "days"),
                  -999),
         
         waiting_time = ifelse(col_perf_n == 1, 
                               case_when(
                                 between(date_diff, 0, 28) ~ "0 to 4 weeks",
                                 between(date_diff, 29, 56) ~ "4 to 8 weeks",
                                 date_diff > 56 ~ ">8 weeks"),
                               "No colonoscopy"
         )
  ) %>%
  # Order levels of waiting_time for output
  # Watch out for NA values
  replace_na(list(waiting_time = "No colonoscopy")) %>%
  mutate(waiting_time = forcats::fct_relevel(waiting_time,
                                             "0 to 4 weeks",
                                             "4 to 8 weeks",
                                             ">8 weeks",
                                             "No colonoscopy")
  )

slim_db %>% count(waiting_time)


# More granular waiting time for health board reports
slim_db <- slim_db %>%
  mutate(waiting_time_hb = ifelse(
    col_perf_n == 1, 
    case_when(
      between(date_diff,   1,  14) ~ "0 to 2 weeks",
      between(date_diff,  15,  28) ~ ">2 to 4 weeks",
      between(date_diff,  29,  42) ~ ">4 to 6 weeks",
      between(date_diff,  43,  56) ~ ">6 to 8 weeks",
      between(date_diff,  57,  70) ~ ">8 to 10 weeks",
      between(date_diff,  71,  84) ~ ">10 to 12 weeks",
      between(date_diff,  85,  98) ~ ">12 to 14 weeks",
      between(date_diff,  99, 112) ~ ">14 to 16 weeks",
      between(date_diff, 113, 126) ~ ">16 to 18 weeks",
      between(date_diff, 127, 140) ~ ">18 to 20 weeks",
      date_diff > 140 ~ ">20 weeks"),
    "No colonoscopy")
  ) %>% 
  # Order levels of waiting_times for output
  # Watch out for NA values.
  replace_na(list(waiting_time_hb = "No colonoscopy")) %>%
  mutate(waiting_time_hb = forcats::fct_relevel(waiting_time_hb,
                                                "0 to 2 weeks",
                                                ">2 to 4 weeks",
                                                ">4 to 6 weeks",
                                                ">6 to 8 weeks",
                                                ">8 to 10 weeks",
                                                ">10 to 12 weeks",
                                                ">12 to 14 weeks",
                                                ">14 to 16 weeks",
                                                ">16 to 18 weeks",
                                                ">18 to 20 weeks",
                                                ">20 weeks",
                                                "No colonoscopy")
  )

slim_db %>% count(waiting_time_hb)      



## Dukes/TNM translation
# Use TNM8 translation from 01/01/2018. This is when it was published, 
# not necessarily when the boards started using
# Translation provided by Frank Carey
slim_db <- slim_db %>%
  mutate(dukes_der =
           ifelse(cancer_n == 1, 
                  ifelse(screresdat >= as.Date("2018-01-01"),
                         case_when(
                           tnmm %in% c("M1","pM1") ~ "D",
                           tnmn %in% c("N1", "N2", "pN1", "pN2") ~ "C",
                           tnmt %in% c("T3","T4","pT3","pT4") ~ "B",
                           tnmt %in% c("T1","T2","pT1","pT2") ~ "A",
                           TRUE ~ "Not known"),
                         
                         ifelse(screresdat < as.Date("2018-01-01"),
                                case_when(
                                  dukes == "04" ~ "D",
                                  dukes %in% c("03A","03B") ~ "C",
                                  dukes == "02" ~ "B",
                                  dukes == "01" ~ "A",
                                  dukes == "99" ~ "Not known",
                                  dukes == "96" ~ "Not supplied",
                                  TRUE ~ "Not supplied"),
                                "")
                  ),
                  "")
  )

slim_db %>% count(dukes_der)  

# Test - monitor % of each dukes stage/numbers over time
stage_chart <- slim_db %>%
  filter(cancer_n == 1) %>%
  mutate(year = year(screresdat))
g <- ggplot(data = stage_chart, aes(x = year, fill = dukes_der)) +
  geom_bar(position = "dodge")
g

# Quick look at hbg concentration and dukes stage
# FIT <- filter(stage_chart, !is.na(haemoglobin))
# h <- ggplot(data = filter(stage_chart, 
#                           !is.na(haemoglobin)), 
#                           aes(x = dukes_der, y = haemoglobin)) + 
#               geom_boxplot()
# h

# shows jump in dukes A, D and unknown in 2018

## Calculate screening participation history
# Create variable for latest round of screening available
uptake_history <- slim_db %>%
  mutate(date_round = recode(date_round,
                             "1" = "uptake_rnd_1",
                             "2" = "uptake_rnd_2",
                             "3" = "uptake_rnd_3",
                             "4" = "uptake_rnd_4",
                             "5" = "uptake_rnd_5",
                             "6" = "uptake_rnd_6")) %>%
  spread(date_round, uptake_n) %>%
  replace_na(list(uptake_rnd_1 = 0,
                  uptake_rnd_2 = 0,
                  uptake_rnd_3 = 0,
                  uptake_rnd_4 = 0,
                  uptake_rnd_5 = 0,
                  uptake_rnd_6 = 0)) %>%
  group_by(chinum) %>%
  mutate(uptake_rnd_1 = max(uptake_rnd_1),
         uptake_rnd_2 = max(uptake_rnd_2),
         uptake_rnd_3 = max(uptake_rnd_3),
         uptake_rnd_4 = max(uptake_rnd_4),         
         uptake_rnd_5 = max(uptake_rnd_5),
         uptake_rnd_6 = max(uptake_rnd_6)
  ) %>%
  ungroup() %>%
  select(chinum, uptake_rnd_1:uptake_rnd_6) %>%
  distinct()

slim_db <- left_join(slim_db, uptake_history, by = "chinum")

# Create uptake history variable
# Need to create something that doesn't need to be updated every time we run
# Looping through variable names with with i 2:6, date_round = i-1, could work
slim_db <- mutate(slim_db,
                  
                  uptake_history = case_when(
                    date_round == 1 ~ "First round",
                    
                    (uptake_rnd_1 + 
                       uptake_rnd_2 +
                       uptake_rnd_3 +
                       uptake_rnd_4 +
                       uptake_rnd_5) == 0 &
                      date_round == 6 ~ "Never participated",
                    (uptake_rnd_1 +
                       uptake_rnd_2 +
                       uptake_rnd_3 +
                       uptake_rnd_4) == 0 &
                      date_round == 5 ~ "Never participated",           
                    (uptake_rnd_1 +
                       uptake_rnd_2 +
                       uptake_rnd_3) == 0 &
                      date_round == 4 ~ "Never participated",
                    (uptake_rnd_1 +
                       uptake_rnd_2) == 0 &
                      date_round == 3 ~ "Never participated",
                    uptake_rnd_1 == 0 &
                      date_round == 2 ~ "Never participated",
                    
                    date_round == 2 &
                      uptake_rnd_1 == 1 ~ "Participated in previous round",
                    date_round == 3 &
                      uptake_rnd_2 == 1 ~ "Participated in previous round",
                    date_round == 4 &
                      uptake_rnd_3 == 1 ~ "Participated in previous round",
                    date_round == 5 & 
                      uptake_rnd_4 == 1 ~ "Participated in previous round",
                    date_round == 6 & 
                      uptake_rnd_5 == 1 ~ "Participated in previous round",
                    
                    date_round == 2 &
                      uptake_rnd_1 == 0 ~ "Didn't participate in previous round",
                    date_round == 3 &
                      uptake_rnd_2 == 0 ~ "Didn't participate in previous round",
                    date_round == 4 &
                      uptake_rnd_3 == 0 ~ "Didn't participate in previous round",
                    date_round == 5 &
                      uptake_rnd_4 == 0 ~ "Didn't participate in previous round",
                    date_round == 6 &
                      uptake_rnd_5 == 0 ~ "Didn't participate in previous round"
                  ) ) 


# Order levels of uptake_history for output
slim_db <- slim_db %>%
  mutate(
    uptake_history = forcats::fct_relevel(uptake_history,
                                          "First round",
                                          "Participated in previous round",
                                          "Didn't participate in previous round",
                                          "Never participated")
  )

slim_db %>% count(uptake_history)

## Check versus FIT/FOBT report
# slim_db %>% 
#   mutate(
#     test = case_when(
#       between(invdate, as.Date("2016-11-20"), as.Date("2017-04-30")) ~ 1,
#       between(invdate, as.Date("2017-11-20"), as.Date("2018-04-30")) ~ 2
#       )
#     ) %>%
#   filter(test %in% 1:2) %>%
#   group_by(test, uptake_history) %>%
#   summarize(invite_n = sum(invite_n),
#             uptake_p = sum(uptake_n)/sum(invite_n))

# Are there any people who have fit positive with no haemoglobin?
check <- slim_db %>% filter(screres == 22 & !is.na(haemoglobin))

#### Percentages in line with SPSS, counts are slightly out (low hundreds)
# Need to pick up those with blank date round 1 with IT

saveRDS(slim_db, file = analysis_db_path)


