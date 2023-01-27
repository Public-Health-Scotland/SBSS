#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 01_analysis_dataset.R
# Gavin Clark
# Aug 2022
# Script 1 of 13
# Data extraction/preparation
# Written/run on R Studio Server
# R version 3.6.1
# This script creates a dataset for summary in later scripts
# This is not a step carried out in the SPSS flow, but should save time 
# over the course of KPI production
# Approximate run time
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Step 0: Housekeeping ----

## loading packages
library(dplyr)
library(haven)
library(janitor)
library(ggplot2)
library(tidyr)
library(reshape2)
library(tidylog)
library(readr)
library(here)
library(lubridate)

## Install/open the PHSmethods package
# install.packages("remotes")
#library(remotes)
# remotes::install_github("Public-Health-Scotland/phsmethods", upgrade = "never")
library(phsmethods)


# set filepaths and extract dates with script 0
rm(list = ls())
source(here::here("Code", "00_housekeeping.R"))
# wd <- paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening",
#              "/Publications/SBoSP-Statistics/20230221")
# source(paste0(wd, "/Code/00_housekeeping.R"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Useful notes on issues created by databasing process in R, May 2021, TG
## i) Variable names are now lowercase because of use of clean_names() function
## ii) Some have been renamed during databasing so need to edit code to reflect that:
# icd10 => icd_10
# tnmt => tnm_t            
# tnmn => tnm_n      
# tnmm => tnm_n  
# fit_thresh => fit_test 
## iii) Data types have been changed:
# In the databasing scripts it was convenient to just have 'dates' and 'characters',
# so data is saved like that after databasing.
# These KPI scripts were written with data types including 'dates', 'characters' and 'numeric'
# variables. In this script we'll need to change them back to the types used when 
# this script was written to save some work adjusting code.
# At some point we should consolidate types across R scripts.
## iv) We now have Health Board geographies for hb2018 and hb2019 (formerly just hb2006 and hb2014)
# We've created hbres18, hbres19, hbr18 and hbr19 to match the hbres14 and hbr14 variables.
# We should probably switch to hbres19/hbr19 for outputs 
# (though it likely make no difference results since the boundary changes to the HBs were minimal)
# 
## v) Need to replace 'spread' function with pivot_wider()
# pivot_wider(names_from = date_round, values_from = uptake_n, values_fill = 0)
# and 'gather' functions with pivot_longer()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### Step 1: Read in SBSP database ----

## Bringing in optins version, as could produce opt-in report from same file

## 1.1 Get full dataset ----

raw_db <- read_rds(sbsdb_path) %>%
          clean_names()

names(raw_db)
glimpse(raw_db)



## Check the format (type) of each variable and re-format
# i.e. from the character and date formats used during databasing,
# to the formats used in these KPI scripts

# glimpse(slim_db)
# Most should be date or character types


## Change variable types from character to numeric, 
# [to agree with how they were originally
# formatted when these KPI scripts were written]
# Note: we should update our code: sex is a categorical & almost all vars are
# listed to be characters in minimum dataset specification
 

raw_db <- raw_db %>%
  mutate(sex     = as.numeric(zap_labels(sex)),    #zap_labels(sex)), ##!! addressed above in temp solution
         screres = as.numeric(zap_labels(screres)), ##!! addressed above in temp solution
         haemoglobin = as.numeric(haemoglobin),
         fit_test   = as.numeric(fit_test),
         err        = as.numeric(err),
         date_round = as.numeric(date_round),
         final_date_round = as.numeric(final_date_round),
         simd2009   = as.numeric(simd2009),
         simd2012   = as.numeric(simd2012),
         simd2016   = as.numeric(simd2016),
         simd2020   = as.numeric(simd2020),
         age        = as.numeric(age),
         age_group  = as.numeric(age_group),
         optin      = as.numeric(optin),
         hbr14      = as.numeric(hbr14),
         hbr18      = as.numeric(hbr18),
         hbr19      = as.numeric(hbr19)
         )

glimpse(raw_db)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## KH--Need to make the variables sex and screres numeric
# However, am having trouble getting " mutate(sex = as.numeric(sex))" to work
# Long roundabout *temporary* solution below 
#slim_db <- slim_db %>%
#            mutate(sex = as_factor(sex),
#                   screres = as_factor(screres)) %>%
#            mutate(sex = case_when(sex == "Male" ~ 1,
#                                   sex == "Female" ~ 2),
#                   screres = case_when(screres == "FOBT Negative" ~ 1,
#                                       screres == "FOBT Weak Positive" ~ 2,
#                                       screres == "FOBT Positive" ~ 3,
#                                       screres == "Wipe Negative" ~ 4,
#                                       screres == "Wipe Weak Positive" ~ 5,
#                                       screres == "Wipe Positive" ~ 6,
#                                       screres == "FIT Negative" ~ 7,
#                                       screres == "FIT Positive" ~ 8,
#                                       screres == "Replaced" ~ 9,
#                                       screres == "Kit Lot Out of Date" ~ 10,
#                                       screres == "Expired" ~ 11,
#                                       screres == "Overdue" ~ 12,
#                                       screres == "Spoiled" ~ 13,
#                                       screres == "Incomplete" ~ 14,
#                                       screres == "Unused" ~ 15,
#                                       screres == "Technical Failure" ~ 16,
#                                       screres == "Non Technical Failure" ~ 17,
#                                       screres == "Unresolved ID Query" ~ 18,
#                                       screres == "Terminated" ~ 19,
#                                       screres == "Still Within Compliance" ~ 20,
#                                       screres == "QFIT Negative" ~ 21,
#                                       screres == "QFIT Positive" ~ 22,
#                                       screres == "Undelivered" ~ 23,
#                                       screres == "QFIT within compliance" ~ 24)
#                   )#
#glimpse(slim_db)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## 1.2 Clean and tidy the data ----

names(raw_db)

# i) Slim down dataset ----
slim_db <- raw_db %>% 
          select(-(patsname:dob),
                 -fit_test,
                 -err,
                 -(final_date_round:transf06),
                 -transf14,
                 -(workaround:hbident),
                 #-precolas,
                 -furthass,
                 -(barenctc:barctdat),
                 -polyp,
                 -polypect,
                 -mort
                 #-(mort:diff)
                 )

names(slim_db)



slim_db %>% count(colperf)
# other   136,878
# NA   10,771,726, Nov 2021

# other 144,486
# NA   11,260,605, May 2022

# other 152,422
# NA   11,721,783, Nov 2022


# ii) Check chinums ----
slim_db <- slim_db %>%
           mutate(chinum = as.character(chinum),
                  chinum = chi_pad(chinum))

slim_db %>% count()
#  9,454,841 records, 
# 10,307,121 records, Nov 2020
# 10,366,026 records, May 2021
# 10,908,604 records, Nov 2021
# 11,405,091 records, May 2022
# 11,874,205 records, Nov 2022



## iii) Remove the flexi. sig. study participants ----
# Define path to file
flexi_sig_path <- paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening/",
                         "Projects/20110101-Flexi-Sig/Data/4June_2016/",
                         "FS_combined_extract.zsav")

# Import full file
flexi_sig <- read_sav(flexi_sig_path) %>%
             clean_names()

names(flexi_sig)
glimpse(flexi_sig)

# Clean flexi sig data
flexi_sig <- flexi_sig %>%
             mutate(chinum = as.character(chinum),
                    chinum = chi_pad(chinum)) %>%
             select(chinum, fsperf, studyarm, fsresult) %>%
             filter(fsresult %in% ('01'))

# Merge flexi sig patient identities onto main screening database
slim_db <- left_join(slim_db, flexi_sig, 
                     by = "chinum") %>%
           mutate(remove = ifelse(fsresult %in% c("01") & 
                                  (invdate >= date("2014-06-01") & 
                                   invdate <= date("2015-12-31")) &
                                  !(screres %in% c(1,3,4,5,6,7,8)),
                                  'yes', 'no')) %>%
           filter(remove %in% c('no') | is.na(remove)) %>%
           select(-(fsperf:remove))
# 27 removed,  9,454,814 remain
# 27 removed, 10,307,094 remain, Nov 2020
# 27 removed, 10,365,999 remain, May 2021
# 27 removed, 10,908,577 remain, Nov 2021
# 27 removed, 11,405,064 remain, May 2022
# removed 27 rows (<1%), 11,874,178 rows remaining

# Remove tables that are no longer needed
rm(flexi_sig_path, flexi_sig, raw_db)


#~~~~~~~~~~~~~~~~~~~~~~~~
### ??) Temporary fix for handling incorrect adenno and adensize ----
## Written by GC; needs to be corrected in databasing scripts
# Does it look more sensible if those missing the leading zero have it appended?

glimpse(slim_db)
slim_db %>% count(adenno) %>% 
            View()

slim_db <- slim_db %>%
    mutate(adenno = if_else(!(adenno %in% c("X", "N")) & 
                                   # Not working based on length == 1, 
                                   # code each level explicitly
                                   adenno %in% c("0", "1", "2", "3", 
                                                 "4", "5", "6", "7", "8", "9"), 
                                   paste0("0", adenno), as.character(adenno))
           )

slim_db %>% count(adenno) %>% 
            View()
###
slim_db %>% count(adensize) %>% 
            View()

slim_db <- slim_db %>%
           mutate(adensize = if_else(!(adensize %in% c("X")) & 
                                   # Not working based on length == 1 oddly, 
                                   # code each level explicitly
                                   adensize %in% c("0", "1", "2", "3", "4", 
                                                   "5", "6", "7", "8", "9"), 
                                   paste0("0", adensize), as.character(adensize))
                  )

slim_db %>% count(adensize) %>% 
            View()
#~~~~~~~~~~~~~~~~~~~~~~~~


### Step 2: Create flags to enable numerator/denominator counts for KPIs ----
# i.e. flag each patient with a 0 or 1 for each variable,
# so we can count up each variable to get numerator and denominators for KPIs

place_holder <- slim_db

## 2.1 Create flag variables for the main KPIs ----

glimpse(slim_db)


slim_db <- slim_db %>%
  mutate(
    #TG - update to use if_else() rather than ifelse()
    invite_n   = ifelse(screres %in% c(1:18, 21:24), 1, 0),
    uptake_n   = ifelse(screres %in% c(1, 3, 4, 5, 6, 7, 8, 21, 22), 1, 0),
    positive_n = ifelse(screres %in% c(3, 5, 6, 8, 22), 1, 0),
    negative_n = ifelse(screres %in% c(1, 4, 7, 21), 1, 0),
    col_perf_n = ifelse(colperf %in% c("01"), 1, 0),
    col_complete_n = ifelse(colcomp %in% c("01"), 1, 0),
    col_complic_n  = ifelse(complicp %in% c("01A", "01B", "01C",
                                             "02", "03", "04", "05", "06", "98") &
                             colperf %in% c("01"), 1, 0),
    cancer_n = ifelse(cancer %in% c("01"), 1, 0),
    polyp_cancer_n = ifelse(polypca %in% c("01"), 1, 0),
    adenoma_n = ifelse(adenoma %in% c("01") & cancer %in% c("00"), 1, 0)
    )

glimpse(slim_db)

# Check for any NAs
slim_db %>% count(invite_n)
slim_db %>% count(uptake_n)
slim_db %>% count(positive_n)
slim_db %>% count(negative_n)
slim_db %>% count(col_perf_n)
slim_db %>% count(col_complete_n)
slim_db %>% count(col_complic_n)
slim_db %>% count(col_perf_n)
slim_db %>% count(cancer_n)  
slim_db %>% count(polyp_cancer_n)  
slim_db %>% count(adenoma_n)


## 2.2 Create adenoma risk stratification variables ----
slim_db <- slim_db %>%
  mutate(
    ## Low risk
    lr_adenoma_n = ifelse(cancer %in% "00" &
                          adenoma %in% "01" &
                          adenno %in% c("01", "02") &
                          adensize %in% c("01", "02", "03", "04", 
                                          "05", "06", "07", "08", "09"),
                          1, 0),
    ## Intermediate risk
    ir_adenoma_n = ifelse(
                          # If adenoma is most serious diagnosis
                          # And there are 3 or 4 adenomas, none bigger than 9mm
                          (cancer %in% "00" & adenoma %in% "01"),
                          if_else((adenno %in% c("03", "04") & 
                                   adensize %in% c("01", "02", "03", "04",
                                                   "05" , "06", "07", "08", "09")),
                          1,
                    # Or if there are 1 or 2 adenomas with one larger than 9mm
                         if_else((adenno %in% c("01","02") &
                                  !(adensize %in% c("00", "01", "02", "03", "04",
                                                    "05", "06", "07", "08", "09",
                                                    "99", "X", ""))), 
                                 1, 0)),
                          0),
    ## High risk
    hr_adenoma_n = ifelse(cancer %in% "00" & adenoma %in% "01",
                           if_else(!(adenno %in% c("00", "01", "02", "03", "04",
                                           "99", "X", "")),
                           1, 
                           if_else(adenno %in% c("03", "04") &
                                   !(adensize %in% c("00", "01", "02", "03", "04",
                                                     "05", "06", "07", "08", "09",
                                                     "99", "X", "")), 
                                   1, 0)),
                           0))

slim_db %>% glimpse()

slim_db %>% count(lr_adenoma_n)  
slim_db %>% count(ir_adenoma_n)  
slim_db %>% count(hr_adenoma_n)

## 2.3 Create additional variables used for numerators to several KPIs ----
# KPIs 21, 22, 24 and 25, conditional on whether a colonoscopy was performed or not.

slim_db <- slim_db %>%
           mutate(canc_col_n       = cancer_n  * col_perf_n,
                  adenoma_col_n    = adenoma_n * col_perf_n,
                  hr_adenoma_col_n = hr_adenoma_n * col_perf_n,
                  canc_hr_n        = (cancer_n * col_perf_n) + (hr_adenoma_n * col_perf_n),
                  all_neoplasia_n  = (cancer_n * col_perf_n) + (adenoma_n    * col_perf_n),
                  icd = substr(icd_10, 1, 3)
                  )
    
## 2.4 Create additional variables used for health board reports ----
# in script 7.

slim_db <- slim_db %>%
           mutate(
             # Colonoscopy complication excluding "other"
             col_complic_o_n = ifelse(complicp %in% "98", 0, as.numeric(col_complic_n)),
             # Death due to colonoscopy complications
             col_death_n = ifelse(complicp %in% "6", 1, 0),
             # Unclassified risk adenomas
             uncl_adenoma_n = ifelse(adenoma_n %in% 1 &
                                     (lr_adenoma_n + ir_adenoma_n + hr_adenoma_n) %in% 0,
                                      1, 0),
             # All adenomas, including those where cancer is diagnosed
             all_adenoma_n = ifelse(adenoma %in% "01", 1, 0)
             )

slim_db %>% glimpse()

slim_db %>% count(col_complic_o_n)  
slim_db %>% count(col_death_n)  
slim_db %>% count(uncl_adenoma_n)
slim_db %>% count(all_adenoma_n)

# 2.5 Sense-check: are KPI values roughly in line with expectations? ----

# TG - make percentages?
quick_kpi_check <- slim_db %>%
  filter(invdate >= dmy("01-11-2019")) %>%
    summarise(
      uptake_p   = sum(uptake_n)   /sum(invite_n), 
      positive_p = sum(positive_n) /sum(uptake_n),
      col_perf_p = sum(col_perf_n) /sum(positive_n),
      col_complete_p     = sum(col_complete_n)     /sum(col_perf_n),
      col_complic_p      = sum(col_complic_n)      /sum(col_perf_n),
      cancer_ppv_approx  = sum(cancer_n)           /sum(col_perf_n),
      cancer_ppv_true    = sum(canc_col_n)         /sum(col_perf_n),
      adenoma_ppv_approx = sum(adenoma_n)          /sum(col_perf_n),
      adenoma_ppv_true   = sum(adenoma_col_n)      /sum(col_perf_n),
      hr_adenoma_ppv_approx = sum(hr_adenoma_n)    /sum(col_perf_n),
      hr_adenoma_ppv_true = sum(hr_adenoma_col_n)  /sum(col_perf_n),
      hr_adenoma_cancer_ppv_true = sum(canc_hr_n)  /sum(col_perf_n),
      all_neoplasia_ppv_true = sum(all_neoplasia_n)/sum(col_perf_n)
      )

View(quick_kpi_check)
rm(quick_kpi_check)


### Step 3: Create categorical variables, for dividing the data ----


## 3.1 Waiting times ----

place_holder2 <- slim_db
#slim_db <- place_holder2

## i) Four-category waiting times ----
slim_db <- slim_db %>%
           mutate(
             date_diff = if_else(col_perf_n %in% 1, 
                                 difftime(datecolperf, screresdat, units = "days"),
                                 -999),
             waiting_time = case_when(col_perf_n %in% 1 & between(date_diff, 0, 28)  ~ "0 to 4 weeks",
                                      col_perf_n %in% 1 & between(date_diff, 29, 56) ~ "4 to 8 weeks",
                                      col_perf_n %in% 1 & date_diff > 56             ~ ">8 weeks",
                                      !(col_perf_n %in% 1) ~ "No colonoscopy",
                                      TRUE ~ 'check_case'))

slim_db %>% count(waiting_time)


# Order levels of waiting_time for output
# Watch out for NA values
slim_db <- slim_db %>%
             replace_na(list(waiting_time = "No colonoscopy")) %>%
             mutate(waiting_time = forcats::fct_relevel(waiting_time,
                                                        "0 to 4 weeks",
                                                        "4 to 8 weeks",
                                                        ">8 weeks",
                                                        "No colonoscopy")
                    )

slim_db %>% count(waiting_time)

#                   Nov 2020 /   May 2021 /   Nov 2021 /   May 2022 /   Nov 2022
# 0-4 wks:            48,755 /     49,319 /     52,313 /     54,449 /     55,821
# 4-8 wks:            39,963 /     40,194 /     43,211 /     45,459 /     47,570
# >8 wks:             22,292 /     22,427 /     24,017 /     26,839 /     30,023
# No colonoscopy: 10,196,084 / 10,254,059 / 10,789,036 / 11,278,317 / 11,740,764


## ii) Twelve-category waiting times ----
# (More granular waiting time for health board reports)

slim_db <- slim_db %>%
  mutate(waiting_time_hb = if_else(col_perf_n %in% 1, 
                                  case_when(
                                     between(date_diff,   1,  14) ~   "0 to 2 weeks",
                                     between(date_diff,  15,  28) ~  ">2 to 4 weeks",
                                     between(date_diff,  29,  42) ~  ">4 to 6 weeks",
                                     between(date_diff,  43,  56) ~  ">6 to 8 weeks",
                                     between(date_diff,  57,  70) ~  ">8 to 10 weeks",
                                     between(date_diff,  71,  84) ~ ">10 to 12 weeks",
                                     between(date_diff,  85,  98) ~ ">12 to 14 weeks",
                                     between(date_diff,  99, 112) ~ ">14 to 16 weeks",
                                     between(date_diff, 113, 126) ~ ">16 to 18 weeks",
                                     between(date_diff, 127, 140) ~ ">18 to 20 weeks",
                                     date_diff > 140 ~ ">20 weeks"),
                                  "No colonoscopy"))
  
slim_db %>% count(waiting_time_hb)

# Order levels of waiting_times for output
  # Watch out for NA values.
  # AM (May 2022):
  # Technically replacing NA values with "No colonoscopy" is wrong because
  # of these 4, all have had a col but 2 are date_diff == 0 and 2 are 
  # datecoloff == NA. Can't with confidence say a waiting time for any of
  # these so best just to 
slim_db <- slim_db %>%
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

#                    Nov 2020  /   May 2021 /   Nov 2021 /   May 2022 /   Nov 2022 /
# 1    0 to 2 weeks      9,573 /      9,700 /     10,061 /     10,273 /     10,377 /
# 2   >2 to 4 weeks     39,179 /     39,616 /     42,249 /     44,172 /     45,440 /
# 3   >4 to 6 weeks     25,969 /     26,151 /     28,074 /     29,622 /     30,907 /
# 4   >6 to 8 weeks     13,994 /     14,043 /     15,137 /     15,837 /     16,663 /
# 5   >8 to 10 weeks     7,011 /      7,034 /      7,583 /      7,950 /      8,597 /
# 6  >10 to 12 weeks     4,799 /      4,813 /      5,221 /      5,726 /      6,290 /
# 7  >12 to 14 weeks     3,369 /      3,373 /      3,649 /      4,434 /      5,120 /
# 8  >14 to 16 weeks     2,408 /      2,412 /      2,548 /      3,132 /      3,682 /
# 9  >16 to 18 weeks     1,405 /      1,411 /      1,487 /      1,698 /      2,028 /
# 10 >18 to 20 weeks       857 /        864 /        914 /      1,016 /      1,182 /
# 11 >20 weeks           2,443 /      2,520 /      2,615 /      2,883 /      3,124 /
# 12 No colonoscopy 10,196,087 / 10,254,062 / 10,789,039 / 11,278,321 / 11,740,768 /



slim_db %>% count(dukes)

#slim_db %>% count(dukes_der) ## There is no variable dukes_der at this point??

## 3.2  Translate TNM staging provided into Dukes staging ----
# Use TNM8 translation from 01/01/2018. This is when it was published, 
# not necessarily when the boards started using
# Translation provided by Frank Carey

# Update to translation provided by Prakash Konanahalli
# 'pT4A and pT4B are Dukes stage B'
# Didn't comment on T4A and T4B but going to assume they are the same since this
# is the case for all other 'p' prefixes and the internet says the prefix 
# describes the method by which the stage was ascertained

slim_db <- slim_db %>%
  mutate(dukes_der = if_else(cancer_n %in% 1, 
                  if_else(screresdat >= as.Date("2018-01-01"),
                         case_when(tnm_m %in% c("M1", "pM1") ~ "D",
                                   tnm_n %in% c("N1", "N2", "pN1", "pN2") ~ "C",
                                   tnm_t %in% c("T3", "T4", "pT3", "pT4",
                                                "T4A", "T4B",
                                                "pT4A", "pT4B") ~ "B",
                                   tnm_t %in% c("T1", "T2", "pT1", "pT2") ~ "A",
                                   TRUE ~ "Not known"),
                         if_else(screresdat < as.Date("2018-01-01"),
                                case_when(dukes %in% "04" ~ "D",
                                          dukes %in% c("03A", "03B") ~ "C",
                                          dukes %in% "02" ~ "B",
                                          dukes %in% "01" ~ "A",
                                          dukes %in% "99" ~ "Not known",
                                          dukes %in% "96" ~ "Not supplied",
                                          TRUE ~ "Not supplied"),
                                as.character(NA))
                         ),
                  as.character(NA))
         )

# GC update - some colonoscopies in 2017 where pathology likely from 2018 and so
# dukes is not populated. Below fix applied to those below, do not change.
# MT update - commented out section below from previous report 
# GC update - 
slim_db <- slim_db %>%
  mutate(dukes_der = if_else(screresdat >= as.Date("2017-05-01") & dukes %in% "96",
                             case_when(tnm_m %in% c("M1", "pM1") ~ "D",
                                       tnm_n %in% c("N1", "N2", "pN1", "pN2") ~ "C",
                                       tnm_t %in% c("T3", "T4", "pT3", "pT4",
                                                    "T4A", "T4B",
                                                    "pT4A", "pT4B") ~ "B",
                                       tnm_t %in% c("T1", "T2", "pT1", "pT2") ~ "A",
                                       TRUE ~ "Not known"), dukes_der)
  )

## GC edit - for those not supplied close to the 
# move to TNM8, apply algorithm
check <- slim_db %>% filter(is.na(dukes_der))

rm(check)
slim_db %>% count(dukes_der)  

#                  Nov 2020 /   May 2021 /   Nov 2021 /   May 2022 /   Nov 2022 /
# 1 ""             10299792 / 10,358,642 / 10,900,801 / 11,396,896 / 11,865,652 /
# 2 "A"                2703 /      2,721 /      2,873 /      3,023 /      3,151 /
# 3 "B"                1789 /      1,814 /      1,918 /      2,041 /      2,140 /
# 4 "C"                1962 /      1,979 /      2,071 /      2,188 /      2,285 /
# 5 "D"                 361 /        368 /        391 /        419 /        428 /
# 6 "Not known"         265 /        270 /        318 /        292 /        317 /
# 7 "Not supplied"      222 /        205 /        205 /        205 /        205 /
## Decrease in 'Not known' due to improved translation. May 2022

# Do patterns make sense?
slim_db %>% count(year = year(invdate), dukes, dukes_der) %>% 
            pivot_wider(names_from = year, values_from = n) %>%
            View()



slim_db %>% count(year = year(invdate), 
                     tnm_t, tnm_n, tnm_m,
                     dukes, dukes_der) %>% 
            pivot_wider(names_from = year, values_from = n) %>%
            View()

slim_db %>% filter(year(invdate) >= 2017) %>%
  count(year = year(invdate), 
                     tnm_t, tnm_n, tnm_m,
                     dukes, dukes_der) %>% 
  pivot_wider(names_from = year, values_from = n) %>%
  View()

test %>% tabyl(tnm_t, dukes)
test %>% tabyl(tnm_t, dukes_der)

# # Test - monitor % of each dukes stage/numbers over time
# stage_chart <- slim_db %>%
#   filter(cancer_n == 1) %>%
#   mutate(year = year(screresdat))
# g <- ggplot(data = stage_chart, aes(x = year, fill = dukes_der)) +
#   geom_bar(position = "dodge")
# g
# 
# # Quick look at hbg concentration and dukes stage
# FIT <- filter(stage_chart, !is.na(haemoglobin))
# h <- ggplot(data = filter(stage_chart,
#                           !is.na(haemoglobin)),
#                           aes(x = dukes_der, y = haemoglobin)) +
#               geom_boxplot()
# h
# 
# # shows jump in dukes A, D and unknown in 2018
# rm(stage_chart, FIT, g, h)


## 3.3 Describe screening participation history ----

# i) Create variable for latest round of screening available ----

# Check how many date_rounds currently exit
slim_db %>% count(date_round)
# 8

uptake_history <- slim_db %>%
                  mutate(date_round = recode(date_round,
                                             "1" = "uptake_rnd_1",
                                             "2" = "uptake_rnd_2",
                                             "3" = "uptake_rnd_3",
                                             "4" = "uptake_rnd_4",
                                             "5" = "uptake_rnd_5",
                                             "6" = "uptake_rnd_6",
                                             "7" = "uptake_rnd_7",
                                             "8" = "uptake_rnd_8")) %>%
                  pivot_wider(names_from = date_round, values_from = uptake_n,values_fill = 0) %>%
                  #spread(date_round, uptake_n) %>%
                  #replace_na(list(uptake_rnd_1 = 0,
                  #                uptake_rnd_2 = 0,
                  #                uptake_rnd_3 = 0,
                  #                uptake_rnd_4 = 0,
                  #                uptake_rnd_5 = 0,
                  #                uptake_rnd_6 = 0,
                  #                uptake_rnd_7 = 0)) %>%
                  group_by(chinum) %>%
                  mutate(uptake_rnd_1 = max(uptake_rnd_1),
                         uptake_rnd_2 = max(uptake_rnd_2),
                         uptake_rnd_3 = max(uptake_rnd_3),
                         uptake_rnd_4 = max(uptake_rnd_4),         
                         uptake_rnd_5 = max(uptake_rnd_5),
                         uptake_rnd_6 = max(uptake_rnd_6),
                         uptake_rnd_7 = max(uptake_rnd_7),
                         uptake_rnd_8 = max(uptake_rnd_8)) %>%
                  ungroup() %>%
                  select(chinum, uptake_rnd_1:uptake_rnd_8) %>%
                  distinct()

slim_db <- left_join(slim_db, uptake_history, by = "chinum")

place_holder3 <- slim_db

# Create uptake history variable ----
# Need to create something that doesn't need to be updated every time we run
# Looping through variable names with with i 2:6, date_round = i-1, could work

slim_db %>% count(date_round)
# CHECK HOW MANY date_round ROUNDS THERE ARE!!!
# YOU MAY NEED TO EXTEND CODE BELOW IF IT DOESN'T MATCH THAT MANY ROUNDS

slim_db <- mutate(slim_db,
                  
 uptake_history = case_when(date_round %in% 1 ~ "First round",
                    (uptake_rnd_1 + 
                       uptake_rnd_2 +
                       uptake_rnd_3 +
                       uptake_rnd_4 +
                       uptake_rnd_5 +
                       uptake_rnd_6 +
                       uptake_rnd_7) %in% 0 &
                      date_round %in% 8 ~ "Never participated",
                    (uptake_rnd_1 + 
                       uptake_rnd_2 +
                       uptake_rnd_3 +
                       uptake_rnd_4 +
                       uptake_rnd_5 +
                       uptake_rnd_6) %in% 0 &
                      date_round %in% 7 ~ "Never participated",
                    (uptake_rnd_1 + 
                       uptake_rnd_2 +
                       uptake_rnd_3 +
                       uptake_rnd_4 +
                       uptake_rnd_5) %in% 0 &
                      date_round %in% 6 ~ "Never participated",
                    (uptake_rnd_1 +
                       uptake_rnd_2 +
                       uptake_rnd_3 +
                       uptake_rnd_4) == 0 &
                      date_round == 5 ~ "Never participated",           
                    (uptake_rnd_1 +
                       uptake_rnd_2 +
                       uptake_rnd_3) %in% 0 &
                      date_round == 4 ~ "Never participated",
                    (uptake_rnd_1 +
                       uptake_rnd_2) %in% 0 &
                      date_round %in% 3 ~ "Never participated",
                    uptake_rnd_1 %in% 0 &
                      date_round %in% 2 ~ "Never participated",
                    
                    date_round %in% 2 &
                      uptake_rnd_1 %in% 1 ~ "Participated in previous round",
                    date_round %in% 3 &
                      uptake_rnd_2 %in% 1 ~ "Participated in previous round",
                    date_round %in% 4 &
                      uptake_rnd_3 %in% 1 ~ "Participated in previous round",
                    date_round %in% 5 & 
                      uptake_rnd_4 %in% 1 ~ "Participated in previous round",
                    date_round %in% 6 & 
                      uptake_rnd_5 %in% 1 ~ "Participated in previous round",
                    date_round %in% 7 & 
                      uptake_rnd_6 %in% 1 ~ "Participated in previous round",
                    date_round %in% 8 & 
                      uptake_rnd_7 %in% 1 ~ "Participated in previous round",
                    
                    date_round %in% 2 &
                      uptake_rnd_1 %in% 0 ~ "Didn't participate in previous round",
                    date_round %in% 3 &
                      uptake_rnd_2 %in% 0 ~ "Didn't participate in previous round",
                    date_round %in% 4 &
                      uptake_rnd_3 %in% 0 ~ "Didn't participate in previous round",
                    date_round %in% 5 &
                      uptake_rnd_4 %in% 0 ~ "Didn't participate in previous round",
                    date_round %in% 6 &
                      uptake_rnd_5 %in% 0 ~ "Didn't participate in previous round",
                    date_round %in% 7 &
                      uptake_rnd_6 %in% 0 ~ "Didn't participate in previous round",
                    date_round %in% 8 &
                      uptake_rnd_7 %in% 0 ~ "Didn't participate in previous round"
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

#                                         Nov 2020 /  May 2021 /  Nov 2021 /  Nov 2022 /
# 1 First round                          2,575,614 / 2,582,506 / 2,677,873 / 2,780,631 /
# 2 Participated in previous round       4,341,720 / 4,452,272 / 4,745,431 / 5,309,710 /
# 3 Didn't participate in previous round   475,904 /   487,915 /   520,207 /   581,728 /
# 4 Never participated                   2,805,558 / 2,843,306 / 2,965,066 / 3,202,109 /
# 5 NA                                     108,298 /         0 /         0 /         0 /

# May 2022 
# 1 First round                          2,734,056
# 2 Participated in previous round       5,033,374
# 3 Didn't participate in previous round   551,738
# 4 Never participated                   3,085,896


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


### 3.4 Data checks ----

# Check for missing values of haemoglobin ----
slim_db %>% mutate(with_haem = if_else((is.na(haemoglobin)| 
                                haemoglobin == ""), 'No', 'Yes')) %>%
            count(screres, with_haem)

  # all screres == (21,22) should have a haemoglobin value
# all correct, Nov 2021
# all correct, May 2022
# all correct, Nov 2022
 
# Check if there any people who have fit positive with no haemoglobin or less than 80?
hame_check2 <- slim_db %>% 
                filter(screres == 22 & (is.na(haemoglobin)|haemoglobin < 80))
hame_check2 %>% count()
#View(hame_check2)
# 57 cases, Nov 2020
#  0 cases, Nov 2021
#  0 cases, May 2022
#  0 cases, Nov 2022

# Check if there any people who have fit negative with no haemoglobin or >= 80?
hame_check3 <- slim_db %>% 
                 filter(screres == 21 & (is.na(haemoglobin)|haemoglobin >= 80))
hame_check3 %>% count()
#View(hame_check3)
# 0 cases, Nov 2020
# 0 cases, May 2022
# 0 cases, Nov 2022

rm(hame_check2, hame_check3)


### Step 4: Save ----

#saveRDS(slim_db, file = analysis_db_path)
 write_rds(slim_db, file = here::here('Temp', 'analysis_dataset.rds'),
           compress = 'gz')
# write_rds(slim_db, paste0(wd, "/Temp/analysis_dataset.rds"),
#           compress = 'gz')


slim_db <- read_rds(here::here('Temp', 'analysis_dataset.rds')) %>%
           clean_names()


