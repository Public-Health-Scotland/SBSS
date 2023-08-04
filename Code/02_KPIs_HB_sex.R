#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 02_KPIs_HB_sex.R
# Gavin Clark
# Aug 2022
# Script 2 of 13
# Data preparation for export
# Written/run on R Studio Server
# R version 3.6.1
# This script creates the 2-year percentage and demography tables for KPI report
# Transcribed from scripts at:
# //stats/CancerGroup1/Topics/BowelScreening/Publications/SBoSP-Statistics/
# 20190205/Syntax/1_KPIs_N18.sps for percentage tables and 
# //stats/CancerGroup1/Topics/BowelScreening/Publications/SBoSP-Statistics/
# 20190205/Syntax/5_Trend_uptake_N18 WIP_unsmoothed_FOBT-FIT_comparison.sps
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Future updates
# i) people often ask for information on absolute numbers:	
# #	How many people are screened per year, or round?
# How many people are sent for colonoscopy?
# How many people have an adenoma or cancer detected?
# Perhaps we should introduce a section/page in the report: ‘The Scottish Bowel Screening Programme in Numbers’?
# - should we produce a new functions to output the counts?
# ii) should we rename KPI_fraction to KPI_percent?
# iii) we should recreate sumtabs skeleton in R format


### Step 0: Housekeeping ----

library(readr)
library(dplyr)
library(tidyr)
library(readxl)
library(here)
library(haven)
library(ggplot2)
library(janitor)
library(tidylog)

## Set filepaths and extract dates with script 0
rm(list = ls())
source(here::here("Code","00_housekeeping.R"))
wd <- paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening",
             "/Publications/SBoSP-Statistics/20230804")
# source(paste0(wd, "/Code/00_housekeeping.R"))


## Define functions

# Calculate % fraction KPIs by health board and sex 

KPI_fraction <- function(num, den, kpi_no) {
  # By sex
  KPI_sex <- analysis_db %>%
             group_by(sex, hbr19) %>%
             summarise(p = sum(!!sym(num)) / sum(!!sym(den)) * 100) %>%
             ungroup()
  
  # For all sexes
  KPI_all <- analysis_db %>%
             group_by(hbr19) %>%
             summarise(p = sum(!!sym(num)) / sum(!!sym(den)) * 100) %>%
             mutate(sex = 3) %>%
             ungroup()
  
  # Scotland sexes
  KPI_Scot_sex <- analysis_db %>%
                  group_by(sex) %>%
                  summarise(p = sum(!!sym(num)) / sum(!!sym(den)) * 100) %>%
                  mutate(hbr19 = 15) %>%
                  ungroup()
  
  # Scotland total
  KPI_Scot_all <- analysis_db %>%
                  group_by() %>%
                  summarise(p = sum(!!sym(num)) / sum(!!sym(den)) * 100) %>%
                  mutate(hbr19 = 15, sex = 3) %>%
                  ungroup()
  
  # Combine all of the above
  
  KPI_all <- bind_rows(KPI_sex, KPI_all, KPI_Scot_sex, KPI_Scot_all) %>%
             mutate(KPI = kpi_no) %>%
             arrange(hbr19, sex) %>%
             pivot_wider(names_from = hbr19, values_from = p)
  
}


# Proportion function
KPI_proportion <- function(filter_column, filter_value, by, count_var, kpi_no) {
    # By sex
    KPI_sex <- analysis_db %>%
                filter(!!sym(filter_column) != filter_value) %>%
                group_by(sex, hbr19,!!sym(by)) %>%
                summarise(n = sum(!!sym(count_var))) %>%
                mutate(p = n / sum(n) ) %>%
                ungroup()
    
    # For all sexes
    KPI_all <- analysis_db %>%
                filter(!!sym(filter_column) != filter_value) %>%
                group_by(hbr19,!!sym(by)) %>%
                summarise(n = sum(!!sym(count_var)))  %>%
                mutate(sex = 3,
                       p = n / sum(n) ) %>%
                ungroup()
    
    # Scotland sexes
    KPI_Scot_sex <- analysis_db %>%
                    filter(!!sym(filter_column) != filter_value) %>%
                    group_by(sex,!!sym(by)) %>%
                    summarise(n = sum(!!sym(count_var))) %>%
                    mutate(hbr19 = 15,
                           p = n / sum(n) ) %>%
                    ungroup()
    
    # Scotland total
    KPI_Scot_all <- analysis_db %>%
                    filter(!!sym(filter_column) != filter_value) %>%
                    group_by(!!sym(by)) %>%
                    summarise(n = sum(!!sym(count_var)) ) %>%
                    mutate(hbr19 = 15, 
                           sex = 3,
                           p = n / sum(n) ) %>%
                    ungroup()
    
    # Combine all of the above
    KPI <- bind_rows(KPI_sex, KPI_all, KPI_Scot_sex, KPI_Scot_all) %>%
            group_by(!!sym(by), sex, hbr19) %>%
            mutate(KPI = kpi_no,
                   p = p * 100) %>%
            select(-n) %>%
            #pivot_wider(names_from = hbr19, values_from = p) # not sure why this doesn't work
            #                                                 # perhaps needs to id_cols specified
            spread(hbr19, p) %>%
            arrange(!!sym(by)) %>%
            ungroup()
}

KPI_proportion_f19 <- function(by, count_var, kpi_no) {
  # By sex
  KPI_sex <- analysis_db %>%
    group_by(sex, hbr19,!!sym(by)) %>%
    summarise(n = sum(!!sym(count_var))) %>%
    mutate(p = n / sum(n) ) %>%
    ungroup()
  
  # For all sexes
  KPI_all <- analysis_db %>%
    group_by(hbr19,!!sym(by)) %>%
    summarise(n = sum(!!sym(count_var)))  %>%
    mutate(sex = 3,
           p = n / sum(n) ) %>%
    ungroup()
  
  # Scotland sexes
  KPI_Scot_sex <- analysis_db %>%
    group_by(sex,!!sym(by)) %>%
    summarise(n = sum(!!sym(count_var))) %>%
    mutate(hbr19 = 15,
           p = n / sum(n) ) %>%
    ungroup()
  
  # Scotland total
  KPI_Scot_all <- analysis_db %>%
    group_by(!!sym(by)) %>%
    summarise(n = sum(!!sym(count_var)) ) %>%
    mutate(hbr19 = 15, 
           sex = 3,
           p = n / sum(n) ) %>%
    ungroup()
  
  # Combine all of the above
  KPI <- bind_rows(KPI_sex, KPI_all, KPI_Scot_sex, KPI_Scot_all) %>%
    group_by(!!sym(by), sex, hbr19) %>%
    mutate(KPI = kpi_no,
           p = p * 100) %>%
    select(-n) %>%
    #pivot_wider(names_from = hbr19, values_from = p) # not sure why this doesn't work
    #                                                 # perhaps needs to id_cols specified
    spread(hbr19, p) %>%
    arrange(!!sym(by)) %>%
    ungroup() |> 
    filter(is.na(icd) & sex == 3)
}



### Step 1: Prepare dataset ----

# Bring in analysis database from script 1_2_5
analysis_db <- read_rds(analysis_db_path) %>%
               filter(optin == 0 &
                      hbr19 %in% c(1:14) &
                      between(invdate, as.Date(date_first), as.Date(date_last))
                      )
dim(analysis_db)
# 1,884,815 - same as SPSS - Nov18 upload
# 1,866,332 - May 19 upload 
# 1,884,318 - Nov 19 upload
# 1,789,634 - Nov 20 upload
# 1,373,273 - May 21 upload
# 1,448,300 - Nov 21 upload
# 1,474,146 - May 22 upload
# 1,573,319 - Nov 22 upload
# 1,980,393 - May 23 upload

# Check overview of invitation dates
analysis_db %>%
  ggplot(aes(x = invdate)) +
  geom_histogram(binwidth = 1) +
  xlab("Invitation date") + ylab("Number of invitations per day") +
  scale_x_date(date_breaks = "months", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
# screening results per day looks sensible



### Step 2: Calculate KPIs ----

# 2.1 Calculate percentage KPIs using KPI_fraction function ----

KPI_1  <- KPI_fraction("uptake_n",        "invite_n",    1)
KPI_3  <- KPI_fraction("positive_n",      "uptake_n",    3)
KPI_5  <- KPI_fraction("col_perf_n",      "positive_n",  5)
KPI_6  <- KPI_fraction("col_complete_n",  "col_perf_n",  6)
KPI_7  <- KPI_fraction("col_complic_n",   "col_perf_n",  7)
KPI_8  <- KPI_fraction("cancer_n",        "uptake_n",    8)
KPI_17 <- KPI_fraction("polyp_cancer_n",  "uptake_n",   17)
KPI_18 <- KPI_fraction("polyp_cancer_n",  "cancer_n",   18)
KPI_19 <- KPI_fraction("adenoma_n",       "uptake_n",   19)
KPI_20 <- KPI_fraction("hr_adenoma_n",    "uptake_n",   20)
KPI_21 <- KPI_fraction("canc_col_n",      "col_perf_n", 21)
KPI_22 <- KPI_fraction("adenoma_col_n",   "col_perf_n", 22)
KPI_23 <- KPI_fraction("hr_adenoma_col_n","col_perf_n", 23)
KPI_24 <- KPI_fraction("canc_hr_n",       "col_perf_n", 24)
KPI_25 <- KPI_fraction("all_neoplasia_n", "col_perf_n", 25)


## 2.2 Calculate number of cancers recorded ----
cancers <- analysis_db %>%
           summarise(col_perf_n = sum(col_perf_n),
                     canc_col_n = sum(canc_col_n)
                     )
cancers


## 2.3 Calculate KPI 2 using bespoke code ----
# KPI 2 is uptake by SIMD 2020, 
# doesn't fit into KPI_fraction function

# By sex
KPI_sex <- analysis_db %>%
           group_by(sex, hbr19, simd2020) %>%
           summarise(p = sum(uptake_n) / sum(invite_n) * 100) %>%
           ungroup()

# For all sexes
KPI_all <- analysis_db %>%
           group_by(hbr19, simd2020) %>%
           summarise(p = sum(uptake_n) / sum(invite_n) * 100) %>%
           mutate(sex = 3) %>%
           ungroup()

# Scotland sexes
KPI_Scot_sex <- analysis_db %>%
                group_by(sex, simd2020) %>%
                summarise(p = sum(uptake_n) / sum(invite_n) * 100) %>%
                mutate(hbr19 = 15) %>%
                ungroup()

# Scotland total
KPI_Scot_all <- analysis_db %>%
                group_by(simd2020) %>%
                summarise(p = sum(uptake_n) / sum(invite_n) * 100) %>%
                mutate(hbr19 = 15, sex = 3) %>%
                ungroup()

# Combine all of the above
KPI_2 <- bind_rows(KPI_sex, KPI_all, KPI_Scot_sex, KPI_Scot_all) %>%
         mutate(KPI = 2) %>%
         arrange(hbr19, sex) %>%
         pivot_wider(names_from = hbr19, values_from = p) %>%
         filter(!is.na(simd2020)) %>%
         arrange(sex, desc(simd2020))




## 2.4 Calculate proportion KPIs using KPI_proportion function ----

## KPI 4 ----

KPI_4 <- KPI_proportion("waiting_time",
                        "No colonoscopy",
                        "waiting_time",
                        "col_perf_n", 4) %>% 
         arrange(sex)


## KPIs 9-16 ----

# Notes: 
# KPI 11 &  KPI 12 used to be Dukes C1 & C2, but these levels are now merged to just C
# (since we now translate this information from TNM codes)
# KPI 11 is now Dukes C-combined (and KPI 12 does not exist)
# KPI 15 [cancers with staging 'not supplied'] is no longer calculated, and
# neither is KPI 16 [cancers with staging supplied] as this is just the reciprocal of KPI 15.
# Nevertheless, since we're not re-numbering KPIs, keep all KPIs here for clarity.
# This also ensures the R data to be pasted into Excel matches the format baked into the Excel.

# Data exploration 
analysis_db %>% count(dukes)
analysis_db %>% count(dukes_der)
analysis_db %>% count(cancer_n, dukes_der)

analysis_db %>% count(cancer_n, dukes, dukes_der) %>% 
                print(n=100)


# Calculate KPIs

KPI_9_16 <- KPI_proportion("dukes_der", "", "dukes_der", "cancer_n", 9) 

KPI_9_16 %>% print()

# Update KPI numbers 
# (we need to add in 12, 15 and 16 manually)
KPI_9_16 <- KPI_9_16 %>%
            mutate(KPI = case_when(dukes_der %in% 'A' ~ 9,
                                   dukes_der %in% 'B' ~ 10,
                                   dukes_der %in% 'C' ~ 11,
                                   dukes_der %in% 'D' ~ 13,
                                   dukes_der %in% 'Not known' ~ 14,
                                   TRUE ~ -99))

KPI_9_16 %>% count(KPI)
KPI_9_16 %>% print()


## 2.5 Add in blank data for KPIs 12, 15 and 16 so that the Excel cell references match up

# KPI 12 - defunct Dukes C2 category
KPI_12 <- tibble(sex = c(1, 2, 3),
                 dukes_der = as.character(c(NA, NA, NA)),
                 KPI = c(12, 12, 12))


# KPI 15 - Dukes cancer stage not supplied
KPI_15 <- tibble(sex = c(1, 2, 3),
                             dukes_der = c("Not supplied", "Not supplied", "Not supplied"),
                             KPI = c(15, 15, 15))

# KPI 16 - Dukes cancer stage supplied
KPI_16 <- tibble(sex = c(1, 2, 3),
                 dukes_der = c("Stage supplied", "Stage supplied", "Stage supplied"),
                 KPI = c(16, 16, 16))

# Append to other KPIs
KPI_9_16 <- KPI_9_16 %>%
            bind_rows(KPI_12) %>%
            bind_rows(KPI_15) %>%
            bind_rows(KPI_16) %>%
            arrange(KPI, sex) 

KPI_9_16 %>% print(n=100)


# GC TO DO, automate this all so that it can be merged with skeleton/
# have the correct number of rows
check <- analysis_db %>% 
         filter(dukes == "99" & dukes_der == "Not known")
View(check)


## KPIs 26-28 ----

KPI_26_28 <- KPI_proportion("icd", "99", "icd", "cancer_n", 26) %>%
             filter(icd %in% c("C18","C19","C20")) %>%
             mutate(KPI = case_when(icd %in% 'C18' ~ 26,
                                    icd %in% 'C19' ~ 27,
                                    icd %in% 'C20' ~ 28,
                                    TRUE ~ -99)
                    )
  
KPI_26_28 %>% count(KPI)
KPI_26_28 %>% print()

# Figure 19
# Get list of cancers with no ICD-10 codes

f19 <- KPI_proportion_f19("icd", "cancer_n", 26)

# Pull together basic, 2-year KPI data
KPI_data <- bind_rows(KPI_1,
                      KPI_2,
                      KPI_3,
                      KPI_4,
                      KPI_5,
                      KPI_6,
                      KPI_7,
                      KPI_8,
                      KPI_9_16,
                      KPI_17,
                      KPI_18,
                      KPI_19,
                      KPI_20,
                      KPI_21,
                      KPI_22,
                      KPI_23,
                      KPI_24,
                      KPI_25,
                      KPI_26_28
                      )

View(KPI_data)
KPI_data




skeleton <- #read_sav(here::here("Temp", "skeleton1_(v02)_data_SUMTABS.sav")) %>%
  read_sav(paste0(wd, "/Temp/skeleton1_(v02)_data_SUMTABS.sav")) %>%
            clean_names() %>%
            filter(kpi %in% c(1:28) &
                   index1 == 3) %>%
            mutate(waiting_time = case_when(time_ref == 1 ~ "0 to 4 weeks",
                                            time_ref == 2 ~ "4 to 8 weeks",
                                            time_ref == 3 ~ ">8 weeks",
                                            TRUE ~ as.character(NA)),
                   simd2020 = simd2016) %>%
            select(kpi, index1, sex, simd2020, waiting_time)



KPI_data_full <- left_join(skeleton, KPI_data, 
                           by = c("kpi" = "KPI", 
                                  "sex", 
                                  "simd2020" = "simd2020",
                                  "waiting_time"))

library(labelled)
KPI_data_full <- KPI_data_full %>%
                 select(kpi, index1, sex, simd2020, waiting_time, '1':'15') %>%
                 arrange(kpi, sex, desc(simd2020)) %>%
                 mutate(index1 = to_character(index1),
                        sex = to_character(sex),
                        simd2020 = to_character(simd2020))

View(KPI_data_full)
dim(KPI_data_full)
# 102 rows


# TO DO - Complications with no colonoscopies performed.Add to QA process in SPSS
# TO DO - Script 1 - cancers detected with no colonoscopy performed, 
# add to QA in SPSS
# TO DO - Not checking for Dukes staging currently, find a way to do this vs.
# spss

# Demography information for appendices
## GC - label HB

sex_dem <- analysis_db %>%
            select(hbr19, sex, invite_n) %>%
            #filter(optin == 1) %>% # Use for optin report only
            group_by(hbr19, sex) %>%
            summarise(invite_n = sum(invite_n)) %>%
            pivot_wider(names_from = sex, values_from = invite_n) %>%
            ungroup()


age_dem <- analysis_db %>%
            select(hbr19, age_group, invite_n) %>%
            group_by(hbr19, age_group) %>%
            summarise(invite_n = sum(invite_n)) %>%
            pivot_wider(names_from = age_group, values_from = invite_n) %>%
            ungroup()

## TO DO - need to figure out how we will order and output by health board, may
## be for the excel output script(s) or may be for here
simd_dem <- analysis_db %>%
            select(hbr19, simd2020, invite_n) %>%
            filter(!is.na(simd2020)) %>%
            group_by(hbr19, simd2020) %>%
            summarise(invite_n = sum(invite_n)) %>%
            pivot_wider(names_from = simd2020, values_from = invite_n) %>%
            ungroup()

# Possible to do for future - number of cancers with no ICD10 included 
# (tends to be zero)




### Step 3: Save files ----

# Unsure of best way to save these files, as current approach requires
# combination of:
# - server R (because file is large)
# - stats network (to load/save PII files)
# - local drive (to upload to github)


# MT 6-1-21 - relative filepaths incorrect, absolute paths used - fix needed
# KH 3/9/21 - switched file paths as absolute not working

# write_rds(KPI_data_full, here::here("Temp", "KPI_data.rds"), 
#           compress = 'gz')
write_rds(KPI_data_full, paste0(wd, "/Temp/KPI_data.rds"), 
          compress = 'gz')


# write_rds(sex_dem, here::here("Temp", "sex_dem.rds"), 
#           compress = 'gz')
write_rds(sex_dem, paste0(wd, "/Temp/sex_dem.rds"), 
          compress = 'gz')


# write_rds(age_dem, here::here("Temp", "age_dem.rds"), 
#           compress = 'gz')
write_rds(age_dem, paste0(wd, "/Temp/age_dem.rds"), 
          compress = 'gz')


# write_rds(simd_dem, here::here("Temp", "simd_dem.rds"), 
#           compress = 'gz')
write_rds(simd_dem, paste0(wd, "/Temp/simd_dem.rds"), 
          compress = 'gz')

write_rds(f19, paste0(wd, "/Temp/figure_19.rds"), 
          compress = 'gz')


# Data_check
KPI_data_full <- read_rds(here::here('Temp', 'KPI_data.rds')) %>%
                 clean_names()
