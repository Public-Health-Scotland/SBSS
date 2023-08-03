#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 07_hb_reports.R
# Gavin Clark, Thomas Godfrey, Karen Hotopp
# Feb 2022
# Script 7 of 13
# Data preparation for export
# Written/run on R Studio Server
# R version 3.2.3
# This script creates the 2-year percentages and counts at health board level 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Step 0: Housekeeping ----

## Load packages
library(readr)
library(dplyr)
library(tidyr)
library(here)
library(haven)
library(stringr)
library(openxlsx)
library(tidylog)


## Set filepaths and extract dates with script 0
rm(list = ls())
source(here::here("Code","00_housekeeping.R"))
wd <- paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening",
             "/Publications/SBoSP-Statistics/20230804")
# source(paste0(wd, "/Code/00_housekeeping.R"))


## Functions

# Combines health board level KPI extracts (7_hb_reports.R)
HB_KPI <- function(hb) {
  
  all <-  filter(summary_all, hbr19 %in% !!sym(as.character("hb"))) %>%
          pivot_longer(!hbr19, names_to = "key", values_to = "value") %>%
          select(value) %>%
          rename(all = value)
  
  male <- filter(summary_male, hbr19 %in% !!sym(as.character("hb"))) %>%
          pivot_longer(!hbr19, names_to = "key", values_to = "value") %>%
          select(value) %>%
          rename(male = value)
  
  female <- filter(summary_female, hbr19 %in% !!sym(as.character("hb"))) %>%
            pivot_longer(!hbr19, names_to = "key", values_to = "value") %>%
            select(value) %>%
            rename(female = value)
  
  togetherNow <- bind_cols(male, female, all)
  
}

# writes to hb report excel
write_hb_report <- function(df, filename) {
  
  fname <- paste0(filename, "_KPI_", report_month, ".xlsx")
  
  hb_name <- gsub("_", " ", filename)
  
  hb_name <- str_to_title(hb_name)
  
  hb_col <- tibble(hb = rep(hb_name, 97)) %>% 
    mutate(hb = gsub(" And ", " and ", hb))
  
  template <- paste0(wd, "/Temp/HB_KPI_template.xlsx")
  
  wb <- loadWorkbook(template)
  
  renameWorksheet(wb, 2, report_month)
  
  writeData(wb, 2, hb_col, startRow = 2, startCol = 2, colNames = FALSE)
  
  writeData(wb, 2, df, startRow = 44, startCol = 6, colNames = FALSE)
  
  saveWorkbook(wb, paste0(here("Output/CONFI-individual-HB-reports/"), fname), 
               overwrite = T)
  
}


### Step 1: Import data ----

## Bring in analysis database from script 1
# Filter on dates - the current two year period is: 01/05/2019 to 30/04/2021
# Remove people over 75 who opted in, and keep only those in hbr14 1 to 14

analysis_db <- read_rds(analysis_db_path) %>%
               filter(hbr19 %in% 1:14 &
                      optin %in% 0 &
                      between(invdate, as.Date(date_first), as.Date(date_last)))



### Step 2: File for male and female combined ----

## i) Summarise variables ----
# (for all males and females) by Health Board
summary_one <- analysis_db %>%
                group_by(hbr19) %>%
                summarise(invite_n   = sum(invite_n),
                          positive_n = sum(positive_n),
                          negative_n = sum(negative_n),
                          uptake_n   = sum(uptake_n),
                          col_perf_n = sum(col_perf_n),
                          col_complete_n   = sum(col_complete_n),
                          col_complic_n    = sum(col_complic_n),
                          col_complic_o_n  = sum(col_complic_o_n),
                          col_death_n      = sum(col_death_n),
                          cancer_n         = sum(cancer_n),
                          polyp_cancer_n   = sum(polyp_cancer_n),
                          hr_adenoma_n     = sum(hr_adenoma_n),
                          ir_adenoma_n     = sum(ir_adenoma_n),
                          lr_adenoma_n     = sum(lr_adenoma_n),
                          # Unclassified risk adenomas
                          uncl_adenoma_n   = sum(uncl_adenoma_n),
                          adenoma_n        = sum(adenoma_n),
                          # Adenomas including those where cancer is diagnosed
                          all_adenoma_n    = sum(all_adenoma_n),
                          canc_hr_n        = sum(canc_hr_n),
                          all_neoplasia_n  = sum(all_neoplasia_n),
                          adenoma_col_n    = sum(adenoma_col_n),
                          hr_adenoma_col_n = sum(hr_adenoma_col_n),
                          canc_col_n       = sum(canc_col_n)) %>%
                ungroup()

summary_one %>% glimpse()


### ii) Invites ----
## Summarise invite numbers (for all males and females) by HB and Deprivation
summary_simd_invite <- analysis_db %>%
              group_by(hbr19, simd2020) %>%
              summarise(invite_n = sum(invite_n)) %>%
              ungroup() %>%
              filter(!is.na(simd2020)) %>%
              mutate(simd2020 = recode(as.character(simd2020),
                                       "1" = "simd_1",
                                       "2" = "simd_2",
                                       "3" = "simd_3",
                                       "4" = "simd_4",
                                       "5" = "simd_5")) %>%
              pivot_wider(names_from = simd2020, values_from = invite_n) %>%
              # Alternatives to pivot_wider( ,values_fill = 0) include:
              mutate(across(.cols = 'simd_1':'simd_5', ~ replace_na(., 0))) %>%
              # replace_na(list(simd_1 = 0, simd_2 = 0, simd_3 = 0, simd_4 = 0, simd_5 = 0))
              rename(simd_1_invite = simd_1,
                     simd_2_invite = simd_2,
                     simd_3_invite = simd_3,
                     simd_4_invite = simd_4,         
                     simd_5_invite = simd_5)

summary_simd_invite %>% glimpse()


## iii) Calculate total number of invited ----
## for screening in each HB
summary_total_invite <- summary_simd_invite %>%
                        mutate(simd_total_invite = simd_1_invite +
                                                   simd_2_invite +
                                                   simd_3_invite +
                                                   simd_4_invite +
                                                   simd_5_invite) %>%
                        select(hbr19, simd_total_invite)

summary_total_invite %>% glimpse()


### iv) Uptake ----
## Summarise uptake numbers (for all males and females) by HB and Deprivation
summary_simd_uptake <- analysis_db %>%
              group_by(hbr19, simd2020) %>%
              summarise(uptake_n = sum(uptake_n)) %>%
              ungroup() %>%
              filter(!is.na(simd2020)) %>%
              mutate(simd2020 = recode(as.character(simd2020),
                                       "1" = "simd_1",
                                       "2" = "simd_2",
                                       "3" = "simd_3",
                                       "4" = "simd_4",
                                       "5" = "simd_5")) %>%
              pivot_wider(names_from = simd2020, values_from = uptake_n) %>%
              mutate(across(.cols = 'simd_1':'simd_5', ~ replace_na(., 0))) %>%
              rename(simd_1_uptake = simd_1,
                     simd_2_uptake = simd_2,
                     simd_3_uptake = simd_3,
                     simd_4_uptake = simd_4,         
                     simd_5_uptake = simd_5)


summary_simd_uptake %>% glimpse()

# v) Calculate total uptake ----
# for screening in each HB
summary_total_uptake <- summary_simd_uptake %>%
                        mutate(simd_total_uptake = simd_1_uptake +
                                                   simd_2_uptake +
                                                   simd_3_uptake +
                                                   simd_4_uptake +
                                                   simd_5_uptake) %>%
                        select(hbr19, simd_total_uptake)

summary_total_uptake %>% glimpse()


### vi) Dukes Diagnoses ----
## Summarise numbers of cancers diagnosed at different dukes stages
# (for all males and females), in each HB
summary_dukes <- analysis_db %>%
                 group_by(hbr19, dukes_der) %>%
                 summarise(cancer_n = sum(cancer_n)) %>%
                 ungroup() %>%
                 arrange(dukes_der, hbr19) %>% 
                 filter(dukes_der != "") %>%
                 mutate(dukes_der = recode(dukes_der,
                                            "A" = "dukes_a",
                                            "B" = "dukes_b",
                                            "C" = "dukes_c",
                                            "D" = "dukes_d",
                                            "Not known" = "dukes_nk",
                                            "Not supplied" = "dukes_ns")) %>%
                 pivot_wider(names_from = dukes_der, values_from = cancer_n) %>%
                 mutate(across(.cols = 'dukes_a':'dukes_nk', ~ replace_na(., 0))) %>%
                 ##Would like to replace above mutate with values_fill in pivot_wider,
                 ##but values_fill not working correctly!
                 # Dukes 'not supplied' no longer applicable (variable 'dukes_ns' must be 0)
                 mutate(dukes_ns = 0) %>%
                 select(hbr19, dukes_a:dukes_ns) %>%
                 glimpse()


### vii) Colonoscopy ----
## Summarise number of patients waiting x-many weeks for colonoscopy 
# (for all males and females), in each HB
summary_wt <- analysis_db %>%
              group_by(hbr19, waiting_time_hb) %>%
              summarise(col_perf_n = sum(col_perf_n)) %>%
              ungroup() %>%
              filter(waiting_time_hb != "No colonoscopy") %>%
              mutate(waiting_time_hb = recode(waiting_time_hb,
                                                "0 to 2 weeks"  = "wt_0_2",
                                               ">2 to 4 weeks"  = "wt_2_4",
                                               ">4 to 6 weeks"  = "wt_4_6",
                                               ">6 to 8 weeks"  = "wt_6_8",
                                               ">8 to 10 weeks" = "wt_8_10",
                                              ">10 to 12 weeks" = "wt_10_12",
                                              ">12 to 14 weeks" = "wt_12_14",
                                              ">14 to 16 weeks" = "wt_14_16",
                                              ">16 to 18 weeks" = "wt_16_18",
                                              ">18 to 20 weeks" = "wt_18_20",
                                              ">20 weeks" = "wt_over_20")) %>%
              pivot_wider(names_from = waiting_time_hb, 
                          values_from = col_perf_n) %>%
              mutate(across(.cols = 'wt_0_2':'wt_over_20', ~ replace_na(., 0))) %>%
              glimpse()


## viii) Summarise numbers of different types of cancers ----
# (for all males and females), in each HB
summary_icd <- analysis_db %>%
               mutate(icd = if_else(is.na(icd), "99", as.character(icd))) %>%
               group_by(hbr19, icd) %>%
               summarise(cancer_n = sum(cancer_n)) %>%
               ungroup() %>%
               mutate(icd = recode(icd,
                                   "99" = "icd_nk",
                                   "C18" = "icd_c18",
                                   "C19" = "icd_c19",
                                   "C20" = "icd_c20")) %>%
               pivot_wider(names_from = icd, values_from = cancer_n) %>%
               mutate(across(.cols = 'icd_nk':'icd_c19', ~ replace_na(., 0))) %>%
               select(hbr19, icd_c18, icd_c19, icd_c20, icd_nk) %>%
               glimpse()


## ix) Join all objects together ----
# to create a single summary object
# First join summary one and invites
summary_all <- left_join(summary_one, summary_simd_invite, 
                         by = "hbr19") %>%
              # then progressively add on subsequent tables.
               left_join(summary_total_invite, by = "hbr19") %>%
               left_join(summary_simd_uptake, by = "hbr19") %>%
               left_join(summary_total_uptake, by = "hbr19") %>%
               left_join(summary_wt, by = "hbr19") %>%
               left_join(summary_dukes, by = "hbr19") %>%
               left_join(summary_icd, by = "hbr19") %>%
              # Select statement to put into proper order
               select(hbr19,
                      invite_n,
                      simd_1_invite:simd_5_invite,
                      simd_total_invite,
                      positive_n,
                      negative_n,
                      uptake_n,
                      simd_1_uptake:simd_5_uptake,
                      simd_total_uptake,
                      wt_0_2,
                      wt_2_4,
                      wt_4_6,
                      wt_6_8,
                      wt_8_10,
                      wt_10_12,
                      wt_12_14,
                      wt_14_16,
                      wt_16_18,
                      wt_18_20,
                      wt_over_20,
                      col_perf_n,
                      col_complete_n,
                      col_complic_n,
                      col_complic_o_n,
                      col_death_n,
                      cancer_n,
                      polyp_cancer_n,
                      dukes_a,
                      dukes_b,
                      dukes_c,
                      dukes_d,
                      dukes_nk,
                      dukes_ns,
                      hr_adenoma_n,
                      ir_adenoma_n,
                      lr_adenoma_n,
                      uncl_adenoma_n,
                      adenoma_n,
                      all_adenoma_n,
                      canc_hr_n,
                      all_neoplasia_n,
                      icd_c18,
                      icd_c19,
                      icd_c20,
                      icd_nk,
                      adenoma_col_n,
                      hr_adenoma_col_n,
                      canc_col_n)

## Remove temporary tables used in this all males and females section,
# so the workspace is fresh for the males (and then females) section below.

rm(summary_dukes, summary_icd, summary_one, summary_simd_invite,
   summary_simd_uptake, summary_total_invite, summary_total_uptake, summary_wt)



### Step 3: File for males only ----
## Bring in analysis database from script 1
analysis_db_male <- analysis_db %>%
                    filter(sex %in% 1) %>%
                    glimpse()



# i) Summarise variables ----
# (for males) by Health Board
summary_one_male <- analysis_db_male %>%
                    group_by(hbr19) %>%
                    summarise(
                      invite_n   = sum(invite_n),
                      positive_n = sum(positive_n),
                      negative_n = sum(negative_n),
                      uptake_n   = sum(uptake_n),
                      col_perf_n = sum(col_perf_n),
                      col_complete_n  = sum(col_complete_n),
                      col_complic_n   = sum(col_complic_n),
                      col_complic_o_n = sum(col_complic_o_n),
                      col_death_n     = sum(col_death_n),
                      cancer_n        = sum(cancer_n),
                      polyp_cancer_n  = sum(polyp_cancer_n),
                      hr_adenoma_n    = sum(hr_adenoma_n),
                      ir_adenoma_n    = sum(ir_adenoma_n),
                      lr_adenoma_n    = sum(lr_adenoma_n),
                      # Unclassified risk adenomas
                      uncl_adenoma_n  = sum(uncl_adenoma_n),
                      adenoma_n       = sum(adenoma_n),
                      # Adenomas including those where cancer is diagnosed
                      all_adenoma_n   = sum(all_adenoma_n),
                      canc_hr_n       = sum(canc_hr_n),
                      all_neoplasia_n = sum(all_neoplasia_n),
                      adenoma_col_n   = sum(adenoma_col_n),
                      hr_adenoma_col_n = sum(hr_adenoma_col_n),
                      canc_col_n      = sum(canc_col_n)) %>%
                    ungroup()


# ii) Summarise Invite numbers ----
# (for males) by HB and Deprivation
summary_simd_invite <- analysis_db_male %>%
                       group_by(hbr19, simd2020) %>%
                       summarise(invite_n = sum(invite_n)) %>%
                       ungroup() %>%
                       filter(!is.na(simd2020)) %>%
                       mutate(simd2020 = recode(as.character(simd2020),
                                                "1" = "simd_1",
                                                "2" = "simd_2",
                                                "3" = "simd_3",
                                                "4" = "simd_4",
                                                "5" = "simd_5")) %>%
                       pivot_wider(names_from = simd2020, values_from = invite_n) %>%
                       mutate(across(.cols = 'simd_1':'simd_5', ~ replace_na(., 0))) %>%
                       rename(simd_1_invite = simd_1,
                              simd_2_invite = simd_2,
                              simd_3_invite = simd_3,
                              simd_4_invite = simd_4,         
                              simd_5_invite = simd_5)


# iii) Calculate total number invited ----
# for screening in each HB
summary_total_invite <- summary_simd_invite %>%
                        mutate(simd_total_invite = simd_1_invite +
                                                   simd_2_invite +
                                                   simd_3_invite +
                                                   simd_4_invite +
                                                   simd_5_invite) %>%
                        select(hbr19, simd_total_invite)


# iv) Summarise Uptake numbers ----
# (for males) by HB and Deprivation
summary_simd_uptake <- analysis_db_male %>%
                       group_by(hbr19, simd2020) %>%
                       summarise(uptake_n = sum(uptake_n)) %>%
                       ungroup() %>%
                       filter(!is.na(simd2020)) %>%
                       mutate(simd2020 = recode(as.character(simd2020),
                                                "1" = "simd_1",
                                                "2" = "simd_2",
                                                "3" = "simd_3",
                                                "4" = "simd_4",
                                                "5" = "simd_5")) %>%
                       pivot_wider(names_from = simd2020, values_from = uptake_n) %>%
                       mutate(across(.cols = 'simd_1':'simd_5', ~ replace_na(., 0))) %>%
                       rename(simd_1_uptake = simd_1,
                              simd_2_uptake = simd_2,
                              simd_3_uptake = simd_3,
                              simd_4_uptake = simd_4,         
                              simd_5_uptake = simd_5)
  

# v) Calculate total number of completeing their test ----
# in each HB
summary_total_uptake <- summary_simd_uptake %>%
                        mutate(simd_total_uptake = simd_1_uptake +
                                                   simd_2_uptake +
                                                   simd_3_uptake +
                                                   simd_4_uptake +
                                                   simd_5_uptake) %>%
                        select(hbr19, simd_total_uptake)


# vi) Summarise numbers of cancers diagnosed at different Dukes stages ----
# (for males), in each HB
summary_dukes <- analysis_db_male %>%
                 group_by(hbr19, dukes_der) %>%
                 summarise(cancer_n = sum(cancer_n)) %>%
                 ungroup() %>%
                 arrange(dukes_der, hbr19) %>% 
                 filter(dukes_der != "") %>%
                 mutate(dukes_der = recode(dukes_der,
                                           "A" = "dukes_a",
                                           "B" = "dukes_b",
                                           "C" = "dukes_c",
                                           "D" = "dukes_d",
                                           "Not known"    = "dukes_nk",
                                           "Not supplied" = "dukes_ns")) %>%
                 pivot_wider(names_from = dukes_der, values_from = cancer_n) %>%
                 mutate(across(.cols = 'dukes_a':'dukes_nk', ~ replace_na(., 0))) %>%
                 # Dukes 'not supplied' no longer applicable (variable 'dukes_ns' must be 0)
                 mutate(dukes_ns = 0) %>%
                 select(hbr19, dukes_a:dukes_ns) %>%
                 glimpse()
                  
  
# vii) Summarise number of patients waiting x-many weeks for colonoscopy ----
# (for males), in each HB
summary_wt <- analysis_db_male %>%
              group_by(hbr19, waiting_time_hb) %>%
              summarise(col_perf_n = sum(col_perf_n)) %>%
              ungroup() %>%
              filter(waiting_time_hb != "No colonoscopy") %>%
              mutate(waiting_time_hb = recode(waiting_time_hb,
                                              "0 to 2 weeks"  = "wt_0_2",
                                              ">2 to 4 weeks" = "wt_2_4",
                                              ">4 to 6 weeks" = "wt_4_6",
                                              ">6 to 8 weeks" = "wt_6_8",
                                              ">8 to 10 weeks"  = "wt_8_10",
                                              ">10 to 12 weeks" = "wt_10_12",
                                              ">12 to 14 weeks" = "wt_12_14",
                                              ">14 to 16 weeks" = "wt_14_16",
                                              ">16 to 18 weeks" = "wt_16_18",
                                              ">18 to 20 weeks" = "wt_18_20",
                                              ">20 weeks"       = "wt_over_20")) %>%
              pivot_wider(names_from = waiting_time_hb, values_from = col_perf_n) %>%
              mutate(across(.cols = 'wt_0_2':'wt_over_20', ~ replace_na(., 0))) %>%
              glimpse()


# viii) Summarise numbers of different types of cancers ----
# (for males), in each HB
summary_icd <- analysis_db_male %>%
               mutate(icd = if_else(is.na(icd), "99", as.character(icd))) %>%
               group_by(hbr19, icd) %>%
               summarise(cancer_n = sum(cancer_n)) %>%
               ungroup() %>%
               mutate(icd = recode(icd,
                                   "99" = "icd_nk",
                                   "C18" = "icd_c18",
                                   "C19" = "icd_c19",
                                   "C20" = "icd_c20")) %>%
               pivot_wider(names_from = icd, values_from = cancer_n) %>%
               mutate(across(.cols = 'icd_nk':'icd_c19', ~ replace_na(., 0))) %>%
               select(hbr19, icd_c18, icd_c19, icd_c20, icd_nk) %>%
               glimpse()


## ix) Join all these objects together to create a single summary object ----
# First join summary one and invites
summary_male <- left_join(summary_one_male, summary_simd_invite, by = "hbr19") %>%
                # then add on subsequent tables
                left_join(summary_total_invite, by = "hbr19") %>%
                left_join(summary_simd_uptake,  by = "hbr19") %>%
                left_join(summary_total_uptake, by = "hbr19") %>%
                left_join(summary_wt,   by = "hbr19") %>%
                left_join(summary_dukes,by = "hbr19") %>%
                left_join(summary_icd,  by = "hbr19") %>%
                # Select statement to put into proper order
                select(hbr19,
                       invite_n,
                       simd_1_invite:simd_5_invite,
                       simd_total_invite,
                       positive_n,
                       negative_n,
                       uptake_n,
                       simd_1_uptake:simd_5_uptake,
                       simd_total_uptake,
                       wt_0_2,
                       wt_2_4,
                       wt_4_6,
                       wt_6_8,
                       wt_8_10,
                       wt_10_12,
                       wt_12_14,
                       wt_14_16,
                       wt_16_18,
                       wt_18_20,
                       wt_over_20,
                       col_perf_n,
                       col_complete_n,
                       col_complic_n,
                       col_complic_o_n,
                       col_death_n,
                       cancer_n,
                       polyp_cancer_n,
                       dukes_a,
                       dukes_b,
                       dukes_c,
                       dukes_d,
                       dukes_nk,
                       dukes_ns,
                       hr_adenoma_n,
                       ir_adenoma_n,
                       lr_adenoma_n,
                       uncl_adenoma_n,
                       adenoma_n,
                       all_adenoma_n,
                       canc_hr_n,
                       all_neoplasia_n,
                       icd_c18,
                       icd_c19,
                       icd_c20,
                       icd_nk,
                       adenoma_col_n,
                       hr_adenoma_col_n,
                       canc_col_n
                       )

## Remove temporary tables used for males section,
# so the workspace is fresh for the females section below.
rm(analysis_db_male, summary_dukes, summary_icd, summary_one_male, summary_simd_invite,
   summary_simd_uptake, summary_total_invite, summary_total_uptake, summary_wt)




### Step 4: File for females only ----
## Bring in analysis database from script 1

analysis_db_female <- analysis_db %>%
                      filter(sex %in% 2)


## i) Summarise variables ----
# (for females) by Health Board
summary_one_female <- analysis_db_female %>%
                      group_by(hbr19) %>%
                      summarise(
                        invite_n        = sum(invite_n),
                        positive_n      = sum(positive_n),
                        negative_n      = sum(negative_n),
                        uptake_n        = sum(uptake_n),
                        col_perf_n      = sum(col_perf_n),
                        col_complete_n  = sum(col_complete_n),
                        col_complic_n   = sum(col_complic_n),
                        col_complic_o_n = sum(col_complic_o_n),
                        col_death_n     = sum(col_death_n),
                        cancer_n        = sum(cancer_n),
                        polyp_cancer_n  = sum(polyp_cancer_n),
                        hr_adenoma_n    = sum(hr_adenoma_n),
                        ir_adenoma_n    = sum(ir_adenoma_n),
                        lr_adenoma_n    = sum(lr_adenoma_n),
                        # Unclassified risk adenomas
                        uncl_adenoma_n  = sum(uncl_adenoma_n),
                        adenoma_n       = sum(adenoma_n),
                        # Adenomas including those where cancer is diagnosed
                        all_adenoma_n    = sum(all_adenoma_n),
                        canc_hr_n        = sum(canc_hr_n),
                        all_neoplasia_n  = sum(all_neoplasia_n),
                        adenoma_col_n    = sum(adenoma_col_n),
                        hr_adenoma_col_n = sum(hr_adenoma_col_n),
                        canc_col_n       = sum(canc_col_n)) %>% 
                      ungroup()


## ii) Summarise Invite numbers ----
# (for females) by HB and Deprivation
summary_simd_invite <- analysis_db_female %>%
                       group_by(hbr19, simd2020) %>%
                       summarise(invite_n = sum(invite_n)) %>%
                       ungroup() %>%
                       filter(!is.na(simd2020)) %>%
                       mutate(simd2020 = recode(as.character(simd2020),
                                                "1" = "simd_1",
                                                "2" = "simd_2",
                                                "3" = "simd_3",
                                                "4" = "simd_4",
                                                "5" = "simd_5")) %>%
                       pivot_wider(names_from = simd2020, values_from = invite_n) %>%
                       mutate(across(.cols = 'simd_1':'simd_5', ~ replace_na(., 0))) %>%
                       rename(simd_1_invite = simd_1,
                              simd_2_invite = simd_2,
                              simd_3_invite = simd_3,
                              simd_4_invite = simd_4,         
                              simd_5_invite = simd_5) %>%
                       glimpse()


## iii) Calculate total number of invited ----
## for screening in each HB
summary_total_invite <- summary_simd_invite %>%
                        mutate(simd_total_invite = simd_1_invite +
                                                   simd_2_invite +
                                                   simd_3_invite +
                                                   simd_4_invite +
                                                   simd_5_invite) %>%
                        select(hbr19, simd_total_invite)


## iv) Summarise Uptake numbers ----
# (for females) by HB and Deprivation
summary_simd_uptake <- analysis_db_female %>%
                       group_by(hbr19, simd2020) %>%
                       summarise(uptake_n = sum(uptake_n)) %>%
                       ungroup() %>%
                       filter(!is.na(simd2020)) %>%
                       mutate(simd2020 = recode(as.character(simd2020),
                                                "1" = "simd_1",
                                                "2" = "simd_2",
                                                "3" = "simd_3",
                                                "4" = "simd_4",
                                                "5" = "simd_5")) %>%
                       pivot_wider(names_from = simd2020, values_from = uptake_n) %>%
                       mutate(across(.cols = 'simd_1':'simd_5', ~ replace_na(., 0))) %>%
                       rename(simd_1_uptake = simd_1,
                              simd_2_uptake = simd_2,
                              simd_3_uptake = simd_3,
                              simd_4_uptake = simd_4,         
                              simd_5_uptake = simd_5) %>%
                       glimpse()
  

## v) Calculate total number of completing their test ----
# in each HB
summary_total_uptake <- summary_simd_uptake %>%
                         mutate(simd_total_uptake = simd_1_uptake +
                                                    simd_2_uptake +
                                                    simd_3_uptake +
                                                    simd_4_uptake +
                                                    simd_5_uptake) %>%
                         select(hbr19, simd_total_uptake)


## vi) Summarise numbers of cancers diagnosed at different dukes' stages ----
# (for females), in each HB
summary_dukes <- analysis_db_female %>%
                 group_by(hbr19, dukes_der) %>%
                 summarise(cancer_n = sum(cancer_n)) %>%
                 ungroup() %>%
                 arrange(dukes_der, hbr19) %>% 
                 filter(dukes_der != "") %>%
                 mutate(dukes_der = recode(dukes_der,
                                           "A" = "dukes_a",
                                           "B" = "dukes_b",
                                           "C" = "dukes_c",
                                           "D" = "dukes_d",
                                           "Not known" = "dukes_nk",
                                           "Not supplied" = "dukes_ns")) %>%
                 pivot_wider(names_from = dukes_der, values_from = cancer_n) %>%
                 mutate(across(.cols = 'dukes_a':'dukes_nk', ~ replace_na(., 0))) %>%
                 # Dukes 'not supplied' no longer applicable (variable 'dukes_ns' must be 0)
                 mutate(dukes_ns = 0) %>%
                 select(hbr19, dukes_a:dukes_ns) %>%
                 glimpse()


## vii) Summarise number of patients waiting x-many weeks for colonoscopy ----
# (for females), in each HB
summary_wt <- analysis_db_female %>%
              group_by(hbr19, waiting_time_hb) %>%
              summarise(col_perf_n = sum(col_perf_n)) %>%
              ungroup() %>%
              filter(waiting_time_hb != "No colonoscopy") %>%
              mutate(waiting_time_hb = recode(waiting_time_hb,
                                              "0 to 2 weeks"  = "wt_0_2",
                                              ">2 to 4 weeks" = "wt_2_4",
                                              ">4 to 6 weeks" = "wt_4_6",
                                              ">6 to 8 weeks" = "wt_6_8",
                                              ">8 to 10 weeks"  = "wt_8_10",
                                              ">10 to 12 weeks" = "wt_10_12",
                                              ">12 to 14 weeks" = "wt_12_14",
                                              ">14 to 16 weeks" = "wt_14_16",
                                              ">16 to 18 weeks" = "wt_16_18",
                                              ">18 to 20 weeks" = "wt_18_20",
                                              ">20 weeks"       = "wt_over_20")) %>%
              pivot_wider(names_from = waiting_time_hb, values_from = col_perf_n) %>%
              mutate(across(.cols = 'wt_0_2':'wt_over_20', ~ replace_na(., 0))) %>%
              glimpse()


## viii) Summarise numbers of different types of cancers ----
# (for females), in each HB
summary_icd <- analysis_db_female %>%
                mutate(icd = if_else(is.na(icd), "99", as.character(icd))) %>%
                group_by(hbr19, icd) %>%
                summarise(cancer_n = sum(cancer_n)) %>%
                ungroup() %>%
                mutate(icd = recode(icd,
                                    "99" = "icd_nk",
                                    "C18" = "icd_c18",
                                    "C19" = "icd_c19",
                                    "C20" = "icd_c20")) %>%
                pivot_wider(names_from = icd, values_from = cancer_n) %>%
                mutate(across(.cols = 'icd_nk':'icd_c19', ~ replace_na(., 0))) %>%
                select(hbr19, icd_c18, icd_c19, icd_c20, icd_nk) %>%
                glimpse()


## ix) Join all these objects together to create a single summary object
# First join summary one and invites
summary_female <- left_join(summary_one_female, summary_simd_invite, 
                            by = "hbr19") %>%
                  # then add on subsequent tables.
                  left_join(summary_total_invite, by = "hbr19") %>%
                  left_join(summary_simd_uptake,  by = "hbr19") %>%
                  left_join(summary_total_uptake, by = "hbr19") %>%
                  left_join(summary_wt,   by = "hbr19") %>%
                  left_join(summary_dukes,by = "hbr19") %>%
                  left_join(summary_icd,  by = "hbr19") %>%
                  # Select statement to put into proper order
                  select(hbr19,
                         invite_n,
                         simd_1_invite:simd_5_invite,
                         simd_total_invite,
                         positive_n,
                         negative_n,
                         uptake_n,
                         simd_1_uptake:simd_5_uptake,
                         simd_total_uptake,
                         wt_0_2,
                         wt_2_4,
                         wt_4_6,
                         wt_6_8,
                         wt_8_10,
                         wt_10_12,
                         wt_12_14,
                         wt_14_16,
                         wt_16_18,
                         wt_18_20,
                         wt_over_20,
                         col_perf_n,
                         col_complete_n,
                         col_complic_n,
                         col_complic_o_n,
                         col_death_n,
                         cancer_n,
                         polyp_cancer_n,
                         dukes_a,
                         dukes_b,
                         dukes_c,
                         dukes_d,
                         dukes_nk,
                         dukes_ns,
                         hr_adenoma_n,
                         ir_adenoma_n,
                         lr_adenoma_n,
                         uncl_adenoma_n,
                         adenoma_n,
                         all_adenoma_n,
                         canc_hr_n,
                         all_neoplasia_n,
                         icd_c18,
                         icd_c19,
                         icd_c20,
                         icd_nk,
                         adenoma_col_n,
                         hr_adenoma_col_n,
                         canc_col_n
                         )


## Remove temporary tables used in this section,
rm(analysis_db, analysis_db_female, summary_dukes, summary_icd, summary_one_female, 
   summary_simd_invite, summary_simd_uptake, summary_total_invite, 
   summary_total_uptake, summary_wt)



### Step 5: Create individual files ----
## Uses HB_KPI function

## A & A
a_a <- HB_KPI(hb = 1)

## Borders
borders <- HB_KPI(hb = 2)

## D & G
d_g <- HB_KPI(hb = 3)

## Fife
fife <- HB_KPI(hb = 4)

## Forth Valley
f_v <- HB_KPI(hb = 5)

## Grampian
grampian <- HB_KPI(hb = 6)

## GGC
ggc <- HB_KPI(hb = 7)

## Highland
highland <- HB_KPI(hb = 8)

## Lanarkshire
lanarkshire <- HB_KPI(hb = 9)

## Lothian
lothian <- HB_KPI(hb = 10)

## Orkney
orkney <- HB_KPI(hb = 11)

## Shetland
shetland <- HB_KPI(hb = 12)

## Tayside
tayside <- HB_KPI(hb = 13)

## Western isles
w_i <- HB_KPI(hb  =  14)


# Clean up workspace
rm(summary_all, summary_female, summary_male)



### Step 6: Write to Excel ----

## NOTE: 
# this code updates the SIMD and waiting times data on lines 44-98 of the HB reports
# lines 2-43 are functions, which calculate KPIs from the data in lines 44-98.
# Once code below is run and Excel files are saved, they must be opened manually,
# and you must press: "Ctrl shift alt F9" to update all the functions in the tab.

write_hb_report(a_a, "Ayrshire_and_Arran")

write_hb_report(borders, "Borders")

write_hb_report(d_g, "Dumfries_and_Galloway")

write_hb_report(fife, "Fife")

write_hb_report(f_v, "Forth_Valley")

write_hb_report(grampian, "Grampian")

write_hb_report(ggc, "GGC")

write_hb_report(highland, "Highland")

write_hb_report(lanarkshire, "Lanarkshire")

write_hb_report(lothian, "Lothian")

write_hb_report(orkney, "Orkney")

write_hb_report(shetland, "Shetland")

write_hb_report(tayside, "Tayside")

write_hb_report(w_i, "Western_Isles")
