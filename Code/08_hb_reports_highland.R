#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 08_hb_reports_highland.R
# Gavin Clark and Thomas Godfrey
# Feb 2022
# Script 8 of 13
# Data preparation for export
# Written/run on R Studio Server
# R version 3.6.1
# This script creates the 2-year percentages and counts for NHS Highland
# split for the 2006 health board configuration
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### Step 0: Housekeeping ----

library(readr)
library(dplyr)
library(tidyr)
library(here)
library(haven)
library(openxlsx)
library(stringr)
library(tidylog)
library(phsmethods)

# set filepaths and extract dates with script 0
rm(list = ls())
source(here::here("Code","00_housekeeping.R"))
wd <- paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening",
             "/Publications/SBoSP-Statistics/20230804")
# source(paste0(wd, "/Code/00_housekeeping.R"))

# Define latest postcode directory

spd_version <- "2023_1"

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
  
  hb_col <- tibble(hb_col = rep(hb_name, 97))
  
  template <- paste0(wd, "/Temp/HB_KPI_template.xlsx")
  
  wb <- loadWorkbook(template)
  
  renameWorksheet(wb, 2, report_month)
  
  writeData(wb, 2, hb_col, startRow = 2, startCol = 2, colNames = FALSE)
  
  writeData(wb, 2, df, startRow = 44, startCol = 6, colNames = FALSE)
  
  saveWorkbook(wb, paste0(here("Output/CONFI-individual-HB-reports/"), fname), 
               overwrite = T)
  
}


### Step 1: Ensure hbres and postcode data are available ----

# Bring in analysis database from script 1 
# and check if hbres and postcode where retained in the file

analysis_db <- read_rds(analysis_db_path) 

names(analysis_db)
# Yes.


#~~~~~~~~~~~~~~~~~~~~~~~~~~
# This used to not be true. We now longer need this section but retaining for now.
#~~~~~~~~~~~~~~~~~~~~~~~~~~
# Bring in full database as well
# so we can recover hbres and postcode (which were excluded from analysis_db)
# can then match these postcodes onto the Scottish Postcode Directory
# to obtain Highland North and Highland South data.

#raw_db <- read_sav(sbsdb_path) %>%
#          clean_names() %>%
#          select(chinum, patpcode, date_round, hbres) %>%
#          mutate(patpcode = str_remove(patpcode, " "))

# Create a new analysis_db by joining analysis_db and raw_db, 
# keeping records for patients present in both, according to patient and round.
#analysis_db2 <- inner_join(analysis_db, raw_db, by = c("chinum" = "chinum",
#                                                       "date_round" = "date_round"))
# create new postcode variable without spaces for matching
#mutate(patpcode = str_remove(patpcode, " "))
# This #analysis_db2 would then be plugged in below.
#~~~~~~~~~~~~~~~~~~~~~~~~~~


# Bring in Scottish postcode directory so we can get hb2006 geographies
spd <- read_rds(paste0("/conf/linkage/output/lookups/Unicode/Geography/",
                       "Scottish Postcode Directory/", 
                       "Scottish_Postcode_Directory_", spd_version, ".rds")) %>%
  # keep using HB2006?
  # checked with NHS Highland (otherwise data continuity issues at their end)
  filter(hb2006 %in% 'S08000008') %>%
  select(hb2006, pc8) %>%
  # create new postcode variable without spaces for matching
  mutate(patpcode = str_remove(pc8, " "))

# Prepare patpcode for matching
analysis_db <- analysis_db %>% 
  mutate(patpcode = str_remove(patpcode, " "))

# Join the analysis_db and Scottish Postcode directory,
# and define Highland North and Highland South
highland <- inner_join(analysis_db, spd, 
                       by  = "patpcode") %>%
  mutate(hbr19 = case_when(hbres %in% c("G", "C") ~ "South",
                           hbres %in% "H" ~ "North")) %>%
  filter(!is.na(hbr19))


rm(analysis_db, analysis_db_path, sbsdb_path)

gc()



### Step 2: File for male and female combined ----

## GC TO DO - create function for male/female/all

## Tidy up the data
# Filter on dates, remove optins

analysis_db <- highland %>%
  filter(optin %in% 0 &
           between(invdate, as.Date(date_first), as.Date(date_last))
  )


# Summarise variables (for all males and females) by Health Board
summary_one <- analysis_db %>%
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
    all_adenoma_n    = sum(all_adenoma_n),
    canc_hr_n        = sum(canc_hr_n),
    all_neoplasia_n  = sum(all_neoplasia_n),
    adenoma_col_n    = sum(adenoma_col_n),
    hr_adenoma_col_n = sum(hr_adenoma_col_n),
    canc_col_n       = sum(canc_col_n)
  )  

# Summarise Invite numbers (for all males and females) by HB and Deprivation
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
  mutate(across(.cols = 'simd_1':'simd_5', ~ replace_na(., 0))) %>%
  rename(simd_1_invite = simd_1,
         simd_2_invite = simd_2,
         simd_3_invite = simd_3,
         simd_4_invite = simd_4,         
         simd_5_invite = simd_5)

# Calculate total number of people invited for screening in each HB
summary_total_invite <- summary_simd_invite %>%
  mutate(simd_total_invite = simd_1_invite +
           simd_2_invite +
           simd_3_invite +
           simd_4_invite +
           simd_5_invite) %>%
  select(hbr19, simd_total_invite)


# Summarise Uptake numbers (for all males and females) by HB and Deprivation
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

# Calculate total number of males completeting their test in each HB
summary_total_uptake <- summary_simd_uptake %>%
  mutate(simd_total_uptake = simd_1_uptake +
           simd_2_uptake +
           simd_3_uptake +
           simd_4_uptake +
           simd_5_uptake) %>%
  select(hbr19, simd_total_uptake)


# Summarise numbers of cancers diagnosed at different dukes' stages
# (for all males and females), in each HB
# Dukes not supplied no longer applicable (i.e. there can be no 'dukes_ns' variable)
summary_dukes <- analysis_db %>%
  group_by(hbr19, dukes_der) %>%
  summarise(cancer_n = sum(cancer_n)) %>%
  ungroup() %>%
  filter(dukes_der != "") %>%
  mutate(dukes_der = recode(dukes_der,
                            "A" = "dukes_a",
                            "B" = "dukes_b",
                            "C" = "dukes_c",
                            "D" = "dukes_d",
                            "Not known" = "dukes_nk",
                            "Not supplied" = "dukes_ns")) %>%
  pivot_wider(names_from = dukes_der, values_from = cancer_n) %>%
  mutate(dukes_nk = 0) %>% # CP added Jan 23 as no dukes_nk in this run
  mutate(across(.cols = 'dukes_a':'dukes_nk', ~ replace_na(., 0))) %>%
  # Insert a variable for dukes_ns = 0. This variable is now defunct
  # But create here and then we keep code consistent with hb_reports
  mutate(dukes_ns = 0) 


# Summarise number of patients waiting x-many weeks for colonoscopy 
# (for all males and females), in each HB
summary_wt <- analysis_db %>%
  group_by(hbr19, waiting_time_hb) %>%
  summarise(col_perf_n = sum(col_perf_n)) %>%
  ungroup() %>%
  filter(waiting_time_hb != "No colonoscopy") %>%
  mutate(waiting_time_hb = recode(waiting_time_hb,
                                  "0 to 2 weeks"    = "wt_0_2",
                                  ">2 to 4 weeks"   = "wt_2_4",
                                  ">4 to 6 weeks"   = "wt_4_6",
                                  ">6 to 8 weeks"   = "wt_6_8",
                                  ">8 to 10 weeks"  = "wt_8_10",
                                  ">10 to 12 weeks" = "wt_10_12",
                                  ">12 to 14 weeks" = "wt_12_14",
                                  ">14 to 16 weeks" = "wt_14_16",
                                  ">16 to 18 weeks" = "wt_16_18",
                                  ">18 to 20 weeks" = "wt_18_20",
                                  ">20 weeks"       = "wt_over_20")) %>%
  pivot_wider(names_from = waiting_time_hb, values_from = col_perf_n) %>%
  mutate(across(where(is.numeric), ~  replace_na(., 0)))


# Summarise numbers of different types of cancers 
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
  mutate(across(.cols = 'icd_nk':'icd_c20', ~ replace_na(., 0))) %>%
  # Insert icd_c19 = 0 - because there are no C19s records
  # this allows us to keep the code consistent with hb_reports
  mutate(icd_c19 = 0) %>%
  select(hbr19, icd_c18, icd_c19, icd_c20, icd_nk)


## Join all these objects together to create a single summary object
# First join summary one and invites
summary_all <- left_join(summary_one, summary_simd_invite, by = "hbr19") %>%
  # then progressively add on subsequent tables.
  left_join(summary_total_invite, by = "hbr19") %>%
  left_join(summary_simd_uptake, by = "hbr19") %>%
  left_join(summary_total_uptake, by = "hbr19") %>%
  left_join(summary_wt, by = "hbr19") %>%
  left_join(summary_dukes, by = "hbr19") %>%
  left_join(summary_icd, by = "hbr19") %>%
  # Select statement to put into proper order
  select(
    hbr19,
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


## Remove temporary tables used in this section
rm(analysis_db, summary_dukes, summary_icd, summary_one, summary_simd_invite,
   summary_simd_uptake, summary_total_invite, summary_total_uptake, summary_wt)



### Step 3: File for males only ----

## Tidy up the data
# Filter on dates, remove optins and ensure only males are selected.
analysis_db <- highland %>%
  filter(optin %in% 0 & 
           between(invdate, 
                   as.Date(date_first), 
                   as.Date(date_last)) &
           sex %in% 1)


# Summarise variables (for males) by Health Board
summary_one <- analysis_db %>%
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
    all_adenoma_n    = sum(all_adenoma_n),
    canc_hr_n        = sum(canc_hr_n),
    all_neoplasia_n  = sum(all_neoplasia_n),
    adenoma_col_n    = sum(adenoma_col_n),
    hr_adenoma_col_n = sum(hr_adenoma_col_n),
    canc_col_n       = sum(canc_col_n)
  ) 

# Summarise Invite numbers (for males) by HB and Deprivation
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
  mutate(across(.cols = simd_1:simd_5, ~ replace_na(., 0))) %>%
  rename(simd_1_invite = simd_1,
         simd_2_invite = simd_2,
         simd_3_invite = simd_3,
         simd_4_invite = simd_4,         
         simd_5_invite = simd_5)


# Calculate total number of people invited for screening in each HB
summary_total_invite <- summary_simd_invite %>%
  mutate(simd_total_invite = simd_1_invite +
           simd_2_invite +
           simd_3_invite +
           simd_4_invite +
           simd_5_invite) %>%
  select(hbr19, simd_total_invite)


# Summarise Uptake numbers (for males) by HB and Deprivation
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
  mutate(across(.cols = simd_1:simd_5, ~ replace_na(., 0))) %>%
  rename(simd_1_uptake = simd_1,
         simd_2_uptake = simd_2,
         simd_3_uptake = simd_3,
         simd_4_uptake = simd_4,         
         simd_5_uptake = simd_5)


# Calculate total number of males completeting their test in each HB
summary_total_uptake <- summary_simd_uptake %>%
  mutate(simd_total_uptake = simd_1_uptake +
           simd_2_uptake +
           simd_3_uptake +
           simd_4_uptake +
           simd_5_uptake) %>%
  select(hbr19, simd_total_uptake)


# Summarise numbers of cancers diagnosed at different dukes' stages
# (for males), in each HB
# Dukes not supplied no longer applicable (i.e. there can be no 'dukes_ns' variable)
summary_dukes <- analysis_db %>%
  group_by(hbr19, dukes_der) %>%
  summarise(cancer_n = sum(cancer_n)) %>%
  ungroup() %>%
  filter(dukes_der != "") %>%
  mutate(dukes_der = recode(dukes_der,
                            "A" = "dukes_a",
                            "B" = "dukes_b",
                            "C" = "dukes_c",
                            "D" = "dukes_d",
                            "Not known" = "dukes_nk",
                            "Not supplied" = "dukes_ns")) %>%
  pivot_wider(names_from = dukes_der, values_from = cancer_n) %>%
  mutate(dukes_nk = 0) %>% # CP added Jan 23 as no dukes_nk in this run
  mutate(across(.cols = dukes_a:dukes_nk, ~ replace_na(., 0))) %>%
  # Insert a variable for dukes_ns = 0. This variable is now defunct
  # But create here and then we keep code consistent with hb_reports
  mutate(dukes_ns = 0) %>%
  select(hbr19, dukes_a, dukes_b, dukes_c, dukes_d, 
         dukes_nk, dukes_ns)


# Summarise number of patients waiting x-many weeks for colonoscopy 
# (for males), in each HB
summary_wt <- analysis_db %>%
  group_by(hbr19, waiting_time_hb) %>%
  summarise(col_perf_n = sum(col_perf_n)) %>%
  ungroup() %>%
  filter(waiting_time_hb != "No colonoscopy") %>%
  mutate(waiting_time_hb = recode(waiting_time_hb,
                                  "0 to 2 weeks"    = "wt_0_2",
                                  ">2 to 4 weeks"   = "wt_2_4",
                                  ">4 to 6 weeks"   = "wt_4_6",
                                  ">6 to 8 weeks"   = "wt_6_8",
                                  ">8 to 10 weeks"  = "wt_8_10",
                                  ">10 to 12 weeks" = "wt_10_12",
                                  ">12 to 14 weeks" = "wt_12_14",
                                  ">14 to 16 weeks" = "wt_14_16",
                                  ">16 to 18 weeks" = "wt_16_18",
                                  ">18 to 20 weeks" = "wt_18_20",
                                  ">20 weeks"       = "wt_over_20")) %>%
  pivot_wider(names_from = waiting_time_hb, values_from = col_perf_n) %>%
  mutate(across(where(is.numeric), ~  replace_na(., 0)))


# Summarise numbers of different types of cancers (for males), in each HB
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
  mutate(across(.cols = icd_nk:icd_c20, ~  replace_na(., 0))) %>%
  # Insert icd_c19 = 0 - because there are no C19s records
  # this allows us to keep the code consistent with hb_reports
  mutate(icd_c19 = 0) %>%  
  select(hbr19, icd_c18, icd_c19, icd_c20, icd_nk)



## Join all these objects together to create a single summary object
# First join summary one and invites
summary_male <- left_join(summary_one, summary_simd_invite, by = "hbr19") %>%
  # then progressively add on subsequent tables.
  left_join(summary_total_invite, by = "hbr19") %>%
  left_join(summary_simd_uptake,  by = "hbr19") %>%
  left_join(summary_total_uptake, by = "hbr19") %>%
  left_join(summary_wt,    by = "hbr19") %>%
  left_join(summary_dukes, by = "hbr19") %>%
  left_join(summary_icd,   by = "hbr19") %>%
  # Select statement to put into proper order
  select(
    hbr19,
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

## Remove temporary tables used in this section.
rm(analysis_db, summary_dukes, summary_icd, summary_one, summary_simd_invite,
   summary_simd_uptake, summary_total_invite, summary_total_uptake, summary_wt)



### Step 4: File for females only ----

## Tidy up the data
# Filter on dates, remove optins and ensure only females are selected.
analysis_db <- highland %>%
  filter(optin %in% 0 &
           between(invdate,
                   as.Date(date_first),
                   as.Date(date_last)) &
           sex %in% 2)


# Summarise variables (for females) by Health Board
summary_one <- analysis_db %>%
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
    polyp_cancer_n = sum(polyp_cancer_n),
    hr_adenoma_n   = sum(hr_adenoma_n),
    ir_adenoma_n   = sum(ir_adenoma_n),
    lr_adenoma_n   = sum(lr_adenoma_n),
    # Unclassified risk adenomas
    uncl_adenoma_n = sum(uncl_adenoma_n),
    adenoma_n      = sum(adenoma_n),
    # Adenomas including those where cancer is diagnosed
    all_adenoma_n    = sum(all_adenoma_n),
    canc_hr_n        = sum(canc_hr_n),
    all_neoplasia_n  = sum(all_neoplasia_n),
    adenoma_col_n    = sum(adenoma_col_n),
    hr_adenoma_col_n = sum(hr_adenoma_col_n),
    canc_col_n       = sum(canc_col_n)
  ) 


# Summarise Invite numbers (for females) by HB and Deprivation
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
  mutate(across(.cols = simd_1:simd_5, ~ replace_na(., 0))) %>%
  rename(simd_1_invite = simd_1,
         simd_2_invite = simd_2,
         simd_3_invite = simd_3,
         simd_4_invite = simd_4,         
         simd_5_invite = simd_5)

# Calculate total number of people invited for screening in each HB
summary_total_invite <- summary_simd_invite %>%
  mutate(simd_total_invite = simd_1_invite +
           simd_2_invite +
           simd_3_invite +
           simd_4_invite +
           simd_5_invite) %>%
  select(hbr19, simd_total_invite)


# Summarise Uptake numbers (for females) by HB and Deprivation
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
  mutate(across(.cols = simd_1:simd_5, ~ replace_na(., 0))) %>%
  rename(simd_1_uptake = simd_1,
         simd_2_uptake = simd_2,
         simd_3_uptake = simd_3,
         simd_4_uptake = simd_4,         
         simd_5_uptake = simd_5)

# Calculate total number of males completeting their test in each HB
summary_total_uptake <- summary_simd_uptake %>%
  mutate(simd_total_uptake = simd_1_uptake +
           simd_2_uptake +
           simd_3_uptake +
           simd_4_uptake +
           simd_5_uptake) %>%
  select(hbr19, simd_total_uptake)


# Summarise numbers of cancers diagnosed at different dukes' stages
# (for all males and females), in each HB
# Dukes not supplied no longer applicable (i.e. there can be no 'dukes_ns' variable)
summary_dukes <- analysis_db %>%
  group_by(hbr19, dukes_der) %>%
  summarise(cancer_n = sum(cancer_n)) %>%
  ungroup() %>%
  filter(dukes_der != "") %>%
  mutate(dukes_der = recode(dukes_der,
                            "A" = "dukes_a",
                            "B" = "dukes_b",
                            "C" = "dukes_c",
                            "D" = "dukes_d",
                            "Not known" = "dukes_nk",
                            "Not supplied" = "dukes_ns")) %>%
  pivot_wider(names_from = dukes_der, values_from = cancer_n) %>%
  mutate(dukes_nk = 0) %>% # CP added Jan 23 as no dukes_nk in this run
  mutate(across(.cols = dukes_a:dukes_nk, ~ replace_na(., 0))) %>%
  # Insert a variable for dukes_ns = 0. This variable is now defunct
  # But create here and then we keep code consistent with hb_reports
  mutate(dukes_ns = 0 ) %>%   
  select(hbr19, dukes_a, dukes_b, dukes_c, dukes_d, dukes_nk, dukes_ns)


# Summarise number of patients waiting x-many weeks for colonoscopy 
# (for females), in each HB
summary_wt <- analysis_db %>%
  group_by(hbr19, waiting_time_hb) %>%
  summarise(col_perf_n = sum(col_perf_n)) %>%
  ungroup() %>%
  filter(waiting_time_hb != "No colonoscopy") %>%
  mutate(waiting_time_hb = recode(waiting_time_hb,
                                  "0 to 2 weeks"    = "wt_0_2",
                                  ">2 to 4 weeks"   = "wt_2_4",
                                  ">4 to 6 weeks"   = "wt_4_6",
                                  ">6 to 8 weeks"   = "wt_6_8",
                                  ">8 to 10 weeks"  = "wt_8_10",
                                  ">10 to 12 weeks" = "wt_10_12",
                                  ">12 to 14 weeks" = "wt_12_14",
                                  ">14 to 16 weeks" = "wt_14_16",
                                  ">16 to 18 weeks" = "wt_16_18",
                                  ">18 to 20 weeks" = "wt_18_20",
                                  ">20 weeks"       = "wt_over_20")) %>%
  pivot_wider(names_from = waiting_time_hb, values_from = col_perf_n) %>%
  mutate(across(where(is.numeric), ~  replace_na(., 0)))


# Summarise numbers of different types of cancers (for females), in each HB
summary_icd <- analysis_db %>%
  mutate(icd = if_else(is.na(icd), "99", as.character(icd))) %>%
  group_by(icd, hbr19) %>%
  summarise(cancer_n = sum(cancer_n)) %>%
  ungroup() %>%
  mutate(icd = recode(icd,
                      "99" = "icd_nk",
                      "C18" = "icd_c18",
                      "C19" = "icd_c19",
                      "C20" = "icd_c20")) %>%
  pivot_wider(names_from = icd, values_from = cancer_n) %>%
  mutate(across(.cols = icd_nk:icd_c20, ~ replace_na(., 0))) %>%
  # Insert icd_c19 = 0 - because there are no C19s records
  # this allows us to keep the code consistent with hb_reports
  mutate(icd_c19 = 0) %>%  
  select(hbr19, icd_c18, icd_c19, icd_c20, icd_nk)


## Join all these objects together to create a single summary object
# First join summary one and invites
summary_female <- left_join(summary_one, summary_simd_invite, by = "hbr19") %>%
  # then progressively add on subsequent tables.
  left_join(summary_total_invite, by = "hbr19") %>%
  left_join(summary_simd_uptake, by = "hbr19") %>%
  left_join(summary_total_uptake, by = "hbr19") %>%
  left_join(summary_wt, by = "hbr19") %>%
  left_join(summary_dukes, by = "hbr19") %>%
  left_join(summary_icd, by = "hbr19") %>%
  # Select statement to put into proper order
  select(
    hbr19,
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


## Remove temporary tables used in this section.
rm(analysis_db, summary_dukes, summary_icd, summary_one, summary_simd_invite,
   summary_simd_uptake, summary_total_invite, summary_total_uptake, summary_wt)



### Step 5: Create individual files ----

## NOTE: 
# this code updates the SIMD and waiting times data on lines 44-98 of the HB reports
# lines 2-43 are functions, which calculate KPIs from the data in lines 44-98.
# Once code below is run and Excel files are saved, they must be open manually,
# and you must press: "Ctrl shift alt F9" to update all the function in the tab.

## North
north <- HB_KPI("North")

## South
south <- HB_KPI("South")


### Step 6: Write to excel ----


# North

write_hb_report(north, "Highland_North")

# South

write_hb_report(south, "Highland_South")
