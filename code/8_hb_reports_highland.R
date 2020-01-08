##########################################################
# 8_hb_reports_highland.R
# Gavin Clark
# 13/09/2019
# Script 8 of ?
# Data preparation for export
# Written/run on R Studio Server
# R version 3.2.3
# This script creates the 2-year percentages and counts for NHS Highland
# split for the 2006 health board configuration
##########################################################

### Step 1 - housekeeping
library(dplyr)
library(tidyr)
library(readxl)
library(here)
library(haven)
library(janitor)
library(XLConnect)
library(stringr)
library(tidylog)

#   set filepaths and extract dates with script 0
source(here::here("code", "0_housekeeping.R"))

### Step 2 - 

# Bring in analysis database from script 1
analysis_db <-
  readRDS(analysis_db_path)

# Bring in fulll database as we need hbres and postcode
raw_db <- read_sav(sbsdb_path) %>%
  clean_names() %>%
  select(chinum, patpcode, date_round, hbres)

analysis_db2 <- inner_join(analysis_db, raw_db, by = c("chinum" = "chinum",
                                                       "date_round" = "date_round")) %>%
  mutate(patpcode = str_remove(patpcode, " "))

# Bring in scottish postcode directory
spd <- readRDS(paste0("/conf/linkage/output/lookups/Unicode/Geography/",
                       "Scottish Postcode Directory/Scottish_Postcode_Directory_2019_2.rds"
                      )) %>%
  filter(HB2006 =='S08000008') %>%
  select(HB2006, pc8) %>%
  mutate(patpcode = str_remove(pc8, " "))

highland <- inner_join(analysis_db2, spd, by  = "patpcode") %>%
  mutate(hbr14 = case_when(
    hbres %in% c("G", "C") ~ "South",
    hbres == "H" ~ "North")
    ) %>%
  filter(!is.na(hbr14))

# rm(list = (analysis_db, raw_))

### File for male and female combined ----
## GC TO DO - create function for male/female/all
# Filter on dates
analysis_db <- filter(highland,
                      between(invdate,
                              as.Date(date_first),
                              as.Date(date_last)) &
                        optin == 0 )

# Summarise for required variables
summary_one <- analysis_db %>%
  group_by(hbr14) %>%
  summarise(
    invite_n = sum(invite_n),
    positive_n = sum(positive_n),
    negative_n = sum(negative_n),
    uptake_n = sum(uptake_n),
    col_perf_n = sum(col_perf_n),
    col_complete_n = sum(col_complete_n),
    col_complic_n = sum(col_complic_n),
    col_complic_o_n = sum(col_complic_o_n),
    col_death_n = sum(col_death_n),
    cancer_n = sum(cancer_n),
    polyp_cancer_n = sum(polyp_cancer_n),
    hr_adenoma_n = sum(hr_adenoma_n),
    ir_adenoma_n = sum(ir_adenoma_n),
    lr_adenoma_n = sum(lr_adenoma_n),
    # Unclassified risk adenomas
    uncl_adenoma_n = sum(uncl_adenoma_n),
    adenoma_n = sum(adenoma_n),
    # Adenomas including those where cancer is diagnosed
    all_adenoma_n = sum(all_adenoma_n),
    canc_hr_n = sum(canc_hr_n),
    all_neoplasia_n = sum(all_neoplasia_n)
  ) 

# Invite numbers by HB
summary_simd_invite <- analysis_db %>%
  group_by(simd2016, hbr14) %>%
  summarise(
    invite_n = sum(invite_n)) %>%
  ungroup() %>%
  filter(!is.na(simd2016)) %>%
  mutate(simd2016 = recode(as.character(simd2016),
                           "1" = "simd_1",
                           "2" = "simd_2",
                           "3" = "simd_3",
                           "4" = "simd_4",
                           "5" = "simd_5")) %>%
  spread(simd2016, invite_n) %>%
  replace_na(list(simd_1 = 0,
                  simd_2 = 0,
                  simd_3 = 0,
                  simd_4 = 0,
                  simd_5 = 0)) %>%
  group_by(hbr14) %>%
  mutate(simd_1_invite = max(simd_1),
         simd_2_invite = max(simd_2),
         simd_3_invite = max(simd_3),
         simd_4_invite = max(simd_4),         
         simd_5_invite = max(simd_5)) %>%
  ungroup() %>%
  select(hbr14, simd_1_invite:simd_5_invite) %>%
  distinct()

# Uptake numbers by SIMD and HB
summary_simd_uptake <- analysis_db %>%
  group_by(simd2016, hbr14) %>%
  summarise(
    uptake_n = sum(uptake_n)
  ) %>%
  ungroup() %>%
  filter(!is.na(simd2016)) %>%
  mutate(simd2016 = recode(as.character(simd2016),
                           "1" = "simd_1",
                           "2" = "simd_2",
                           "3" = "simd_3",
                           "4" = "simd_4",
                           "5" = "simd_5")) %>%
  spread(simd2016, uptake_n) %>%
  replace_na(list(simd_1 = 0,
                  simd_2 = 0,
                  simd_3 = 0,
                  simd_4 = 0,
                  simd_5 = 0)) %>%
  group_by(hbr14) %>%
  mutate(simd_1_uptake = max(simd_1),
         simd_2_uptake = max(simd_2),
         simd_3_uptake = max(simd_3),
         simd_4_uptake = max(simd_4),         
         simd_5_uptake = max(simd_5)) %>%
  select(hbr14, simd_1_uptake:simd_5_uptake) %>%
  distinct()

# dukes
summary_dukes <- analysis_db %>%
  group_by(dukes_der, hbr14) %>%
  summarise(
    cancer_n = sum(cancer_n)
  ) %>%
  ungroup() %>%
  filter(dukes_der != "") %>%
  mutate(dukes_der = recode(dukes_der,
                            "A" = "dukes_a",
                            "B" = "dukes_b",
                            "C" = "dukes_c",
                            "D" = "dukes_d",
                            "Not known" = "dukes_nk",
                            "Not supplied" = "dukes_ns"
  )) %>%
  spread(dukes_der, cancer_n) %>%
  # Insert dukes_ns = 0 as this is blank and causes problems further down
  # GC TO DO  - fix for next time
  mutate(dukes_ns = 0) %>%
  replace_na(list(dukes_a = 0,
                  dukes_b = 0,
                  dukes_c = 0,
                  dukes_d = 0,
                  dukes_nk = 0,
                  dukes_ns = 0)) %>%
  group_by(hbr14) %>%
  mutate(dukes_a = max(dukes_a),
         dukes_b = max(dukes_b),
         dukes_c = max(dukes_c),
         dukes_d = max(dukes_d),         
         dukes_nk = max(dukes_nk),
         dukes_ns = max(dukes_ns)) %>%
  select(hbr14, dukes_a:dukes_ns) %>%
  distinct()


summary_wt <- analysis_db %>%
  group_by(waiting_time, hbr14) %>%
  summarise(
    col_perf_n = sum(col_perf_n)
  ) %>%
  ungroup() %>%
  filter(waiting_time != "No colonoscopy") %>%
  mutate(waiting_time = recode(waiting_time,
                               "0 to 2 weeks" = "wt_0_2",
                               ">2 to 4 weeks" = "wt_2_4",
                               ">4 to 6 weeks" = "wt_4_6",
                               ">6 to 8 weeks" = "wt_6_8",
                               ">8 to 10 weeks" = "wt_8_10",
                               ">10 to 12 weeks" = "wt_10_12",
                               ">12 to 14 weeks" = "wt_12_14",
                               ">14 to 16 weeks" = "wt_14_16",
                               ">16 to 18 weeks" = "wt_16_18",
                               ">18 to 20 weeks" = "wt_18_20",
                               ">20 weeks" = "wt_over_20")) %>%
  spread(waiting_time, col_perf_n) %>%
  replace_na(list(wt_0_2 = 0,
                  wt_2_4 = 0,
                  wt_4_6 = 0,
                  wt_6_8 = 0,
                  wt_8_10 = 0,
                  wt_10_12 = 0,
                  wt_12_14 = 0,
                  wt_14_16 = 0,
                  wt_16_18 = 0,
                  wt_18_20 = 0,
                  wt_over_20 = 0
                  )) %>%
  group_by(hbr14) %>%
  mutate(wt_0_2 = max(wt_0_2),
         wt_2_4 = max(wt_2_4),
         wt_4_6 = max(wt_4_6),
         wt_6_8 = max(wt_6_8),
         wt_8_10 = max(wt_8_10),
         wt_10_12 = max(wt_10_12),
         wt_12_14 = max(wt_12_14),
         wt_14_16 = max(wt_14_16),
         wt_16_18 = max(wt_16_18),
         wt_18_20 = max(wt_18_20),
         wt_over_20 = max(wt_over_20)) %>%
  select(hbr14, wt_0_2:wt_over_20) %>%
  distinct()


summary_icd <- analysis_db %>%
  mutate(
    icd = if_else(icd == "", "99", as.character(icd))
  ) %>%
  group_by(icd, hbr14) %>%
  summarise(
    cancer_n = sum(cancer_n)
  ) %>%
  ungroup() %>%
  mutate(icd = recode(icd,
                            "99" = "icd_nk",
                            "C18" = "icd_c18",
                            "C19" = "icd_c19",
                            "C20" = "icd_c20")) %>%
  spread(icd, cancer_n) %>%
  # Insert icd_c19 = 0 as this is blank and causes problems further down
  # GC TO DO  - fix for next time
  mutate(icd_c19 = 0) %>%
  replace_na(list(icd_nk = 0,
                  icd_c18 = 0,
                  icd_c19 = 0,
                  icd_c20 = 0)) %>%
  group_by(hbr14) %>%
  mutate(icd_nk = max(icd_nk),
         icd_c18 = max(icd_c18),
         icd_c19 = max(icd_c19),
         icd_c20 = max(icd_c20)) %>%
  select(hbr14, icd_c18, icd_c19, icd_c20, icd_nk) %>%
  distinct()


summary_total_invite <- summary_simd_invite %>%
  mutate(simd_total_invite = 
           simd_1_invite +
           simd_2_invite +
           simd_3_invite +
           simd_4_invite +
           simd_5_invite) %>%
  select(hbr14, simd_total_invite)


summary_total_uptake <- summary_simd_uptake %>%
  mutate(simd_total_uptake = 
           simd_1_uptake +
           simd_2_uptake +
           simd_3_uptake +
           simd_4_uptake +
           simd_5_uptake) %>%
  select(hbr14, simd_total_uptake)



summary_all <- left_join(
  summary_one,
  summary_simd_invite, by = "hbr14") %>%
  left_join(summary_total_invite, by = "hbr14") %>%
  left_join(summary_simd_uptake, by = "hbr14") %>%
  left_join(summary_total_uptake, by = "hbr14") %>%
  left_join(summary_wt, by = "hbr14") %>%
  left_join(summary_dukes, by = "hbr14") %>%
  left_join(summary_icd, by = "hbr14") %>%
  # Select statement to put into proper order
  select(
    hbr14,
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
    icd_nk
  )

## Clear unnecessary tables

rm(analysis_db, summary_dukes, summary_icd, summary_one, summary_simd_invite,
        summary_simd_uptake, summary_total_invite, summary_total_uptake, summary_wt)


### File for males only ----
# Filter on dates
analysis_db <- filter(highland,
                      between(invdate,
                              as.Date(date_first),
                              as.Date(date_last)) &
                        optin == 0 &
                        sex ==1)


# Summarise for required variables
summary_one <- analysis_db %>%
  group_by(hbr14) %>%
  summarise(
    invite_n = sum(invite_n),
    positive_n = sum(positive_n),
    negative_n = sum(negative_n),
    uptake_n = sum(uptake_n),
    col_perf_n = sum(col_perf_n),
    col_complete_n = sum(col_complete_n),
    col_complic_n = sum(col_complic_n),
    col_complic_o_n = sum(col_complic_o_n),
    col_death_n = sum(col_death_n),
    cancer_n = sum(cancer_n),
    polyp_cancer_n = sum(polyp_cancer_n),
    hr_adenoma_n = sum(hr_adenoma_n),
    ir_adenoma_n = sum(ir_adenoma_n),
    lr_adenoma_n = sum(lr_adenoma_n),
    # Unclassified risk adenomas
    uncl_adenoma_n = sum(uncl_adenoma_n),
    adenoma_n = sum(adenoma_n),
    # Adenomas including those where cancer is diagnosed
    all_adenoma_n = sum(all_adenoma_n),
    canc_hr_n = sum(canc_hr_n),
    all_neoplasia_n = sum(all_neoplasia_n)
  ) 

# Invite numbers by HB
summary_simd_invite <- analysis_db %>%
  group_by(simd2016, hbr14) %>%
  summarise(
    invite_n = sum(invite_n)) %>%
  ungroup() %>%
  filter(!is.na(simd2016)) %>%
  mutate(simd2016 = recode(as.character(simd2016),
                           "1" = "simd_1",
                           "2" = "simd_2",
                           "3" = "simd_3",
                           "4" = "simd_4",
                           "5" = "simd_5")) %>%
  spread(simd2016, invite_n) %>%
  replace_na(list(simd_1 = 0,
                  simd_2 = 0,
                  simd_3 = 0,
                  simd_4 = 0,
                  simd_5 = 0)) %>%
  group_by(hbr14) %>%
  mutate(simd_1_invite = max(simd_1),
         simd_2_invite = max(simd_2),
         simd_3_invite = max(simd_3),
         simd_4_invite = max(simd_4),         
         simd_5_invite = max(simd_5)) %>%
  ungroup() %>%
  select(hbr14, simd_1_invite:simd_5_invite) %>%
  distinct()

# Uptake numbers by SIMD and HB
summary_simd_uptake <- analysis_db %>%
  group_by(simd2016, hbr14) %>%
  summarise(
    uptake_n = sum(uptake_n)
  ) %>%
  ungroup() %>%
  filter(!is.na(simd2016)) %>%
  mutate(simd2016 = recode(as.character(simd2016),
                           "1" = "simd_1",
                           "2" = "simd_2",
                           "3" = "simd_3",
                           "4" = "simd_4",
                           "5" = "simd_5")) %>%
  spread(simd2016, uptake_n) %>%
  replace_na(list(simd_1 = 0,
                  simd_2 = 0,
                  simd_3 = 0,
                  simd_4 = 0,
                  simd_5 = 0)) %>%
  group_by(hbr14) %>%
  mutate(simd_1_uptake = max(simd_1),
         simd_2_uptake = max(simd_2),
         simd_3_uptake = max(simd_3),
         simd_4_uptake = max(simd_4),         
         simd_5_uptake = max(simd_5)) %>%
  select(hbr14, simd_1_uptake:simd_5_uptake) %>%
  distinct()


# dukes
summary_dukes <- analysis_db %>%
  group_by(dukes_der, hbr14) %>%
  summarise(
    cancer_n = sum(cancer_n)
  ) %>%
  ungroup() %>%
  filter(dukes_der != "") %>%
  mutate(dukes_der = recode(dukes_der,
                            "A" = "dukes_a",
                            "B" = "dukes_b",
                            "C" = "dukes_c",
                            "D" = "dukes_d",
                            "Not known" = "dukes_nk",
                            "Not supplied" = "dukes_ns"
  )) %>%
  spread(dukes_der, cancer_n) %>%
  # Insert dukes_ns = 0 as this is blank and causes problems further down
  # GC TO DO  - fix for next time
  mutate(dukes_ns = 0) %>%
  replace_na(list(dukes_a = 0,
                  dukes_b = 0,
                  dukes_c = 0,
                  dukes_d = 0,
                  dukes_nk = 0,
                  dukes_ns = 0)) %>%
  group_by(hbr14) %>%
  mutate(dukes_a = max(dukes_a),
         dukes_b = max(dukes_b),
         dukes_c = max(dukes_c),
         dukes_d = max(dukes_d),         
         dukes_nk = max(dukes_nk),
         dukes_ns = max(dukes_ns)) %>%
  select(hbr14, dukes_a:dukes_ns) %>%
  distinct()


summary_wt <- analysis_db %>%
  group_by(waiting_time, hbr14) %>%
  summarise(
    col_perf_n = sum(col_perf_n)
  ) %>%
  ungroup() %>%
  filter(waiting_time != "No colonoscopy") %>%
  mutate(waiting_time = recode(waiting_time,
                               "0 to 2 weeks" = "wt_0_2",
                               ">2 to 4 weeks" = "wt_2_4",
                               ">4 to 6 weeks" = "wt_4_6",
                               ">6 to 8 weeks" = "wt_6_8",
                               ">8 to 10 weeks" = "wt_8_10",
                               ">10 to 12 weeks" = "wt_10_12",
                               ">12 to 14 weeks" = "wt_12_14",
                               ">14 to 16 weeks" = "wt_14_16",
                               ">16 to 18 weeks" = "wt_16_18",
                               ">18 to 20 weeks" = "wt_18_20",
                               ">20 weeks" = "wt_over_20")) %>%
  spread(waiting_time, col_perf_n) %>%
  replace_na(list(wt_0_2 = 0,
                  wt_2_4 = 0,
                  wt_4_6 = 0,
                  wt_6_8 = 0,
                  wt_8_10 = 0,
                  wt_10_12 = 0,
                  wt_12_14 = 0,
                  wt_14_16 = 0,
                  wt_16_18 = 0,
                  wt_18_20 = 0,
                  wt_over_20 = 0
  )) %>%
  group_by(hbr14) %>%
  mutate(wt_0_2 = max(wt_0_2),
         wt_2_4 = max(wt_2_4),
         wt_4_6 = max(wt_4_6),
         wt_6_8 = max(wt_6_8),
         wt_8_10 = max(wt_8_10),
         wt_10_12 = max(wt_10_12),
         wt_12_14 = max(wt_12_14),
         wt_14_16 = max(wt_14_16),
         wt_16_18 = max(wt_16_18),
         wt_18_20 = max(wt_18_20),
         wt_over_20 = max(wt_over_20)) %>%
  select(hbr14, wt_0_2:wt_over_20) %>%
  distinct()


summary_icd <- analysis_db %>%
  mutate(
    icd = if_else(icd == "", "99", as.character(icd))
  ) %>%
  group_by(icd, hbr14) %>%
  summarise(
    cancer_n = sum(cancer_n)
  ) %>%
  ungroup() %>%
  mutate(icd = recode(icd,
                      "99" = "icd_nk",
                      "C18" = "icd_c18",
                      "C19" = "icd_c19",
                      "C20" = "icd_c20")) %>%
  spread(icd, cancer_n) %>%
  # Insert icd_c19 = 0 as this is blank and causes problems further down
  # GC TO DO  - fix for next time
  mutate(icd_c19 = 0) %>%
  replace_na(list(icd_nk = 0,
                  icd_c18 = 0,
                  icd_c19 = 0,
                  icd_c20 = 0)) %>%
  group_by(hbr14) %>%
  mutate(icd_nk = max(icd_nk),
         icd_c18 = max(icd_c18),
         icd_c19 = max(icd_c19),
         icd_c20 = max(icd_c20)) %>%
  select(hbr14, icd_c18, icd_c19, icd_c20, icd_nk)  %>%
  distinct()


summary_total_invite <- summary_simd_invite %>%
  mutate(simd_total_invite = 
           simd_1_invite +
           simd_2_invite +
           simd_3_invite +
           simd_4_invite +
           simd_5_invite) %>%
  select(hbr14, simd_total_invite)


summary_total_uptake <- summary_simd_uptake %>%
  mutate(simd_total_uptake = 
           simd_1_uptake +
           simd_2_uptake +
           simd_3_uptake +
           simd_4_uptake +
           simd_5_uptake) %>%
  select(hbr14, simd_total_uptake)


summary_male <- left_join(
  summary_one,
  summary_simd_invite, by = "hbr14") %>%
  left_join(summary_total_invite, by = "hbr14") %>%
  left_join(summary_simd_uptake, by = "hbr14") %>%
  left_join(summary_total_uptake, by = "hbr14") %>%
  left_join(summary_wt, by = "hbr14") %>%
  left_join(summary_dukes, by = "hbr14") %>%
  left_join(summary_icd, by = "hbr14") %>%
  # Select statement to put into proper order
  select(
    hbr14,
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
    icd_nk
  )

## Clear unnecessary tables

rm(analysis_db, summary_dukes, summary_icd, summary_one, summary_simd_invite,
   summary_simd_uptake, summary_total_invite, summary_total_uptake, summary_wt)



### File for females only ----
# Filter on dates
analysis_db <- filter(highland,
                      between(invdate,
                              as.Date(date_first),
                              as.Date(date_last)) &
                        optin == 0 &
                        sex == 2)


# Summarise for required variables
summary_one <- analysis_db %>%
  group_by(hbr14) %>%
  summarise(
    invite_n = sum(invite_n),
    positive_n = sum(positive_n),
    negative_n = sum(negative_n),
    uptake_n = sum(uptake_n),
    col_perf_n = sum(col_perf_n),
    col_complete_n = sum(col_complete_n),
    col_complic_n = sum(col_complic_n),
    col_complic_o_n = sum(col_complic_o_n),
    col_death_n = sum(col_death_n),
    cancer_n = sum(cancer_n),
    polyp_cancer_n = sum(polyp_cancer_n),
    hr_adenoma_n = sum(hr_adenoma_n),
    ir_adenoma_n = sum(ir_adenoma_n),
    lr_adenoma_n = sum(lr_adenoma_n),
    # Unclassified risk adenomas
    uncl_adenoma_n = sum(uncl_adenoma_n),
    adenoma_n = sum(adenoma_n),
    # Adenomas including those where cancer is diagnosed
    all_adenoma_n = sum(all_adenoma_n),
    canc_hr_n = sum(canc_hr_n),
    all_neoplasia_n = sum(all_neoplasia_n)
  ) 

# Invite numbers by HB
summary_simd_invite <- analysis_db %>%
  group_by(simd2016, hbr14) %>%
  summarise(
    invite_n = sum(invite_n)) %>%
  ungroup() %>%
  filter(!is.na(simd2016)) %>%
  mutate(simd2016 = recode(as.character(simd2016),
                           "1" = "simd_1",
                           "2" = "simd_2",
                           "3" = "simd_3",
                           "4" = "simd_4",
                           "5" = "simd_5")) %>%
  spread(simd2016, invite_n) %>%
  replace_na(list(simd_1 = 0,
                  simd_2 = 0,
                  simd_3 = 0,
                  simd_4 = 0,
                  simd_5 = 0)) %>%
  group_by(hbr14) %>%
  mutate(simd_1_invite = max(simd_1),
         simd_2_invite = max(simd_2),
         simd_3_invite = max(simd_3),
         simd_4_invite = max(simd_4),         
         simd_5_invite = max(simd_5)) %>%
  ungroup() %>%
  select(hbr14, simd_1_invite:simd_5_invite) %>%
  distinct()


# Uptake numbers by SIMD and HB
summary_simd_uptake <- analysis_db %>%
  group_by(simd2016, hbr14) %>%
  summarise(
    uptake_n = sum(uptake_n)
  ) %>%
  ungroup() %>%
  filter(!is.na(simd2016)) %>%
  mutate(simd2016 = recode(as.character(simd2016),
                           "1" = "simd_1",
                           "2" = "simd_2",
                           "3" = "simd_3",
                           "4" = "simd_4",
                           "5" = "simd_5")) %>%
  spread(simd2016, uptake_n) %>%
  replace_na(list(simd_1 = 0,
                  simd_2 = 0,
                  simd_3 = 0,
                  simd_4 = 0,
                  simd_5 = 0)) %>%
  group_by(hbr14) %>%
  mutate(simd_1_uptake = max(simd_1),
         simd_2_uptake = max(simd_2),
         simd_3_uptake = max(simd_3),
         simd_4_uptake = max(simd_4),         
         simd_5_uptake = max(simd_5)) %>%
  select(hbr14, simd_1_uptake:simd_5_uptake) %>%
  distinct()


# dukes
summary_dukes <- analysis_db %>%
  group_by(dukes_der, hbr14) %>%
  summarise(
    cancer_n = sum(cancer_n)
  ) %>%
  ungroup() %>%
  filter(dukes_der != "") %>%
  mutate(dukes_der = recode(dukes_der,
                            "A" = "dukes_a",
                            "B" = "dukes_b",
                            "C" = "dukes_c",
                            "D" = "dukes_d",
                            "Not known" = "dukes_nk",
                            "Not supplied" = "dukes_ns"
  )) %>%
  spread(dukes_der, cancer_n) %>%
  # Insert dukes_ns = 0 as this is blank and causes problems further down
  # GC TO DO  - fix for next time
  mutate(dukes_ns = 0) %>%
  replace_na(list(dukes_a = 0,
                  dukes_b = 0,
                  dukes_c = 0,
                  dukes_d = 0,
                  dukes_nk = 0,
                  dukes_ns = 0)) %>%
  group_by(hbr14) %>%
  mutate(dukes_a = max(dukes_a),
         dukes_b = max(dukes_b),
         dukes_c = max(dukes_c),
         dukes_d = max(dukes_d),         
         dukes_nk = max(dukes_nk),
         dukes_ns = max(dukes_ns)) %>%
  select(hbr14, dukes_a:dukes_ns) %>%
  distinct()


summary_wt <- analysis_db %>%
  group_by(waiting_time, hbr14) %>%
  summarise(
    col_perf_n = sum(col_perf_n)
  ) %>%
  ungroup() %>%
  filter(waiting_time != "No colonoscopy") %>%
  mutate(waiting_time = recode(waiting_time,
                               "0 to 2 weeks" = "wt_0_2",
                               ">2 to 4 weeks" = "wt_2_4",
                               ">4 to 6 weeks" = "wt_4_6",
                               ">6 to 8 weeks" = "wt_6_8",
                               ">8 to 10 weeks" = "wt_8_10",
                               ">10 to 12 weeks" = "wt_10_12",
                               ">12 to 14 weeks" = "wt_12_14",
                               ">14 to 16 weeks" = "wt_14_16",
                               ">16 to 18 weeks" = "wt_16_18",
                               ">18 to 20 weeks" = "wt_18_20",
                               ">20 weeks" = "wt_over_20")) %>%
  spread(waiting_time, col_perf_n) %>%
  replace_na(list(wt_0_2 = 0,
                  wt_2_4 = 0,
                  wt_4_6 = 0,
                  wt_6_8 = 0,
                  wt_8_10 = 0,
                  wt_10_12 = 0,
                  wt_12_14 = 0,
                  wt_14_16 = 0,
                  wt_16_18 = 0,
                  wt_18_20 = 0,
                  wt_over_20 = 0
  )) %>%
  group_by(hbr14) %>%
  mutate(wt_0_2 = max(wt_0_2),
         wt_2_4 = max(wt_2_4),
         wt_4_6 = max(wt_4_6),
         wt_6_8 = max(wt_6_8),
         wt_8_10 = max(wt_8_10),
         wt_10_12 = max(wt_10_12),
         wt_12_14 = max(wt_12_14),
         wt_14_16 = max(wt_14_16),
         wt_16_18 = max(wt_16_18),
         wt_18_20 = max(wt_18_20),
         wt_over_20 = max(wt_over_20)) %>%
  select(hbr14, wt_0_2:wt_over_20) %>%
  distinct()


summary_icd <- analysis_db %>%
  mutate(
    icd = if_else(icd == "", "99", as.character(icd))
  ) %>%
  group_by(icd, hbr14) %>%
  summarise(
    cancer_n = sum(cancer_n)
  ) %>%
  ungroup() %>%
  mutate(icd = recode(icd,
                      "99" = "icd_nk",
                      "C18" = "icd_c18",
                      "C19" = "icd_c19",
                      "C20" = "icd_c20")) %>%
  spread(icd, cancer_n) %>%
  # Insert icd_c19 = 0 as this is blank and causes problems further down
  # GC TO DO  - fix for next time
  mutate(icd_c19 = 0) %>%
  replace_na(list(icd_nk = 0,
                  icd_c18 = 0,
                  icd_c19 = 0,
                  icd_c20 = 0)) %>%
  group_by(hbr14) %>%
  mutate(icd_nk = max(icd_nk),
         icd_c18 = max(icd_c18),
         icd_c19 = max(icd_c19),
         icd_c20 = max(icd_c20)) %>%
  select(hbr14, icd_c18, icd_c19, icd_c20, icd_nk) %>%
  distinct()


summary_total_invite <- summary_simd_invite %>%
  mutate(simd_total_invite = 
           simd_1_invite +
           simd_2_invite +
           simd_3_invite +
           simd_4_invite +
           simd_5_invite) %>%
  select(hbr14, simd_total_invite)


summary_total_uptake <- summary_simd_uptake %>%
  mutate(simd_total_uptake = 
           simd_1_uptake +
           simd_2_uptake +
           simd_3_uptake +
           simd_4_uptake +
           simd_5_uptake) %>%
  select(hbr14, simd_total_uptake)


summary_female <- left_join(
  summary_one,
  summary_simd_invite, by = "hbr14") %>%
  left_join(summary_total_invite, by = "hbr14") %>%
  left_join(summary_simd_uptake, by = "hbr14") %>%
  left_join(summary_total_uptake, by = "hbr14") %>%
  left_join(summary_wt, by = "hbr14") %>%
  left_join(summary_dukes, by = "hbr14") %>%
  left_join(summary_icd, by = "hbr14") %>%
  # Select statement to put into proper order
  select(
    hbr14,
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
    icd_nk
  )

## Clear unnecessary tables

rm(analysis_db, summary_dukes, summary_icd, summary_one, summary_simd_invite,
   summary_simd_uptake, summary_total_invite, summary_total_uptake, summary_wt)

### Create individual files ---
## GC TO DO - function for this
## North
all <- filter(summary_all, hbr14 == "North") %>%
  gather() %>%
  filter(key != "hbr14") %>%
  select(value) %>%
  rename(all = value)

male <- filter(summary_male, hbr14 == "North") %>%
  gather() %>%
  filter(key != "hbr14") %>%
  select(value) %>%
  rename(male = value)

female <- filter(summary_female, hbr14 == "North") %>%
  gather() %>%
  filter(key != "hbr14") %>%
  select(value) %>%
  rename(female = value)

north <- bind_cols(male, female, all)

## South
all <- filter(summary_all, hbr14 == "South") %>%
  gather() %>%
  filter(key != "hbr14") %>%
  select(value) %>%
  rename(all = value)

male <- filter(summary_male, hbr14 == "South") %>%
  gather() %>%
  filter(key != "hbr14") %>%
  select(value) %>%
  rename(male = value)

female <- filter(summary_female, hbr14 == "South") %>%
  gather() %>%
  filter(key != "hbr14") %>%
  select(value) %>%
  rename(female = value)

south <- bind_cols(male, female, all)

### Write to excel ---
## GC TO DO - create function for this part also
# North
wb <- loadWorkbook(here("Output/CONFI-individual-HB-reports",
                        "Highland_North_KPI_May_2019.xls"))

setStyleAction(wb, XLC$"STYLE_ACTION.NONE")

writeWorksheet(wb, north,"May-19",
               startRow = 44, startCol = 6, header = FALSE)

saveWorkbook(wb)


# South
wb <- loadWorkbook(here("Output/CONFI-individual-HB-reports",
                        "Highland_South_KPI_May_2019.xls"))

setStyleAction(wb, XLC$"STYLE_ACTION.NONE")

writeWorksheet(wb, south,"May-19",
               startRow = 44, startCol = 6, header = FALSE)

saveWorkbook(wb)
