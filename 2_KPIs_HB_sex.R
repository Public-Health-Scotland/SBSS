##########################################################
# 2_KPIs_HB_sex.R
# Gavin Clark
# 26/03/2019
# Script 2 of ?
# Data preparation for export
# Written/run on R Studio Server
# R version 3.2.3
# This script creates the 2-year percentage tables for KPI report
# Transcribed from scrip at:
# \\stats\CancerGroup1\Topics\BowelScreening\Publications\SBoSP-Statistics\
# 20190205\Syntax\1_KPIs_N18.sps
##########################################################

# Step 1 - housekeeping
library(dplyr)
library(tidyr)

date_first <- "2016-05-01"
date_last <- "2018-04-30"


# Bring in analysis database from script 1
analysis_db <- readRDS(paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening/",
                              "TPP/KPIs/Code + DB/analysis_dataset.rds"))

# Filter on dates
# Select statement to keep only required variables
analysis_db <- filter(analysis_db, between(invdate, 
                                           as.Date(date_first), 
                                           as.Date(date_last)) &
                        optin == 0 &
                        hbr14 %in% 1:14
)
# 1,844,815 - same as SPSS
# Next step in SPSS is flexi-sig removal, this has been done in R script 1

# Calculate uptake
# 
# # By sex
# KPI_1_sex <- analysis_db %>%
#   group_by(sex, hbr14) %>%
#   summarise(uptake_p = sum(uptake_n)/sum(invite_n))
# 
# # For all sexes
# KPI_1_all <- analysis_db %>%
#   group_by(hbr14) %>%
#   summarise(uptake_p = sum(uptake_n)/sum(invite_n)) %>%
#   mutate(sex = 3)
# 
# # Scotland sexes
# KPI_1_Scot_sex <- analysis_db %>%
#   group_by(sex) %>%
#   summarise(uptake_p = sum(uptake_n)/sum(invite_n)) %>%
#   mutate(hbr14 = 15)
# 
# # Scotland total
# KPI_1_Scot_all <- analysis_db %>%
#   group_by() %>%
#   summarise(uptake_p = sum(uptake_n)/sum(invite_n)) %>%
#   mutate(hbr14 = 15, sex = 3)
# 
# # Combine all of the above
# KPI_1 <- bind_rows(KPI_1_sex, KPI_1_all, KPI_1_Scot_sex, KPI_1_Scot_all) %>%
#   mutate(KPI = 1) %>%
#   arrange(hbr14, sex) %>%
#   spread(hbr14, uptake_p)

# Create function

KPI_hb_sex <- function(num, den, kpi_no) 
{
  
  # By sex
  KPI_sex <- analysis_db %>%
    group_by(sex, hbr14) %>%
    summarise(p = sum(!! sym(num))/sum(!! sym(den))*100)
  
  
  # For all sexes
  KPI_all <- analysis_db %>%
    group_by(hbr14) %>%
    summarise(p = sum(!! sym(num))/sum(!! sym(den))*100) %>%
    mutate(sex = 3)
  
  # Scotland sexes
  KPI_Scot_sex <- analysis_db %>%
    group_by(sex) %>%
    summarise(p = sum(!! sym(num))/sum(!! sym(den))*100) %>%
    mutate(hbr14 = 15)
  
  # Scotland total
  KPI_Scot_all <- analysis_db %>%
    group_by() %>%
    summarise(p = sum(!! sym(num))/sum(!! sym(den))*100) %>%
    mutate(hbr14 = 15, sex = 3)
  
  # Combine all of the above
  KPI_all <- bind_rows(KPI_sex, KPI_all, KPI_Scot_sex, KPI_Scot_all) %>%
    mutate(KPI = kpi_no) %>%
    arrange(hbr14, sex) %>%
    spread(hbr14, p)
  
}

# KPIs 24 and 25 require variables to be added together
analysis_db <- analysis_db %>%
  mutate(
    canc_hr_n = cancer_n + hr_adenoma_n,
    all_neoplasia_n = cancer_n + adenoma_n
  )

# Different arguments to produce relevant KPIs
KPI_1 <- KPI_hb_sex("uptake_n", "invite_n", 1)
KPI_3 <- KPI_hb_sex("positive_n", "uptake_n", 3)
KPI_5 <- KPI_hb_sex("col_perf_n", "positive_n", 5)
KPI_6 <- KPI_hb_sex("col_complete_n", "col_perf_n", 6)
KPI_7 <- KPI_hb_sex("col_complic_n", "col_perf_n", 7)
KPI_8 <- KPI_hb_sex("cancer_n", "uptake_n", 8)
KPI_17 <- KPI_hb_sex("polyp_cancer_n", "uptake_n", 17)
KPI_18 <- KPI_hb_sex("polyp_cancer_n", "cancer_n", 18)
KPI_19 <- KPI_hb_sex("adenoma_n", "uptake_n", 19)
KPI_20 <- KPI_hb_sex("hr_adenoma_n", "uptake_n", 20)
KPI_21 <- KPI_hb_sex("cancer_n", "col_perf_n", 21)
KPI_22 <- KPI_hb_sex("adenoma_n", "col_perf_n", 22)
KPI_23 <- KPI_hb_sex("hr_adenoma_n", "col_perf_n", 23)
KPI_24 <- KPI_hb_sex("canc_hr_n", "col_perf_n", 24)
KPI_25 <- KPI_hb_sex("all_neoplasia_n", "col_perf_n", 25)

# KPI 2 is uptake by SIMD 2016

# By sex
KPI_sex <- analysis_db %>%
  group_by(sex, hbr14, simd2016) %>%
  summarise(uptake_p = sum(uptake_n)/sum(invite_n))

# For all sexes
KPI_all <- analysis_db %>%
  group_by(hbr14, simd2016) %>%
  summarise(uptake_p = sum(uptake_n)/sum(invite_n)) %>%
  mutate(sex = 3)

# Scotland sexes
KPI_Scot_sex <- analysis_db %>%
  group_by(sex, simd2016) %>%
  summarise(uptake_p = sum(uptake_n)/sum(invite_n)) %>%
  mutate(hbr14 = 15)

# Scotland total
KPI_Scot_all <- analysis_db %>%
  group_by(simd2016) %>%
  summarise(uptake_p = sum(uptake_n)/sum(invite_n)) %>%
  mutate(hbr14 = 15, sex = 3)

# Combine all of the above
KPI_2 <- bind_rows(KPI_sex, KPI_all, KPI_Scot_sex, KPI_Scot_all) %>%
  mutate(KPI = 2) %>%
  arrange(hbr14, sex) %>%
  spread(hbr14, uptake_p) %>%
  filter(!is.na(simd2016))

## KPI 4 - waiting time proportions
## Fix levels
# By sex
KPI_sex <- analysis_db %>%
  filter(waiting_time != "No colonoscopy") %>%
  group_by(sex, hbr14, waiting_time) %>%
  summarise(col_perf_n = sum(col_perf_n)) %>%
  mutate(
    p = col_perf_n /sum(col_perf_n)
  )

# For all sexes
KPI_all <- analysis_db %>%
  filter(waiting_time != "No colonoscopy") %>%
  group_by(hbr14, waiting_time) %>%
  summarise(col_perf_n = sum(col_perf_n)) %>%
  mutate(sex = 3)


# Scotland sexes
KPI_Scot_sex <- analysis_db %>%
  filter(waiting_time != "No colonoscopy") %>%
  group_by(sex, waiting_time) %>%
  summarise(col_perf_n = sum(col_perf_n)) %>%
  mutate(hbr14 = 15)

# Scotland total
KPI_Scot_all <- analysis_db %>%
  filter(waiting_time != "No colonoscopy") %>%
  group_by(waiting_time) %>%
  summarise(col_perf_n = sum(col_perf_n)) %>%
  mutate(hbr14 = 15, sex = 3)

# Combine all of the above
KPI_4 <- bind_rows(KPI_sex, KPI_all, KPI_Scot_sex, KPI_Scot_all) %>%
  group_by(sex, hbr14) %>%
  mutate(KPI = 4,
         p = col_perf_n/sum(col_perf_n)) %>%
  select(-col_perf_n) %>%
  spread(hbr14, p)

# Proportion function

KPI_by <- function(filter, )
  # By sex
  KPI_sex <- analysis_db %>%
  filter(waiting_time != "No colonoscopy") %>%
  group_by(sex, hbr14, waiting_time) %>%
  summarise(col_perf_n = sum(col_perf_n)) %>%
  mutate(
    p = col_perf_n /sum(col_perf_n)
  )

# For all sexes
KPI_all <- analysis_db %>%
  filter(waiting_time != "No colonoscopy") %>%
  group_by(hbr14, waiting_time) %>%
  summarise(col_perf_n = sum(col_perf_n)) %>%
  mutate(sex = 3)


# Scotland sexes
KPI_Scot_sex <- analysis_db %>%
  filter(waiting_time != "No colonoscopy") %>%
  group_by(sex, waiting_time) %>%
  summarise(col_perf_n = sum(col_perf_n)) %>%
  mutate(hbr14 = 15)

# Scotland total
KPI_Scot_all <- analysis_db %>%
  filter(waiting_time != "No colonoscopy") %>%
  group_by(waiting_time) %>%
  summarise(col_perf_n = sum(col_perf_n)) %>%
  mutate(hbr14 = 15, sex = 3)

# Combine all of the above
KPI_4 <- bind_rows(KPI_sex, KPI_all, KPI_Scot_sex, KPI_Scot_all) %>%
  group_by(sex, hbr14) %>%
  mutate(KPI = 4,
         p = col_perf_n/sum(col_perf_n)) %>%
  select(-col_perf_n) %>%
  spread(hbr14, p)










