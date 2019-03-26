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

# By sex
KPI_1_sex <- analysis_db %>%
  group_by(sex, hbr14) %>%
  summarise(uptake_p = sum(uptake_n)/sum(invite_n))

# For all sexes
KPI_1_all <- analysis_db %>%
  group_by(hbr14) %>%
  summarise(uptake_p = sum(uptake_n)/sum(invite_n)) %>%
  mutate(sex = 3)

# Combine the two
KPI_1 <- bind_rows(KPI_1_sex, KPI_1_all)

# Include HB for all Scotland
