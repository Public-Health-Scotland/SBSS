##########################################################
# 3_funnel_plot_data.R
# Gavin Clark
# 26/03/2019
# Script 3 of ?
# Data preparation for export
# Written/run on R Studio Server
# R version 3.2.3
# This script creates a dataset to create funnel plots in excel
##########################################################

# Step 1 - housekeeping
library(dplyr)
library(binom)
# Select statement to keep onl required variables
# Filter on dates
# Bring in analysis database from script 1
analysis_db <- readRDS(paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening/",
                              "TPP/KPIs/Code + DB/analysis_dataset.rds"))


## Step 2 - HB denominator info for x-axis of funnel plots
denom_n_hb <- analysis_db %>%
  filter(hbr14 %in% 1:14) %>%
  group_by(hbr14) %>%
  summarise(
    uptake_n = sum(uptake_n),
    col_perf_n = sum(col_perf_n)
  )

# Calculate Scotland level percentages
scot_p <- analysis_db %>%
  summarise(
    positive_p = sum(positive_n)/sum(uptake_n),
    col_complic_p = sum(col_complic_n)/sum(col_perf_n),
    cancer_p = sum(cancer_n)/sum(uptake_n),
    polyp_cancer_p = sum(polyp_cancer_n)/sum(uptake_n),
    adenoma_p = sum(adenoma_n)/sum(uptake_n),
    hr_adenoma_p = sum(hr_adenoma_n)/sum(uptake_n)   
  )