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

# Bring in 
saveRDS(slim_db, file = paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening/",
                               "TPP/KPIs/Code + DB/analysis_dataset.rds"))