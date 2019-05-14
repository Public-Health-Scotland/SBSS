##########################################################
# 0_housekeeping.R
# Gavin Clark
# 14/05/2019
# Script 0 of ?
# Define some housekeeping variables used by subsequent scripts
# Written/run on R Studio Server
# R version 3.2.3

##########################################################

# Define dates
date_first <- "2016-05-01"
date_last <- "2018-04-30"

# Define bowel screening database (SPSS database)
sbsdb_path <- (paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening/Data/",
                      "Programme/2018-11/combined_extract_all.zsav"))

# Define location of analysis database (created in script 1)
analysis_db_path <- (paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening/",
                            "TPP/KPIs/Code + DB/analysis_dataset.rds"))

