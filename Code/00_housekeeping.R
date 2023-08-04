#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 00_housekeeping.R
# Gavin Clark & Karen Hotopp
# Aug 2022
# Script 0 of 13
# Define housekeeping variables used by subsequent scripts
# Written/run on R Studio Server
# R version 3.6.1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### 1) Define invitation period for this publication ----
## May updates: 
# Upload period runs:     01-11-20xx to 30-04-20xx (30 months over 2 years)
# Invitation period runs: 01-11-20xx to 31-10-20xx (24 months over 2 years)
## Nov updates:
# Upload period runs:     01-05-20xx to 31-10-20xx (30 months over 2 years)
# Invitation period runs: 01-05-20xx to 30-04-20xx (24 months over 2 years)

date_first <- "2020-11-01"
date_last  <- "2022-10-31"

report_month <- "May_2023"

### 2) Define bowel screening database (SPSS database) ----
#sbsdb_path2 <- (paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening/Data/",
#                       "Programme/2021-05/combined_extract_all.zsav"))

sbsdb_path <- paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening/Data/",
                     "Programme/2023-05/combined_extract_all.rds")

### 3) Define location of analysis database (created in script 1) ----
analysis_db_path <- paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening/",
                           "Publications/SBoSP-Statistics/20230804/Temp/", 
                           "analysis_dataset.rds")


