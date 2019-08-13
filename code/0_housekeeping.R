##########################################################
# 0_housekeeping.R
# Gavin Clark
# 28/06/2019
# Script 0 of ?
# Define some housekeeping variables used by subsequent scripts
# Written/run on R Studio Server
# R version 3.2.3

##########################################################

# Define dates
date_first <- "2016-11-01"
date_last <- "2018-10-31"

# Define bowel screening database (SPSS database)
sbsdb_path <- (paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening/Data/",
                      "Programme/2019-05/combined_extract_all.zsav"))

# Define location of analysis database (created in script 1)
analysis_db_path <- (paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening/",
                            "Publications/SBoSP-Statistics/20190806/Temp/",
                            "analysis_dataset.rds"))
