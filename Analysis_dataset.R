
##########################################################
# Analysis_dataset_.R
# Gavin Clark
# 11/03/2019
# Data extraction/preparation
# Written/run on R Studio Server
# R version 3.2.3
# This script creates a dataset for summary
# Approximate run time
##########################################################

### 1 - Housekeeping ----
# clear workspace if not already clear
rm(list = ls())

#   loading packages
library(dplyr)
library(haven)
library(janitor)
library(lubridate)
library(ggplot2)
library(tidyr)
library(reshape2)

#   setting filepaths and extract dates

### Step 1 - Extract database, drop unnecessary columns and save as R file
## Note - may make sense to do this as separate script/create the count 
## variables before using further scripts for different sections KPIs, 
## time-series, funnel, demography, comparison, etc.
## Bringing in optins version, as could produce opt-in report from same file

raw_db <- read_sav(paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening/Data/",
                          "Programme/2018-11/combined_extract_all.zsav"))

slim_db <- raw_db %>% select(
  -(PATSNAME:HBRES),
  -FIT_THRESH,
  -ERR,
  -(final_date_round:transf06),
  -transf14,
  -(optin:HBIDENT),
  -PRECOLAS,
  -FURTHASS,
  -(BARENCTC:BARCTDAT),
  -POLYP,
  -POLYPECT,
  -(MORT:diff)
) %>%
  clean_names()

## glimpse() shows that date fields are being correctly read in
## Test?
## Tests also for screres, simd etc?

### Remove flexi. sig. study participants
flexi_sig <- read_sav(paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening/",
                             "Projects/20110101-Flexi-Sig/Data/4June_2016/",
                             "FS_combined_extract.zsav")) %>%
  mutate(chinum = as.numeric(CHINUM)) %>%
  select(chinum, FSPERF, STUDYARM, FSRESULT) %>%
  filter(FSRESULT == '01')

## Merge on to bs database
slim_db <- left_join(slim_db, flexi_sig, by = "chinum") %>%
  mutate(remove = ifelse(FSRESULT == "01" & 
                           (invdate >= as.Date("2014-06-01") & invdate <= as.Date("2015-12-31")) &
                           screres %in% c(1,3,4,5,6,7,8), 1, 0)) %>%
  filter(remove == 0|is.na(remove)) %>%
  select(-(FSPERF:remove))

# how many matched? Create test to check this
# table(slim_db$remove)

saveRDS(slim_db, file = paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening/",
                               "TPP/KPIs/Code + DB/SBSD_slim_nov18.rds"))

#slim_db <- readRDS(paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening/",
#                          "TPP/KPIs/Code + DB/SBSD_slim_nov18.rds"))

# Remove tables that are no longer needed
rm(list = c("flexi_sig", "raw_db"))

### Step 2 - Create flags for KPI numerator/denominator
slim_db <- slim_db %>%
  mutate(
    
    invite_n = ifelse(screres %in% 1:18|21:24, 1, 0),
    uptake_n = ifelse(screres %in% c(1,3,4,5,6,7,8,21,22), 1, 0),
    positive_n = ifelse(screres %in% c(3,5,6,8,22), 1, 0),
    col_perf_n = ifelse(colperf == "01", 1, 0),
    col_complete_n = ifelse(colcomp == "01", 1, 0),
    col_complic_n = ifelse(complicp %in% 
                             c("01A","01B","01C","02","03","04","05","06","98"),
                           1, 0),
    cancer_n = ifelse(cancer == "01", 1, 0),
    polyp_cancer_n = ifelse(polypca == "01", 1, 0),
    adenoma_n = ifelse(adenoma == "01", 1, 0),
    # adenoma risk stratification
    # low
    lr_adenoma_n = ifelse(
      cancer == "00" &
        adenoma == "01" &
        adenno %in% c("01","02") &
        adensize %in% c("01","02","03","04","05","06","07","08","09"),
      1, 0),
    # intermediate
    ir_adenoma_n = ifelse(
      #If adenoma is most serious diagnosis
      (cancer == "00" & adenoma == "01"), 
      # And there are 3 or 4 adenomas, none bigger than 9mm
      ifelse(
        (adenno %in% c("03","04") &
           adensize %in% c("01","02","03","04","05","06","07","08","09")), 
        1,
        # Or if there are 1 or 2 adenomas with one larger than 9mm
        ifelse(
          (adenno %in% c("01","02") &
             !(adensize %in% c("00","01","02","03","04","05","06","07","08","09",
                               "99","X", ""))),
          1, 0)),
      0),
    # high
    hr_adenoma_n = ifelse(
      cancer == "00" &
        adenoma == "01",
      ifelse(
        !(adenno %in% c("00","01","02","03","04","99","X","")),
        1, ifelse(
          adenno %in% c("03","04") &
            !(adensize %in% c("00","01","02","03","04","05","06","07","08","09",
                              "99","X", "")), 1, 0)), 
      0)
  )

# Test - are percentages in line with expectation?
check <- slim_db %>%
  summarise(
    uptake_p = sum(uptake_n)/sum(invite_n),
    positive_p = sum(positive_n)/sum(uptake_n),
    col_perf_p = sum(col_perf_n)/sum(positive_n),
    col_complete_p = sum(col_complete_n)/sum(col_perf_n),
    col_complic_p = sum(col_complic_n)/sum(col_perf_n),
    cancer_ppv = sum(cancer_n)/sum(col_perf_n),
    adenoma_ppv = sum(adenoma_n)/sum(col_perf_n),
    hr_adenoma_ppv = sum(hr_adenoma_n)/sum(col_perf_n)   
  )

### Step 3 - Create 'by' variables

# Waiting times
slim_db <- slim_db %>%
  mutate(date_diff = 
           ifelse(col_perf_n == 1,
                  difftime(datecolperf, screresdat, units = "days"),
                  0),
         waiting_time = ifelse(
           col_perf_n == 1, 
           case_when(
             between(date_diff, 1, 28) ~ "0 to 4 weeks",
             between(date_diff, 29, 56) ~ "4 to 8 weeks",
             date_diff > 56 ~ ">8 weeks"),
           "No colonoscopy"
         ),
         
         # Dukes/TNM translation
         # Use TNM8 translation from 01/01/2018. This is when it was published, 
         # not necessarily when the boards started using
         # Translation provided by Frank Carey
         dukes_der =
           ifelse(cancer_n == 1, 
                  ifelse(screresdat >= as.Date("2018-01-01"),
                         
                         case_when(
                           tnmm %in% c("M1","pM1") ~ "D",
                           tnmn %in% c("N1", "N2", "pN1", "pN2") ~ "C",
                           tnmt %in% c("T3","T4","pT3","pT4") ~ "B",
                           tnmt %in% c("T1","T2","pT1","pT2") ~ "A",
                           TRUE ~ "Not known"),
                         
                         ifelse(screresdat < as.Date("2018-01-01"),
                                case_when(
                                  dukes == "04" ~ "D",
                                  dukes %in% c("03A","03B") ~ "C",
                                  dukes == "02" ~ "B",
                                  dukes == "01" ~ "A",
                                  dukes %in% c("96","99") ~ "Not known",
                                  TRUE ~ "Not known"),
                                "")
                  ),
                  ""
           )
  )

# check what is causing NAs
# table(slim_db$dukes, slim_db$dukes_der, useNA = "always")
# visualise staging over time

# stage_chart <- slim_db %>%
#   filter(cancer_n == 1) %>%
#   mutate(year = year(screresdat))
# g <- ggplot(data = stage_chart, aes(x = year, fill = dukes_der)) + 
#   geom_bar(position = "dodge")
# g

# Test - monitor % of each dukes stage/numbers over time?

#### GC at this point - 19/03/2019

### Calculate screening participation history

# Horrible way of doing this but can't figure out a way around it
# Create dataset for each person's round of screening and then merge that

# Create variable for latest round of screening available
max_round <- max(slim_db$date_round)


for(i in seq_along(x)){
  assign(paste('X', i, sep=''), x[i])
}

for (i in 1:max_round)
  
  {
       uptake_rnd_i <- slim_db %>%
         select(chinum, date_round, uptake_n) %>%
         filter(date_round == i) %>%
         mutate(paste0('uptake_n_', i) = uptake_n) %>%
         select(-date_round)
       assign(paste("uptake_rnd_", i, sep = ''), uptake_n_rnd)
}

# Need to find a way of automating this so that there is a dataset/variable for
# the latest round
uptake_rnd_i <- left_join(uptake_rnd_1, uptake_rnd_2, by = "chinum") %>%
  left_join(uptake_rnd_3, by = "chinum") %>%
  left_join(uptake_rnd_4, by = "chinum") %>%
  left_join(uptake_rnd_5, by = "chinum") %>%
  left_join(uptake_rnd_6, by = "chinum")

invdate_hist <- slim_db %>%
  arrange(chinum, invdate) %>%
  select(chinum, date_round, invdate) %>%
  dcast(chinum ~ date_round)

# Check number of unique rows
slim_db %>% 
  distinct(chinum) %>%
  count()
# Agrees with screening history file




%>%
  mutate(
    
    
    
    
    screening_hist =
      case_when(
        is.na(screres_1) ~ "Error - no round 1",
        
        
        
      )
  )
# Need to pick up those with blank date round 1 with IT, flag for now


























