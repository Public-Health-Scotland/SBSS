#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 03_funnel_plot_limits_data.R
# Thomas Godfrey
# Aug 2022
# Script 3 of 13
# Preparation of data for export (See Purpose)
# Written/run on R Studio server: and so uses //PHI_conf/
# R version 3.6.1 
# This script creates Confidence Interval data for funnel plots in Excel
# Transcribed from SPSS script "2_KPI_graphs_limits_N18.sps"
# At:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Purpose of this script
# Previous Script generates "KPI_data.rds" containing results for KPIs 1-26 by HB.
# For some KPIs (3,7,8,17,19 & 20),'funnel plots' are used to check that HB results 
# are within acceptable warning and control limits (95 and 99% CIs, respectively).
# These plots are cartesian graphs with KPI rates on the Y-axis and rate-denominators
# on the X-axis. The 'funnel' consists of warning & control CIs estimated for the
# Scotland-level KPI rate, but using a continuous series of possible values for 
# the denominator/x-axis - which is the number of people 'n' in the population
# of patients appropriate for that KPI. HBs are then plotted individually, with
# their KPI rate and the number of people 'n' in the HB - in the category of 
# patients used as the rate-denominator - providing y- and x-axis co-ordinates).

### This script has two aims:
# 1. to export relevant KPI rate-denominator values to provide the x-axis
#     co-ordinates needed to plot HB datapoints on funnel plots (corresponding
#     Y-axis values (HB KPI estimates) are available from 'KPI data.xlsx')
# 2. to generate a dataset, for each KPI, of 95% and 99% confidence intervals, 
#     for the Scotland-level KPI rate, using a series of possible values for the
#     KPI denominator. These datasets of upper and lower CIs then provide
#     the x- and y-axis co-ordinates for plotting warning and control limits.

### Notes: 
## KPIs with funnel plots:
# 3. % of people with a positive screening test result for both sexes, by HB.
# 7. % of colonoscopic complications, by HB.
# 8. % of people that had a cancer detected for both sexes, by HB.
#17. % of people that had a polyp cancer detected for both sexes, by HB.
#19. % of people with adenomas detected for both sexes, by HB.
#20. % of people with high risk adenomas detected for both sexes, by HB.

## Hence, the two sets of KPI denominator values required are:
# the Number of people with a completed screening test result (for KPIs 3,8,17,19,20)
# the Number of people who have had a colonoscopy performed (for KPI 7)

### Key outputs
# 1. no.of people who completed kits with a final result by HB (x-axis)
# 2. no.of people with a positive result who have a colonoscopy by HB (x-axis)
# 3. Wilson Score confidence intervals for funnel plot limits for each KPI 
#Note: Outputs generated by this syntax are copied into the KPI report Excel file,
#      an then 'hidden' but used to generate charts. 


### Step 0: Housekeeping ----

## Load packages
library(here)
library(dplyr)
library(readr)
library(janitor)
library(magrittr)
library(tidyr)
library(lubridate)
library(haven)
library(tidylog)

## Set filepaths to the SBSP analysis database and import reporting period dates 
rm(list = ls())
source(here::here("Code", "00_housekeeping.R"))
wd <- paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening",
             "/Publications/SBoSP-Statistics/20230804")
# source(paste0(wd, "/Code/00_housekeeping.R"))



## Define functions
# To calculate confidence intervals, we use the Wilson score method.
# Currently we use a formula directly converted from the IBM-SPSS website.
# This implements the Wilson Score method for the 100(1–alpha)% confidence limits 
# for the proportion; where 'p' is a proportion (not a percentage representation), 
# 'alpha' is a significance level and 'n' is the denominator of the proportion 
# (e.g. total number of individuals in the sample/population). 		
# Reference: http://www-01.ibm.com/support/docview.wss?uid=swg21480513 

Wilson_lowerCI <- function(kpi_p, alpha, n){
  ((kpi_p + qchisq((1-alpha),df=1)/(2*n) - qnorm(1-(alpha/2),mean=0,sd=1)
    *sqrt((kpi_p*(1-kpi_p)+qchisq((1-alpha),df=1)/(4*n))/n))) / 
    (1+qchisq((1-alpha),df=1)/n)   
}

Wilson_upperCI <- function(kpi_p, alpha, n){
  ((kpi_p + qchisq((1-alpha),df=1)/(2*n) + qnorm(1-(alpha/2),mean=0,sd=1)
    *sqrt((kpi_p*(1-kpi_p)+qchisq((1-alpha),df=1)/(4*n))/n))) / 
    (1+qchisq((1-alpha),df=1)/n)   
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Source function call currently working. Delete this code when properly running scripts.
#analysis_db <- readRDS(paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening/",
#                            "TPP/KPIs/Code + DB/TPP/data/analysis_dataset.rds"))
# Directly define dates but delete later.
#date_first <- "2016-05-01"
#date_last  <- "2018-04-30"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### Step 1: Import data ----

#Import analysis database from script 1
analysis_db <- read_rds(analysis_db_path)

dim(analysis_db)
names(analysis_db)

# Then use Select statement to keep only required variables
slim_db <- select(analysis_db, 
                  invdate,sex, hbr19, optin, 
                  invite_n, uptake_n, col_perf_n, positive_n, 
                  polyp_cancer_n, adenoma_n,hr_adenoma_n)
dim(slim_db)


# Use Filter statement to keep only data from reporting period
slim_db <- slim_db %>%
            filter(hbr19 %in% 1:14) %>%
            filter(optin == 0) %>%
            filter(invdate >= as.Date(date_first) & 
                   invdate <= as.Date(date_last))

dim(slim_db)
head(slim_db)


## Step 2: Calculate HB-level KPI denominator values ---- 

# for x-axis co-ordinates i.e.:
# the number of people who completed kits with a final result
# the number of people with positive result having a colonoscopy performed
denom_n_hb <- slim_db %>%
              group_by(hbr19) %>%
              summarise(uptake_n = sum(uptake_n),
                        col_perf_n = sum(col_perf_n)) %>%
              ungroup()

# Save output file
# write_rds(denom_n_hb, here::here("Temp", "Funnel-data_HB-denominators.rds"),
#           compress = 'gz')
write_rds(denom_n_hb, paste0(wd, "/Temp/Funnel-data_HB-denominators.rds"),
          compress = "gz")


## Step 3: Calculate Wilson Score confidence intervals ----

## Analysis process:
# Create a skeleton containing all combinations of KPI number, sex and series 'n'
# Note that here sex is just '3' (all persons).
# Import Scotland-level KPI rates from 'KPI data.xlsx'
# Match the KPI rates to the skeleton by KPI number and sex.
# Convert KPI rates from %s to probabilities.
# Calculate lower and upper Wilson score CIs
# Convert CI values from probabilities to %s.
# Save.

## Create skeleton
kpi <- c(3,7,8,17,19,20)
sex <- "Persons"
n <- c(1, 
       seq(10, 50,  by=10),
       seq(100, 2000, by=50),
       seq(2100, 2300, by=100),
       seq(2500, 10000, by=500),
       seq(11000, 19000, by=1000),
       seq(22000, 250000,  by=3000))

funnel_limits_skeleton <- crossing(kpi, sex, n)


## Import Scotland-level KPI estimates from 'KPI_data.rds' produced in script 2.
# Note: could alternatively recalculate estimates on-the-fly & label with KPI no. & sex.

# scotland_KPIs_db <- read_rds(here::here("Temp","KPI_data.rds"))
scotland_KPIs_db <- read_rds(paste0(wd, "/Temp/KPI_data.rds"))



dim(scotland_KPIs_db)
names(scotland_KPIs_db)

scotland_KPIs_db <- select(scotland_KPIs_db, kpi, sex, "15") %>%
                    filter(sex == "Persons") %>%
                    filter(kpi %in% c(3,7,8,17,19,20)) %>%
                    rename(scotland_rate = "15")

# Match them to add rates to skeleton.
conf_limits_db <- left_join(funnel_limits_skeleton, scotland_KPIs_db, 
                           by = c("kpi","sex"))
names(conf_limits_db)
head(conf_limits_db)

### Step 4: Calculate Warning and Control limits ----

# First, convert Scotland KPIs from percentages back to proportions.
conf_limits_db$p <- (conf_limits_db$scotland_rate)/100

# Calculate 95%CIs
conf_limits_db$lower95 <- Wilson_lowerCI(conf_limits_db$p, alpha=0.05, conf_limits_db$n)
conf_limits_db$upper95 <- Wilson_upperCI(conf_limits_db$p, alpha=0.05, conf_limits_db$n)

# Calculate 99%CIs
conf_limits_db$lower99 <- Wilson_lowerCI(conf_limits_db$p, alpha=0.01, conf_limits_db$n)
conf_limits_db$upper99 <- Wilson_upperCI(conf_limits_db$p, alpha=0.01, conf_limits_db$n)

#Convert CLs into %
conf_limits_db$lower95 <- conf_limits_db$lower95 * 100
conf_limits_db$upper95 <- conf_limits_db$upper95 * 100
conf_limits_db$lower99 <- conf_limits_db$lower99 * 100
conf_limits_db$upper99 <- conf_limits_db$upper99 * 100

head(conf_limits_db)
glimpse(conf_limits_db)


## Finally reshape the data

#Simplify dataset
conf_limits_db <- select(conf_limits_db, -sex, -p)

#Create subsets by KPI to re-join
kpi_3  <- filter(conf_limits_db, kpi ==  3)
kpi_7  <- filter(conf_limits_db, kpi ==  7)
kpi_8  <- filter(conf_limits_db, kpi ==  8)
kpi_17 <- filter(conf_limits_db, kpi == 17)
kpi_19 <- filter(conf_limits_db, kpi == 19)
kpi_20 <- filter(conf_limits_db, kpi == 20)

#Rename columns to add a KPI ID prefix
colnames(kpi_3)  <- paste("kpi3",  colnames(kpi_3),  sep = "_")
colnames(kpi_7)  <- paste("kpi7",  colnames(kpi_7),  sep = "_")
colnames(kpi_8)  <- paste("kpi8",  colnames(kpi_8),  sep = "_")
colnames(kpi_17) <- paste("kpi17", colnames(kpi_17), sep = "_")
colnames(kpi_19) <- paste("kpi19", colnames(kpi_19), sep = "_")
colnames(kpi_20) <- paste("kpi20", colnames(kpi_20), sep = "_")

#Bind tables together
conf_limits_output <- bind_cols(kpi_3, kpi_7, kpi_8, kpi_17, kpi_19, kpi_20, 
                                .id = NULL)

#Tidy up columns by removing KPI-label columns and redundant columns of 'n'
names(conf_limits_output)
conf_limits_output <-  select(conf_limits_output, 
                              -kpi3_kpi, 
                              -kpi7_kpi, -kpi7_n,
                              -kpi8_kpi, -kpi8_n,
                              -kpi17_kpi,-kpi17_n,
                              -kpi19_kpi,-kpi19_n, 
                              -kpi20_kpi,-kpi20_n) %>%
                        rename(population_n = kpi3_n)


### Step 5: Save output file ----

# write_excel_csv(conf_limits_output, 
#                 path = paste0("/PHI_conf/CancerGroup1/Topics/BowelScreening/TPP/KPIs/",
#                               "Code + DB/TPP/data/Funnel-data_Confidence-limits.csv"))
#                               
# write_rds(conf_limits_output,
#           here::here("Temp","Funnel-data_Confidence-limits.rds"),
#           compress = 'gz')

write_rds(conf_limits_output, 
          paste0(wd, "/Temp/Funnel-data_Confidence-limits.rds"))
          