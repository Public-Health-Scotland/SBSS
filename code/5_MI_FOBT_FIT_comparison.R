##########################################################
# 5_MI_FOBT_FIT_comparison.R
# Thomas Godfrey
# 25/06/2019
# Script 5
# Management Information:
# COmparison of KPIs between FOBT and qFIT data.
# Written/run on R Studio server: and so uses //PHI_conf/
# R version 3.5.1 
# This script creates FIT/FOBT comparison data outputs
# Transcribed from SPSS script "FIT comparison KPIs.sps"
# At: 
#\\stats\CancerGroup1\Topics\BowelScreening\Publications\SBoSP-Statistics\20190205\Syntax
##########################################################


### Key outputs:

### Notes: 
# Get relevant records for the KPI report.
# Create variables required.
# Calculate uptake and positivity.
# GC TO DO - check the screening history calculation
start <- Sys.time()
### Step 1: Housekeeping
## Load packages
library(dplyr)
library(readr)
library(haven)
library(janitor)
library(magrittr)
library(lubridate)
library(invgamma)
library(here)
library(tidyr)
library(tidylog)

## Define functions
####################################
#Test
KPI_rate <- function(num, den, kpi_name) {
  
  # KPI for all persons
  KPI_all <- test_comp_db %>%
    group_by(test_type) %>%
    summarise(
      regs_n = sum(!!sym(num)),
      regs_d = sum(!!sym(den)),
      p = sum(!!sym(num)) / sum(!!sym(den))) %>%
    ungroup() %>%
    mutate(group_label = kpi_name)
  
  # KPI for uptake history
  KPI_hist <- test_comp_db %>%
    group_by(test_type, uptake_history) %>%
    summarise(
      regs_n = sum(!!sym(num)),
      regs_d = sum(!!sym(den)),
      p = sum(!!sym(num)) / sum(!!sym(den))) %>%
    ungroup() %>%
    mutate(group_label = kpi_name)
  
  # KPI for sex
  KPI_sex <- test_comp_db %>%
    group_by(test_type, sex) %>%
    summarise(
      regs_n = sum(!!sym(num)),
      regs_d = sum(!!sym(den)),
      p = sum(!!sym(num)) / sum(!!sym(den))) %>%
    ungroup() %>%
    mutate(group_label = kpi_name)
  
  # KPI for age
  KPI_age <- test_comp_db %>%
    group_by(test_type, age_group) %>%
    summarise(
      regs_n = sum(!!sym(num)),
      regs_d = sum(!!sym(den)),
      p = sum(!!sym(num)) / sum(!!sym(den))) %>%
    ungroup() %>%
    mutate(group_label = kpi_name)
  
  #By SIMD2016.
  KPI_simd <- test_comp_db %>%
    group_by(test_type, simd2016) %>%
    summarise(
      regs_n = sum(!!sym(num)),
      regs_d = sum(!!sym(den)),
      p = sum(!!sym(num)) / sum(!!sym(den))) %>%
    ungroup() %>%
    filter(!is.na(simd2016)) %>%
    mutate(group_label = kpi_name)
  
  test_comp_KPI <- bind_rows(
    KPI_all,
    KPI_hist,
    KPI_sex,
    KPI_age,
    KPI_simd) %>% 
    select(group_label, test_type, uptake_history, sex,age_group,
           simd2016, regs_n, regs_d, p) %>%
  # Calculate upper and lower 95% confidence intervals
    # To calculate confidence intervals, we use the Wilson score method.
    # Currently we use a formula directly converted from the IBM-SPSS website.
    # This implements the Wilson Score method for the 100(1â€“alpha)% confidence limits 
    # for the proportion; where 'p' is a proportion (not a percentage representation), 
    # 'alpha' is a significance level and 'n' is the denominator of the proportion 
    # (e.g. total number of individuals in the sample/population). 		
    # Reference: http://www-01.ibm.com/support/docview.wss?uid=swg21480513 
    mutate(
      lower95CI = 
        ((p + qchisq((1-0.05),df=1)/(2*regs_d) - qnorm(1-(0.05/2),mean=0,sd=1)
          *sqrt((p*(1-p)+qchisq((1-0.05),df=1)/(4*regs_d))/regs_d))) / 
        (1+qchisq((1-0.05),df=1)/regs_d),
      upper95CI = 
        ((p + qchisq((1-0.05),df=1)/(2*regs_d) + qnorm(1-(0.05/2),mean=0,sd=1)
          *sqrt((p*(1-p)+qchisq((1-0.05),df=1)/(4*regs_d))/regs_d))) / 
        (1+qchisq((1-0.05),df=1)/regs_d) )
}


## Set filepaths and import reporting period dates from Script 0
# Defines location of combined_extract_all and analsysis_dataset
source(here::here("code", "0_housekeeping.R"))

### Step 2: Import data
sbsp_analysis_db <- readRDS(analysis_db_path)

### Step 3: Get relevant records for the KPI report.
# Open the analysis database.
# Select test comparisosn dates.
# Ensure: no optins and only valid health boards.
# Select only participating individuals.

sbsp_slimdb <- sbsp_analysis_db %>%
  filter(optin == 0 &
           hbr14 %in% 1:14 &
           screres %in% c(1:18,21,22,24)) %>%
  mutate(
    canc_col_n = cancer_n * col_perf_n,
    adenoma_col_n = adenoma_n * col_perf_n,
    hr_adenoma_col_n = hr_adenoma_n * col_perf_n
  )
dim(sbsp_slimdb)
names(sbsp_slimdb)

#Select report time period and comparison time period
test_comp_db <- sbsp_slimdb %>% 
  mutate(test_type = case_when(
    (invdate >= as.Date("2015-11-20") & invdate <= as.Date("2017-04-30")) ~ 1,
    (invdate >= as.Date("2017-11-20") & invdate <= as.Date("2019-04-30")) ~ 2)
  ) %>%
  filter(test_type ==1|test_type ==2) #%>%
# GC TO DO - Need to think about whether it is still appropriate to exclude 
# those with no simd
#filter(simd2016 %in% 1:5)
# 1,779,452 - same in SPSS
# 2,691,777 Nov 19

#################################################
#Calculate test comparison for each KPI 

#Uptake all
test_comp_uptake <- KPI_rate('uptake_n','invite_n','Uptake')

saveRDS(test_comp_uptake, file = here::here("Temp", "test_comp_uptake.rds"))

#Positivity
test_comp_positivity <- KPI_rate('positive_n','uptake_n','Positivity')

saveRDS(test_comp_positivity, file = here::here("Temp", "test_comp_positivity.rds"))

# Cancer PPV
# GC have run with cancer_n as a check and it matches what was done previously, 
# however canc_col_n should be used as this is only cases of cancer where a 
# colonoscopy has been performed
test_comp_cancer_ppv <- KPI_rate('canc_col_n','col_perf_n','Cancer PPV')

saveRDS(test_comp_cancer_ppv, file = here::here("Temp", "test_comp_cancer_ppv.rds"))

#Adenoma PPV - same comment applies as with cancer
test_comp_adenoma_ppv <- KPI_rate('adenoma_col_n','col_perf_n','Adenoma PPV')

saveRDS(test_comp_adenoma_ppv, file = here::here("Temp", "test_comp_adenoma_ppv.rds"))

# HR adenoma PPV - same comment applies as with cancer
test_comp_hr_adenoma_ppv <- KPI_rate('hr_adenoma_col_n','col_perf_n',
                                     'Adenoma PPV')

saveRDS(test_comp_hr_adenoma_ppv, file = here::here("Temp", "test_comp_hr_adenoma_ppv.rds"))

# Cancer detection
test_comp_cancer_det <- KPI_rate('cancer_n','uptake_n','Cancer detection')

saveRDS(test_comp_cancer_det, file = here::here("Temp", "test_comp_cancer_det.rds"))

# Adenoma detection
test_comp_adenoma_det <- KPI_rate('adenoma_n','uptake_n','Adenoma detection')

saveRDS(test_comp_adenoma_det, file = here::here("Temp", "test_comp_adenoma_det.rds"))


###################################################
# Calculate stats by haemoglobin concentration
## GC TO DO - deal with possible different cancer numbers detected by
## colonoscopy/non-colonoscopy for PPV calculations

hbg <- test_comp_db %>%
  mutate(hbg20 =
           # Want to round down to nearest 20 for purposes of test threshold
           # min part isn't working for some reason, though the parts work
           # separately
           case_when(
             test_type == 1 ~ "FOBT",
             haemoglobin >= 200 ~ "200+",
             haemoglobin < 80 ~ "<80",
             haemoglobin >= 80 & haemoglobin < 200 
             ~ as.character(floor(haemoglobin/20) * 20)),
         hbg20 = forcats::fct_relevel(hbg20,
                                      "<80","80","100","120","140","160","180","200+",
                                      "FOBT")
  ) %>%
  group_by(hbg20) %>%
  summarise(
    number_screened = sum(uptake_n),
    positive_tests = sum(positive_n),
    colonoscopies_performed = sum(col_perf_n),
    cancers_detected = sum(canc_col_n),
    hr_adenomas_detected = sum(hr_adenoma_col_n),
    all_adenomas_detected = sum(adenoma_col_n)
  ) %>% 
  ungroup() %>%
  filter(!is.na(hbg20)) 
#Creates this warning but does the job: 
#Factor `hbg20` contains implicit NA, consider using `forcats::fct_explicit_na` 

hbg <- as.data.frame(t(hbg))

saveRDS(hbg, file = here::here("Temp", "hbg.rds"))


###############################################################################
# Check numbers detected with no colonoscopy performed for cover sheet

check <- test_comp_db %>%
  group_by(test_type) %>%
  summarise(cancer_n = sum(cancer_n),
            canc_col_n = sum(canc_col_n),
            canc_diff = cancer_n - canc_col_n,
            adenoma_n = sum(adenoma_n),
            adenoma_col_n = sum(adenoma_col_n),
            adenoma_diff = adenoma_n - adenoma_col_n,
            hr_adenoma_n = sum(hr_adenoma_n),
            hr_adenoma_col_n = sum(hr_adenoma_col_n),
            hr_adenoma_diff = hr_adenoma_n - hr_adenoma_col_n)
end <- Sys.time()


###############################################################################
# Calculate absolute numbers for key indicators
# i) actual numbers testing positive- and therefore being 
# ii) actual number diagnosed with cancer


# i) Total persons testing positive
total_positive <- test_comp_db %>%
                group_by(test_type) %>%
                summarise(total_n_positive = sum(positive_n)) %>%
                ungroup()
total_positive

# Save
write_csv(total_positive, here("Temp","total_positive.csv"))



# ii) Total persons diagnosed with cancer.
total_diagnosed_cancer <- test_comp_db %>%
                group_by(test_type) %>%
                summarise(total_n_cancer = sum(cancer_n)) %>%
                ungroup()
total_diagnosed_cancer

# Save
write_csv(total_diagnosed_cancer, here("Temp", "total_diagnosed_cancer.csv"))


