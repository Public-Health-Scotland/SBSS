##########################################################
# markdown_report_charts.R
# Gavin Clark
# 27/06/2019
# Data summary/visualisation/export
# Written/run on R Studio Server
# R version 3.2.3
# This script creates charts required for the publication report
##########################################################

# GC TO DO - could rewrite a lot of this as a function

### Step 1 - housekeeping
library(dplyr)
library(tidyr)
library(janitor)
library(ggplot2)
library(here)
library(zoo)
library(scales)

#   set filepaths and extract dates with script 0
source(here("code", "0_housekeeping.R"))

# Health board list for table
hblist <- data.frame(hbr14 = c(1:15), 
                     hb = c('Ayrshire and Arran',
                            'Borders',
                            'Dumfries and Galloway',
                            'Fife',
                            'Forth Valley',
                            'Grampian',
                            'Greater Glasgow and Clyde',
                            'Highland',
                            'Lanarkshire',
                            'Lothian',
                            'Orkney',
                            'Shetland',
                            'Tayside',
                            'Western Isles',
                            'Scotland'))

# bring in analysis database
analysis_db <- readRDS(analysis_db_path)

### Step 2 - chart creation

## Uptake by test type
test_comp_uptake <- readRDS(here("Temp", "test_comp_uptake.rds")) %>%
  slice(1:2) %>%
  select(test_type, p) %>%
  mutate(
    test_type = forcats::fct_relevel(as.factor(
      case_when(
        test_type == 1 ~ "FOBT",
        test_type == 2 ~ "FIT"
      )), "FOBT", "FIT"),
    p = p *100
  )

# Create chart
uptake_test_chart <- ggplot(data = test_comp_uptake, aes(test_type)) +
  geom_bar(aes(y = p, fill = "navyblue"), stat = "identity") +
  geom_hline(aes(yintercept = 60, 
                 group = 1, 
                 colour = "red"),
             size = 1.1) +
  xlab("Test type") + 
  ylab("Uptake %") +
  scale_fill_manual(values = c("navyblue"),
                    labels = c("Test type")) +
  scale_colour_manual(values = c("red"),
                      labels = c("60% standard")) +
  guides(guide_legend(reverse = TRUE)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        axis.line = element_line(colour = "grey"),
        axis.ticks = element_line(colour = "grey"),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.spacing.x = unit(0.2, "cm"),
        legend.key.height = unit(0.1, "cm"),
        legend.key.width = unit(0.8, "cm"),
        legend.position = "top")
uptake_test_chart

# Chart height decided by seeing what size of image would fit in word document
# and using right-click size to get the image size. Default in word is cm,
# ggsave takes inches (I'm sure there will be a conversion function in R?)
ggsave(here::here("RMarkdown","uptake_test.png"), plot = uptake_test_chart, device = "png", 
       dpi = 300, width = 6.88, height  = 4.22)

## Uptake by SIMD
# Summarise number of invites, screened and uptake by health board and sex
simd_uptake <- analysis_db %>%
  filter(between(invdate,
                 as.Date(date_first),
                 as.Date(date_last)) &
           optin == 0 &
           hbr14 %in% 1:14 &
           simd2016 %in% 1:5) %>%
  group_by(simd2016, sex) %>%
  summarise(
    invite_n = sum(invite_n),
    uptake_n = sum(uptake_n)
  ) %>%
  ungroup() %>%
  mutate(uptake_p = round_half_up(uptake_n/invite_n*100, 1),
         simd2016 = forcats::fct_relevel(as.factor(
           case_when(
             simd2016 == 1 ~ "1 most deprived",
             simd2016 == 2 ~ "2",
             simd2016 == 3 ~ "3",
             simd2016 == 4 ~ "4",
             simd2016 == 5 ~ "5 least deprived")),
           "5 least deprived",
           "4",
           "3",
           "2",
           "1 most deprived")
           ,sex = as.factor(sex)) %>%
  select(simd2016, sex, uptake_p)

# Create chart
simd_chart <- ggplot(data = simd_uptake, aes(simd2016)) +
  geom_bar(aes(y = uptake_p, 
               group = sex, 
               fill = sex), 
           stat = "identity", 
           position = "dodge") +
  geom_hline(aes(yintercept = 60, 
                 group = 1, 
                 colour = "red"),
             size = 1.1) +
  xlab("Deprivation quintile") + 
  ylab("Uptake %") +
  scale_fill_manual(values = c("1" = "navyblue", 
                               "2" = "cornflowerblue"),
                    labels = c("Male", "Female")) +
  scale_colour_manual(values = c("red"),
                      labels = c("60% standard")) +
  guides(guide_legend(reverse = TRUE)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 20, hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        axis.line = element_line(colour = "grey"),
        axis.ticks = element_line(colour = "grey"),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.spacing.x = unit(0.2, "cm"),
        legend.key.height = unit(0.1, "cm"),
        legend.key.width = unit(0.8, "cm"),
        legend.position = "top")

ggsave(here::here("RMarkdown","uptake_simd.png"), plot = simd_chart, device = "png", 
       dpi = 300, width = 6.88, height  = 4.22)

### Positivity by test type
test_comp_positivity <- readRDS(here("Temp", "test_comp_positivity.rds")) %>%
  slice(1:2) %>%
  select(test_type, p) %>%
  mutate(
    test_type = forcats::fct_relevel(as.factor(
      case_when(
        test_type == 1 ~ "FOBT",
        test_type == 2 ~ "FIT"
      )), "FOBT", "FIT"),
    p = p *100
  )

# Create chart
positivity_test_chart <- ggplot(data = test_comp_positivity, aes(test_type)) +
  geom_bar(aes(y = p, fill = "navyblue"), stat = "identity") +
  xlab("Test type") + 
  ylab("Positivity %") +
  scale_fill_manual(values = c("navyblue"),
                    labels = c("Test type")) +
  guides(guide_legend(reverse = TRUE)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 4)) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        axis.line = element_line(colour = "grey"),
        axis.ticks = element_line(colour = "grey"),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.spacing.x = unit(0.2, "cm"),
        legend.key.height = unit(0.1, "cm"),
        legend.key.width = unit(0.8, "cm"),
        legend.position = "none")
positivity_test_chart

# Chart height decided by seeing what size of image would fit in word document
# and using right-click size to get the image size. Default in word is cm,
# ggsave takes inches (I'm sure there will be a conversion function in R?)
ggsave(here::here("RMarkdown","positivity_test.png"), plot = positivity_test_chart, device = "png", 
       dpi = 300, width = 6.88, height  = 4.22)

### Colonoscopies completed by HB

# Summarise number of invites, screened and uptake by health board and sex
hb_table <- analysis_db %>%
  filter(between(invdate,
                 as.Date(date_first),
                 as.Date(date_last)) &
           optin == 0 &
           hbr14 %in% 1:14) %>%
  group_by(hbr14, sex) %>%
  summarise(
    positive_n = sum(positive_n),
    col_perf_n = sum(col_perf_n)
  ) %>%
  ungroup()

scotland_table <- hb_table %>%
  group_by(sex) %>%
  summarise(
    positive_n = sum(positive_n),
    col_perf_n = sum(col_perf_n)
  ) %>% 
  ungroup() %>%
  mutate(hbr14 = 15)

# Add Scotland to hb table
col_perf_table <- bind_rows(hb_table, scotland_table) %>%
  mutate(col_perf_p = round_half_up(col_perf_n/positive_n*100, 1),
         sex = as.factor(sex)) %>%
  left_join(hblist, by = "hbr14") %>%
  select(hb, sex, col_perf_p) %>%
  mutate(hb = forcats::fct_relevel(hb,'Ayrshire and Arran',
                                   'Borders',
                                   'Dumfries and Galloway',
                                   'Fife',
                                   'Forth Valley',
                                   'Grampian',
                                   'Greater Glasgow and Clyde',
                                   'Highland',
                                   'Lanarkshire',
                                   'Lothian',
                                   'Orkney',
                                   'Shetland',
                                   'Tayside',
                                   'Western Isles',
                                   'Scotland'))
# gather("type","value")

# Create chart
col_perf_chart <- ggplot(data = col_perf_table, aes(hb)) +
  geom_bar(aes(y = col_perf_p, 
               group = sex, 
               fill = sex), 
           stat = "identity", 
           position = "dodge") +
  xlab("Health board of residence") + 
  ylab("Colonoscopies performed %") +
  scale_fill_manual(values = c("1" = "navyblue", 
                               "2" = "cornflowerblue"),
                    labels = c("Male", "Female")) +
  guides(guide_legend(reverse = TRUE)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        axis.line = element_line(colour = "grey"),
        axis.ticks = element_line(colour = "grey"),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.spacing.x = unit(0.2, "cm"),
        legend.key.height = unit(0.1, "cm"),
        legend.key.width = unit(0.8, "cm"),
        legend.position = "top")

# Chart height decided by seeing what size of image would fit in word document
# and using right-click size to get the image size. Default in word is cm,
# ggsave takes inches (I'm sure there will be a conversion function in R?)
ggsave(here::here("RMarkdown","col_perf_hb.png"), plot = col_perf_chart, 
       device = "png", dpi = 300, width = 6.88, height  = 4.22)

### Cancer detection rate by test type
test_comp_canc_det <- readRDS(here("Temp", "test_comp_cancer_det.rds")) %>%
  slice(1:2) %>%
  select(test_type, p) %>%
  mutate(
    test_type = forcats::fct_relevel(as.factor(
      case_when(
        test_type == 1 ~ "FOBT",
        test_type == 2 ~ "FIT"
      )), "FOBT", "FIT"),
    p = p *100
  )

# Create chart
cancer_det_test_chart <- ggplot(data = test_comp_canc_det, aes(test_type)) +
  geom_bar(aes(y = p, fill = "navyblue"), stat = "identity") +
  xlab("Test type") + 
  ylab("Cancer detection %") +
  scale_fill_manual(values = c("navyblue"),
                    labels = c("Test type")) +
  guides(guide_legend(reverse = TRUE)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.15)) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        axis.line = element_line(colour = "grey"),
        axis.ticks = element_line(colour = "grey"),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.spacing.x = unit(0.2, "cm"),
        legend.key.height = unit(0.1, "cm"),
        legend.key.width = unit(0.8, "cm"),
        legend.position = "none")
cancer_det_test_chart

# Chart height decided by seeing what size of image would fit in word document
# and using right-click size to get the image size. Default in word is cm,
# ggsave takes inches (I'm sure there will be a conversion function in R?)
ggsave(here::here("RMarkdown","cancer_det_test.png"), plot = cancer_det_test_chart, device = "png", 
       dpi = 300, width = 6.88, height  = 4.22)

### Dukes staging 
dukes <- analysis_db %>%
  filter(between(invdate,
                 as.Date(date_first),
                 as.Date(date_last)) &
           optin == 0 &
           hbr14 %in% 1:14 &
           cancer_n == 1) %>%
  group_by(dukes_der) %>%
  summarise(
    cancer_n = sum(cancer_n),
  ) %>%
  ungroup() %>%
  mutate(
    freq = cancer_n/sum(cancer_n) * 100
  )

# Create chart
dukes_chart <- ggplot(data = dukes, aes(x = dukes_der, y = freq)) +
  geom_histogram(stat = "identity", fill = "navyblue") +
  xlab("Dukes stage") + 
  ylab("Proportion of cancers (%)") +
  guides(guide_legend(reverse = TRUE)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50)) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        axis.line = element_line(colour = "grey"),
        axis.ticks = element_line(colour = "grey"),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.spacing.x = unit(0.2, "cm"),
        legend.key.height = unit(0.1, "cm"),
        legend.key.width = unit(0.8, "cm"),
        legend.position = "none")
dukes_chart

# Chart height decided by seeing what size of image would fit in word document
# and using right-click size to get the image size. Default in word is cm,
# ggsave takes inches (I'm sure there will be a conversion function in R?)
ggsave(here::here("RMarkdown","dukes_chart.png"), plot = dukes_chart, 
       device = "png", dpi = 300, width = 6.88, height  = 4.22)


### Adenoma detection rate by test type
test_comp_adenoma_det <- readRDS(here("Temp", "test_comp_adenoma_det.rds")) %>%
  slice(1:2) %>%
  select(test_type, p) %>%
  mutate(
    test_type = forcats::fct_relevel(as.factor(
      case_when(
        test_type == 1 ~ "FOBT",
        test_type == 2 ~ "FIT"
      )), "FOBT", "FIT"),
    p = p *100
  )

# Create chart
adenoma_det_test_chart <- ggplot(data = test_comp_adenoma_det, aes(test_type)) +
  geom_bar(aes(y = p, fill = "navyblue"), stat = "identity") +
  xlab("Test type") + 
  ylab("Adenoma detection %") +
  scale_fill_manual(values = c("navyblue"),
                    labels = c("Test type")) +
  guides(guide_legend(reverse = TRUE)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.5)) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        axis.line = element_line(colour = "grey"),
        axis.ticks = element_line(colour = "grey"),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.spacing.x = unit(0.2, "cm"),
        legend.key.height = unit(0.1, "cm"),
        legend.key.width = unit(0.8, "cm"),
        legend.position = "none")
adenoma_det_test_chart

# Chart height decided by seeing what size of image would fit in word document
# and using right-click size to get the image size. Default in word is cm,
# ggsave takes inches (I'm sure there will be a conversion function in R?)
ggsave(here::here("RMarkdown","adenoma_det_test.png"), plot = adenoma_det_test_chart, device = "png", 
       dpi = 300, width = 6.88, height  = 4.22)

### Cancer ppv by test type
test_comp_cancer_ppv <- readRDS(here("Temp", "test_comp_cancer_ppv.rds")) %>%
  slice(1:2) %>%
  select(test_type, p) %>%
  mutate(
    test_type = forcats::fct_relevel(as.factor(
      case_when(
        test_type == 1 ~ "FOBT",
        test_type == 2 ~ "FIT"
      )), "FOBT", "FIT"),
    p = p *100
  )

# Create chart
cancer_ppv_test_chart <- ggplot(data = test_comp_cancer_ppv, aes(test_type)) +
  geom_bar(aes(y = p, fill = "navyblue"), stat = "identity") +
  xlab("Test type") + 
  ylab("Cancer PPV %") +
  scale_fill_manual(values = c("navyblue"),
                    labels = c("Test type")) +
  guides(guide_legend(reverse = TRUE)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 8)) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        axis.line = element_line(colour = "grey"),
        axis.ticks = element_line(colour = "grey"),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.spacing.x = unit(0.2, "cm"),
        legend.key.height = unit(0.1, "cm"),
        legend.key.width = unit(0.8, "cm"),
        legend.position = "none")
cancer_ppv_test_chart

# Chart height decided by seeing what size of image would fit in word document
# and using right-click size to get the image size. Default in word is cm,
# ggsave takes inches (I'm sure there will be a conversion function in R?)
ggsave(here::here("RMarkdown","cancer_ppv_test.png"), plot = cancer_ppv_test_chart, device = "png", 
       dpi = 300, width = 6.88, height  = 4.22)


### adenoma ppv by test type
test_comp_adenoma_ppv <- readRDS(here("Temp", "test_comp_adenoma_ppv.rds")) %>%
  slice(1:2) %>%
  select(test_type, p) %>%
  mutate(
    test_type = forcats::fct_relevel(as.factor(
      case_when(
        test_type == 1 ~ "FOBT",
        test_type == 2 ~ "FIT"
      )), "FOBT", "FIT"),
    p = p *100
  )

# Create chart
adenoma_ppv_test_chart <- ggplot(data = test_comp_adenoma_ppv, aes(test_type)) +
  geom_bar(aes(y = p, fill = "navyblue"), stat = "identity") +
  xlab("Test type") + 
  ylab("Adenoma PPV %") +
  scale_fill_manual(values = c("navyblue"),
                    labels = c("Test type")) +
  guides(guide_legend(reverse = TRUE)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50)) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        axis.line = element_line(colour = "grey"),
        axis.ticks = element_line(colour = "grey"),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.spacing.x = unit(0.2, "cm"),
        legend.key.height = unit(0.1, "cm"),
        legend.key.width = unit(0.8, "cm"),
        legend.position = "none")
adenoma_ppv_test_chart

# Chart height decided by seeing what size of image would fit in word document
# and using right-click size to get the image size. Default in word is cm,
# ggsave takes inches (I'm sure there will be a conversion function in R?)
ggsave(here::here("RMarkdown","adenoma_ppv_test.png"), plot = adenoma_ppv_test_chart, device = "png", 
       dpi = 300, width = 6.88, height  = 4.22)
