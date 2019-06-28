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

#   set filepaths and extract dates with script 0
source(here::here("code", "0_housekeeping.R"))

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

### Step 2 - chart creation

# Bring in analysis database from script 1
analysis_db <-
  readRDS(analysis_db_path)

# Summarise number of invites, screened and uptake by health board and sex
hb_table <- analysis_db %>%
  filter(between(invdate,
                 as.Date(date_first),
                 as.Date(date_last)) &
           optin == 0 &
           hbr14 %in% 1:14) %>%
  group_by(hbr14, sex) %>%
  summarise(
    invite_n = sum(invite_n),
    uptake_n = sum(uptake_n)
  ) %>%
  ungroup()

scotland_table <- hb_table %>%
  group_by(sex) %>%
  summarise(
    invite_n = sum(invite_n),
    uptake_n = sum(uptake_n)
  ) %>% 
  mutate(hbr14 = 15)

# Add Scotland to hb table
uptake_table <- bind_rows(hb_table, scotland_table) %>%
  mutate(uptake_p = round_half_up(uptake_n/invite_n*100, 1),
         standard = 60,
         sex = as.factor(sex)) %>%
  left_join(hblist, by = "hbr14") %>%
  select(hb, sex, uptake_p, standard) %>%
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
  #gather("type","value")

# Create chart
uptake_hb_chart <- ggplot(data = uptake_table, aes(hb)) +
  geom_bar(aes(y = uptake_p, group = sex, fill = sex), 
           stat= "identity", position = "dodge") +
  geom_line(aes(y = standard, group = 1, colour = "red")) + 
  xlab("Health board of residence") + 
  ylab("Uptake %") +
  scale_fill_manual(values = c( "1"="navyblue", "2"="cornflowerblue"),
                    labels = c("Male", "Female")) +
  # scale_linetype_manual(values = c(red),
  #                       labels = c("60% standard")) +
  scale_colour_manual(values = c("red"),
                      labels = c("60% standard")) +
  guides(guide_legend(reverse = TRUE)) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(0,10,0,0)),
        axis.line = element_line(colour="grey"),
        axis.ticks = element_line(colour="grey"),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.position = "top")
chart


# Chart height decided by seeing what size of image would fit in word document
# and using right-click size to get the image size. Default in word is cm,
# ggsave takes inches (I'm sure there will be a conversion function in R?)
ggsave(here::here("RMarkdown/charts","uptake_hb.png"), plot = uptake_hb_chart, device = "png", 
       dpi = 300, width = 6.1653543, height  = 2.582677)
