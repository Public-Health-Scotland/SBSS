
##########################################################
# Uptake_by_month.R
# Gavin Clark
# 31/05/2019
# Chart creation (temporary script, to be merged with Markdown)
# Written/run on R Studio Server
# R version 3.2.3
##########################################################

### Step 1 - housekeeping
library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)
library(scales)

# set filepaths and extract dates with script 0
source(here::here("code", "0_housekeeping.R"))

analysis_db <-
  readRDS(analysis_db_path) %>%
  filter(optin == 0 &
           hbr14 %in% 1:14 &
           invdate >= as.Date("2015-11-01") & 
           invdate <= as.Date("2018-10-31")) %>%
  select(invdate, invite_n, uptake_n) %>%
  mutate(
    yearmon = as.yearmon(invdate)
    )

uptake_by_month <- analysis_db %>%
  group_by(yearmon) %>%
  summarise(uptake = sum(uptake_n)/sum(invite_n) * 100) 
  # mutate(
  #   Standard = 60
  # ) %>%
  # group_by(yearmon) %>%
  # gather("measure", "value")

chart <- ggplot(data = uptake_by_month, aes(x = as.Date(yearmon), y = uptake)) +
  geom_line(aes(linetype = "Uptake %")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 0, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(0,10,0,0)),
        axis.line = element_line(colour="grey"),
        axis.ticks = element_line(colour="grey"),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.3, 0.9),
        legend.box = "horizontal") +
  scale_x_date(labels = date_format("%b-%Y"), date_breaks = "6 months") +
  ylim(0,100) +
  xlab("Month of invitation") + 
  geom_hline(aes(yintercept=60, color = "red"), 
             linetype = "dashed") +
             #show.legend = TRUE) +
  ylab("Uptake %") +
  ggtitle("Uptake by month from November 2015 to October 2018") +
  # Why is this bit not working?
  annotate("text", x = as.Date("2017-11-20"), y = 80, label = "FIT introduced") +
  annotate("segment", x = as.Date("2017-11-20"), xend = as.Date("2017-11-20"), 
           y = 75, yend = 65, colour = "black", 
           size=1, alpha=0.6, 
           arrow=arrow(angle = 20, 
                       length = unit(0.11, "inches"),
                       ends = "last")) + 
  scale_colour_manual(labels = c("60% Standard"),
                      values = c("red" = "red")) +
  scale_linetype_manual(values = c("solid","dashed"), 
                        guide = guide_legend(reverse = FALSE))
chart

# Chart height decided by seeing what size of image would fit in word document
# and using right-click size to get the image size. Default in word is cm,
# ggsave takes inches (I'm sure there will be a conversion function in R?)
ggsave("RMarkdown/chart1.png", plot = chart, device = "png", 
       dpi = 300, width = 6.1653543, height  = 2.582677)

