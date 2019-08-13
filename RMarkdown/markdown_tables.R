##########################################################
# markdown_tables.R
# Gavin Clark
# 27/06/2019
# Data summary/export
# Written/run on R Studio Server
# R version 3.2.3
# This script creates tables as required for the publication report
# produced by
##########################################################

### Step 1 - housekeeping
library(dplyr)
library(tidyr)
library(janitor)

#   set filepaths and extract dates with script 0
source(here::here("code", "0_housekeeping.R"))


# Health board list for table
hblist <- data.frame(hb = c('Ayrshire and Arran',
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
                            'Western Isles'))


### Step 2 - table creation

# Bring in analysis database from script 1
analysis_db <-
  readRDS(analysis_db_path)

# Summarise number of invites, screened and uptake by health board
hb_table <- analysis_db %>%
  filter(between(invdate,
                 as.Date(date_first),
                 as.Date(date_last)) &
           optin == 0 &
           hbr14 %in% 1:14) %>%
  group_by(hbr14) %>%
  summarise(
    invite_n = sum(invite_n),
    uptake_n = sum(uptake_n)
  ) %>%
  ungroup() %>%
  bind_cols(hblist) %>%
  select(hb, invite_n, uptake_n)

scotland_table <- hb_table %>%
  summarise(
    invite_n = sum(invite_n),
    uptake_n = sum(uptake_n)
  ) %>% 
  mutate(hb = "Scotland")

# Add Scotland to hb table
uptake_table <- bind_rows(hb_table, scotland_table) %>%
  mutate(uptake_p = round_half_up(uptake_n/invite_n*100, 1))


# Save for import to markdown
saveRDS(uptake_table, here::here("RMarkdown/tables", "uptake_table.rds"))
