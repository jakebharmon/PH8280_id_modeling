library(tidyverse)
library(readr)
library(viridis) 

# Downloaded from https://gis.cdc.gov/grasp/fluview/FluHospChars.html
flu_clinical_df <- read_csv(
  file = ("~/Documents/PH8280_id_modeling/manuscript/data/source/ICL_NREVSS_Clinical_Labs.csv"),
  skip = 1) %>% 
  rename(
    "season" = 3,
    "week_num" = 4
  ) %>% 
  drop_na() %>% 
  
  # Create diy columns with factored week numbers as well as summed flu count
  mutate(week_num = week_num %>% as_factor() %>% as.numeric(),
         flu_count = rowSums(.[6:7],),
         source = "clinical") %>% 
  select(season, week_num, flu_count, source) %>%
  
  # Select seasons 2010-11 through 2020-21
  filter(season %in% 2015:2020)


flu_public_df <- read_csv(
  file = ("~/Documents/PH8280_id_modeling/manuscript/data/source/ICL_NREVSS_Public_Health_Labs.csv"),
  skip = 1) %>% 
  rename(
    "season" = 3,
    "week_num" = 4
  ) %>% 
  drop_na() %>% 
  
  # Create diy columns with factored week numbers as well as summed flu count
  mutate(week_num = week_num %>% as_factor() %>% as.numeric(),
         flu_count = rowSums(.[6:7]),
         source = "public") %>% 
  select(season, week_num, flu_count, source) %>%
  
  # Select seasons 2010-11 through 2020-21
  filter(season %in% 2015:2020)

# For graphing
flu_time_series <- rbind(flu_clinical_df, flu_public_df) %>% 
  summarize(flu_count = sum(flu_count), 
            .by = c(season, week_num)) %>% 
  mutate(source = "total") %>% 
  rbind(flu_clinical_df, flu_public_df)