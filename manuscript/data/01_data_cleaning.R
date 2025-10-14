library(tidyverse)
library(readr)
library(viridis) 

# Downloaded from https://gis.cdc.gov/grasp/fluview/FluHospChars.html
flu_clinical_df <- read_csv(
  file = ("~/Documents/PH8280_id_modeling/manuscript/data/source/ICL_NREVSS_Clinical_Labs.csv"),
  skip = 1) %>% 
  rename(
    "year" = 3,
    "week_num" = 4,
    "flu_a" = 6,
    "flu_b" = 7
  ) %>% 
  drop_na() %>% 
  
  # Create diy columns with factored week numbers as well as summed flu count
  mutate(week_num = week_num %>% 
           as_factor() %>% 
           as.numeric()) %>% 
  select(year, week_num, flu_a, flu_b) %>%
  
  # Select seasons 2010-11 through 2020-21
  filter(year %in% 2021:2024)


flu_public_df <- read_csv(
  file = ("~/Documents/PH8280_id_modeling/manuscript/data/source/ICL_NREVSS_Public_Health_Labs.csv"),
  skip = 1)  %>% 
  drop_na() %>% 
  # Create diy columns with factored week numbers as well as summed flu count
  mutate(week_num = WEEK %>% as_factor() %>% as.numeric(),
         flu_a = rowSums(.[c(6:8,12:13)]),
         flu_b = rowSums(.[9:11])) %>%
  rename(
    "year" = 3
  ) %>% 
  select(year, week_num, flu_a, flu_b) %>%
  
  # Select seasons 2010-11 through 2020-21
  filter(year %in% 2021:2024)

# For graphing
flu_time_series <- bind_rows(flu_clinical_df, flu_public_df) %>%
  group_by(year, week_num) %>%
  summarize(
    flu_a = sum(flu_a, na.rm = TRUE),
    flu_b = sum(flu_b, na.rm = TRUE),
    total = flu_a + flu_b,
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(flu_a, flu_b, total),
    names_to = "type",
    values_to = "amount"
  )

# Saving 2024 for analysis
influenza_df <- flu_time_series %>% 
  filter((year == 2023 & week_num > 30) |
          (year == 2024 & week_num < 30)) %>% 
  pivot_wider(
    id_cols = c(year, week_num),
    names_from = type,
    values_from = amount) %>% 
  mutate(days = rownames(.)) %>% 
  rename("cases1" = 3,
         "cases2" = 4) %>% 
  select(days, cases1, cases2)

writexl::write_xlsx(
  influenza_df, 
  path = "/Users/jake/Documents/PH8280_id_modeling/manuscript/data/BayesianFitForecast/influenza_df.xlsx"
)
