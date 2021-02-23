# NDFA Water Quality
# Purpose: Import and prepare data from LIB station for testing outlier detection methods
# Author: Dave Bosworth
# Contacts: David.Bosworth@water.ca.gov


# Load packages
library(tidyverse)
library(lubridate)

# Define directory for local repository
temp_fp_local_repo <- "C:/Repositories/ND-FASTR"

# Source global WQ functions
source(file.path(temp_fp_local_repo, "global_ndfa_funcs.R"))
source(file.path(temp_fp_local_repo, "Water_Quality/global_wq_funcs.R"))

# Define relative file path for processed continuous WQ data file for LIB
temp_fp_rel_rtm_data <- "WQ_Subteam/Processed_Data/Continuous/Filtered_Dates/RTM_OUTPUT_LIB_formatted_filt.csv"

# Define absolute file path
temp_fp_abs_rtm_data <- ndfa_abs_sp_path(temp_fp_rel_rtm_data)

# Set System Timezone as "Etc/GMT+8" (PST) to make it consistent with the data 
Sys.setenv(TZ = "Etc/GMT+8")

# Import data
df_orig <- import_rtm_data(temp_fp_abs_rtm_data, 11)

# Clean original data and nest it by year and parameter to test out outlier detection methods
df_clean_nest <- df_orig %>% 
  # parse date-time variable and define tz as PST
  mutate(DateTime = ymd_hms(DateTime, tz = "Etc/GMT+8")) %>% 
  # only keep Turbidity, Chla, and fDOM parameters for testing
  select(DateTime, Turbidity, Chla, fDOM) %>% 
  # pivot data longer for nesting
  pivot_longer(
    cols = -DateTime,
    names_to = "parameter",
    values_to = "value"
  ) %>% 
  # add a year variable
  mutate(yr = year(DateTime)) %>% 
  # remove data with NA values
  filter(!is.na(value)) %>% 
  # group nest by year and parameter
  nest(data = -c(yr, parameter)) %>%
  # fill in datetime values for gaps of missing data
  mutate(
    data_full_dt = if_else(
      yr == 2013,
      map(data, ~complete(.x, DateTime = seq.POSIXt(min(DateTime), max(DateTime), by = "30 min"))),
      map(data, ~complete(.x, DateTime = seq.POSIXt(min(DateTime), max(DateTime), by = "15 min")))
    )
  ) %>% 
  select(-data)

# Remove all temporary objects
rm(list = ls()[str_detect(ls(), "^temp")])

