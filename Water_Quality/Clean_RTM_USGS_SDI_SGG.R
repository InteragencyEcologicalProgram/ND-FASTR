# NDFA Water Quality
# Purpose: Code to import, clean, and export continuous water quality data for stations SDI and SGG
# collected by USGS
# Author: Amanda Maguire

# Load packages
library(tidyverse)
library(lubridate)

# Import Data -------------------------------------------------------------

# Define path on SharePoint site for data - this works if you have the SharePoint site synced
# to your computer
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/Office of Water Quality and Estuarine Ecology - Water Quality Subteam"
  )
)
# SDI station - 11455478
sdi_orig <- read_csv(
  file = paste0(sharepoint_path, "/Raw_Data/Continuous/RTM_RAW_USGS_SDI.csv"),
  col_types = paste0("-cc", str_c(rep("dc", 11), collapse = ""), "-")
)

# SGG station - 11455276
sgg_orig <- read_csv(
  file = paste0(sharepoint_path, "/Raw_Data/Continuous/RTM_RAW_USGS_SGG.csv"),
  col_types = paste0("-cc", str_c(rep("dc", 7), collapse = ""), "-")
)

# Clean Data --------------------------------------------------------------

# USGS PARAMETER CODES:
# 00060 - Discharge (cfs)
# 72137 - Discharge, tidally-filtered (cfs)
# 00010 - Water Temperature (Celsius)
# 00300 - Dissolved Oxygen (mg/L)
# 00095 - Specific Conductance at 25 C (uS/cm)
# 00400 - pH
# 63680 - Turbidity (FNU)
# 32315 - Chlorophyll relative fluorescence (RFU)
# 32316 - Chlorophyll concentration estimated from reference material (ug/L)
# 32295 - Dissolved organic matter fluorescence (fDOM) (ug/L as QSE)
# 32321 - Phycocyanin relative fluorescence (RFU)
# 99133 - Nitrate plus nitrite (mg/L as N)

# Clean SDI data
sdi_clean <- sdi_orig %>% 
  mutate(
    # Parse date time variable
    DateTime = ymd_hms(dateTime),
    # Convert Station name to NDFA standardized name
    StationCode = "SDI"
  ) %>% 
  select(-dateTime)

# Clean SGG data
sgg_clean <- sgg_orig %>% 
  mutate(
    # Parse date time variable
    DateTime = ymd_hms(dateTime),
    # Convert Station name to NDFA standardized name
    StationCode = "SGG"
  ) %>% 
  select(-dateTime)

# Standardize parameter variable names
# Data values
sdi_values <- sdi_clean %>% 
  select(!ends_with("_cd")) %>% 
  pivot_longer(
    cols = -c(DateTime, StationCode, site_no),
    names_to = "parameter",
    values_to = "value"
  ) %>% 
  filter(!is.na(value)) %>% 
  mutate(
    parameter = case_when(
      str_detect(parameter, "00010") ~ "WaterTemp",
      str_detect(parameter, "00300") ~ "DO",
      str_detect(parameter, "00095") ~ "SpCnd",
      str_detect(parameter, "00400") ~ "pH",
      str_detect(parameter, "63680") ~ "Turbidity",
      str_detect(parameter, "32316") ~ "Chla",
      str_detect(parameter, "32295") ~ "fDOM",
      str_detect(parameter, "99133") ~ "NitrateNitrite"
    )
  ) %>% 
  pivot_wider(names_from = parameter, values_from = value)
