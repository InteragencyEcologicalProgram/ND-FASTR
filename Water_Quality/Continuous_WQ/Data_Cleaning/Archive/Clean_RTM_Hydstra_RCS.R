# NDFA Water Quality
# Purpose: Code to import, clean, and export RCS continuous water quality data
# downloaded from Hydstra
# Author: Amanda Maguire

# Load packages
library(tidyverse)
library(lubridate)
library(readxl)

# Import Data -------------------------------------------------------------

# Define path on SharePoint site for data - this works if you have the SharePoint site synced
# to your computer
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/Office of Water Quality and Estuarine Ecology - North Delta Flow Action/WQ_Subteam"
  )
)

# Import data
rcs_wq_orig <- read_csv(
  file = paste0(sharepoint_path, "/Raw_Data/Continuous/RTM_RAW_DWR RCS_2014-2019.csv"),
  col_names = FALSE,
  skip = 3,
  col_types = "cdd-------dd-dd-dd-dd-dd-"  # "c" = character, "d" = numeric, "-" = skip
) 

rcs_flow_orig <- read_excel(
  path = paste0(sharepoint_path, "/Raw_Data/Continuous/RTM_RAW_DWR RCS LIS Flow_2011-2019.xlsx"),
  sheet = "RCS",
  col_names = c("DateTime", "Flow", "Flow_Qual"),
  skip = 3,
  col_types = c("date", "numeric", "numeric")
) 

glimpse(rcs_wq_orig)
glimpse(rcs_flow_orig)


# Clean Data --------------------------------------------------------------

# HYDSTRA PARAMETER CODES:
# 450 - Water Temperature (Celcius)
# 630 - Depth below water surface (meters)
# 806 - Salinity (ppt)
# 810 - Turbidity (NTU)
# 821 - Specific Conductance at 25 C (uS/cm)
# 860 - pH
# 865 - Dissolved Oxygen (% saturation)
# 2351 - Dissolved Oxygen (mg/L)
# 7004 - Chlorophyll (ug/L)

# Clean data
# Change variable names for WQ dataframe - using NDFA standardized names
names(rcs_wq_orig) <- c(
  "DateTime",
  "WaterTemp",
  "WaterTemp_Qual",
  "Turbidity",
  "Turbidity_Qual",
  "SpCnd",
  "SpCnd_Qual",
  "pH",
  "pH_Qual",
  "DO",
  "DO_Qual",
  "Chla",
  "Chla_Qual"
)

# Parse date time variable on the WQ dataframe
rcs_wq_clean <- rcs_wq_orig %>% 
  mutate(DateTime = mdy_hm(DateTime))

glimpse(rcs_wq_clean)

# Round DateTime variable in both dataframes to the nearest 15 minute interval so that they can 
# be joined together
rcs_wq_clean <- rcs_wq_clean %>% 
  mutate(DateTime = round_date(DateTime, unit = "15 minute"))

rcs_flow_clean <- rcs_flow_orig %>% 
  mutate(DateTime = round_date(DateTime, unit = "15 minute"))

# Join the WQ and flow dataframes together, use full join to preserve all data
rcs_all_orig <- full_join(rcs_flow_clean, rcs_wq_clean)

# Finish cleaning lis_all_orig dataframe
rcs_all_clean <- rcs_all_orig %>% 
  mutate(StationCode = "RCS") %>% 
  arrange(DateTime)


# Export Data -------------------------------------------------------------

# Export formatted data as a .csv file 
rcs_all_clean %>% 
  write_excel_csv(
    path = paste0(sharepoint_path, "/Processed_Data/Continuous/RTM_OUTPUT_RCS_formatted.csv"),
    na = ""
  )

# For easier importing of this file in the future should either:
# 1) convert file to .xlsx file after exporting, or
# 2) manually format the 'DateTime' variable in the .csv file to "yyyy-mm-dd hh:mm:ss"

