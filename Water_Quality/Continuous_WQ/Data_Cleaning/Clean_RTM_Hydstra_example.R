# NDFA Water Quality
# Purpose: Example code to import, clean, and export continuous water quality data
# downloaded from Hydstra
# Author: Dave Bosworth

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
    "California Department of Water Resources/Office of Water Quality and Estuarine Ecology - North Delta Flow Action/WQ_Subteam/Raw_Data/Continuous"
  )
)

# Import data
lis_wq_orig <- read_csv(
  file = paste0(sharepoint_path, "/RTM_RAW_DWR LIS_2013-2019.csv"),
  col_names = FALSE,
  skip = 3,
  col_types = "cdd-------dd-dd-dd-dd-dd-"  # "c" = character, "d" = numeric, "-" = skip
) 

lis_flow_orig <- read_excel(
  path = paste0(sharepoint_path, "/RTM_RAW_DWR RCS LIS Flow_2011-2019.xlsx"),
  sheet = "LIS",
  col_names = c("DateTime", "Flow", "Flow_Qual"),
  skip = 3,
  col_types = c("date", "numeric", "numeric")
) 

glimpse(lis_wq_orig)
glimpse(lis_flow_orig)


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
  names(lis_wq_orig) <- c(
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
  lis_wq_clean <- lis_wq_orig %>% 
    mutate(DateTime = mdy_hm(DateTime))

  glimpse(lis_wq_clean)
  
  # Round DateTime variable in both dataframes to the nearest 15 minute interval so that they can 
    # be joined together
  lis_wq_clean <- lis_wq_clean %>% 
    mutate(DateTime = round_date(DateTime, unit = "15 minute"))
  
  lis_flow_clean <- lis_flow_orig %>% 
    mutate(DateTime = round_date(DateTime, unit = "15 minute"))
  
  # Join the WQ and flow dataframes together, use full join to preserve all data
  lis_all_orig <- full_join(lis_flow_clean, lis_wq_clean)
  
  # Finish cleaning lis_all_orig dataframe
  lis_all_clean <- lis_all_orig %>% 
    mutate(StationCode = "LIS") %>% 
    arrange(DateTime)

  
# Export Data -------------------------------------------------------------

# Export formatted data as a .csv file 
i80_clean %>% 
  write_excel_csv(
    path = paste0(sharepoint_path, "/Processed_Data/Continuous/RTM_OUTPUT_I80_formatted.csv"),
    na = ""
  )

# For easier importing of this file in the future should either:
# 1) convert file to .xlsx file after exporting, or
# 2) manually format the 'DateTime' variable in the .csv file to "yyyy-mm-dd hh:mm:ss"

