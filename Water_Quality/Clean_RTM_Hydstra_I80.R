# NDFA Water Quality
# Purpose: Code to import, clean, and export I80 continuous water quality data
# downloaded from Hydstra
# Author: Dave Bosworth & Amanda Maguire

# Load packages
library(tidyverse)
library(lubridate)

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
i80_orig <- read_csv(
  file = paste0(sharepoint_path, "/Raw_Data/Continuous/RTM_RAW_DWR I80_2013-2019.csv"),
  col_names = FALSE,
  skip = 3,
  col_types = "cdd-------dd-dd-dd-dd-dd-"  # "c" = character, "d" = numeric, "-" = skip
) 

glimpse(i80_orig)


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
# Change variable names - using NDFA standardized names
names(i80_orig) <- c(
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

# Parse date time variable, and create StationCode variable
i80_clean <- i80_orig %>% 
  mutate(
    DateTime = mdy_hm(DateTime),
    StationCode = "I80"
  )

glimpse(i80_clean)


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

