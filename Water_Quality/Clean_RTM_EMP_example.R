# NDFA Water Quality
# Purpose: Example code to import, clean, and export continuous water quality data
# collected by EMP-Field
# Author: Dave Bosworth

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
# 2011-2014 data
rvb_orig1 <- read_csv(
  file = paste0(sharepoint_path, "/Raw_Data/Continuous/RTM_RAW_DWR RVB_2011-2014.csv"),
  skip = 4
) 

# 2015-2017 data
rvb_orig2 <- read_csv(
  file = paste0(sharepoint_path, "/Raw_Data/Continuous/RTM_RAW_DWR RVB_2015-2017.csv"),
  skip = 4
) 

# 2018-2019 data
rvb_orig3 <- read_csv(
  file = paste0(sharepoint_path, "/Raw_Data/Continuous/RTM_RAW_DWR RVB_2018-2019.csv"),
  skip = 4
) 

# Check if rvb_orig1 and rvb_orig2 have same variable names
names(rvb_orig1)
names(rvb_orig2)
names(rvb_orig3)
# Yes they do, so they can be binded together without trouble

# Bind rvb_orig1 and rvb_orig2
rvb_orig <- bind_rows(rvb_orig1, rvb_orig2, rvb_orig3)

glimpse(rvb_orig)


# Clean Data --------------------------------------------------------------

# EMP-Field PARAMETER CODES:
  # [Water]  Dissolved Oxygen (mg/L) -(Dissolved) YSI Sonde { Data }  @ 15 min Inst 1 m deep
  # [Water]  pH (pH Units) -(n/a) YSI Sonde { Data }  @ 15 min Inst 1 m deep
  # [Water]  Fluorescence (FU) -(n/a) YSI Sonde { Data }  @ 15 min Inst 1 m deep
  # [Water]  SpC (µS/cm) -(n/a) YSI Sonde { Data }  @ 15 min Inst 1 m deep
  # [Water]  Temperature (°C) -(n/a) YSI Sonde { Data }  @ 15 min Inst 1 m deep
  # [Water]  Turbidity (NTU) -(n/a) YSI Sonde { Data }  @ 15 min Inst 1 m deep

# Clean data
rvb_clean <- rvb_orig %>% 
  mutate(
    # Parse date time variable
    DateTime = mdy_hm(DATE),
    # Convert Station name to NDFA standardized name
    StationCode = "RVB",
    # Convert Parameter names to NDFA standardized names
    READING_TYPE = case_when(
      str_detect(READING_TYPE, ".{9}Diss") ~ "DO",
      str_detect(READING_TYPE, ".{9}Fluor") ~ "Chla",
      str_detect(READING_TYPE, ".{9}pH") ~ "pH",
      str_detect(READING_TYPE, ".{9}SpC") ~ "SpCnd",
      str_detect(READING_TYPE, ".{9}Temp") ~ "WaterTemp",
      str_detect(READING_TYPE, ".{9}Turb") ~ "Turbidity"
    )
  ) %>%
  select(DateTime, StationCode, READING_TYPE, VALUE, `QAQC Flag`)
  
  # THE CHLOROPHYLL DATA MAY BE IN RFU UNITS, NEED TO CHECK ON THIS AND 
    # FIGURE OUT HOW TO CONVERT TO ug/L

# Pivot dataframe to wide format
  # Values
  rvb_values <- rvb_clean %>% 
    pivot_wider(
      id_cols = -`QAQC Flag`,
      names_from = READING_TYPE,
      values_from = VALUE
    )
  
  # Qual Codes
  rvb_qual <- rvb_clean %>% 
    pivot_wider(
      id_cols = -VALUE,
      names_from = READING_TYPE,
      values_from = `QAQC Flag`
    ) %>% 
    # rename parameter variables
    rename(
      Chla_Qual = Chla,
      SpCnd_Qual = SpCnd,
      WaterTemp_Qual = WaterTemp,
      Turbidity_Qual = Turbidity,
      pH_Qual = pH,
      DO_Qual = DO
    )

  # Join two wide dataframes together
  rvb_clean1 <- 
    left_join(rvb_values, rvb_qual) %>% 
    # Reorder variables
    select(
      DateTime,
      StationCode,
      starts_with("Ch"),
      starts_with("Sp"),
      starts_with("Wa"),
      starts_with("Tu"),
      starts_with("pH"),
      starts_with("DO")
    )
  
  glimpse(rvb_clean1)
  

# Export Data -------------------------------------------------------------

# Export formatted data as a .csv file 
rvb_clean1 %>% 
  write_excel_csv(
    path = paste0(sharepoint_path, "/Processed_Data/Continuous/RTM_OUTPUT_RVB_formatted.csv"),
    na = ""
  )

# For easier importing of this file in the future should either:
# 1) convert file to .xlsx file after exporting, or
# 2) manually format the 'DateTime' variable in the .csv file to "yyyy-mm-dd hh:mm:ss"

