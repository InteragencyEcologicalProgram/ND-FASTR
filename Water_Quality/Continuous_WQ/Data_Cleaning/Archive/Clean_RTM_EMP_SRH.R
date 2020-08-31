# NDFA Water Quality
# Purpose: Code to import, clean, and export SRH continuous water quality data
# collected by EMP-Field
# Author: Amanda Maguire

# Load packages
library(tidyverse)
library(lubridate)

# Set System Timezone as "Etc/GMT+8" to make it consistent with all df's 
Sys.setenv(TZ = "Etc/GMT+8")
Sys.time()
Sys.timezone()


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
# 2011-2014 water temperature data.
srh_orig1_wt <- read_csv(
  file = paste0(sharepoint_path, "/Raw_Data/Continuous/RTM_RAW_DWR SRH WT_2011-2014.csv"),
  skip = 4
) 

# 2011-2014 pH data
srh_orig1_ph <- read_csv(
  file = paste0(sharepoint_path, "/Raw_Data/Continuous/RTM_RAW_DWR SRH pH_2011-2014.csv"),
  skip = 4
) 

# 2011-2014 data
srh_orig1 <- read_csv(
  file = paste0(sharepoint_path, "/Raw_Data/Continuous/RTM_RAW_DWR SRH_2011-2014.csv"),
  skip = 4
) 

# 2015-2017 data
srh_orig2 <- read_csv(
  file = paste0(sharepoint_path, "/Raw_Data/Continuous/RTM_RAW_DWR SRH_2015-2017.csv"),
  skip = 4
) 

# 2018-2019 data
srh_orig3 <- read_csv(
  file = paste0(sharepoint_path, "/Raw_Data/Continuous/RTM_RAW_DWR SRH_2018-2019.csv"),
  skip = 4
) 

# Check if srh_orig1_wt and srh_orig1_ph and srh_orig1 and srh_orig2 and srh_orig3 have same variable names
names(srh_orig1_wt)
names(srh_orig1_ph)
names(srh_orig1)
names(srh_orig2)
names(srh_orig3)
# Yes they do, so they can be bound together without trouble

# Bind all raw SRH data together
srh_all_raw <- bind_rows(srh_orig1, srh_orig1_ph, srh_orig1_wt, srh_orig2, srh_orig3)

glimpse(srh_all_raw)


# Clean Data --------------------------------------------------------------

# EMP-Field PARAMETER CODES:
# [Water]  Dissolved Oxygen (mg/L) -(Dissolved) YSI Sonde { Data }  @ 15 min Inst 1 m deep
# [Water]  pH (pH Units) -(n/a) YSI Sonde { Data }  @ 15 min Inst 1 m deep
# [Water]  Fluorescence (FU) -(n/a) YSI Sonde { Data }  @ 15 min Inst 1 m deep
# [Water]  SpC (µS/cm) -(n/a) YSI Sonde { Data }  @ 15 min Inst 1 m deep
# [Water]  Temperature (°C) -(n/a) YSI Sonde { Data }  @ 15 min Inst 1 m deep
# [Water]  Turbidity (NTU) -(n/a) YSI Sonde { Data }  @ 15 min Inst 1 m deep

# Clean data
srh_all_clean <- srh_all_raw %>% 
  # Remove values with QAQC Flag of X
  filter(`QAQC Flag` != "X") %>% 
  mutate(
    # Parse date time variable and round to nearest 15 minute interval
    DateTime = round_date(mdy_hm(DATE), unit = "15 minute"),
    # Define Timezone of date_time as PST (Etc/GMT+8) without daylight savings adjustment
    DateTime = force_tz(DateTime, tzone = "Etc/GMT+8"),
    # Convert Station name to NDFA standardized name
    StationCode = "SRH",
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
  select(DateTime, StationCode, READING_TYPE, VALUE, `QAQC Flag`) %>% 
  # Remove duplicate values
  distinct() %>% 
  # Remove some duplicates with different values
    filter(
      !(DateTime == "2011-03-15 11:30:00" & READING_TYPE == "Chla" & VALUE == 3.80),
      !(DateTime == "2011-03-15 11:30:00" & READING_TYPE == "DO" & VALUE == 10.21),
      !(DateTime == "2011-03-15 11:30:00" & READING_TYPE == "WaterTemp" & VALUE == 11.62)
    )
  
# THE CHLOROPHYLL DATA MAY BE IN RFU UNITS, NEED TO CHECK ON THIS AND 
# FIGURE OUT HOW TO CONVERT TO ug/L

# Pivot dataframe to wide format
# Values
srh_all_values <- srh_all_clean %>% 
  pivot_wider(
    id_cols = -'QAQC Flag',
    names_from = READING_TYPE,
    values_from = VALUE
  )

# Qual Codes
srh_all_qual <- srh_all_clean %>% 
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
srh_all_clean_f <- 
  left_join(srh_all_values, srh_all_qual) %>% 
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

glimpse(srh_all_clean_f)


# Export Data -------------------------------------------------------------

# Export formatted data as a .csv file 
srh_all_clean_f %>% 
  write_excel_csv(
    path = paste0(sharepoint_path, "/Processed_Data/Continuous/RTM_OUTPUT_SRH_formatted.csv"),
    na = ""
  )

# For easier importing of this file in the future should either:
# 1) convert file to .xlsx file after exporting, or
# 2) manually format the 'DateTime' variable in the .csv file to "yyyy-mm-dd hh:mm:ss"

