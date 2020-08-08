# NDFA Water Quality
# Purpose: Script to clean LIBCUT data
# collected by USGS
# Author: Dave Bosworth & Traci Treleaven

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
#  LIB station 
#- is skip, cc bring in column header as character, str_c is combing into a sting (rep is repeating) dc is numeric/character combo which is 38 columns divided by 2, final part gets rid of tz time zone column 

libcut_orig <- read_csv(
  file = paste0(sharepoint_path, "/Raw_Data/Continuous/RTM_RAW_USGS_LIBCUT.csv"),
  col_types = paste0("-cc", str_c(rep("dc", 11), collapse = ""), "-")
) 

glimpse(libcut_orig)


# Clean Data --------------------------------------------------------------

# USGS PARAMETER CODES:
# 00060 - Discharge (cfs)
# 72137 - Discharge, tidally-filtered (cfs)
# 00010 - Water Temperature (Celcius)
# 00300 - Dissolved Oxygen (mg/L)
# 00095 - Specific Conductance at 25 C (uS/cm)
# 00400 - pH
# 63680 - Turbidity (FNU)
# 32315 - Chlorophyll relative fluorescence (RFU)
# 32316 - Chlorophyll concentration estimated from reference material (ug/L)
# 32295 - Dissolved organic matter fluorescence (fDOM) (ug/L as QSE)
# 32321 - Phycocyanin relative fluorescence (RFU)
# 99133 - Nitrate plus nitrite (mg/L as N)

# Clean data
libcut_clean <- libcut_orig %>% 
  mutate(
    # Parse date time variable
    DateTime = ymd_hms(dateTime),
    # Convert Station name to NDFA standardized name
    StationCode = "LIBCUT"
  ) %>% 
  select(-dateTime)

glimpse(libcut_clean)

# Standardize parameter variable names
# Data values
libcut_values <- libcut_clean %>% 
  select(!ends_with("_cd")) %>% 
  pivot_longer(
    cols = -c(DateTime, StationCode, site_no),
    names_to = "parameter",
    values_to = "value"
  ) %>% 
  filter(!is.na(value)) %>% 
  mutate(
    parameter = case_when(
      str_detect(parameter, "00060") ~ "Flow",
      str_detect(parameter, "72137") ~ "FlowTF",
      str_detect(parameter, "00010") ~ "WaterTemp",
      str_detect(parameter, "00300") ~ "DO",
      str_detect(parameter, "00095") ~ "SpCnd",
      str_detect(parameter, "00400") ~ "pH",
      str_detect(parameter, "63680") ~ "Turbidity",
      str_detect(parameter, "32315") ~ "Chla_RFU",
      str_detect(parameter, "32316") ~ "Chla",
      str_detect(parameter, "32295") ~ "fDOM",
      str_detect(parameter, "32321") ~ "Phyco_RFU",
      str_detect(parameter, "99133") ~ "NitrateNitrite"
    )
  ) %>% 
  pivot_wider(names_from = parameter, values_from = value)

# Qual Codes
libcut_qual <- libcut_clean %>% 
  select(site_no, DateTime, ends_with("_cd")) %>% 
  pivot_longer(
    cols = -c(site_no, DateTime),
    names_to = "parameter",
    values_to = "value"
  ) %>% 
  filter(!is.na(value)) %>% 
  mutate(
    parameter = case_when(
      str_detect(parameter, "00060") ~ "Flow_Qual",
      str_detect(parameter, "72137") ~ "FlowTF_Qual",
      str_detect(parameter, "00010") ~ "WaterTemp_Qual",
      str_detect(parameter, "00300") ~ "DO_Qual",
      str_detect(parameter, "00095") ~ "SpCnd_Qual",
      str_detect(parameter, "00400") ~ "pH_Qual",
      str_detect(parameter, "63680") ~ "Turbidity_Qual",
      str_detect(parameter, "32315") ~ "Chla_RFU_Qual",
      str_detect(parameter, "32316") ~ "Chla_Qual",
      str_detect(parameter, "32295") ~ "fDOM_Qual",
      str_detect(parameter, "32321") ~ "Phyco_RFU_Qual",
      str_detect(parameter, "99133") ~ "NitrateNitrite_Qual"
    )
  ) %>% 
  pivot_wider(names_from = parameter, values_from = value)


glimpse(libcut_qual)


# Join two wide dataframes together
libcut_clean <- 
  left_join(libcut_values, libcut_qual) %>% 
  # Reorder variables
  select(
    site_no,
    DateTime,
    StationCode,
    Flow,
    Flow_Qual,
    FlowTF,
    FlowTF_Qual,
    starts_with("Wa"),
    starts_with("Tu"),
    starts_with("Sp"),
    starts_with("DO"),
    starts_with("pH", ignore.case = FALSE),
    Chla,
    Chla_Qual,
    Chla_RFU,
    Chla_RFU_Qual,
    starts_with("fD"),
    starts_with("Phy"),
    starts_with("Ni")
  )


glimpse(libcut_clean)


# Export Data -------------------------------------------------------------


# Export formatted data as a .csv file 


libcut_clean %>% 
  write_excel_csv(
    path = paste0(sharepoint_path, "/Processed_Data/Continuous/RTM_OUTPUT_LIBCUT_formatted.csv"),
    na = ""
  )


# For easier importing of this file in the future should either:
# 1) convert file to .xlsx file after exporting, or
# 2) manually format the 'DateTime' variable in the .csv file to "yyyy-mm-dd hh:mm:ss"