# NDFA Water Quality
# Purpose: Example code to import, clean, and export continuous water quality data
# downloaded from Hydstra
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
    "California Department of Water Resources/Office of Water Quality and Estuarine Ecology - North Delta Flow Action/WQ_Subteam/Raw_Data/Continuous"
  )
)

# Import data
i80_orig <- read_csv(
  file = paste0(sharepoint_path, "/RTM_RAW_DWR I80_2013-2019.csv"),
  col_names = FALSE,
  skip = 3,
  col_types = "cdd-------dd-dd-dd-dd-dd-"
) 

glimpse(i80_orig)

# Clean data
  # Change variable names
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

