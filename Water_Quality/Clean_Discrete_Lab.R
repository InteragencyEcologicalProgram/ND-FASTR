# Bryte Lab Data 
# Author: Jenna Rinde

# Load packages
library(tidyverse)
library(readxl)
library(lubridate)


# Import Data -------------------------------------------------------------

# Define path on SharePoint site for data - this works if you have the SharePoint site synced
  # to your computer
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/Office of Water Quality and Estuarine Ecology - North Delta Flow Action/WQ_Subteam/Raw_Data/Discrete/Raw_WDL_export_June2020"
  )
)

# Create a character vectors of all discrete lab data files in sharepoint_path
dis_lab_files <- dir(sharepoint_path, pattern = "\\.xlsx$", full.names = T)

# Define column types for data to be imported with read_excel
dis_lab_col_types <- c(
  rep("numeric", 2),
  rep("text", 3),
  "date",
  rep("text", 2),
  "numeric",
  rep("text", 8)
)

# Import and combine all of the discrete lab data
dis_lab_data_orig <- map_dfr(dis_lab_files, ~read_excel(.x, col_types = dis_lab_col_types))

# Clean up variable names in dis_lab_data_orig
names(dis_lab_data_orig) <- str_replace_all(names(dis_lab_data_orig), "[:space:]", "")

# What are all of the Stations in the dataset?
sort(unique(dis_lab_data_orig$StationName))
# No field duplicates or blanks- do we care about this?

# What are all of the Stations in the dataset?
sort(unique(dis_lab_data_orig$Analyte))
# Looks good? We have some parameters to filter out

