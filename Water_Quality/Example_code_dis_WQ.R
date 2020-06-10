# Example code for working with discrete water quality data
# Author: Jenna Rinde

# Load packages- install these packages if you haven't already using install.packages()
library(tidyverse)
library(readxl)
library(lubridate)
library(hms)

# Import Data -------------------------------------------------------------

# Define path on SharePoint site for data - this works if you have the SharePoint site synced
  # to your computer
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/Office of Water Quality and Estuarine Ecology - North Delta Flow Action/WQ_Subteam/Raw_Data/Discrete"
  )
)

# 1. Importing nutrient data from SharePoint

nuts <- read_excel(
  path = paste0(sharepoint_path, "/DWQ_nuts_raw.xlsx"), sheet = "WQData (11)"
)

head(nuts)

str(nuts)

# Look at unique values in the Time variable
sort(unique(nuts$Time))
# There are two different types of formats in this variable which is why it imported 
  # as a character variable

# Are there any NA values in the Time variable?
anyNA(nuts$Time)
# No- that is good

# Look at unique values in the Result variable
sort(unique(nuts$Result))
# All values below the Reporting Limit are marked with "< R.L."

# Clean up Time and Result variables
nuts_clean <- nuts %>% 
  mutate(
    # Convert Time variable from character to hms/difftime - THIS STILL NEEDS WORK
    Time1 = case_when(
      str_detect(Time, "PM$") & str_sub(Time, end = 2) == 12 ~ parse_hms(Time),
      str_detect(Time, "PM$") ~ parse_hms(paste0(as.character(as.numeric(str_sub(Time, end = 1)) + 12), str_sub(Time, start = 2))),
      str_detect(Time, "AM$") ~ parse_hms(Time),
      TRUE ~ as_hms(round(as.numeric(Time) * 24 * 60 * 60, 0))
    ),
    # Create a new variable to identify values below the Reporting Limit
    Lab_Detect = if_else(
      str_detect(Result, "<"),
      "Non-detect",
      "Detect"
    ),
    # Convert Result variable from character to numeric
    Result = if_else(
      str_detect(Result, "<"),
      1,  # USED 1 AS A PLACEHOLDER, NEED TO ADD REPORTING LIMITS TO THIS DATAFRAME
      as.numeric(Result)
    )
  )

# Look for Lab Replicates
nuts_lab_reps <- nuts_clean %>%
  count(StationCode, Date, Analyte, Purpose) %>% 
  filter(n > 1)
# There are 1,061 Lab replicate pairs in this dataset
# NEED TO DECIDE WHAT TO DO WITH FIELD DUPLICATES AND LAB REPLICATES


#example of old code for different study for subsetting by station
CM40<- subset(CM, station== "CM40")
CM42<- subset(CM, station== "CM42")
CM43<- subset(CM, station== "CM43")
CM48<- subset(CM, station== "CM48")
RRI<- subset(CM, station== "CM42_RRI")
