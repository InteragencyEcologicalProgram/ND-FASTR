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
    "California Department of Water Resources/Office of Water Quality and Estuarine Ecology - North Delta Flow Action/WQ_Subteam/Raw_Data"
  )
)

# 1. Importing nutrient data from SharePoint

# Trying to resolve differences between 3 spreadsheets on SharePoint site:
# DWQ_nuts_raw.xlsx - 13714 obs
nuts <- read_excel(
  path = paste0(sharepoint_path, "/Discrete/DWQ_nuts_raw.xlsx"), sheet = "WQData (11)"
)

# WQ_discrete_nutrients_RAW (1).xlsx - 12369 obs
nuts1 <- read_excel(paste0(sharepoint_path, "/Discrete/WQ_discrete_nutrients_RAW (1).xlsx"))

# WQ_discrete_nutrients_RAW.xlsx - 14361 obs
nuts2 <- read_excel(paste0(sharepoint_path, "/WQ_discrete_nutrients_RAW.xlsx"))

nuts_select <- nuts %>% select(StationCode, Date, Analyte)
nuts1_select <- nuts1 %>% select(StationCode, Date, Analyte)
nuts2_select <- nuts2 %>% select(StationCode, Date, Analyte)

nuts_nuts1_diff <- anti_join(nuts_select, nuts1_select)
nuts_nuts2_diff <- anti_join(nuts2_select, nuts_select)
# The WQ_discrete_nutrients_RAW.xlsx file contains some additional data for WWT and DWT that are
  # not within the DWQ_nuts_raw.xlsx file. This needs to be resolved.


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
    Time = case_when(
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

# Look at how many values are below the reporting limit for each Analyte
nuts_clean %>% count(Analyte, Lab_Detect) %>% filter(Lab_Detect == "Non-detect")


# NEED TO DECIDE WHAT TO DO WITH LAB REPLICATES -
  # For now we will pull them out, calculate RPD's for each pair, and keep the first value of the
  # pair in the cleaned data set

nuts_lab_reps <- nuts_clean %>%
  count(StationCode, Date, Analyte, Purpose) %>% 
  filter(n > 1)

unique(nuts_lab_reps$n)
# Lab replicates are only in duplicate

nuts_lab_reps <- nuts_lab_reps %>% select(-n)
nuts_lab_rep_pairs <- inner_join(nuts_clean, nuts_lab_reps)

nuts_lab_rep_pairs %>% select(-c(Result, Lab_Detect)) %>% distinct()
# back to 1,061 obs, so all other variables besides Result and Lab_Detect are in duplicate

# Add a variable to define obs as either Rep1 or Rep2
nuts_lab_reps_num <- nuts_lab_rep_pairs %>%
  select(StationCode, Date, Analyte, Purpose, Result, Lab_Detect) %>% 
  group_by(StationCode, Date, Analyte, Purpose) %>% 
  mutate(Rep = row_number()) %>% 
  ungroup()

# Widen the Result variable to arrange Replicate Results side-by-side in same row
nuts_lab_reps_r_wide <- nuts_lab_reps_num %>%   
  pivot_wider(
    id_cols = -Lab_Detect,
    names_from = Rep, 
    values_from = Result,
    names_prefix = "Result_"
  )

# Widen the Lab_Detect variable to arrange Replicates side-by-side in same row
nuts_lab_reps_ld_wide <- nuts_lab_reps_num %>%   
  pivot_wider(
    id_cols = -Result,
    names_from = Rep, 
    values_from = Lab_Detect,
    names_prefix = "Lab_Detect_"
  )

# Join all lab rep dataframes back together
nuts_lab_reps_wide <- nuts_lab_rep_pairs %>% 
  select(-c(Result, Lab_Detect)) %>% 
  distinct() %>% 
  left_join(nuts_lab_reps_r_wide) %>% 
  left_join(nuts_lab_reps_ld_wide)

# Clean up
rm(nuts_lab_rep_pairs, nuts_lab_reps_num, nuts_lab_reps_r_wide, nuts_lab_reps_ld_wide)

# Calculate RPD's for each lab replicate pair
# For now using placeholder of "1" for the values below the reporting limit
nuts_lab_reps_wide_rpd <- nuts_lab_reps_wide %>% 
  mutate(RPD = round(abs(Result_1 - Result_2)/((Result_1 + Result_2)/2), 3))
  
# Look for replicate pairs that have RPD's greater than 25%
nuts_lab_reps_wide_rpd %>% filter(RPD > 0.25)



#example of old code for different study for subsetting by station
CM40<- subset(CM, station== "CM40")
CM42<- subset(CM, station== "CM42")
CM43<- subset(CM, station== "CM43")
CM48<- subset(CM, station== "CM48")
RRI<- subset(CM, station== "CM42_RRI")
