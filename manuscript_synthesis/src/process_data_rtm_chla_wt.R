# NDFS Synthesis Manuscript
# Purpose: Process the 15-minute continuous chlorophyll and water temperature
  # data collected from the Yolo Bypass and downstream during years 2013-2019.
  # Calculate daily averages which are used in figures and analysis for the NDFS
  # synthesis manuscript.
# Author: Dave Bosworth
# Contacts: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
library(fs)
library(here)
library(conflicted)

# Source functions
source(here("manuscript_synthesis/src/global_functions.R"))

# Declare package conflict preferences 
conflicts_prefer(dplyr::filter())

# Check if we are in the correct working directory
i_am("manuscript_synthesis/src/process_data_rtm_chla_wt.R")


# Import Continuous Data --------------------------------------------------

# Define directory for the continuous WQ data on the NDFA SharePoint
fp_rtm_wq <- ndfa_abs_sp_path("2011-2019 Synthesis Study-FASTR/WQ_Subteam/Processed_Data/Continuous")

# Import QA'ed and cleaned continuous chlorophyll and water temperature data for
  # the NDFS period of interest
df_rtm_wq <- read_csv(
  file = path(fp_rtm_wq, "RTM_INPUT_all_2021-04-20.csv"),
  col_types = cols_only(
    StationCode = "c",
    DateTime = "c",
    WaterTemp = "d",
    Chla = "d"
  )
)


# Calculate Daily Averages ------------------------------------------------

df_wq_daily_avg <- df_rtm_wq %>% 
  # parse date-time variable and define tz as PST; add date variable
  mutate(
    DateTime = ymd_hms(DateTime, tz = "Etc/GMT+8"),
    Date = date(DateTime)
  ) %>% 
  # calculate daily average chlorophyll and water temperature values
  summarize(across(c(WaterTemp, Chla), ~ mean(.x, na.rm = TRUE)), .by = c(StationCode, Date)) %>% 
  # remove all NaN values in both chlorophyll and water temperature columns
  drop_na(WaterTemp, Chla) %>% 
  # filter to years 2013-2019 and only keep core stations with a long-term record
  filter(
    year(Date) %in% 2013:2019,
    StationCode %in% c("I80", "LIB", "LIS", "RCS", "RD22", "RVB", "STTD")
  ) %>% 
  arrange(StationCode, Date) 


# Save and Export Data ----------------------------------------------------

# Save daily average chlorophyll and water temperature data as csv and rds files
df_wq_daily_avg %>% write_csv(here("manuscript_synthesis/data/processed/chla_wt_daily_avg_2013-2019.csv"))
df_wq_daily_avg %>% saveRDS(here("manuscript_synthesis/data/processed/chla_wt_daily_avg_2013-2019.rds"))

