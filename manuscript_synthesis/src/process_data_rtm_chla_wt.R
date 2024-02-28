# NDFS Synthesis Manuscript
# Purpose: Process the 15-minute continuous chlorophyll, water temperature, and
  # specific conductance data collected from the Yolo Bypass and downstream during
  # years 2013-2019. Calculate daily and weekly averages which are used in figures
  # and analysis for the NDFS synthesis manuscript.
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

# Create vector of parameter names to be manipulated in later functions
wq_params <- c("WaterTemp", "Chla", "SpCnd")


# Import Continuous Data --------------------------------------------------

# Define directory for the continuous WQ data on the NDFA SharePoint
fp_rtm_wq <- ndfa_abs_sp_path("2011-2019 Synthesis Study-FASTR/WQ_Subteam/Processed_Data/Continuous")

# Import QA'ed and cleaned continuous WQ data for the NDFS period of interest
df_rtm_wq <- read_csv(
  file = path(fp_rtm_wq, "RTM_INPUT_all_2021-04-20.csv"),
  col_types = cols_only(
    StationCode = "c",
    DateTime = "c",
    WaterTemp = "d",
    SpCnd = "d",
    Chla = "d"
  )
)


# Prepare Continuous Data -------------------------------------------------

df_rtm_wq_c <- df_rtm_wq %>% 
  # parse date-time variable and define tz as PST; add date, year, and week variables
  mutate(
    DateTime = ymd_hms(DateTime, tz = "Etc/GMT+8"),
    Date = date(DateTime),
    Year = year(Date),
    Week = week(Date)
  ) %>% 
  # filter to years 2013-2019 and only keep core stations with a long-term record
  filter(
    Year %in% 2013:2019,
    StationCode %in% c("I80", "LIB", "LIS", "RCS", "RD22", "RVB", "RYI", "STTD")
  )


# Calculate Daily Averages ------------------------------------------------

df_wq_daily_avg <- df_rtm_wq_c %>% 
  # calculate daily average chlorophyll, water temperature, and specific
    # conductance values
  summarize(
    across(all_of(wq_params), ~ mean(.x, na.rm = TRUE)), 
    .by = c(StationCode, Date)
  ) %>% 
  # convert NaN values to NA values
  mutate(across(all_of(wq_params), ~ if_else(is.nan(.x), NA_real_, .x))) %>% 
  # remove rows where all WQ parameters are NA
  filter(!if_all(all_of(wq_params), is.na)) %>% 
  arrange(StationCode, Date)


# Calculate Weekly Averages -----------------------------------------------

df_wq_week_avg <- df_rtm_wq_c %>% 
  # calculate weekly average chlorophyll, water temperature, and specific
    # conductance values
  summarize(
    across(all_of(wq_params), ~ mean(.x, na.rm = TRUE)), 
    .by = c(StationCode, Year, Week)
  ) %>% 
  # convert NaN values to NA values
  mutate(across(all_of(wq_params), ~ if_else(is.nan(.x), NA_real_, .x))) %>% 
  # remove rows where all WQ parameters are NA
  filter(!if_all(all_of(wq_params), is.na)) %>% 
  arrange(StationCode, Year, Week)


# Save and Export Data ----------------------------------------------------

# Save daily and weekly average chlorophyll, water temperature, and specific
  # conductance data as csv and rds files
df_wq_daily_avg %>% write_csv(here("manuscript_synthesis/data/processed/chla_wt_daily_avg_2013-2019.csv"))
df_wq_daily_avg %>% saveRDS(here("manuscript_synthesis/data/processed/chla_wt_daily_avg_2013-2019.rds"))

df_wq_week_avg %>% write_csv(here("manuscript_synthesis/data/processed/chla_wt_week_avg_2013-2019.csv"))
df_wq_week_avg %>% saveRDS(here("manuscript_synthesis/data/processed/chla_wt_week_avg_2013-2019.rds"))

