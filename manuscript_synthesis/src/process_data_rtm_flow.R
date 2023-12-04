# NDFS Synthesis Manuscript
# Purpose: Process and integrate the continuous flow data collected from the
  # Yolo Bypass and downstream during years 2013-2019. Calculate daily and weekly
  # averages which are used in figures and analysis for the NDFS synthesis
  # manuscript.
# Author: Dave Bosworth
# Contacts: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
library(fs)
library(readxl)
library(here)
library(conflicted)

# Source functions
source(here("manuscript_synthesis/src/global_functions.R"))

# Declare package conflict preferences 
conflicts_prefer(dplyr::filter())

# Check if we are in the correct working directory
i_am("manuscript_synthesis/src/process_data_rtm_flow.R")


# Import Continuous Flow Data -----------------------------------------------

# Define directory for the continuous flow data on the NDFA SharePoint
fp_rtm_flow_sp <- ndfa_abs_sp_path("2011-2019 Synthesis Study-FASTR/WQ_Subteam/Processed_Data/Continuous")

# Import QA'ed and cleaned continuous flow data for the NDFS period of interest
df_rtm_flow <- read_csv(
  file = path(fp_rtm_flow_sp, "RTM_INPUT_all_2021-04-20.csv"),
  col_types = cols_only(
    StationCode = "c",
    DateTime = "c",
    Flow = "d",
    FlowTF = "d"
  )
)

# Import instantaneous and tidally-filtered flow data from LIS collected from
  # 2013-2022
df_rtm_flow_lis <- read_excel(
  here("Water_Quality/Continuous_WQ/data-raw/LIS_FlowData_2013_2022.xlsx"),
  sheet = "15 Min Net Flow",
  skip = 10
)


# Clean and Integrate Continuous Data -------------------------------------

# Prepare tidally-filtered flow data collected at LIS to be combined to all
  # other data
df_rtm_flow_lis_c <- df_rtm_flow_lis %>% 
  transmute(
    StationCode = "LIS",
    DateTime = round_date(force_tz(`Date & Time (PST)`, tzone = "Etc/GMT+8"), unit = "15 minute"),
    Date = date(DateTime),
    Flow = `Net Flow (cfs)`
  ) %>% 
  drop_na(Flow) %>% 
  # only include data within the NDFS period of interest
  filter(year(Date) %in% 2013:2019) %>% 
  ndfa_action_periods() %>% 
  drop_na(FlowActionPeriod) %>% 
  select(-c(Year, FlowActionPeriod))

# Look for duplicated time stamps in the LIS flow data
df_rtm_flow_lis_c %>%
  count(DateTime) %>%
  filter(n > 1)
# No duplicated time stamps present in data set

# Prepare all other continuous flow data for averaging
df_rtm_flow_c <- df_rtm_flow %>% 
  # only keep RCS, RD22, LIB, and RVB stations
  filter(StationCode %in% c("RCS", "RD22", "LIB", "RVB")) %>% 
  mutate(
    # parse date-time variable and define tz as PST; add date variables
    DateTime = ymd_hms(DateTime, tz = "Etc/GMT+8"),
    Date = date(DateTime),
    # use standard flow data for RCS and RD22 and tidally-filtered flow data for
      # LIB and RVB
    Flow = case_when(
      StationCode %in% c("RCS", "RD22") ~ Flow,
      StationCode %in% c("LIB", "RVB") ~ FlowTF
    )
  ) %>% 
  # remove rows with NA values in Flow
  drop_na(Flow) %>% 
  # remove FlowTF variable because its no longer needed
  select(-FlowTF) %>% 
  # filter to years 2013-2019
  filter(year(Date) %in% 2013:2019)

# Combine LIS flow data to all other flow data
df_rtm_flow_all <- 
  bind_rows(df_rtm_flow_lis_c, df_rtm_flow_c) %>% 
  # Add Year and Week variables
  mutate(
    Year = year(Date),
    Week = week(Date)
  )


# Calculate Daily Averages ------------------------------------------------

# Inspect number of measurements per day for each station
df_rtm_flow_all %>% 
  count(StationCode, Date, name = "NumMeas") %>% 
  count(StationCode, NumMeas, name = "NumDays") %>% 
  arrange(StationCode, desc(NumMeas)) %>% 
  print(n = 90)

# Every station has at least a few days with large numbers of missing values.
  # We'll only calculate daily averages for days that have 10% or fewer missing
  # timestamps, which is 86-96 timestamps for LIS, RCS, and RD22 (collected at
  # 15-min intervals) and 24-22 timestamps for LIB and RVB (collected at 1-hour
  # intervals).

df_flow_daily_avg <- df_rtm_flow_all %>% 
  # calculate daily average flow values
  summarize(
    Flow = mean(Flow),
    NumMeas = n(),
    .by = c(StationCode, Date)
  ) %>% 
  # Remove days with >10% missing timestamps
  filter(
    StationCode %in% c("LIS", "RCS", "RD22") & NumMeas >= 86 | 
      StationCode %in% c("LIB", "RVB") & NumMeas >= 22
  ) %>% 
  select(-NumMeas) %>% 
  arrange(StationCode, Date)


# Calculate Weekly Averages -----------------------------------------------

df_flow_week_avg <- df_rtm_flow_all %>% 
  # calculate weekly average flow values
  summarize(Flow = mean(Flow), .by = c(StationCode, Year, Week)) %>% 
  arrange(StationCode, Year, Week)


# Save and Export Data ----------------------------------------------------

# Save daily and weekly average flow data as csv and rds files
df_flow_daily_avg %>% write_csv(here("manuscript_synthesis/data/processed/flow_daily_avg_2013-2019.csv"))
df_flow_daily_avg %>% saveRDS(here("manuscript_synthesis/data/processed/flow_daily_avg_2013-2019.rds"))

df_flow_week_avg %>% write_csv(here("manuscript_synthesis/data/processed/flow_week_avg_2013-2019.csv"))
df_flow_week_avg %>% saveRDS(here("manuscript_synthesis/data/processed/flow_week_avg_2013-2019.rds"))

