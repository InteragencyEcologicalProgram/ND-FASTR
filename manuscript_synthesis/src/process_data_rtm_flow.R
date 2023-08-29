# NDFS Synthesis Manuscript
# Purpose: Process and integrate the continuous flow data collected from the
  # Yolo Bypass and downstream during years 2013-2019. Calculate daily averages
  # which are used in figures and analysis for the NDFS synthesis manuscript.
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
i_am("manuscript_synthesis/src/process_data_rtm_flow.R")


# Import Continuous Flow Data -----------------------------------------------

# Define directory for the continuous flow data on the NDFA SharePoint
fp_rtm_flow <- ndfa_abs_sp_path("2011-2019 Synthesis Study-FASTR/WQ_Subteam/Processed_Data/Continuous")

# Import QA'ed and cleaned continuous flow data for the NDFS period of interest
df_rtm_flow <- read_csv(
  file = path(fp_rtm_flow, "RTM_INPUT_all_2021-04-20.csv"),
  col_types = cols_only(
    StationCode = "c",
    DateTime = "c",
    Flow = "d",
    FlowTF = "d"
  )
)

# Import daily average tidally-filtered flow data for LIS
df_flow_tf_lis <- readRDS(here("Water_Quality/Data_Processed/LIS_SR_DailyAvgFlow_2013-2022.rds"))


# Clean and Integrate Continuous Data -------------------------------------

# Prepare daily average tidally-filtered flow data collected at LIS to be
  # combined to all other data
df_flow_tf_lis_c <- df_flow_tf_lis %>% 
  transmute(
    StationCode = "LIS",
    Date, 
    Flow = LIS_DailyAvgNetFlow
  ) %>% 
  drop_na(Flow) %>% 
  # only include data within the NDFS period of interest
  ndfa_action_periods() %>% 
  drop_na(FlowActionPeriod) %>% 
  select(-c(Year, FlowActionPeriod))

# Prepare all other continuous flow data for averaging
df_rtm_flow_c <- df_rtm_flow %>% 
  mutate(
    # parse date-time variable and define tz as PST; add date variable
    DateTime = ymd_hms(DateTime, tz = "Etc/GMT+8"),
    Date = date(DateTime),
    # use standard flow data for RCS and RD22 and tidally-filtered flow data for
      # LIB and RVB
    FlowCombined = case_when(
      StationCode %in% c("RCS", "RD22") ~ Flow,
      StationCode %in% c("LIB", "RVB") ~ FlowTF,
      TRUE ~ NA_real_
    )
  ) %>% 
  # remove rows with NA values in FlowCombined, which only keeps RCS, RD22, LIB,
    # and RVB
  drop_na(FlowCombined)


# Calculate Daily Averages ------------------------------------------------

df_flow_dv <- df_rtm_flow_c %>% 
  # calculate daily average flow values
  summarize(Flow = mean(FlowCombined), .by = c(StationCode, Date)) %>% 
  # add daily average tidally-filtered flow data collected at LIS to all other
    # daily average flow data
  bind_rows(df_flow_tf_lis_c) %>% 
  # filter to years 2013-2019
  filter(year(Date) %in% 2013:2019) %>% 
  arrange(StationCode, Date)


# Save and Export Data ----------------------------------------------------

# Save daily average flow data as csv and rds files
df_flow_dv %>% write_csv(here("manuscript_synthesis/data/processed/flow_daily_avg_2013-2019.csv"))
df_flow_dv %>% saveRDS(here("manuscript_synthesis/data/processed/flow_daily_avg_2013-2019.rds"))

