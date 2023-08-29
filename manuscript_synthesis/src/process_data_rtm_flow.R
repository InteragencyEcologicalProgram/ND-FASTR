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

# Declare package conflict preferences 
conflicts_prefer(dplyr::filter())

# Check if we are in the correct working directory
i_am("manuscript_synthesis/src/process_data_rtm_flow.R")


# Functions ---------------------------------------------------------------

# Define absolute file path for NDFS SharePoint
# Function creates a filepath to the NDFA SharePoint site
# The optional fp_rel argument allows for more specific file paths beyond the
  # NDFA SharePoint root directory
ndfa_abs_sp_path <- function(fp_rel = NULL) {
  fp_fastr <- "California Department of Water Resources/North Delta Flow Action - Documents"
  
  if (is.null(fp_rel)) {
    fp_abs <- path(Sys.getenv('USERPROFILE'), fp_fastr)
  } else {
    fp_abs <- path(Sys.getenv('USERPROFILE'), fp_fastr, fp_rel)
  }
  
  return(fp_abs)
}

# Add Flow Action Periods - Before, During, After flow action
ndfa_action_periods <- function(df) {
  # Import dates for flow action periods
  fp_act_dates <- ndfa_abs_sp_path("2011-2019 Synthesis Study-FASTR/Data Management/FlowDatesDesignations_45days.csv")
  df_act_dates_orig <- read_csv(fp_act_dates)
  
  # Convert date columns in df_act_dates_orig to date type
  df_act_dates_clean <- df_act_dates_orig %>% mutate(across(starts_with(c("Pre", "Post")), mdy))
  
  # Add a Year variable to df if it doesn't already exist
  if (!"Year" %in% names(df)) {
    df <- df %>% mutate(Year = year(Date))
    message("Adding a Year variable to the dataframe.")
  }
  
  # Join flow action dates to df and add FlowActionPeriod variable
  df %>%  
    left_join(df_act_dates_clean, by  = "Year") %>% 
    mutate(
      FlowActionPeriod = case_when(
        Date >= PreFlowStart & Date <= PreFlowEnd ~ "Before",
        Date > PreFlowEnd & Date < PostFlowStart ~ "During",
        Date >= PostFlowStart & Date <= PostFlowEnd ~ "After"
      )
    ) %>% 
    # Remove some variables from df_act_dates_clean
    select(-c(WYType, FlowPulseType, NetFlowDays, starts_with(c("PreFlow", "PostFlow"))))
}


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

