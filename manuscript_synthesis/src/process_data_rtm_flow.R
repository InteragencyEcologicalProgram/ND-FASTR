# NDFS Synthesis Manuscript
# Purpose: Process and integrate the continuous flow data collected from the
  # Yolo Bypass and downstream from July 1 - Nov 15 during years 2013-2019.
  # Calculate daily averages which are used in figures and analysis for the NDFS
  # synthesis manuscript.
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


# Import Continuous Flow Data -----------------------------------------------

# Define root directory for the continuous WQ data on the NDFA SharePoint
fp_rtm_wq_root <- ndfa_abs_sp_path("2011-2019 Synthesis Study-FASTR/WQ_Subteam/Processed_Data/Continuous/All_Dates")

# Create vectors of all relevant file paths for the processed flow data
# Not tidally-filtered
fp_flow_std <- dir_ls(path = path(fp_rtm_wq_root), regexp = "RTM_OUTPUT_(RCS|RD22)_formatted_all\\.csv$")

# Tidally-filtered
fp_flow_tf <- dir_ls(path = path(fp_rtm_wq_root), regexp = "RTM_OUTPUT_(LIB|RVB)_formatted_all\\.csv$")

# Import processed continuous flow data for RCS and RD22 (not tidally-filtered)
df_flow_std <- 
  map(
    fp_flow_std,
    ~ read_csv(
      file = .x, 
      col_types = cols_only(
        StationCode = "c",
        DateTime = "c",
        Flow = "d"
      )
    )
  ) %>% 
  list_rbind()

# Import processed continuous flow data for LIB and RVB (Tidally-filtered)
df_flow_tf <- 
  map(
    fp_flow_tf,
    ~ read_csv(
      file = .x, 
      col_types = cols_only(
        StationCode = "c",
        DateTime = "c",
        FlowTF = "d"
      )
    )
  ) %>% 
  list_rbind()

# Import daily average tidally-filtered flow data for LIS
df_flow_tf_lis <- readRDS(here("Water_Quality/Data_Processed/LIS_SR_DailyAvgFlow_2013-2022.rds"))


# Clean and Integrate Continuous Data -------------------------------------

# Combine continuous flow data and prepare for averaging
df_flow_rtm_all <- 
  bind_rows(
    df_flow_std,
    df_flow_tf %>% rename(Flow = FlowTF)
  ) %>% 
  # parse date-time variable and define tz as PST
  mutate(DateTime = ymd_hms(DateTime, tz = "Etc/GMT+8"))

# Prepare daily average tidally-filtered flow data collected at LIS to be
  # combined to all other data
df_flow_tf_lis_c <- df_flow_tf_lis %>% 
  transmute(
    StationCode = "LIS",
    Date, 
    FlowAvg = LIS_DailyAvgNetFlow
  ) %>% 
  drop_na(FlowAvg)


# Calculate Daily Averages ------------------------------------------------

df_flow_dv <- df_flow_rtm_all %>% 
  # remove NA Flow values
  drop_na(Flow) %>% 
  mutate(Date = date(DateTime)) %>% 
  # calculate daily average flow values
  summarize(FlowAvg = mean(Flow), .by = c(StationCode, Date))
  
# Add daily average tidally-filtered flow data collected at LIS to all other
  # daily average flow data
df_flow_dv_all <- bind_rows(df_flow_dv, df_flow_tf_lis_c)

# filter flow data to July 1 - Nov 15 for years 2013-2019
df_flow_dv_all_f <- df_flow_dv_all %>% 
  filter(
    month(Date) %in% 7:11,
    !(month(Date) == 11 & day(Date) > 15),
    year(Date) %in% 2013:2019
  ) %>% 
  arrange(StationCode, Date)


# Save and Export Data ----------------------------------------------------

# Save daily average chlorophyll data as csv and rds files
df_flow_dv_all_f %>% write_csv(here("manuscript_synthesis/data/processed/flow_daily_avg_2013-2019.csv"))
df_flow_dv_all_f %>% saveRDS(here("manuscript_synthesis/data/processed/flow_daily_avg_2013-2019.rds"))

