# NDFS Synthesis Manuscript
# Purpose: Process and integrate the 15-minute continuous chlorophyll data
  # collected from the Yolo Bypass and downstream from July 1 - Nov 15 during
  # years 2013-2019. Calculate daily averages which are used in figures and
  # analysis for the NDFS synthesis manuscript.
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
i_am("manuscript_synthesis/src/process_data_rtm_chla.R")


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

# Import continuous chlorophyll data from formatted csv files
import_rtm_chla <- function(fp) {
  read_csv(
    file = fp, 
    col_types = cols_only(
      StationCode = "c",
      DateTime = "c",
      Chla = "d"
    )
  )
}

# Add Flow Action Periods - Before, During, After flow action
ndfa_action_periods <- function(df) {
  # Import dates for flow action periods
  fp_act_dates <- ndfa_abs_sp_path("2011-2019 Synthesis Study-FASTR/Data Management/FlowDatesDesignations_45days.csv")
  df_act_dates <- read_csv(fp_act_dates, show_col_types = FALSE)
  
  # Convert date columns in df_act_dates_orig to date type
  df_act_dates_c <- df_act_dates %>% mutate(across(starts_with(c("Pre", "Post")), mdy))
  
  # Join flow action dates to df and add FlowActionPeriod variable
  df %>%  
    left_join(df_act_dates_c, by = join_by(Year)) %>% 
    mutate(
      FlowActionPeriod = case_when(
        Date >= PreFlowStart & Date <= PreFlowEnd ~ "Before",
        Date > PreFlowEnd & Date < PostFlowStart ~ "During",
        Date >= PostFlowStart & Date <= PostFlowEnd ~ "After"
      )
    ) %>% 
    # Remove some variables from df_act_dates_c
    select(-c(WYType, FlowPulseType, NetFlowDays, starts_with(c("PreFlow", "PostFlow"))))
}


# Import Continuous Chlorophyll Data -----------------------------------------------

# Define root directory for the continuous WQ data on the NDFA SharePoint
fp_rtm_wq_root <- ndfa_abs_sp_path("2011-2019 Synthesis Study-FASTR/WQ_Subteam/Processed_Data/Continuous/")

# Import QA'ed and cleaned continuous chlorophyll data collected year-round for
  # LIB, LIS, RVB, and STTD (2016-2019)
df_chla_qa_all_yr <- import_rtm_chla(path(fp_rtm_wq_root, "RTM_INPUT_yr_round_2021-09-02.csv"))

# Import QA'ed and cleaned continuous chlorophyll data for the NDFS period of interest
df_chla_qa_ndfs <- import_rtm_chla(path(fp_rtm_wq_root, "RTM_INPUT_all_2021-04-20.csv"))

# Create a vector of all relevant file paths for the processed continuous chlorophyll data
fp_rtm_wq_proc <- dir_ls(
  path = path(fp_rtm_wq_root, "All_Dates"),
  regexp = "RTM_OUTPUT_(I80|RCS|RD22|RYI|STTD)_formatted_all\\.csv$"
)

# Import processed continuous chlorophyll data into a dataframe
df_chla_proc <- 
  map(fp_rtm_wq_proc, import_rtm_chla) %>% 
  list_rbind()


# Clean and Integrate Continuous Data -------------------------------------

# Prepare processed data outside of the NDFS period of interest
df_chla_proc_c <- df_chla_proc %>% 
  # parse date-time variable and define tz as PST; add date and year variables
  mutate(
    DateTime = ymd_hms(DateTime, tz = "Etc/GMT+8"),
    Date = date(DateTime),
    Year = year(DateTime)
  ) %>% 
  # add flow action periods to the data frame
  ndfa_action_periods() %>%
  # only keep data outside of the NDFS period of interest
  filter(is.na(FlowActionPeriod)) %>%
  # remove STTD data collected from 2016-2019 since that's already been QA'ed 
  filter(!(StationCode == "STTD" & Year >= 2016)) %>% 
  # there are ~5,500 chlorophyll values equal to zero collected at the RYI
    # station in 2015-2016; we'll remove these values; otherwise, all other
    # chlorophyll data appears to be good based on visual inspection of the data
  filter(Chla > 0) %>% 
  # remove unnecessary variables
  select(-c(Date, Year, FlowActionPeriod))

# Prepare QA'ed and cleaned continuous chlorophyll data collected year-round to
  # be combined to the processed data
df_chla_qa_all_yr_c <- df_chla_qa_all_yr %>% 
  # parse date-time variable and define tz as PST
  mutate(DateTime = ymd_hms(DateTime, tz = "Etc/GMT+8"))

# Prepare QA'ed and cleaned continuous chlorophyll data for the NDFS period of
  # interest to be combined to the processed data
df_chla_qa_ndfs_c <- df_chla_qa_ndfs %>% 
  # parse date-time variable and define tz as PST
  mutate(DateTime = ymd_hms(DateTime, tz = "Etc/GMT+8")) %>% 
  # only keep core stations with a long-term record
  filter(StationCode %in% c("I80", "RCS", "RD22", "RYI", "STTD")) %>% 
  # Remove STTD data collected from 2016-2019 since that's already been QA'ed 
  filter(!(StationCode == "STTD" & year(DateTime) >= 2016))

# Combine the raw data and the QA'ed and cleaned data
df_chla_all <- bind_rows(df_chla_qa_all_yr_c, df_chla_qa_ndfs_c, df_chla_proc_c)


# Calculate Daily Averages ------------------------------------------------

df_chla_daily_avg <- df_chla_all %>% 
  # remove NA Chla values and filter data to July 1 - Nov 15 for each year
  drop_na(Chla) %>% 
  mutate(Date = date(DateTime)) %>% 
  filter(
    month(Date) %in% 7:11,
    !(month(Date) == 11 & day(Date) > 15)
  ) %>% 
  # calculate daily average chlorophyll values
  summarize(ChlaAvg = mean(Chla), .by = c(StationCode, Date)) %>% 
  arrange(StationCode, Date) %>% 
  # filter to years 2013-2019
  filter(year(Date) %in% 2013:2019)


# Save and Export Data ----------------------------------------------------

# Save daily average chlorophyll data as csv and rds files
df_chla_daily_avg %>% write_csv(here("manuscript_synthesis/data/processed/chla_daily_avg_2013-2019.csv"))
df_chla_daily_avg %>% saveRDS(here("manuscript_synthesis/data/processed/chla_daily_avg_2013-2019.rds"))

