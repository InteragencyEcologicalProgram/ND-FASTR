# NDFS Synthesis Manuscript
# Purpose: Global functions to be used across all scripts for the NDFS Synthesis
  # manuscript
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
library(fs)

# Define absolute file path for NDFA SharePoint
  # Function creates a filepath to the NDFA SharePoint site
  # The optional fp_rel argument allows for more specific file paths beyond the NDFA SharePoint
    # root directory
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

