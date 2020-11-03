# NDFA Synthesis
# Purpose: Global functions to be used across analyses of all NDFA data types
# Authors: Sarah Perry, Dave Bosworth
# Contacts: seperry83@gmail.com; David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
library(lubridate)
library(assertthat)


# Define absolute file path for NDFA SharePoint
  # Function creates a filepath to the NDFA SharePoint site
  # The optional fp_rel argument allows for more specific file paths beyond the NDFA SharePoint
    # root directory
ndfa_abs_sp_path <- function(fp_rel = NULL) {
  fp_fastr <- "California Department of Water Resources/North Delta Flow Action - Documents/"
  
  if (is.null(fp_rel)) {
    fp_abs <- normalizePath(file.path(Sys.getenv('USERPROFILE'), fp_fastr))
  } else {
    fp_abs <- normalizePath(file.path(Sys.getenv('USERPROFILE'), fp_fastr, fp_rel))
  }
  
  return(fp_abs)
}


# Add Flow Action Periods - Before, During, After flow action
  # Function requires that the dataframe (df) has a 'Date' variable that represents sample 
    # dates in the date data type (yyyy-mm-dd)
  # Function adds a 'Year' variable that represents the year during which each sample was collected
    # if it doesn't exist in the dataframe (df) already
  # The na_action_remove argument specifies whether the sampling dates outside of the before,
    # during, and after flow action period window are removed from the dataframe. If it is 
    # TRUE, then the sampling dates outside of the window are removed. If it is FALSE, all
    # sampling dates will be retained. Defaults to TRUE.
ndfa_action_periods <- function(df, na_action_remove = TRUE) {
  # Make sure Date variable exists in df
  assert_that(
    "Date" %in% names(df),
    msg = "ndfa_action_periods function\nDataframe requires a 'Date' variable that represents sample dates in the date data type (yyyy-mm-dd)."
  )

    # Make sure Date variable in df is date data type
  assert_that(
    is.date(df$Date),
    msg = "ndfa_action_periods function\n'Date' variable must be in the date data type (yyyy-mm-dd)."
  )
  
  # Import dates for flow action periods
  fp_act_dates <- ndfa_abs_sp_path("Data Management/FlowDatesDesignations_45days.csv")
  df_act_dates_orig <- read_csv(fp_act_dates)
  
  # Convert date columns in df_act_dates_orig to date type
  df_act_dates_clean <- df_act_dates_orig %>% 
    mutate(across(starts_with(c("Pre", "Post")), mdy))
  
  # Add a Year variable to df if it doesn't already exist
  if (!"Year" %in% names(df)) {
    df <- df %>% mutate(Year = year(Date))
    message("Adding a Year variable to the dataframe.")
  }
  
  # Join flow action dates to df and add FlowActionPeriod variable
  df_join <- 
    left_join(df, df_act_dates_clean, by  = "Year") %>% 
    mutate(
      FlowActionPeriod = case_when(
        Date >= PreFlowStart & Date <= PreFlowEnd ~ "Before",
        Date > PreFlowEnd & Date < PostFlowStart ~ "During",
        Date >= PostFlowStart & Date <= PostFlowEnd ~ "After"
      )
    ) %>% 
    # Remove variables from df_act_dates_clean
    select(-c(PreFlowStart:NetFlowDays))
  
  # Remove sampling dates outside of before, during, and after flow action period window 
    # if na_action_remove is TRUE
  if (na_action_remove == TRUE) {
    df_join <- df_join %>% filter(!is.na(FlowActionPeriod))
  }

  return(df_join)
}

