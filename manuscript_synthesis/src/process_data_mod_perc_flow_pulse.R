# NDFS Synthesis Manuscript
# Purpose: Process and integrate the percent flow pulse data modelled by Anchor
  # QEA for the Yolo Bypass and downstream during years 2016-2019. Calculate daily
  # averages which are used in analyses for the NDFS synthesis manuscript.
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
i_am("manuscript_synthesis/src/process_data_mod_perc_flow_pulse.R")


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


# Import Percent Flow Pulse Data ------------------------------------------

# Define root directory for the percent flow pulse data on the NDFA SharePoint
fp_pfp_root <- ndfa_abs_sp_path("2011-2019 Synthesis Study-FASTR/Manuscript/Hydrodynamics")

# Create vector of all relevant file paths for the percent flow pulse data
fp_pfp <- 
  dir_ls(path = path(fp_pfp_root), regexp = "percent_flow_pulse_water_(201[6-9])_.+\\.csv$") %>% 
  str_subset("Prospect Slough\\.csv$", negate = TRUE)

names(fp_pfp) <- fp_pfp

# Import percent flow pulse data as a named list
ls_pfp <- 
  map(
    fp_pfp,
    ~ read_csv(
      file = .x, 
      col_select = c(Year, Month, Day, Flow_pulse_water_percent),
      na = "NaN"
    )
  )


# Clean and Integrate Data ------------------------------------------------

# Combine percent flow pulse data and prepare for averaging
df_pfp_all <- 
  bind_rows(ls_pfp, .id = "StationCode") %>% 
  # standardize StationCodes
  mutate(
    StationCode = case_when(
      str_detect(StationCode, "Toe Drain below Lisbon Weir\\.csv$") ~ "LIS",
      str_detect(StationCode, "Screw Trap at Toe Drain\\.csv$") ~ "STTD",
      str_detect(StationCode, "Base of Liberty Island\\.csv$") ~ "LIB",
      str_detect(StationCode, "Cache Slough at Ryer Island\\.csv$") ~ "RYI",
      str_detect(StationCode, "Sacramento River at Rio Vista Bridge\\.csv$") ~ "RVB"
    )
  ) %>% 
  # convert Year, Month, Day variables into a Date variable
  mutate(Date = ymd(paste(Year, Month, Day, sep = "-"))) %>% 
  # remove NA values in Flow_pulse_water_percent
  drop_na(Flow_pulse_water_percent) %>% 
  select(StationCode, Year, Date, Flow_pulse_water_percent)


# Calculate Daily Averages ------------------------------------------------

df_pfp_dv <- df_pfp_all %>% 
  # calculate daily average flow values and count of values per day
  summarize(
    PercFlowPulseAvg = mean(Flow_pulse_water_percent),
    NumValues = n(),
    .by = c(StationCode, Year, Date)
  ) %>% 
  # remove days that have greater than 2 hours of missing data
  filter(NumValues >= 96 - 8) %>%
  select(-NumValues) %>% 
  # fill in zeros for days with no data within each flow action period of each year
  group_by(StationCode, Year) %>% 
  complete(Date = seq.Date(min(Date), max(Date), by = "1 day"), fill = list(PercFlowPulseAvg = 0)) %>% 
  ungroup() %>% 
  select(-Year)


# Save and Export Data ----------------------------------------------------

# Save daily average chlorophyll data as csv and rds files
df_pfp_dv %>% write_csv(here("manuscript_synthesis/data/processed/perc_flow_pulse_daily_avg_2016-2019.csv"))
df_pfp_dv %>% saveRDS(here("manuscript_synthesis/data/processed/perc_flow_pulse_daily_avg_2016-2019.rds"))

