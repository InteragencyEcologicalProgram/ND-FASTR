# NDFA Water Quality
# Purpose: Create summaries of sample counts for each station, year, and parameter for both
  # the discrete and continuous water quality data
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# 1. Global Code and Functions -----------------------------------------------

# Load packages
library(tidyverse)
library(lubridate)

# Source functions
source("global_ndfa_funcs.R")
source("Water_Quality/global_wq_funcs.R")

# Create a vector for the factor order of StationCode
sta_order <- c(
  "RMB",
  "RCS",
  "RD22",
  "WWT",
  "I80",
  "DWT",
  "LIS",
  "TOE",
  "STTD",
  "LIBCUT",
  "SGG",
  "BL5",
  "LIB",
  "RYI",
  "RVB",
  "SDI",
  "SHR",
  "SRH",
  "SRV"
)


# 2. Continuous Data ------------------------------------------------------

# Define relative file path for file containing all QA'ed and cleaned continuous WQ data
# Most current data set from 4/20/2021
fp_rel_rtm_data <- "WQ_Subteam/Processed_Data/Continuous/RTM_INPUT_all_2021-04-20.csv"

# Define absolute file path
fp_abs_rtm_data <- ndfa_abs_sp_path(fp_rel_rtm_data)

# Import continuous WQ data
df_rtm_orig <- import_rtm_data(fp_abs_rtm_data, 10)

# Clean continuous WQ data for summarizing
df_rtm_clean <- df_rtm_orig %>% 
  mutate(
    DateTime = ymd_hms(DateTime),
    Year = year(DateTime)
  ) %>%
  # Don't include Flow, FlowTF, and all Qual variables
  select(-c(ends_with("_Qual"), starts_with("Flow"))) %>% 
  # Pivot parameters in the long format
  pivot_longer(cols = where(is.numeric) & !Year, names_to = "Parameter", values_to = "Value") %>% 
  # Remove all NA values
  filter(!is.na(Value))

# Summarize sample counts for each station, year, and parameter
df_rtm_samp_counts <- df_rtm_clean %>% 
  count(Year, Parameter, StationCode) %>% 
  mutate(StationCode = fct_drop(factor(StationCode, levels = sta_order)))
  
# Pivot summary by StationCode and export as a csv file
df_rtm_samp_counts %>% 
  arrange(StationCode) %>% 
  pivot_wider(names_from = StationCode, values_from = n) %>% 
  complete(Year, Parameter) %>% 
  arrange(Year, Parameter) %>% 
  write_excel_csv("rtm_sample_counts_by_station.csv", na = "")

# Pivot summary by Parameter and export as a csv file
df_rtm_samp_counts %>% 
  arrange(Parameter) %>% 
  pivot_wider(names_from = Parameter, values_from = n) %>% 
  complete(Year, StationCode) %>% 
  arrange(Year, StationCode) %>% 
  write_excel_csv("rtm_sample_counts_by_parameter.csv", na = "")


# 3. Discrete Data --------------------------------------------------------

# Define relative file path for file containing discrete WQ data
fp_rel_dwq_data <- "WQ_Subteam/Processed_Data/Discrete/WQ_INPUT_Discrete_Lab_2021-01-25.csv"

# Define absolute file path
fp_abs_dwq_data <- ndfa_abs_sp_path(fp_rel_dwq_data)

# Import Discrete WQ data
df_dwq_orig <- read_csv(fp_abs_dwq_data, col_types = "ccccccnncc")

# Clean discrete WQ data for summarizing
df_dwq_clean <- df_dwq_orig %>% 
  mutate(
    DateTime = ymd_hms(DateTime),
    Date = date(DateTime),
    Year = year(DateTime)
  ) %>% 
  # Restrict data to only flow action periods and 45 days before and after
  ndfa_action_periods()

# Summarize sample counts for each station, year, and analyte
df_dwq_samp_counts <- df_dwq_clean %>% 
  count(Year, Analyte, StationCode) %>% 
  mutate(StationCode = fct_drop(factor(StationCode, levels = sta_order)))

# Pivot summary by StationCode and export as a csv file
df_dwq_samp_counts %>% 
  arrange(StationCode) %>% 
  pivot_wider(names_from = StationCode, values_from = n) %>% 
  complete(Year, Analyte) %>% 
  arrange(Year, Analyte) %>% 
  write_excel_csv("dwq_sample_counts_by_station.csv", na = "")

# Pivot summary by Analyte and export as a csv file
df_dwq_samp_counts %>% 
  arrange(Analyte) %>% 
  pivot_wider(names_from = Analyte, values_from = n) %>% 
  complete(Year, StationCode) %>% 
  arrange(Year, StationCode) %>% 
  write_excel_csv("dwq_sample_counts_by_parameter.csv", na = "")

