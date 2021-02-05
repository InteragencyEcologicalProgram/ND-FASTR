# NDFA Water Quality
# Purpose: Download the USGS continuous water quality data
# Author: Dave Bosworth
# Final download of data on 2/3/2021

# Load packages
library(tidyverse)
library(dataRetrieval)

# Source global WQ functions
source("global_ndfa_funcs.R")

# Create vectors for parameters, start and end dates
start_date <- "2011-01-01" 
end_date <- "2019-12-31" 
params <- c(
  "00060",  # Discharge (cfs)
  "72137",  # Discharge, tidally-filtered (cfs)
  "00010",  # Water Temperature (Celcius)
  "00300",  # Dissolved Oxygen (mg/L)
  "00095",  # Specific Conductance at 25 C (uS/cm)
  "00400",  # pH
  "63680",  # Turbidity (FNU)
  "32315",  # Chlorophyll relative fluorescence (RFU)
  "32316",  # Chlorophyll concentration estimated from reference material (ug/L)
  "32295",  # Dissolved organic matter fluorescence (fDOM) (ug/L as QSE)
  "32321",  # Phycocyanin relative fluorescence (RFU)
  "99133"   # Nitrate plus nitrite (mg/L as N)
)

# Download data for each station individually since doing it all at once doesn't work correctly
rd22 <- readNWISuv("11453000", params, start_date, end_date, tz = "Etc/GMT+8")
rvb <- readNWISuv("11455420", params, start_date, end_date, tz = "Etc/GMT+8")
lib <- readNWISuv("11455315", params, start_date, end_date, tz = "Etc/GMT+8")
ryi1 <- readNWISuv("11455350", params, start_date, end_date, tz = "Etc/GMT+8")
ryi2 <- readNWISuv("11455385", params, start_date, end_date, tz = "Etc/GMT+8")
toe1 <- readNWISuv("11455140", params, start_date, end_date, tz = "Etc/GMT+8")
toe2 <- readNWISuv("11455139", params, start_date, end_date, tz = "Etc/GMT+8")
sdi <- readNWISuv("11455478", params, start_date, end_date, tz = "Etc/GMT+8")
sgg <- readNWISuv("11455276", params, start_date, end_date, tz = "Etc/GMT+8")
libcut <- readNWISuv("11455146", params, start_date, end_date, tz = "Etc/GMT+8")

# Combine all data files into a named list and batch convert them to tibbles 
usgs_data <- 
  list(
    RD22 = rd22,
    RVB = rvb,
    LIB = lib,
    RYI_1 = ryi1,
    RYI_2 = ryi2,
    TOE_1 = toe1,
    TOE_2 = toe2,
    SDI = sdi,
    SGG = sgg,
    LIBCUT = libcut
  ) %>% 
  map(as_tibble)
  
# Define relative file path to export raw continuous USGS data files to
fp_rel_rtm_wq_raw <- "WQ_Subteam/Raw_Data/Continuous"

# Define absolute file path
fp_abs_rtm_wq_raw <- ndfa_abs_sp_path(fp_rel_rtm_wq_raw)

# Export raw data as .csv files for each site
iwalk(
  usgs_data,
  .f = ~write_excel_csv(
    .x, 
    file = paste0(fp_abs_rtm_wq_raw, "/RTM_RAW_USGS_", .y, ".csv"),
    na = ""
  )
)

