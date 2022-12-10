# NDFA Water Quality
# Purpose: Download the 2015-2019 contaminant concentration data in water and suspended sediment 
  # collected and analyzed by USGS from NWIS. This data was used to add MDL values to the water 
  # and suspended sediment data provided by Jim Orlando in Excel spreadsheets.
# Author: Dave Bosworth
# Contacts: David.Bosworth@water.ca.gov
# Final download of data on 2/9/2021

# Load packages
library(tidyverse)
library(dataRetrieval)
library(readxl)
library(here)

# Source global NDFA functions
source(here("global_ndfa_funcs.R"))

# Define relative file path for the raw contaminants USGS data
fp_rel_contam_raw <- "2011-2019 Synthesis Study/WQ_Subteam/Raw_Data/Contaminants"

# Define absolute file path
fp_abs_contam_raw <- ndfa_abs_sp_path(fp_rel_contam_raw)

# Create objects for start and end dates
start_date <- "2015-10-19"
end_date <- "2019-10-16"

# Import parameter codes to download
params <- 
  read_excel(
    file.path(fp_abs_contam_raw, "USGS_Data_Download_metadata.xlsx"), 
    sheet = "Parameter Codes"
  ) %>% 
  filter(!is.na(usgs_parameter_code)) %>% 
  pull(usgs_parameter_code)

params <- as.character(params)

# Create a convenience function to download contaminants data from USGS
get_usgs_contam_data <- function(site_no) {
  df <- readNWISqw(
    site_no, 
    parameterCd = params, 
    startDate = start_date, 
    endDate = end_date, 
    tz = "Etc/GMT+8"
  )
  
  # Convert to a tibble
  df_tibb <- as_tibble(df)
  
  return(df_tibb)
}

# Download data for each station individually since it doesn't work to download all at once
bl5 <- get_usgs_contam_data("381627121395101")
i80 <- get_usgs_contam_data("383423121345901")
lis <- get_usgs_contam_data("382829121351801")
rcs <- get_usgs_contam_data("384737121433201")
rd22 <- get_usgs_contam_data("384035121383801")
rmb <- get_usgs_contam_data("11390890")
ryi <- get_usgs_contam_data("11455350")
shr <- get_usgs_contam_data("383155121314101")
sttd <- get_usgs_contam_data("382113121383501")

# Combine all data files into a dataframe 
usgs_data <- 
  list(
    BL5 = bl5,
    I80 = i80,
    LIS = lis,
    RCS = rcs,
    RD22 = rd22,
    RMB = rmb,
    RYI = ryi,
    SHR = shr,
    STTD = sttd
  ) %>% 
  bind_rows(.id = "StationCode")

# Export raw data as a .csv file
usgs_data %>% write_excel_csv(
  file = file.path(fp_abs_contam_raw, "CONTAM_RAW_NWIS_2015-2019.csv"),
    na = ""
  )

