# NDFA Water Quality
# Purpose: Download the 2015-2019 contaminant concentration data in water and suspended sediment 
  # collected and analyzed by USGS from NWIS. Jim Orlando provided contaminant concentration 
  # data in zooplankton from 2017-2019 since this data is not available on NWIS at this time.
# Author: Dave Bosworth
# Contacts: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
library(dataRetrieval)
library(readxl)

# Source global WQ functions
source("Water_Quality/global_wq_funcs.R")

# Define main NDFA file path for WQ subteam (assumes synced with SharePoint)
fp_fastr <- "California Department of Water Resources/Office of Water Quality and Estuarine Ecology - North Delta Flow Action/WQ_Subteam/"

# Define relative file path to export raw continuous USGS data files to
fp_rel <- paste0(fp_fastr, "Raw_Data/Contaminants")

# Define absolute file path
fp_abs <- get_abs_path(fp_rel)

# Create objects for start and end dates
start_date <- "2015-10-19"
end_date <- "2019-10-16"

# Import parameter codes to download
params <- 
  read_excel(paste0(fp_abs, "/USGS_Data_Download_metadata.xlsx"), sheet = "Parameter Codes") %>% 
  pull(parameter_code)

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
  path = paste0(fp_abs, "/CONTAM_RAW_NWIS_2015-2019.csv"),
    na = ""
  )

