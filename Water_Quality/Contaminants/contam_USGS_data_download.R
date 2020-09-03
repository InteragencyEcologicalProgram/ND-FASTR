# NDFA Water Quality
# Purpose: Download the 2015-2018 contaminant water concentration data collected and analyzed by 
  # USGS from NWIS. The water concentration data for 2019 is not available on NWIS at this time 
  # and was provided by Jim Orlando from the USGS. Jim Orlando also provided contaminant 
  # concentration data in zooplankton and suspended sediment from 2017-2019 since this data is not 
  # available on NWIS at this time either.
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
end_date <- "2018-11-10"

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

# Download data for each station individually
bl5 <- get_usgs_contam_data("381627121395101")
i80 <- get_usgs_contam_data("383423121345901")
lis <- get_usgs_contam_data("382829121351801")
rcs <- get_usgs_contam_data("384737121433201")
rd22 <- get_usgs_contam_data("384035121383801")
ryi <- get_usgs_contam_data("11455350")
shr <- get_usgs_contam_data("383155121314101")
sttd <- get_usgs_contam_data("382113121383501")

# Combine all data files into a named list for export 
usgs_data <- 
  list(
    BL5 = bl5,
    I80 = i80,
    LIS = lis,
    RCS = rcs,
    RD22 = rd22,
    RYI = ryi,
    SHR = shr,
    STTD = sttd
  )

# Export raw data as .csv files for each site
iwalk(
  usgs_data,
  .f = ~write_excel_csv(
    .x, 
    path = paste0(fp_abs, "/CONTAM_RAW_", .y, "_2015-2018.csv"),
    na = ""
  )
)

