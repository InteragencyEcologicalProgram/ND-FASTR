# NDFA Water Quality
# Purpose: Download the USGS continuous water quality data
# Author: Dave Bosworth

# Load packages
library(tidyverse)
library(dataRetrieval)

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

# Create a dataframe for downloading data for each site
usgs_data <- tibble(
  site_name = c(
    "RD22",
    "RVB",
    "LIB",
    "RYI_1",
    "RYI_2",
    "TOE_1",
    "TOE_2",
    "SDI",
    "SGG",
    "LIBCUT"
  ),
  site_numb = c(
    "11453000",
    "11455420",
    "11455315",
    "11455350",
    "11455385",
    "11455140",
    "11455139",
    "11455478",
    "11455276",
    "11455146"
  )
)

# Download data
usgs_data <- usgs_data %>% 
  mutate(
    raw_data = map(
      site_numb, 
      .f = readNWISuv, 
      parameterCd = params, 
      start_date, 
      end_date, 
      tz = "Etc/GMT+8"
    ),
    raw_data_t = map(raw_data, .f = as_tibble)
  )

