# NDFA Water Quality
# Purpose: Functions to be used for importing and cleaning USGS contaminants data
# Author: Dave Bosworth
# Contacts: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
library(readxl)


# Data Import Functions ---------------------------------------------------

# Import USGS contaminants data from SharePoint that was downloaded from NWIS
import_nwis_data <- function(file_path){
  df <- read_csv(
    file = file_path,
    col_types = "---------c--------ncd-ccdc-----c-c-"
  )
  
  return(df)
}


# Standardizing functions -------------------------------------------------


