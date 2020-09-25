# NDFA Water Quality
# Purpose: Functions to be used for importing and cleaning USGS contaminants data
# Author: Dave Bosworth
# Contacts: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
library(readxl)


# Data Import Functions ---------------------------------------------------

# Import USGS contaminants data from SharePoint that was downloaded from NWIS
import_nwis_data <- function(file_path) {
  df <- read_csv(
    file = file_path,
    col_types = "c--c------c--------ncn---n------c---"
  )
  
  return(df)
}


# Cleaning functions -------------------------------------------------

# Convert NA values, where NA (missing) values represent measurements below the Method Detection 
  # Limit and "NA" values represent instances when the parameter wasn't analyzed
convert_na_val <- function(df, var_idx_start, var_idx_end) {
  df %>% 
    mutate_at(
      c(var_idx_start:var_idx_end),
      ~case_when(
        is.na(.x) ~ "Non-detect",
        .x == "NA" ~ "Not analyzed",
        TRUE ~ .x
      )
    )
}

