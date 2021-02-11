# NDFA Water Quality
# Purpose: Functions to be used for importing and cleaning USGS contaminants data
# Author: Dave Bosworth
# Contacts: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)


# Data Import Functions ---------------------------------------------------

# Import USGS contaminants data from SharePoint that was downloaded from NWIS
import_nwis_data <- function(file_path) {
  df <- read_csv(
    file = file_path,
    col_types = cols_only(
      StationCode = col_character(),
      sample_dt = col_character(),
      medium_cd = col_character(),
      parm_cd = col_double(),
      rpt_lev_va = col_double()
    )
  )
  
  return(df)
}


# Cleaning functions -------------------------------------------------

# Convert NA values, where NA (missing) values represent measurements below the Method Detection 
  # Limit and "NA" values represent instances when the parameter wasn't analyzed
convert_na_val <- function(df, var_start, var_end) {
  # Convert var_start and var_end to quosures for tidy evaluation
  var_start_enquo <- enquo(var_start)
  var_end_enquo <- enquo(var_end)
  
  # Convert NA values for specified columns
  df %>% 
    mutate(
      across(
        !!var_start_enquo:!!var_end_enquo,
        ~case_when(
          is.na(.x) ~ "< MDL",
          .x == "NA" ~ "Not analyzed",
          TRUE ~ .x
        )
      )
    )
}

# Round values to appropriate number of significant figures
  # 1 number to the right of the decimal for values less than 100
  # 3 significant figures for values greater than or equal to 100
round_val <- function(df, val_var) {
  # Convert val_var to symbol and quosure for tidy evaluation
  val_var_ensym <- ensym(val_var)
  val_var_enquo <- enquo(val_var)
  
  # Round values in val_var
  df %>% 
    mutate(
      !!val_var_ensym := if_else(
        !!val_var_enquo < 100,
        round(!!val_var_enquo, 1),
        signif(!!val_var_enquo, 3)
      )
    )
}

