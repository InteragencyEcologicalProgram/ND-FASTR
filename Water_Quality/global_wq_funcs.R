# NDFA Water Quality
# Purpose: Global functions to be used across analyses of all NDFA water quality data
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)


# Import continuous WQ Data from SharePoint
import_rtm_data <- function(file_path, num_params) {
  df <- read_csv(
    file = file_path,
    col_types = paste0("cc", str_dup("dc", num_params))
  )
  
  return(df)
}

