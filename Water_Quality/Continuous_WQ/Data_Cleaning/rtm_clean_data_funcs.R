# NDFA Water Quality
# Purpose: Functions to be used for importing and cleaning continuous water quality data
# Author: Dave Bosworth
# Contacts: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
library(readxl)


# Data Import Functions ---------------------------------------------------

# Import continuous WQ Data from SharePoint collected by USGS
import_usgs_data <- function(file_path, num_params){
  df <- read_csv(
    file = file_path,
    col_types = paste0("-cc", str_dup("dc", num_params), "-")
  )
  
  return(df)
}

# Import continuous WQ Data from SharePoint collected by DWR-NCRO-WQES
import_ncro_data_wq <- function(file_path){
  # Create vector of col_names for the WQES WQ data
  wqes_col_names <- c(
    "DateTime",
    "WaterTemp",
    "WaterTemp_Qual",
    "Turbidity",
    "Turbidity_Qual",
    "SpCnd",
    "SpCnd_Qual",
    "pH",
    "pH_Qual",
    "DO",
    "DO_Qual",
    "Chla",
    "Chla_Qual"
  )
  
  df <- read_csv(
    file = file_path,
    col_names = wqes_col_names,
    skip = 3,
    col_types = "cdd-------dd-dd-dd-dd-dd-"
  )
  
  return(df)
}

# Import continuous flow Data from SharePoint collected by DWR-NCRO
import_ncro_data_flow <- function(file_path, sheet_name){
  df <- read_excel(
    path = file_path,
    sheet = sheet_name,
    col_names = c("DateTime", "Flow", "Flow_Qual"),
    skip = 3,
    col_types = c("date", "numeric", "numeric")
  ) 
  
  return(df)
}

# Import continuous WQ Data from SharePoint collected by DWR-DES-EMP
import_emp_data <- function(file_path){
  df <- read_csv(
    file = file_path,
    skip = 4
  ) 
      
  return(df)
}


# Standardizing functions -------------------------------------------------

# Standardize parameter variable names for data collected by USGS
  # USGS PARAMETER CODES:
    # 00060 - Discharge (cfs)
    # 72137 - Discharge, tidally-filtered (cfs)
    # 00010 - Water Temperature (Celsius)
    # 00300 - Dissolved Oxygen (mg/L)
    # 00095 - Specific Conductance at 25 C (uS/cm)
    # 00400 - pH
    # 63680 - Turbidity (FNU)
    # 32315 - Chlorophyll relative fluorescence (RFU)
    # 32316 - Chlorophyll concentration estimated from reference material (ug/L)
    # 32295 - Dissolved organic matter fluorescence (fDOM) (ug/L as QSE)
    # 32321 - Phycocyanin relative fluorescence (RFU)
    # 99133 - Nitrate plus nitrite (mg/L as N)

std_param_vars_usgs <- function(df, param_var, var_type = c("data_values", "qc_codes")) {
  # Evaluate choices for var_type
  var_type <- match.arg(var_type, c("data_values", "qc_codes"))
  
  # Convert param_var to symbol and quosure for tidy evaluation
  param_var_ensym <- ensym(param_var)
  param_var_enquo <- enquo(param_var)
  
  # Standardize variable names for either data values or Quality codes based on var_type argument
  if (var_type == "data_values") {
    df1 <- df %>% 
      mutate(
        !!param_var_ensym := case_when(
          str_detect(!!param_var_enquo, "00060") ~ "Flow",
          str_detect(!!param_var_enquo, "72137") ~ "FlowTF",
          str_detect(!!param_var_enquo, "00010") ~ "WaterTemp",
          str_detect(!!param_var_enquo, "00300") ~ "DO",
          str_detect(!!param_var_enquo, "00095") ~ "SpCnd",
          str_detect(!!param_var_enquo, "00400") ~ "pH",
          str_detect(!!param_var_enquo, "63680") ~ "Turbidity",
          str_detect(!!param_var_enquo, "32315") ~ "Chla_RFU",
          str_detect(!!param_var_enquo, "32316") ~ "Chla",
          str_detect(!!param_var_enquo, "32295") ~ "fDOM",
          str_detect(!!param_var_enquo, "32321") ~ "Phyco_RFU",
          str_detect(!!param_var_enquo, "99133") ~ "NitrateNitrite"
        )
      )
  } else {
    df1 <- df %>% 
      mutate(
        !!param_var_ensym := case_when(
          str_detect(!!param_var_enquo, "00060") ~ "Flow_Qual",
          str_detect(!!param_var_enquo, "72137") ~ "FlowTF_Qual",
          str_detect(!!param_var_enquo, "00010") ~ "WaterTemp_Qual",
          str_detect(!!param_var_enquo, "00300") ~ "DO_Qual",
          str_detect(!!param_var_enquo, "00095") ~ "SpCnd_Qual",
          str_detect(!!param_var_enquo, "00400") ~ "pH_Qual",
          str_detect(!!param_var_enquo, "63680") ~ "Turbidity_Qual",
          str_detect(!!param_var_enquo, "32315") ~ "Chla_RFU_Qual",
          str_detect(!!param_var_enquo, "32316") ~ "Chla_Qual",
          str_detect(!!param_var_enquo, "32295") ~ "fDOM_Qual",
          str_detect(!!param_var_enquo, "32321") ~ "Phyco_RFU_Qual",
          str_detect(!!param_var_enquo, "99133") ~ "NitrateNitrite_Qual"
        )
      )
  }
  
  return(df1)
}

# Standardize parameter variable names for data collected by DWR-DES-EMP
std_param_vars_emp <- function(df, param_var) {
  # Convert param_var to symbol and quosure for tidy evaluation
  param_var_ensym <- ensym(param_var)
  param_var_enquo <- enquo(param_var)
  
  # Standardize parameter names
  df1 <- df %>% 
    mutate(
      !!param_var_ensym := case_when(
        str_detect(!!param_var_enquo, ".{9}Diss") ~ "DO",
        str_detect(!!param_var_enquo, ".{9}Fluor") ~ "Chla",
        str_detect(!!param_var_enquo, ".{9}pH") ~ "pH",
        str_detect(!!param_var_enquo, ".{9}SpC") ~ "SpCnd",
        str_detect(!!param_var_enquo, ".{9}Temp") ~ "WaterTemp",
        str_detect(!!param_var_enquo, ".{9}Turb") ~ "Turbidity"
      )
    )
  
  return(df1)
}

# Apply a consistent variable order
apply_var_order <- function(df) {
  df1 <- df %>% 
    select(
      DateTime,
      StationCode,
      matches("Flow$"),
      matches("Flow_Qual$"),
      starts_with("FlowTF"),
      starts_with("Wa"),
      starts_with("Tu"),
      starts_with("Sp"),
      starts_with("DO"),
      starts_with("pH", ignore.case = FALSE),
      matches("Chla$"),
      matches("Chla_Qual$"),
      starts_with("Chla_RFU"),
      starts_with("fD"),
      starts_with("Phy"),
      starts_with("Ni")
    )
  
  return(df1)
}

