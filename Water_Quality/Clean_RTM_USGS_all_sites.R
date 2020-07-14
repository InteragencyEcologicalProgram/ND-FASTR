# NDFA Water Quality
# Purpose: Import, clean, and export continuous water quality data collected by USGS
# Authors: Amanda Maguire, Dave Bosworth, Traci Treleaven

# Load packages
library(tidyverse)
library(lubridate)


# Code for Entire Script --------------------------------------------------

# Define path on SharePoint site for data
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/Office of Water Quality and Estuarine Ecology - North Delta Flow Action/WQ_Subteam"
  )
)

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

# Create a function to standardize parameter variable names for the data values and Quality codes
standardize_param_vars <- function(df, param_var, var_type = c("data_values", "qc_codes")) {
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

obj_keep <- c("obj_keep", "sharepoint_path", "standardize_param_vars")


# SDI ---------------------------------------------------------------------

# Define standardized station code for easier updating
sta_code <- "SDI"

# Define number of parameters collected at site
num_params <- 11

# Import data
sdi_orig <- read_csv(
  file = paste0(sharepoint_path, "/Raw_Data/Continuous/RTM_RAW_USGS_", sta_code, ".csv"),
  col_types = paste0("-cc", str_c(rep("dc", num_params), collapse = ""), "-")
)

# Clean data
sdi_clean <- sdi_orig %>% 
  mutate(
    # Parse date time variable
    DateTime = ymd_hms(dateTime),
    # Convert Station name to NDFA standardized name
    StationCode = sta_code
  ) %>% 
  select(-dateTime)

# Remove overlapping Specific Conductance and Water Temperature data
  # Keep the BGC project data when there was more than one study collecting 
  # data during the same time period
sdi_hydro_wt_spc <- sdi_clean %>% 
  select(
    DateTime,
    X_.HYDRO.EXO._00010_00000,
    X_.HYDRO.EXO._00010_00000_cd,
    X_.HYDRO.EXO._00095_00000,
    X_.HYDRO.EXO._00095_00000_cd
  ) %>% 
  filter(date(DateTime) <= "2013-01-24")

sdi_clean <- sdi_clean %>% 
  select(
    -c(
      X_.HYDRO.EXO._00010_00000,
      X_.HYDRO.EXO._00010_00000_cd,
      X_.HYDRO.EXO._00095_00000,
      X_.HYDRO.EXO._00095_00000_cd
    )
  ) %>% 
  left_join(sdi_hydro_wt_spc)

# Standardize parameter variable names
  # Data values
  sdi_values <- sdi_clean %>% 
    select(!ends_with("_cd")) %>% 
    pivot_longer(
      cols = -c(DateTime, StationCode, site_no),
      names_to = "parameter",
      values_to = "value"
    ) %>% 
    filter(!is.na(value)) %>% 
    standardize_param_vars(parameter, "data_values") %>% 
    pivot_wider(names_from = parameter, values_from = value)

  # Qual Codes
  sdi_qual <- sdi_clean %>% 
    select(site_no, DateTime, ends_with("_cd")) %>% 
    pivot_longer(
      cols = -c(site_no, DateTime),
      names_to = "parameter",
      values_to = "value"
    ) %>% 
    filter(!is.na(value)) %>% 
    standardize_param_vars(parameter, "qc_codes") %>% 
    pivot_wider(names_from = parameter, values_from = value)
  
# Join two wide dataframes together
sdi_clean_f <- 
  left_join(sdi_values, sdi_qual) %>% 
  # Reorder variables
  select(
    site_no,
    DateTime,
    StationCode,
    starts_with("Wa"),
    starts_with("Tu"),
    starts_with("Sp"),
    starts_with("DO"),
    starts_with("pH", ignore.case = FALSE),
    Chla,
    Chla_Qual,
    starts_with("fD"),
    starts_with("Ni")
  )

# Export formatted data as a .csv file 
sdi_clean_f %>% 
  write_excel_csv(
    path = paste0(sharepoint_path, "/Processed_Data/Continuous/RTM_OUTPUT_", sta_code, "_formatted.csv"),
    na = ""
  )

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# SGG ---------------------------------------------------------------------

# Define standardized station code for easier updating
sta_code <- "SGG"

# Define number of parameters collected at site
num_params <- 7

# Import data
sgg_orig <- read_csv(
  file = paste0(sharepoint_path, "/Raw_Data/Continuous/RTM_RAW_USGS_", sta_code, ".csv"),
  col_types = paste0("-cc", str_c(rep("dc", num_params), collapse = ""), "-")
)
  
# Clean data
sgg_clean <- sgg_orig %>% 
  mutate(
    # Parse date time variable
    DateTime = ymd_hms(dateTime),
    # Convert Station name to NDFA standardized name
    StationCode = sta_code
  ) %>% 
  select(-dateTime)

# Standardize SGG parameter variable names
  # Data values
  sgg_values <- sgg_clean %>% 
    select(!ends_with("_cd")) %>% 
    pivot_longer(
      cols = -c(DateTime, StationCode, site_no),
      names_to = "parameter",
      values_to = "value"
    ) %>% 
    filter(!is.na(value)) %>% 
    standardize_param_vars(parameter, "data_values") %>%
    pivot_wider(names_from = parameter, values_from = value)
  
  # SGG Qual Codes
  sgg_qual <- sgg_clean %>% 
    select(site_no, DateTime, ends_with("_cd")) %>% 
    pivot_longer(
      cols = -c(site_no, DateTime),
      names_to = "parameter",
      values_to = "value"
    ) %>% 
    filter(!is.na(value)) %>% 
    standardize_param_vars(parameter, "qc_codes") %>%
    pivot_wider(names_from = parameter, values_from = value)

# Join two wide dataframes together
sgg_clean_f <- 
  left_join(sgg_values, sgg_qual) %>% 
  # Reorder variables
  select(
    site_no,
    DateTime,
    StationCode,
    Flow,
    Flow_Qual,
    FlowTF,
    FlowTF_Qual,
    starts_with("Wa"),
    starts_with("Tu"),
    starts_with("Sp"),
    starts_with("DO"),
    starts_with("pH", ignore.case = FALSE)
  )

# Export formatted data as a .csv file 
sgg_clean_f %>% 
  write_excel_csv(
    path = paste0(sharepoint_path, "/Processed_Data/Continuous/RTM_OUTPUT_", sta_code, "_formatted.csv"),
    na = ""
  )

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# TOE ---------------------------------------------------------------------

# Define standardized station code for easier updating
sta_code <- "TOE"

# Define number of parameters collected at site
num_params_1 <- 11
num_params_2 <- 3

# Import data
  # First station
  toe_orig_1 <- read_csv(
    file = paste0(sharepoint_path, "/Raw_Data/Continuous/RTM_RAW_USGS_", sta_code, "_1.csv"),
    col_types = paste0("-cc", str_c(rep("dc", num_params_1), collapse = ""), "-")
  )

  # Second station
  toe_orig_2 <- read_csv(
    file = paste0(sharepoint_path, "/Raw_Data/Continuous/RTM_RAW_USGS_", sta_code, "_2.csv"),
    col_types = paste0("-cc", str_c(rep("dc", num_params_2), collapse = ""), "-")
  )
  
# Bind toe_orig1 and toe_orig2
toe_orig <- bind_rows(toe_orig_1, toe_orig_2)

# Clean data
toe_clean <- toe_orig %>% 
  mutate(
    # Parse date time variable
    DateTime = ymd_hms(dateTime),
    # Convert Station name to NDFA standardized name
    StationCode = sta_code
  ) %>% 
  select(-dateTime)

# Standardize parameter variable names
  # Data values
  toe_values <- toe_clean %>% 
    select(!ends_with("_cd")) %>% 
    pivot_longer(
      cols = -c(DateTime, StationCode, site_no),
      names_to = "parameter",
      values_to = "value"
    ) %>% 
    filter(!is.na(value)) %>% 
    standardize_param_vars(parameter, "data_values") %>%
    pivot_wider(names_from = parameter, values_from = value)
  
  # Qual Codes
  toe_qual <- toe_clean %>% 
    select(site_no, DateTime, ends_with("_cd")) %>% 
    pivot_longer(
      cols = -c(site_no, DateTime),
      names_to = "parameter",
      values_to = "value"
    ) %>% 
    filter(!is.na(value)) %>% 
    standardize_param_vars(parameter, "qc_codes") %>%
    pivot_wider(names_from = parameter, values_from = value)

# Join two wide dataframes together
toe_clean_f <- 
  left_join(toe_values, toe_qual) %>% 
  # Reorder variables
  select(
    site_no,
    DateTime,
    StationCode,
    Flow,
    Flow_Qual,
    FlowTF,
    FlowTF_Qual,
    starts_with("Wa"),
    starts_with("Tu"),
    starts_with("Sp"),
    starts_with("DO"),
    starts_with("pH", ignore.case = FALSE),
    Chla,
    Chla_Qual,
    Chla_RFU,
    Chla_RFU_Qual,
    #starts_with("fD"),
    starts_with("Phy"),
    starts_with("Ni")
  )

# Export formatted data as a .csv file 
toe_clean_f %>% 
  write_excel_csv(
    path = paste0(sharepoint_path, "/Processed_Data/Continuous/RTM_OUTPUT_", sta_code, "_formatted.csv"),
    na = ""
  )

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# RYI ---------------------------------------------------------------------

# Define standardized station code for easier updating
sta_code <- "RYI"

# Define number of parameters collected at site
num_params_1 <- 11
num_params_2 <- 12

# Import data
  # First station
  ryi_orig_1 <- read_csv(
    file = paste0(sharepoint_path, "/Raw_Data/Continuous/RTM_RAW_USGS_", sta_code, "_1.csv"),
    col_types = paste0("-cc", str_c(rep("dc", num_params_1), collapse = ""), "-")
  )
  
  # Second station
  ryi_orig_2 <- read_csv(
    file = paste0(sharepoint_path, "/Raw_Data/Continuous/RTM_RAW_USGS_", sta_code, "_2.csv"),
    col_types = paste0("-cc", str_c(rep("dc", num_params_2), collapse = ""), "-")
  )

  # Bind ryi_orig1 and ryi_orig2
  ryi_orig <- bind_rows(ryi_orig_1, ryi_orig_2)

# Clean data
ryi_clean <- ryi_orig %>% 
  mutate(
    # Parse date time variable
    DateTime = ymd_hms(dateTime),
    # Convert Station name to NDFA standardized name
    StationCode = sta_code
  ) %>% 
  select(-dateTime)
  
# Standardize parameter variable names
  # Data values
  ryi_values <- ryi_clean %>% 
    select(!ends_with("_cd")) %>% 
    pivot_longer(
      cols = -c(DateTime, StationCode, site_no),
      names_to = "parameter",
      values_to = "value"
    ) %>% 
    filter(!is.na(value)) %>% 
    standardize_param_vars(parameter, "data_values") %>% 
    pivot_wider(names_from = parameter, values_from = value)
  
  # Qual Codes
  ryi_qual <- ryi_clean %>% 
    select(site_no, DateTime, ends_with("_cd")) %>% 
    pivot_longer(
      cols = -c(site_no, DateTime),
      names_to = "parameter",
      values_to = "value"
    ) %>% 
    filter(!is.na(value)) %>% 
    standardize_param_vars(parameter, "qc_codes") %>%
    pivot_wider(names_from = parameter, values_from = value)
  
  # Join two wide dataframes together
  ryi_clean_f <- 
    left_join(ryi_values, ryi_qual) %>% 
    # Reorder variables
    select(
      site_no,
      DateTime,
      StationCode,
      Flow,
      Flow_Qual,
      FlowTF,
      FlowTF_Qual,
      starts_with("Wa"),
      starts_with("Tu"),
      starts_with("Sp"),
      starts_with("DO"),
      starts_with("pH", ignore.case = FALSE),
      Chla,
      Chla_Qual,
      Chla_RFU,
      Chla_RFU_Qual,
      starts_with("fD"),
      starts_with("Phy"),
      starts_with("Ni")
    )
  
# Export formatted data as a .csv file 
ryi_clean_f %>% 
  write_excel_csv(
    path = paste0(sharepoint_path, "/Processed_Data/Continuous/RTM_OUTPUT_", sta_code, "_formatted.csv"),
    na = ""
  )

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# LIB ---------------------------------------------------------------------




# LIBCUT ------------------------------------------------------------------


