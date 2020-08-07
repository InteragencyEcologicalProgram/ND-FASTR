# NDFA Water Quality
# Purpose: Import, clean, and export continuous water quality data collected by USGS, NCRO, and EMP
# Authors: Amanda Maguire, Dave Bosworth, Traci Treleaven

# Load packages
library(tidyverse)
library(lubridate)


# Code for Entire Script --------------------------------------------------

# Source global WQ functions
source("Water_Quality/global_wq_funcs.R")

# Source continuous WQ data cleaning functions
source("Water_Quality/Continuous_WQ/Data_Cleaning/clean_rtm_data_funcs.R")

# Define main NDFA file path for WQ subteam (assumes synced with SharePoint)
fp_fastr <- "California Department of Water Resources/Office of Water Quality and Estuarine Ecology - North Delta Flow Action/WQ_Subteam/"

# Define relative file paths for raw and processed continuous WQ data files
fp_rel_wq_raw <- paste0(fp_fastr, "Raw_Data/Continuous")
fp_rel_wq_proc <- paste0(fp_fastr, "Processed_Data/Continuous")

# Define absolute file paths
fp_abs_wq_raw <- get_abs_path(fp_rel_wq_raw)
fp_abs_wq_proc <- get_abs_path(fp_rel_wq_proc)

# Clean up
rm(fp_rel_wq_raw, fp_rel_wq_proc)

# Create a vector of object names to keep throughout the script
obj_keep <- append(objects(), "obj_keep")


# USGS Data ---------------------------------------------------------------------

# Create a vector of all file paths for the raw USGS continuous data
usgs_cont_fp <- dir(fp_abs_wq_raw, pattern = "USGS", full.names = T)

# Remove the file with SDI flow data for now
usgs_cont_fp_f <- discard(usgs_cont_fp, str_detect(usgs_cont_fp, "Flow_2011-2018"))

# Create a tibble of all raw USGS continuous data files
usgs_cont_files <- tibble(
  filename = usgs_cont_fp_f,
  n_params = c(19, 11, 4, 6, 11, 12, 11, 7, 12, 3)
)

# Import USGS data into a nested dataframe
usgs_cont_orig <- usgs_cont_files %>% 
  mutate(
    sta_code = str_sub(filename, start = 176, end = -5),
    df = map2(filename, n_params, .f = import_usgs_data)
  ) %>% 
  select(sta_code, df)

# Parse date time variable
usgs_cont_dt_clean <- usgs_cont_orig %>% 
  mutate(
    df = map(
      df, 
      ~mutate(.x, DateTime = ymd_hms(dateTime)) %>% 
        select(-dateTime)
    )
  )

temp <- usgs_cont_dt_clean %>% 
  mutate(
    df_count = map(
      df, 
      ~select(.x, !ends_with("_cd")) %>% 
        pivot_longer(
          cols = -c(DateTime, site_no),
          names_to = "parameter",
          values_to = "value"
        ) %>% 
        filter(!is.na(value)) %>% 
        std_param_vars_usgs(parameter, "data_values") %>% 
        count(DateTime, parameter) %>% 
        filter(n > 1)
    )
  )

# Remove some overlapping data for the SDI and LIB stations:
  # SDI - Specific Conductance and Water Temperature data- Keep the BGC project data 
    # when there was more than one study collecting data during the same time period
  sdi <- usgs_cont_dt_clean %>% 
    filter(sta_code == "SDI") %>% 
    pull(df) %>% 
    chuck(1)
  
  sdi_hydro_wt_spc <- sdi %>% 
    select(
      DateTime,
      X_.HYDRO.EXO._00010_00000,
      X_.HYDRO.EXO._00010_00000_cd,
      X_.HYDRO.EXO._00095_00000,
      X_.HYDRO.EXO._00095_00000_cd
    ) %>% 
    filter(date(DateTime) <= "2013-01-24")
  
  sdi_clean <- sdi %>% 
    select(
      -c(
        X_.HYDRO.EXO._00010_00000,
        X_.HYDRO.EXO._00010_00000_cd,
        X_.HYDRO.EXO._00095_00000,
        X_.HYDRO.EXO._00095_00000_cd
      )
    ) %>% 
    left_join(sdi_hydro_wt_spc)
  
  # LIB - remove data from the Chlorophyll inter-calibration project
  lib <- usgs_cont_dt_clean %>% 
    filter(sta_code == "LIB") %>% 
    pull(df) %>% 
    chuck(1)
  
  lib_clean <- lib %>% select(!starts_with("X_CHLOR"))

# Combine data for RYI and TOE stations
ryi_c <- usgs_cont_dt_clean %>% 
  filter(str_detect(sta_code, "^RYI")) %>% 
  select(df) %>% 
  unnest(df)

toe_c <- usgs_cont_dt_clean %>% 
  filter(str_detect(sta_code, "^TOE")) %>% 
  select(df) %>% 
  unnest(df)

# Add cleaned data for SDI, LIB, RYI, and TOE stations back to main dataframe
lib_sdi_ryi_toe_clean <- tibble(
  sta_code = c("LIB", "SDI", "RYI", "TOE"),
  df = list(lib_clean, sdi_clean, ryi_c, toe_c)
)

usgs_cont_dt_clean_m <- usgs_cont_dt_clean %>% 
  filter(!str_detect(sta_code, "LIB$|^SDI|^RYI|^TOE")) %>% 
  bind_rows(lib_sdi_ryi_toe_clean)

temp <- usgs_cont_dt_clean_m %>% 
  mutate(
    df_mod = map(
      df, 
      ~select(.x, !ends_with("_cd")) %>% 
        pivot_longer(
          cols = -c(DateTime, site_no),
          names_to = "parameter",
          values_to = "value"
        ) %>% 
        filter(!is.na(value)) %>% 
        std_param_vars_usgs(parameter, "data_values")
    ),
    df_count1 = map(
      df_mod,
      ~mutate(.x, Date = as_date(DateTime)) %>% 
        count(site_no, Date, parameter) %>% 
        filter(n > 96)
    ),
    df_count2 = map(
      df_mod,
      ~count(.x, site_no, DateTime, parameter) %>% 
        filter(n > 1)
    )
  )

# Finish cleaning the continuous USGS data
usgs_cont_clean_f <- usgs_cont_dt_clean_m %>% 
  mutate(
    # Standardize parameter names for data values
    df_dv = map(
      df, 
      ~select(.x, !ends_with("_cd")) %>% 
        pivot_longer(
          cols = -c(DateTime, site_no),
          names_to = "parameter",
          values_to = "value"
        ) %>% 
        filter(!is.na(value)) %>% 
        std_param_vars_usgs(parameter, "data_values") %>% 
        pivot_wider(names_from = parameter, values_from = value)
    ),
    # Standardize parameter names for qual codes
    df_qc = map(
      df, 
      ~select(.x, site_no, DateTime, ends_with("_cd")) %>% 
        pivot_longer(
          cols = -c(DateTime, site_no),
          names_to = "parameter",
          values_to = "value"
        ) %>% 
        filter(!is.na(value)) %>% 
        std_param_vars_usgs(parameter, "qc_codes") %>% 
        pivot_wider(names_from = parameter, values_from = value)
    ),
    # Join df_dv and df_qc together
    df_join = map2(df_dv, df_qc, left_join),
    # Add NDFA standardized station names to each dataframe
    df_names = map2(df_join, sta_code, ~mutate(.x, StationCode = .y)),
    # Reorder variables in a consistent pattern
    df_f = map(df_names, .f = apply_var_order, collector = "usgs")
  )

# Save RD22 and RVB data to be joined with other data later
rd22_usgs <- usgs_cont_clean_f %>% 
  filter(sta_code == "RD22") %>% 
  pull(df_f) %>% 
  chuck(1)

rvb_usgs <- usgs_cont_clean_f %>% 
  filter(sta_code == "RVB") %>% 
  pull(df_f) %>% 
  chuck(1)

# Remove RD22 and RVB data from dataframe so they won't be exported at this point
usgs_cont_clean_f_e <- usgs_cont_clean_f %>% 
  filter(!sta_code %in% c("RD22", "RVB"))

# Export remaining data as a .csv files 
walk2(
  usgs_cont_clean_f_e$df_f, 
  usgs_cont_clean_f_e$sta_code,
  .f = ~write_excel_csv(
    .x,
    path = paste0(fp_abs_wq_proc, "/RTM_OUTPUT_", .y, "_formatted.csv"),
    na = ""
  )
)

# Add rd22_usgs and rvb_usgs to obj_keep to keep them in the global environment
obj_keep <- append(obj_keep, c("rd22_usgs", "rvb_usgs"))

# Clean up
rm(list = ls()[!(ls() %in% obj_keep)])


# NCRO - Hydstra data ---------------------------------------------------------------------



