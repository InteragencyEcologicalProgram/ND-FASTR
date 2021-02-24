# NDFA Water Quality
# Purpose: Functions to be used for QA processing of continuous water quality data
# Author: Dave Bosworth
# Contacts: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
library(rlang)
library(scales)
library(glue)


# General Functions -------------------------------------------------------

# Pull out data for single station from nested data frame
pull_df_sta <- function(df_nest, sta_code) {
  df_nest %>% 
    filter(StationCode == sta_code) %>% 
    pull(df) %>% 
    chuck(1)
}


# Data Removal and Flagging Functions ---------------------------------------------------

# Remove and Flag value of a single timestamp
rm_flag_val_one <- function(df, param, rm_date) {
  
  # Create a symbol and quosure for the related Qual variable
  param_qual_sym <- sym(glue("{as_name(enquo(param))}_Qual"))
  param_qual_enquo <- enquo(param_qual_sym)
  
  # Remove and flag data of a single timestamp
  df %>% 
    mutate(
      "{{ param }}" := replace(
        {{ param }}, 
        DateTime == rm_date, 
        NA_real_
      ),
      !!param_qual_sym := replace(
        !!param_qual_enquo,
        DateTime == rm_date,
        "unreliable"
      )
    )
}


# Remove and Flag values within a date range (inclusive)
rm_flag_val_range <- function(df, param, min_date, max_date) {
  
  # Create a symbol and quosure for the related Qual variable
  param_qual_sym <- sym(glue("{as_name(enquo(param))}_Qual"))
  param_qual_enquo <- enquo(param_qual_sym)
  
  # Remove and flag data within inclusive date range
  df %>% 
    mutate(
      "{{ param }}" := replace(
        {{ param }}, 
        DateTime >= min_date & DateTime <= max_date, 
        NA_real_
      ),
      !!param_qual_sym := replace(
        !!param_qual_enquo,
        DateTime >= min_date & DateTime <= max_date,
        "unreliable"
      )
    )
}


# Plot functions -------------------------------------------------

# Add extra datetime values for gaps of missing data greater than a half a day to 
  # eliminate lines connecting the data
add_extra_dt_halfday <- function(df, param) {
  df %>% 
    # Remove NA values to define min and max of DateTime
    filter(!is.na({{ param }})) %>% 
    # add all timestamps between min and max DateTime
    complete(DateTime = seq.POSIXt(min(DateTime), max(DateTime), by = "15 min")) %>% 
    # count the number of consecutive missing timestamps
    arrange(DateTime) %>% 
    mutate(
      na_val = is.na({{ param }}),
      na_val_run_total = sequence(rle(na_val)$lengths)
    ) %>% 
    # remove gaps of missing data less than a half day
    filter(!(na_val == TRUE & na_val_run_total < 48)) %>% 
    # remove temporary variables
    select(!starts_with("na_val"))
}


# Create a custom ggplot layer for the x-axis DateTime scale
rtm_xaxis_scale <- 
  list(
    scale_x_datetime(
      name = "Date",
      breaks = breaks_pretty(15),
      labels = label_date_short()
    )
  )


# Plot an individual parameter in a simple ggplot
plot_indiv_param <- function(df, param) {
  
  # Create a string from param for defining y-axis label
  param_name <- as_name(enquo(param))
  
  # Define y-axis label
  yaxis_lab <- case_when(
    param_name == "Chla" ~ "Chlorophyll (ug/L)",
    param_name == "Chla_RFU" ~ "Chlorophyll Fluorescence (RFU)",
    param_name == "DO" ~ "Dissolved Oxygen (mg/L)",
    param_name == "fDOM" ~ "Dissolved Organic Matter Fluorescence (ug/L as QSE)",
    param_name == "Flow" ~ "Flow (cfs)",
    param_name == "FlowTF" ~ "Tidally-filtered Flow (cfs)",
    param_name == "NitrateNitrite" ~ "Nitrate + Nitrite (mg/L as N)",
    param_name == "pH" ~ "pH (pH units)",
    param_name == "SpCnd" ~ "Specific Conductance (uS/cm)",
    param_name == "Turbidity" ~ "Turbidity (FNU)",
    param_name == "WaterTemp" ~ "Water Temperature (degrees C)",
  )
  
  # Create plot
  df %>% 
    add_extra_dt_halfday({{ param }}) %>% 
    ggplot(aes(DateTime, {{ param }})) +
      geom_line() +
      rtm_xaxis_scale +
      scale_y_continuous(
        name = yaxis_lab,
        labels = label_comma()
      )
}

