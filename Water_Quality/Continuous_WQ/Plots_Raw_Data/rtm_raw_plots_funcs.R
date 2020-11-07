# NDFA Water Quality
# Purpose: Functions to be used for creating plots of the raw continuous water quality data
# Author: Dave Bosworth
# Contacts: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
library(scales)
library(plotly)


# Import processed continuous WQ Data from SharePoint
import_rtm_data <- function(file_path, num_params) {
  df <- read_csv(
    file = file_path,
    col_types = paste0("cc", str_dup("dc", num_params))
  )
  
  return(df)
}


# Restructure data to long format to allow for a more generalized plotting function
restructure_df_long <- function(df) {
  df %>% 
    select(-c(StationCode, ends_with("_Qual"))) %>% 
    pivot_longer(
      cols = -DateTime,
      names_to = "parameter",
      values_to = "value"
    )
}


# Remove data before the first year when data was collected for each parameter so 
  # that the x-axis is centered correctly on the plots of all years of data
trim_to_first_yr <- function(df) {
  # Find the first year with data for each parameter
  data_first_yr <- df %>% 
    filter(!is.na(value)) %>% 
    group_by(parameter) %>% 
    summarize(min_yr = min(yr))
  
  
  # Filter data in df
  df %>% 
    left_join(data_first_yr, by = "parameter") %>% 
    filter(yr >= min_yr) %>% 
    select(-min_yr)
}


# Add extra datetime values so that the lines connecting years are eliminated in the 
  # plots of all years of data
add_extra_dt_values <- function(df) {
  extra_dt <- df %>% 
    group_by(yr, parameter) %>% 
    summarize(max_dt = max(DateTime), .groups = "drop") %>% 
    mutate(DateTime = max_dt + minutes(15)) %>%  
    select(-max_dt)
  
  bind_rows(df, extra_dt)  
}


# Internal function to define y-axis labels in plots
int_define_yaxis_lab <- function(param) {
  yaxis_lab <- case_when(
    param == "Chla" ~ "Chlorophyll (ug/L)",
    param == "Chla_RFU" ~ "Chlorophyll Fluorescence (RFU)",
    param == "DO" ~ "Dissolved Oxygen (mg/L)",
    param == "fDOM" ~ "Dissolved Organic Matter Fluorescence (ug/L as QSE)",
    param == "Flow" ~ "Flow (cfs)",
    param == "FlowTF" ~ "Tidally-filtered Flow (cfs)",
    param == "NitrateNitrite" ~ "Nitrate + Nitrite (mg/L as N)",
    param == "pH" ~ "pH (pH units)",
    param == "SpCnd" ~ "Specific Conductance (uS/cm)",
    param == "Turbidity" ~ "Turbidity (FNU)",
    param == "WaterTemp" ~ "Water Temperature (degrees C)",
  )
  
  return(yaxis_lab)
}


# Create simple ggplot of continuous WQ Data - for plots of all years of data
create_ts_ggplot <- function(df) {
  # define y-axis label
  y_lab <- int_define_yaxis_lab(df$parameter[1])
  
  # create plot
  p <-
    ggplot(
      data = df,
      aes(
        x = DateTime, 
        y = value
      )
    ) +
    geom_line() +
    scale_x_datetime(
      name = "Date",
      breaks = breaks_pretty(15),
      labels = label_date_short()
    ) +
    scale_y_continuous(
      name = y_lab,
      labels = label_comma()
    )
  
  return(p)
}


# Create simple interactive timeseries plotly plot of continuous WQ Data - for plots 
  # of individual years
create_ts_plotly <- function(df) {
  # use create_ts_ggplot to make timeseries ggplot
  p <- create_ts_ggplot(df)
  
  # convert ggplot to interactive plotly plot
  ggplotly(p)
}

