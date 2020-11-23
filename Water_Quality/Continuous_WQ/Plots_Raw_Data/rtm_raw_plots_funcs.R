# NDFA Water Quality
# Purpose: Functions to be used for creating plots of the raw continuous water quality data
# Author: Dave Bosworth
# Contacts: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
library(scales)
library(plotly)


# Restructure data to long format to allow for a more generalized plotting function
restructure_df_long <- function(df) {
  # pivot data values to long format
  df_data_values <-  df %>% 
    select(-c(StationCode, ends_with("_Qual"))) %>% 
    pivot_longer(
      cols = -DateTime,
      names_to = "parameter",
      values_to = "value"
    )
  
  # pivot qual codes to long format
  df_qual_codes <-  df %>% 
    select(DateTime, ends_with("_Qual")) %>% 
    rename_with(~str_remove(.x, "_Qual"), ends_with("_Qual")) %>%
    pivot_longer(
      cols = -DateTime,
      names_to = "parameter",
      values_to = "qual_code"
    )
  
  # Join data values and qual codes together
  left_join(df_data_values, df_qual_codes, by = c("DateTime", "parameter"))
}


# Add extra datetime values for gaps of missing data greater than a half a day to 
  # eliminate lines connecting the data
add_extra_dt_halfday <- function(df, time_int = c("15min", "1hr")) {
  # check arguments in time_int
  time_int = match.arg(time_int, c("15min", "1hr"))
  
  # add all timestamps between min and max DateTime depending upon time interval
  if (time_int == "15min") {
    df_dt_full <- complete(df, DateTime = seq.POSIXt(min(DateTime), max(DateTime), by = "15 min"))
  } else {
    df_dt_full <- complete(df, DateTime = seq.POSIXt(min(DateTime), max(DateTime), by = "hour"))
  }
  
  # count the number of consecutive missing timestamps
  df_dt_full_count <- df_dt_full %>% 
    arrange(DateTime) %>% 
    mutate(
      na_val = is.na(value),
      na_val_run_total = sequence(rle(na_val)$lengths)
    )
  
  # remove gaps of missing data less than a half day depending upon time interval
  if (time_int == "15min") {
    df_dt_filt <- filter(df_dt_full_count, !(na_val == TRUE & na_val_run_total < 48))
  } else {
    df_dt_filt <- filter(df_dt_full_count, !(na_val == TRUE & na_val_run_total < 12))
  }
  
  # remove temporary variables
  df_dt_filt %>% select(!starts_with("na_val"))
}


# Add extra datetime values to eliminate the lines connecting the years in the plots 
  # of all years of data
add_extra_dt_yr <- function(df) {
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
create_ts_ggplot <- function(df, param) {
  # define y-axis label
  y_lab <- int_define_yaxis_lab(param)
  
  # create plot
  p <-
    ggplot(
      data = df,
      aes(
        x = DateTime, 
        y = value
      )
    ) +
    geom_line(na.rm = TRUE) +
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
create_ts_plotly <- function(df, param) {
  # use create_ts_ggplot to make timeseries ggplot
  p <- create_ts_ggplot(df, param)
  
  # convert ggplot to interactive plotly plot
  ggplotly(p)
}

# Wrapper to print plots
print_plot <- function(df) {
  df %>% pull(plot) %>% chuck(1)
}

