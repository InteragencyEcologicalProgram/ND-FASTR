# NDFA Water Quality
# Purpose: Functions to be used for creating plots of the raw continuous water quality data for 
  # the stations with year-round data
# Author: Dave Bosworth
# Contacts: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
library(scales)
library(plotly)


# Add extra datetime values for gaps of missing data greater than a half a day - 
  # this is necessary to eliminate lines connecting the data
add_extra_dt_halfday <- function(df) {
  # add all 15-min time stamps between min and max DateTime
  df_dt_full <- complete(df, DateTime = seq.POSIXt(min(DateTime), max(DateTime), by = "15 min"))
  
  # count the number of consecutive missing time stamps
  df_dt_full_count <- df_dt_full %>% 
    arrange(DateTime) %>% 
    mutate(
      na_val = is.na(Value),
      na_val_run_total = sequence(rle(na_val)$lengths)
    )
  
  # only keep NA values for gaps of missing data greater than a half day by keeping the 
    # 48th value in each sequence of filled-in NA values
  df_dt_filt <- filter(df_dt_full_count, !(na_val == TRUE & na_val_run_total != 48))
  
  # remove temporary variables
  df_dt_filt %>% select(!starts_with("na_val"))
}


# Add an extra row with an NA value to delineate the transition between raw and QA'ed data -  
  # this is necessary to eliminate lines connecting the data when using a grouping variable for
  # line color
add_extra_dt_btw_status <- function(df) {
  # Find where the transitions occur between raw and QA'ed data in the data frame
  df_status <- df %>% mutate(Status_diff = if_else(Status == lag(Status), TRUE, FALSE))
  
  # Pull out the specific rows where the transitions occur and modify them to add back to
    # the original data frame
  df_status_switch <- df_status %>% 
    filter(Status_diff == FALSE) %>% 
    # subtract one minute from the date times and convert the values to NA
    mutate(
      DateTime = DateTime - minutes(1),
      Value = NA_real_
    ) %>% 
    select(-Status_diff)
  
  # Add extra rows back to the original data frame and clean it up
  bind_rows(df, df_status_switch) %>% arrange(DateTime)
}


# Internal function to define y-axis labels in plots
int_define_yaxis_lab <- function(param) {
  yaxis_lab <- case_when(
    param == "Chla" ~ "Chlorophyll (ug/L)",
    param == "DO" ~ "Dissolved Oxygen (mg/L)",
    param == "fDOM" ~ "Dissolved Organic Matter Fluorescence (ug/L as QSE)",
    param == "NitrateNitrite" ~ "Nitrate + Nitrite (mg/L as N)",
    param == "pH" ~ "pH (pH units)",
    param == "SpCnd" ~ "Specific Conductance (uS/cm)",
    param == "Turbidity" ~ "Turbidity (FNU)",
    param == "WaterTemp" ~ "Water Temperature (degrees C)",
  )
  
  return(yaxis_lab)
}


# Create simple interactive timeseries plotly plot of continuous WQ Data
create_ts_plotly <- function(df, param) {
  # define y-axis label
  y_lab <- int_define_yaxis_lab(param)
  
  # create plot
  p <-
    ggplot(
      data = df[complete.cases(df),],
      aes(
        x = DateTime, 
        y = Value,
        color = Status
      )
    ) +
    geom_line(aes(group = grp), na.rm = TRUE) +
    scale_x_datetime(
      name = "Date",
      breaks = breaks_pretty(15),
      labels = label_date_short()
    ) +
    scale_y_continuous(
      name = y_lab,
      labels = label_comma()
    ) +
    scale_color_manual(values = c("QA" = "gray40", "Raw" = "brown"))
  
  ggplotly(p)
}


# Wrapper to print plots
print_plot <- function(df) {
  df %>% pull(plt) %>% chuck(1)
}

