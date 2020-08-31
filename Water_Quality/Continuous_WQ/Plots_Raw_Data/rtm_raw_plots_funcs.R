# NDFA Water Quality
# Purpose: Functions to be used for creating plots of the raw continuous water quality data
# Author: Dave Bosworth
# Contacts: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
library(scales)

# Import processed continuous WQ Data from SharePoint
import_rtm_data <- function(file_path, num_params){
  df <- read_csv(
    file = file_path,
    col_types = paste0("cc", str_dup("dc", num_params))
  )
  
  return(df)
}


# Add variable for y-axis labels in plots
add_yaxis_var <- function(df, param_var) {
  # Convert param_var to quosure for tidy evaluation
  param_var_enquo <- enquo(param_var)
  
  # Add variable to dataframe
  df1 <- df %>% 
    mutate(
      yaxis_lab = case_when(
        !!param_var_enquo == "Chla" ~ "Chlorophyll (ug/L)",
        !!param_var_enquo == "DO" ~ "Dissolved Oxygen (mg/L)",
        !!param_var_enquo == "Flow" ~ "Flow (cfs)",
        !!param_var_enquo == "FlowTF" ~ "Tidally-filtered Flow (cfs)",
        !!param_var_enquo == "pH" ~ "pH (pH units)",
        !!param_var_enquo == "SpCnd" ~ "Specific Conductance (uS/cm)",
        !!param_var_enquo == "Turbidity" ~ "Turbidity (FNU)",
        !!param_var_enquo == "WaterTemp" ~ "Water Temperature (degrees C)",
      )
    )
  
  return(df1)
}


# Create simple timeseries plots of continuous WQ Data
create_ts_plot <- function(df, y_lab) {
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

