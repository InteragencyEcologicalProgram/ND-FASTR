# NDFA Water Quality
# Purpose: Functions to be used for creating plots and summary tables for 3-factor ANOVA analysis
# Author: Dave Bosworth
# Contacts: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
library(scales)
library(gt)


# Internal function to define y-axis labels in plots
int_define_yaxis_lab <- function(param) {
  yaxis_lab <- case_when(
    param == "Chla" ~ "Chlorophyll a (ug/L)",
    param == "DisAmmonia" ~ "Dissolved Ammonia (mg/L as N)",
    param == "DisCalcium" ~ "Dissolved Calcium (mg/L)",
    param == "DisChloride" ~ "Dissolved Chloride (mg/L)",
    param == "DisNitrateNitrite" ~ "Dissolved Nitrate + Nitrite (mg/L as N)",
    param == "DisSilica" ~ "Dissolved Silica (mg/L)",
    param == "DOC" ~ "Dissolved Organic Carbon (mg/L as C)",
    param == "DON" ~ "Dissolved Organic Nitrogen (mg/L as N)",
    param == "DOP" ~ "Dissolved Ortho-phosphate (mg/L as P)",
    param == "Pheo" ~ "Pheophytin a (ug/L)",
    param == "TDS" ~ "Total Dissolved Solids (mg/L)",
    param == "TKN" ~ "Total Kjeldahl Nitrogen (mg/L as N)",
    param == "TOC" ~ "Total Organic Carbon (mg/L as C)",
    param == "TOP" ~ "Total Phosphorus (mg/L as P)",
    param == "TSS" ~ "Total Suspended Solids (mg/L)",
    param == "VSS" ~ "Volatile Suspended Solids (mg/L)"
  )
  
  return(yaxis_lab)
}


# Create discrete WQ plots
plot_dwq <- function(df, param) {
  # define y-axis label
  y_lab <- int_define_yaxis_lab(param)
  
  # create plot
  p <- df %>% 
    ggplot(aes(x = Date, y = Result, color = FlowActionPeriod)) +
    geom_point() +
    facet_grid(rows = vars(StationCode)) +
    scale_x_date(
      name = "Date",
      breaks = breaks_pretty(15),
      labels = label_date_short()
    ) +
    labs(
      y = y_lab,
      color = "Flow Action Period:"
    ) +
    theme(legend.position = "bottom")
  
  return(p)
}


# Calculate summary table for sample counts within each Action Period
calc_summ_tbl <- function(df) {
  df %>% 
    count(StationCode, FlowActionPeriod) %>% 
    complete(StationCode, FlowActionPeriod, fill = list(n = 0)) %>% 
    pivot_wider(names_from = FlowActionPeriod, values_from = n)
}


# Add NDFA Region to summary table
add_ndfa_region <- function(df) {
  # create a tibble with region assignments for each station
  sta_regions <- tibble(
    StationCode = c("RCS", "RD22", "I80", "LIS", "STTD", "BL5", "LIB", "RYI", "RVB"),
    Region = c(rep("Yolo Bypass", 5), rep("Downstream", 4))
  )
  
  # add regions to dataframe
  left_join(df, sta_regions, by = "StationCode")
}


# Convert summary table into gt table object
conv_gt_table <- function(df) {
  df %>% 
    gt(rowname_col = "StationCode", groupname_col = "Region") %>% 
    tab_header(title = "Sample counts within each Action Period") %>% 
    summary_rows(
      groups = TRUE,
      columns = vars(Before, During, After),
      fns = list("Sum of Region" = ~sum(.)),
      decimals = 0
    )
}

