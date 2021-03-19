# NDFA Water Quality
# Purpose: Functions to be used for creating multi-station plots of the continuous water quality data
# Author: Dave Bosworth
# Contacts: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
library(scales)

# Internal function to define y-axis labels in plots
int_define_yaxis_lab <- function(param) {
  yaxis_lab <- case_when(
    param == "Chla" ~ "Chlorophyll (ug/L)",
    param == "DO" ~ "Diss Oxygen (mg/L)",
    param == "pH" ~ "pH (pH units)",
    param == "SpCnd" ~ "Sp Cond (uS/cm)",
    param == "Turbidity" ~ "Turbidity (FNU)",
    param == "WaterTemp" ~ "Water Temp (C)",
  )
  
  return(yaxis_lab)
}

# Internal function to define the min and max dates of the flow action periods 
  # and the x-axis limits for each year
int_define_fa_dates <- function(yr) {
  df_yr <- df_fa_dates_f %>% filter(Year == yr)
  
  list(
    x_lim_min = df_yr$PreFlowStart,
    x_lim_max = df_yr$PostFlowEnd,
    action_min = df_yr$PreFlowEnd, 
    action_max = df_yr$PostFlowStart
  )
}

# Internal function to titles for each plot
int_define_plot_title <- function(region) {
  plot_title <- case_when(
    region == "a_Upstream_upper" ~ "Upstream Region - CBD and Upper Yolo Bypass stations",
    region == "b_Upstream_central" ~ "Upstream Region - Central and Lower Yolo Bypass stations",
    region == "c_Downstream" ~ "Downstream Region",
    region == "d_Sac_River" ~ "Middle Sacramento River"
  )
  
  return(plot_title)
}

# Create simple ggplot of continuous WQ Data - for plots of all years of data
create_ts_plot <- function(df, param, yr, plot_pos) {
  # define y-axis label
  y_lab <- int_define_yaxis_lab(param)
  
  # define dates of flow action period and x-axis limits
  fa_dates <- int_define_fa_dates(yr)
  
  # define plot label
  plot_title <- int_define_plot_title(plot_pos)
  
  # create base plot
  p <- df %>% 
    ggplot(aes(x = Date, y = Daily_avg, color = StationCode)) +
    geom_line() +
    theme_light() +
    theme(panel.grid.minor = element_blank()) +
    ggtitle(plot_title) +
    scale_y_continuous(
      name = y_lab,
      labels = label_comma()
    ) +
    scale_color_viridis_d(
      name = "Station:", 
      option = "plasma", 
      end = 0.9
    ) +
    annotate(
      "rect", 
      xmin = fa_dates$action_min, 
      xmax = fa_dates$action_max, 
      ymin = -Inf, 
      ymax = Inf, 
      alpha = 0.1,
      fill = "brown"
    )
  
  # apply formatting for x-axis based on plot_pos
  if (plot_pos == "d_Sac_River") {
    # only the bottom most plot gets x-axis labels and tick marks
    p <- p +
      scale_x_date(
        name = "Date",
        limits = c(fa_dates$x_lim_min, fa_dates$x_lim_max),
        breaks = breaks_pretty(15),
        labels = label_date_short(),
        expand = expansion(mult = 0.01)
      )
  } else {
    p <- p +
      scale_x_date(
        name = NULL,
        limits = c(fa_dates$x_lim_min, fa_dates$x_lim_max),
        breaks = breaks_pretty(15),
        labels = NULL,
        expand = expansion(mult = 0.01)
      ) +
      theme(axis.ticks.x = element_blank())
  }

  return(p)
}

