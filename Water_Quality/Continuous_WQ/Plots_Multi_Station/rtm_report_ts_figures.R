# NDFA Water Quality
# Purpose: Create time series plots of the continuous water quality data for the report figures
# Author: Dave Bosworth & Amanda Maguire
# Contact: David.Bosworth@water.ca.gov


# 1. Global Code and Functions -----------------------------------------------

# Load packages
library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)

# Source functions
source("global_ndfa_funcs.R")
source("Water_Quality/global_wq_funcs.R")


# 2. Import and Prepare Data -------------------------------------------------

# Define relative file path for file containing all QA'ed and cleaned continuous WQ data
fp_rel_rtm_data <- "WQ_Subteam/Processed_Data/Continuous/RTM_INPUT_all_2021-02-19.csv"

# Define relative file path for file containing dates of flow action periods
fp_rel_fa_dates <- "Data Management/FlowDatesDesignations_45days.csv"

# Define absolute file paths
fp_abs_rtm_data <- ndfa_abs_sp_path(fp_rel_rtm_data)
fp_abs_fa_dates <- ndfa_abs_sp_path(fp_rel_fa_dates)

# Import continuous WQ data
df_rtm_orig <- import_rtm_data(fp_abs_rtm_data, 10)

# Import dates of flow action periods
df_fa_dates_orig <- read_csv(fp_abs_fa_dates)

# Create a vector for the factor order of StationCode
sta_order <- c(
  "RMB",
  "RCS",
  "RD22",
  "I80",
  "LIS",
  "TOE",
  "STTD",
  "LIBCUT",
  "SGG",
  "LIB",
  "RYI",
  "RVB",
  "SRH"
)

# Clean original continuous WQ data
df_rtm_clean <- df_rtm_orig %>% 
  mutate(
    DateTime = ymd_hms(DateTime),
    Date = date(DateTime),
    Year = year(DateTime)
  ) %>%
  # Don't include Flow, FlowTF, and all Qual variables
  select(-c(ends_with("_Qual"), starts_with("Flow"))) %>% 
  # Exclude SDI from the plots and remove data for years 2011-2012
  filter(
    StationCode != "SDI",
    Year > 2012
  ) %>% 
  # Apply factor order to StationCode
  mutate(StationCode = factor(StationCode, levels = sta_order)) %>% 
  # Pivot parameters in the long format
  pivot_longer(cols = where(is.numeric) & !Year, names_to = "Parameter", values_to = "Value") %>% 
  # Remove all NA values
  filter(!is.na(Value))

# Calculate daily averages of continuous WQ data
df_rtm_daily_avg <- df_rtm_clean %>% 
  group_by(Parameter, Year, StationCode, Date) %>% 
  summarize(Daily_avg = mean(Value)) %>% 
  ungroup()

# Structure continuous WQ data to be able to apply plotting functions
df_rtm_daily_avg_f <- df_rtm_daily_avg %>%
  # Fill in missing dates with NA values for geom_line to not interpolate data gaps
  group_by(Parameter, Year, StationCode) %>% 
  complete(Date = seq.Date(min(Date), max(Date), by = "1 day")) %>% 
  ungroup() %>% 
  # Add a grouping variable for Region and apply factor order for it
  mutate(
    Region = factor(
      case_when(
        StationCode %in% c("RMB", "RCS", "RD22", "I80") ~ "a_Upstream_upper",
        StationCode %in% c("LIS", "TOE", "STTD", "LIBCUT", "SGG") ~ "b_Upstream_central",
        StationCode %in% c("LIB", "RYI", "RVB") ~ "c_Downstream",
        StationCode == "SRH" ~ "d_Sac_River"
      )
    )
  )

# Prepare dates of flow action periods to highlight the flow action periods for each year 
  # in the plots and to define consistent x-axis limits
df_fa_dates_f <- df_fa_dates_orig %>% 
  select(Year, starts_with(c("Pre", "Post"))) %>% 
  mutate(across(where(is.character), mdy)) %>% 
  filter(Year > 2012) %>% 
  # add 1 day to PreFlowEnd so that the highlight for the flow action periods aligns correctly
  mutate(PreFlowEnd = PreFlowEnd + days(1))


# 3. Load Plot Functions -----------------------------------------------------

# Internal function to define y-axis labels in plots
int_define_yaxis_lab <- function(param) {
  yaxis_lab <- case_when(
    param == "Chla" ~ "Chlorophyll (ug/L)",
    param == "DO" ~ "Diss Oxygen (mg/L)",
    param == "fDOM" ~ "fDOM (ug/L as QSE)",
    param == "NitrateNitrite" ~ "Nitrate + Nitrite (mg/L as N)",
    param == "pH" ~ "pH (pH units)",
    param == "SpCnd" ~ "Sp Cond (uS/cm)",
    param == "Turbidity" ~ "Turbidity (FNU)",
    param == "WaterTemp" ~ "Water Temp (C)"
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
create_ts_plot <- function(df, plot_pos = c("other", "bottom")) {
  match.arg(plot_pos, c("other", "bottom"))
  
  # define y-axis label
  y_lab <- int_define_yaxis_lab(df$Parameter[1])
  
  # define dates of flow action period and x-axis limits
  fa_dates <- int_define_fa_dates(df$Year[1])
  
  # define plot label
  plot_title <- int_define_plot_title(df$Region[1])
  
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
  if (plot_pos == "bottom") {
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


# 4. Create Plots ------------------------------------------------------------

# 4.1 Chlorophyll -------------------------------------------------------------

# 2013
chla13 <- df_rtm_daily_avg_f %>% 
  filter(
    Parameter == "Chla",
    Year == 2013
  )

chla13_top <- chla13 %>% 
  filter(str_detect(Region, "^a_")) %>% 
  create_ts_plot("other")

chla13_mid1 <- chla13 %>% 
  filter(str_detect(Region, "^b_")) %>% 
  create_ts_plot("other")

chla13_mid2 <- chla13 %>% 
  filter(str_detect(Region, "^c_")) %>% 
  create_ts_plot("other")

chla13_bottom <- chla13 %>% 
  filter(str_detect(Region, "^d_")) %>% 
  create_ts_plot("bottom")

chla13_top / chla13_mid1 / chla13_mid2 / chla13_bottom
ggsave("Chla_2013.jpg", width = 7, height = 8.5, units = "in", dpi = 300)

# 2016
chla16 <- df_rtm_daily_avg_f %>% 
  filter(
    Parameter == "Chla",
    Year == 2016
  )

chla16_top <- chla16 %>% 
  filter(str_detect(Region, "^a_")) %>% 
  create_ts_plot("other")

chla16_mid1 <- chla16 %>% 
  filter(str_detect(Region, "^b_")) %>% 
  create_ts_plot("other")

chla16_mid2 <- chla16 %>% 
  filter(str_detect(Region, "^c_")) %>% 
  create_ts_plot("other")

chla16_bottom <- chla16 %>% 
  filter(str_detect(Region, "^d_")) %>% 
  create_ts_plot("bottom")

chla16_top / chla16_mid1 / chla16_mid2 / chla16_bottom
ggsave("Chla_2016.jpg", width = 7, height = 8.5, units = "in", dpi = 300)

# 2018
chla18 <- df_rtm_daily_avg_f %>% 
  filter(
    Parameter == "Chla",
    Year == 2018
  )

chla18_top <- chla18 %>% 
  filter(str_detect(Region, "^a_")) %>% 
  create_ts_plot("other")

chla18_mid1 <- chla18 %>% 
  filter(str_detect(Region, "^b_")) %>% 
  create_ts_plot("other")

chla18_mid2 <- chla18 %>% 
  filter(str_detect(Region, "^c_")) %>% 
  create_ts_plot("other")

chla18_bottom <- chla18 %>% 
  filter(str_detect(Region, "^d_")) %>% 
  create_ts_plot("bottom")

chla18_top / chla18_mid1 / chla18_mid2 / chla18_bottom
ggsave("Chla_2018.jpg", width = 7, height = 8.5, units = "in", dpi = 300)

# 2019
chla19 <- df_rtm_daily_avg_f %>% 
  filter(
    Parameter == "Chla",
    Year == 2019
  )

chla19_top <- chla19 %>% 
  filter(str_detect(Region, "^a_")) %>% 
  create_ts_plot("other")

chla19_mid1 <- chla19 %>% 
  filter(str_detect(Region, "^b_")) %>% 
  create_ts_plot("other")

chla19_mid2 <- chla19 %>% 
  filter(str_detect(Region, "^c_")) %>% 
  create_ts_plot("other")

chla19_bottom <- chla19 %>% 
  filter(str_detect(Region, "^d_")) %>% 
  create_ts_plot("bottom")

chla19_top / chla19_mid1 / chla19_mid2 / chla19_bottom
ggsave("Chla_2019.jpg", width = 7, height = 8.5, units = "in", dpi = 300)


# 4.2 Dissolved Oxygen ----------------------------------------------------

# 2015
do15 <- df_rtm_daily_avg_f %>% 
  filter(
    Parameter == "DO",
    Year == 2015
  )

do15_top <- do15 %>% 
  filter(str_detect(Region, "^a_")) %>% 
  create_ts_plot("other")

do15_mid1 <- do15 %>% 
  filter(str_detect(Region, "^b_")) %>% 
  create_ts_plot("other")

do15_mid2 <- do15 %>% 
  filter(str_detect(Region, "^c_")) %>% 
  create_ts_plot("other")

do15_bottom <- do15 %>% 
  filter(str_detect(Region, "^d_")) %>% 
  create_ts_plot("bottom")

do15_top / do15_mid1 / do15_mid2 / do15_bottom
ggsave("DO_2015.jpg", width = 7, height = 8.5, units = "in", dpi = 300)


# 2016
do16 <- df_rtm_daily_avg_f %>% 
  filter(
    Parameter == "DO",
    Year == 2016
  )

do16_top <- do16 %>% 
  filter(str_detect(Region, "^a_")) %>% 
  create_ts_plot("other")

do16_mid1 <- do16 %>% 
  filter(str_detect(Region, "^b_")) %>% 
  create_ts_plot("other")

do16_mid2 <- do16 %>% 
  filter(str_detect(Region, "^c_")) %>% 
  create_ts_plot("other")

do16_bottom <- do16 %>% 
  filter(str_detect(Region, "^d_")) %>% 
  create_ts_plot("bottom")

do16_top / do16_mid1 / do16_mid2 / do16_bottom
ggsave("DO_2016.jpg", width = 7, height = 8.5, units = "in", dpi = 300)

# 2017
do17 <- df_rtm_daily_avg_f %>% 
  filter(
    Parameter == "DO",
    Year == 2017
  )

do17_top <- do17 %>% 
  filter(str_detect(Region, "^a_")) %>% 
  create_ts_plot("other")

do17_mid1 <- do17 %>% 
  filter(str_detect(Region, "^b_")) %>% 
  create_ts_plot("other")

do17_mid2 <- do17 %>% 
  filter(str_detect(Region, "^c_")) %>% 
  create_ts_plot("other")

do17_bottom <- do17 %>% 
  filter(str_detect(Region, "^d_")) %>% 
  create_ts_plot("bottom")

do17_top / do17_mid1 / do17_mid2 / do17_bottom
ggsave("DO_2017.jpg", width = 7, height = 8.5, units = "in", dpi = 300)

# 2019
do19 <- df_rtm_daily_avg_f %>% 
  filter(
    Parameter == "DO",
    Year == 2019
  )

do19_top <- do19 %>% 
  filter(str_detect(Region, "^a_")) %>% 
  create_ts_plot("other")

do19_mid1 <- do19 %>% 
  filter(str_detect(Region, "^b_")) %>% 
  create_ts_plot("other")

do19_mid2 <- do19 %>% 
  filter(str_detect(Region, "^c_")) %>% 
  create_ts_plot("other")

do19_bottom <- do19 %>% 
  filter(str_detect(Region, "^d_")) %>% 
  create_ts_plot("bottom")

do19_top / do19_mid1 / do19_mid2 / do19_bottom
ggsave("DO_2019.jpg", width = 7, height = 8.5, units = "in", dpi = 300)


# 4.3 Turbidity -----------------------------------------------------------

# 2015
turb15 <- df_rtm_daily_avg_f %>% 
  filter(
    Parameter == "Turbidity",
    Year == 2015
  )

turb15_top <- turb15 %>% 
  filter(str_detect(Region, "^a_")) %>% 
  create_ts_plot("other")

turb15_mid1 <- turb15 %>% 
  filter(str_detect(Region, "^b_")) %>% 
  create_ts_plot("other")

turb15_mid2 <- turb15 %>% 
  filter(str_detect(Region, "^c_")) %>% 
  create_ts_plot("other")

turb15_bottom <- turb15 %>% 
  filter(str_detect(Region, "^d_")) %>% 
  create_ts_plot("bottom")

turb15_top / turb15_mid1 / turb15_mid2 / turb15_bottom
ggsave("Turbidity_2015.jpg", width = 7, height = 8.5, units = "in", dpi = 300)

# 2017
turb17 <- df_rtm_daily_avg_f %>% 
  filter(
    Parameter == "Turbidity",
    Year == 2017
  )

turb17_mid1 <- turb17 %>% 
  filter(str_detect(Region, "^b_")) %>% 
  create_ts_plot("other")

turb17_mid2 <- turb17 %>% 
  filter(str_detect(Region, "^c_")) %>% 
  create_ts_plot("other")

turb17_bottom <- turb17 %>% 
  filter(str_detect(Region, "^d_")) %>% 
  create_ts_plot("bottom")

turb17_mid1 / turb17_mid2 / turb17_bottom
ggsave("Turbidity_2017.jpg", width = 7, height = 8.5, units = "in", dpi = 300)

# 2018
turb18 <- df_rtm_daily_avg_f %>% 
  filter(
    Parameter == "Turbidity",
    Year == 2018
  )

turb18_mid1 <- turb18 %>% 
  filter(str_detect(Region, "^b_")) %>% 
  create_ts_plot("other")

turb18_mid2 <- turb18 %>% 
  filter(str_detect(Region, "^c_")) %>% 
  create_ts_plot("other")

turb18_bottom <- turb18 %>% 
  filter(str_detect(Region, "^d_")) %>% 
  create_ts_plot("bottom")

turb18_mid1 / turb18_mid2 / turb18_bottom
ggsave("Turbidity_2018.jpg", width = 7, height = 8.5, units = "in", dpi = 300)

# 2019
turb19 <- df_rtm_daily_avg_f %>% 
  filter(
    Parameter == "Turbidity",
    Year == 2019
  )

turb19_top <- turb19 %>% 
  filter(str_detect(Region, "^a_")) %>% 
  create_ts_plot("other")

turb19_mid1 <- turb19 %>% 
  filter(str_detect(Region, "^b_")) %>% 
  create_ts_plot("other")

turb19_mid2 <- turb19 %>% 
  filter(str_detect(Region, "^c_")) %>% 
  create_ts_plot("other")

turb19_bottom <- turb19 %>% 
  filter(str_detect(Region, "^d_")) %>% 
  create_ts_plot("bottom")

turb19_top / turb19_mid1 / turb19_mid2 / turb19_bottom
ggsave("Turbidity_2019.jpg", width = 7, height = 8.5, units = "in", dpi = 300)


# 4.4 fDOM ----------------------------------------------------------------

# 2018
fdom18 <- df_rtm_daily_avg_f %>% 
  filter(
    Parameter == "fDOM",
    Year == 2018
  )

fdom18_mid1 <- fdom18 %>% 
  filter(str_detect(Region, "^b_")) %>% 
  create_ts_plot("other")

fdom18_mid2 <- fdom18 %>% 
  filter(str_detect(Region, "^c_")) %>% 
  create_ts_plot("bottom")

fdom18_mid1 / fdom18_mid2
ggsave("fDOM_2018.jpg", width = 7, height = 8.5, units = "in", dpi = 300)

# 2019
fdom19 <- df_rtm_daily_avg_f %>% 
  filter(
    Parameter == "fDOM",
    Year == 2019
  )

fdom19_mid1 <- fdom19 %>% 
  filter(str_detect(Region, "^b_")) %>% 
  create_ts_plot("other")

fdom19_mid2 <- fdom19 %>% 
  filter(str_detect(Region, "^c_")) %>% 
  create_ts_plot("bottom")

fdom19_mid1 / fdom19_mid2
ggsave("fDOM_2019.jpg", width = 7, height = 8.5, units = "in", dpi = 300)


# 4.5 Nitrate plus Nitrite ------------------------------------------------

# 2014
nitr14 <- df_rtm_daily_avg_f %>% 
  filter(
    Parameter == "NitrateNitrite",
    Year == 2014
  )

create_ts_plot(nitr14, "bottom")
ggsave("NitrateNitrite_2014.jpg", width = 7, height = 3, units = "in", dpi = 300)

# 2015
nitr15 <- df_rtm_daily_avg_f %>% 
  filter(
    Parameter == "NitrateNitrite",
    Year == 2015
  )

create_ts_plot(nitr15, "bottom")
ggsave("NitrateNitrite_2015.jpg", width = 7, height = 3, units = "in", dpi = 300)

# 2016
nitr16 <- df_rtm_daily_avg_f %>% 
  filter(
    Parameter == "NitrateNitrite",
    Year == 2016
  )

create_ts_plot(nitr16, "bottom")
ggsave("NitrateNitrite_2016.jpg", width = 7, height = 3, units = "in", dpi = 300)

# 2018
nitr18 <- df_rtm_daily_avg_f %>% 
  filter(
    Parameter == "NitrateNitrite",
    Year == 2018
  )

nitr18_top <- nitr18 %>% 
  filter(str_detect(Region, "^b_")) %>% 
  create_ts_plot("other")

nitr18_bottom <- nitr18 %>% 
  filter(str_detect(Region, "^c_")) %>% 
  create_ts_plot("bottom")

nitr18_top / nitr18_bottom
ggsave("NitrateNitrite_2018.jpg", width = 7, height = 6, units = "in", dpi = 300)

# 2019
nitr19 <- df_rtm_daily_avg_f %>% 
  filter(
    Parameter == "NitrateNitrite",
    Year == 2019
  )

nitr19_top <- nitr19 %>% 
  filter(str_detect(Region, "^b_")) %>% 
  create_ts_plot("other")

nitr19_bottom <- nitr19 %>% 
  filter(str_detect(Region, "^c_")) %>% 
  create_ts_plot("bottom")

nitr19_top / nitr19_bottom
ggsave("NitrateNitrite_2019.jpg", width = 7, height = 6, units = "in", dpi = 300)

