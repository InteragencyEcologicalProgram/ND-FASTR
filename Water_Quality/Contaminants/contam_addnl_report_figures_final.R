# NDFA Water Quality
# Purpose: Create additional figures of the contaminants data for the NDFA
# Synthesis report:
# Stacked barplots showing the average of total concentrations for water and zooplankton by site
  # from 2016-2019 grouped by pesticide type
# The code for all other contaminants figures used in the Synthesis report are in the 
  # ND-FASTR/Water_Quality/Contaminants/Analyses/Contaminants_analysis.R script
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov


# 1. Global Code and Functions -----------------------------------------------

# Load packages
library(tidyverse)
library(lubridate)
library(scales)
library(here)

# Source functions
source(here("global_ndfa_funcs.R"))

# Define file path in the repository for figure and table outputs
fp_output <- here("Water_Quality/Output_Report")


# 2. Import Data -------------------------------------------------------------

# Import contaminants data
load(here("Water_Quality/Data_Processed/contam_proc_data.RData"))


# 3. Clean Data ---------------------------------------------------------

# Water and zooplankton data:
contam_clean <- 
  list(
    water = contam_water,
    zoop = contam_zoop
  ) %>% 
  map(
    # Add Date and Year variables
    ~ mutate(
      .x,
      Date = date(DateTime),
      Year = year(DateTime)
    ) %>% 
    # Add flow pulse periods
    ndfa_action_periods() %>% 
    # Add pesticide types
    left_join(contam_type_class) %>% 
    # Clean up data frame
    select(
      StationCode,
      Date,
      Year,
      FlowActionPeriod,
      Analyte,
      PesticideType,
      Result
    ) %>% 
    # Convert < MDL values to zero and the Result variable to numeric
    mutate(Result = as.numeric(if_else(str_detect(Result, "^<"), "0", Result)))
  )


# 4. Make Calculations for Plots ------------------------------------------

# Sum concentrations in water by Pesticide Type and then average them over 
  # the study period (2016-2019) grouped by site
contam_water_sum_type <- contam_clean$water %>% 
  # Remove data for 2015 and only include RD22, BL5, and SHR
  filter(
    Year != 2015,
    StationCode %in% c("RD22", "BL5", "SHR")
  ) %>%
  group_by(StationCode, PesticideType, Date) %>% 
  summarize(TotalConc = sum(Result)) %>% 
  summarize(TotalConc_avg = mean(TotalConc)) %>% 
  ungroup() %>% 
  # Remove any averages equal to zero to only include Pesticide Types with detected values
  filter(TotalConc_avg > 0) %>% 
  # Apply plotting order for stations
  mutate(StationCode = factor(StationCode, levels = c("RD22", "BL5", "SHR")))

# Sum concentrations in zooplankton by Pesticide Type and then average them over 
  # the study period (2017-2019) grouped by site
contam_zoop_sum_type <- contam_clean$zoop %>% 
  group_by(StationCode, PesticideType, Date) %>% 
  summarize(TotalConc = sum(Result)) %>% 
  summarize(TotalConc_avg = mean(TotalConc)) %>% 
  ungroup() %>% 
  # Remove any averages equal to zero to only include Pesticide Types with detected values
  filter(TotalConc_avg > 0) %>% 
  # Apply plotting order for stations
  mutate(StationCode = factor(StationCode, levels = c("STTD", "SHR")))


# 5. Create Plots ---------------------------------------------------------

# Stacked barplot of water data
plt_water_stack <- contam_water_sum_type %>% 
  ggplot(aes(StationCode, TotalConc_avg, fill = PesticideType)) +
  geom_col() +
  theme_light() +
  theme(strip.text = element_text(color = "black")) +
  xlab("Station") +
  scale_y_continuous(name = "Average Total Concentration (ng/L)", labels = label_comma()) +
  scale_fill_viridis_d(name = "Pesticide Type:", option = "plasma") 

# Stacked barplot of zooplankton data
plt_zoop_stack <- contam_zoop_sum_type %>% 
  ggplot(aes(StationCode, TotalConc_avg, fill = PesticideType)) +
  geom_col() +
  theme_light() +
  theme(strip.text = element_text(color = "black")) +
  xlab("Station") +
  scale_y_continuous(name = "Average Total Concentration (ng/g)", labels = label_comma()) +
  scale_fill_viridis_d(name = "Pesticide Type:", option = "plasma") 


# 6. Export Plots ---------------------------------------------------------

# Export Plots
ggsave(
  file.path(fp_output, "contaminants_water_barplot.jpg"),
  plot = plt_water_stack, 
  width = 6.5, 
  height = 3, 
  units = "in", 
  dpi = 300
)

ggsave(
  file.path(fp_output, "contaminants_zoop_barplot.jpg"),
  plot = plt_zoop_stack, 
  width = 5.5, 
  height = 3, 
  units = "in", 
  dpi = 300
)

