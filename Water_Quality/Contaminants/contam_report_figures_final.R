# NDFA Water Quality
# Purpose: Create figures of the contaminants data for the NDFA Synthesis report:
  # 1) Stacked barplots showing the average of total concentrations for water and zooplankton by site
    # from 2016-2019 grouped by pesticide type
  # 2) Barplots showing the percent of samples that exceed EPA benchmarks for aquatic life, facetted
    # by year and benchmark category
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov


# 1. Global Code and Functions -----------------------------------------------

# Load packages
library(tidyverse)
library(lubridate)
library(scales)

# Source functions
source("global_ndfa_funcs.R")


# 2. Import Data -------------------------------------------------------------

# Define relative file path for processed contaminants data files
fp_rel_proc_data <- "WQ_Subteam/Processed_Data/Contaminants"

# Define relative file path for data file with counts of EPA benchmark exceedances
fp_rel_tox_data <- "WQ_Subteam/Analysis_Results/Contaminants/chronic_fish_toxicity.csv"

# Define absolute file paths
fp_abs_proc_data <- ndfa_abs_sp_path(fp_rel_proc_data)
fp_abs_tox_data <- ndfa_abs_sp_path(fp_rel_tox_data)

# Import contaminants concentrations in water data
contam_water_orig <- read_csv(file.path(fp_abs_proc_data, "WQ_INPUT_Contam_Water_2021-02-11.csv"))

# Import contaminants concentrations in zooplankton data
contam_zoop_orig <- read_csv(
  file.path(fp_abs_proc_data, "WQ_INPUT_Contam_Zoop_2021-02-11.csv"),
  col_types = "cccccdc"
)

# Import counts of EPA benchmark exceedances
epa_exceed_orig <- read_csv(fp_abs_tox_data)

# Import pesticide type and class designations
contam_type_class <- read_csv(file.path(fp_abs_proc_data, "Contam_Types_and_Classes.csv"))


# 3. Clean Data ---------------------------------------------------------

# Water data:
contam_water_clean <- contam_water_orig %>% 
  # Parse DateTime variable, add Date and Year variables
  mutate(
    DateTime = ymd_hms(DateTime),
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

# Zooplankton data:
contam_zoop_clean <- contam_zoop_orig %>% 
  # Parse DateTime variable, add Date and Year variables
  mutate(
    DateTime = ymd_hms(DateTime),
    Date = date(DateTime),
    Year = year(DateTime)
  ) %>% 
  # Add flow pulse periods to limit data to pulse periods and 45 days before and after
  ndfa_action_periods() %>%
  # Add pesticide types
  left_join(contam_type_class) %>% 
  # Clean up data frame
  select(
    StationCode,
    Date,
    Analyte,
    PesticideType,
    Result
  ) %>% 
  # Convert < MDL values to zero and the Result variable to numeric
  mutate(Result = as.numeric(if_else(str_detect(Result, "^<"), "0", Result)))

# Counts of EPA benchmark exceedances:
epa_exceed_clean <- epa_exceed_orig %>% 
  rename(FlowActionPeriod = FlowPeriod) %>% 
  pivot_longer(
    cols = N_pesticides_chronic_fish:N_pesticides_acute_invert, 
    names_to = "Benchmark", 
    values_to = "Exceedances"
  )


# 4. Make Calculations for Plots ------------------------------------------

# Sum concentrations in water by Pesticide Type and then average them over 
  # the study period (2016-2019) grouped by site
contam_water_sum_type <- contam_water_clean %>% 
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
contam_zoop_sum_type <- contam_zoop_clean %>% 
  group_by(StationCode, PesticideType, Date) %>% 
  summarize(TotalConc = sum(Result)) %>% 
  summarize(TotalConc_avg = mean(TotalConc)) %>% 
  ungroup() %>% 
  # Remove any averages equal to zero to only include Pesticide Types with detected values
  filter(TotalConc_avg > 0) %>% 
  # Apply plotting order for stations
  mutate(StationCode = factor(StationCode, levels = c("STTD", "SHR")))

# Count the number of water samples collected by year and flow pulse period
contam_water_n_samples <- contam_water_clean %>% count(Year, FlowActionPeriod)

# Calculate the percent of water samples that exceed EPA benchmarks 
  # grouped by year and flow pulse period
epa_exceed_perc <- epa_exceed_clean %>% 
  left_join(contam_water_n_samples) %>% 
  mutate(Perc_Exceedances = Exceedances/n) %>% 
  # Apply plotting order for flow pulse period
  mutate(FlowActionPeriod = factor(FlowActionPeriod, levels = c("Before", "During", "After")))


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

# Barplots of the percent of samples that exceed EPA benchmarks
# Create a named vector for the Benchmark facet labels
benchmark_label <- c(
  "N_pesticides_acute_fish" = "A) Acute Fish",
  "N_pesticides_acute_invert" = "B) Acute Invertebrates",
  "N_pesticides_chronic_fish" = "C) Chronic Fish",
  "N_pesticides_chronic_invert" = "D) Chronic Invertebrates"
)

plt_epa_exceed_perc <- epa_exceed_perc %>% 
  ggplot(aes(FlowActionPeriod, Perc_Exceedances)) +
  geom_col() +
  facet_grid(
    cols = vars(Benchmark), 
    rows = vars(Year),
    labeller = labeller(Benchmark = benchmark_label)
  ) +
  theme_light() +
  theme(
    strip.text = element_text(color = "black"),
    panel.grid.minor = element_blank()
  ) +
  xlab("Flow Pulse Period") +
  scale_y_continuous(name = "Percent of samples with exceedances", labels = label_percent())


# 6. Export Plots ---------------------------------------------------------

# Define file path to export plots to
fp_abs_contam_plt <- ndfa_abs_sp_path("WQ_Subteam/Plots/Contaminants/Report")

# Export Plots
ggsave(
  file.path(fp_abs_contam_plt, "contam_water_barplot.jpg"),
  plot = plt_water_stack, 
  width = 6.5, 
  height = 3, 
  units = "in", 
  dpi = 300
)

ggsave(
  file.path(fp_abs_contam_plt, "contam_zoop_barplot.jpg"),
  plot = plt_zoop_stack, 
  width = 5.5, 
  height = 3, 
  units = "in", 
  dpi = 300
)

ggsave(
  file.path(fp_abs_contam_plt, "contam_epa_exceed.jpg"),
  plot = plt_epa_exceed_perc, 
  width = 7, 
  height = 7, 
  units = "in", 
  dpi = 300
)

