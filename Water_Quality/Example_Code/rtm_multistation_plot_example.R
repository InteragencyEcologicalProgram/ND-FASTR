# NDFA Water Quality
# Purpose: Example code for working with continuous water quality data
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
library(scales)

# Load functions
source("global_ndfa_funcs.R")
source("Water_Quality/global_wq_funcs.R")

# Define relative file path for final RTM data
fp_rel_proc <- "WQ_Subteam/Processed_Data/Continuous/RTM_INPUT_all_2021-02-19.csv"

# Define absolute file path
fp_abs_proc <- ndfa_abs_sp_path(fp_rel_proc)

# Import RTM data
rtm_orig <- import_rtm_data(fp_abs_proc, 10)

# Define plot order for StationCode
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
  "SDI",
  "SRH"
)

# Prepare data - 2014 Turbidity
rtm_clean <- rtm_orig %>% 
  mutate(DateTime = ymd_hms(DateTime)) %>% 
  filter(
    year(DateTime) == 2014,
    !is.na(Turbidity)
  ) %>% 
  mutate(StationCode = factor(StationCode, levels = sta_order)) %>% 
  select(StationCode, DateTime, Turbidity)

# Define flow action duration for 2014
action_min <- as_datetime("2014-09-09 00:00:00")
action_max <- as_datetime("2014-09-24 00:00:00")

# Create multi-station plot
rtm_clean %>% 
  ggplot(aes(x = DateTime, y = Turbidity)) +
  geom_point(
    shape = "circle open",
    size = 1,
    color = "gray30",
    alpha = 0.3
  ) +
  geom_smooth(
    formula = y ~ x, 
    method = "loess", 
    span = 0.1, 
    se = FALSE,
    color = "blue"
  ) +
  facet_grid(rows = vars(StationCode)) +
  ylab("Turbidity (FNU)") +
  scale_x_datetime(
    name = "Date",
    breaks = breaks_pretty(15),
    labels = label_date_short()
  ) +
  theme_light() +
  theme(
    strip.text = element_text(color = "black"),
    panel.grid.minor = element_blank()
  ) +
  annotate(
    "rect", 
    xmin = action_min, 
    xmax = action_max, 
    ymin = -Inf, 
    ymax = Inf, 
    alpha = 0.15,
    fill = "firebrick"
  )

ggsave("example.jpg", width = 7, height = 9.5, units = "in", dpi = 300)

