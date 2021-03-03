<<<<<<< HEAD
# NDFA Water Quality - Continuous Specific Conductivity
=======
# NDFA Water Quality - Continuous Conductivity
>>>>>>> 9456140a683c026eb0e35b4bfa9c6a6f786c1104
# Purpose: Create time series plot using continuous water quality data
# Author: Dave Bosworth & Amanda Maguire
# Contact: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
library(lubridate)
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
<<<<<<< HEAD
  "SRH"
)

# Prepare data - All years Conductivity
rtm_clean_spc <- rtm_orig %>% 
  mutate(DateTime = ymd_hms(DateTime)) %>% 
  filter(
    !StationCode == "SDI",
=======
  "SDI",
  "SRH"
)

# Prepare data - 2013 Conductivity
rtm_clean_spc_2013 <- rtm_orig %>% 
  mutate(DateTime = ymd_hms(DateTime)) %>% 
  filter(
    year(DateTime) == 2013,
    #!StationCode == "SDI",
>>>>>>> 9456140a683c026eb0e35b4bfa9c6a6f786c1104
    !is.na(SpCnd)
  ) %>% 
  mutate(StationCode = factor(StationCode, levels = sta_order)) %>% 
  select(StationCode, DateTime, SpCnd)

# Define flow action duration for 2013
<<<<<<< HEAD
rtm_clean_spc_2013 <- rtm_clean_spc %>%
  filter(year(DateTime) == 2013)
=======
>>>>>>> 9456140a683c026eb0e35b4bfa9c6a6f786c1104
action_min <- as_datetime("2013-08-22 00:00:00")
action_max <- as_datetime("2013-10-03 00:00:00")

# Create multi-station plot
rtm_clean_spc_2013 %>% 
  ggplot(aes(x = DateTime, y = SpCnd)) +
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
  ylab("Specific Conductivity (uS/cm)") +
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

ggsave("SpCnd_2013.jpg", width = 7, height = 9.5, units = "in", dpi = 300)

<<<<<<< HEAD
## Define flow action duration for 2014
rtm_clean_spc_2014 <- rtm_clean_spc %>%
  filter(year(DateTime) == 2014)
=======
# Prepare data - 2014 Conductivity
rtm_clean_spc_2014 <- rtm_orig %>% 
  mutate(DateTime = ymd_hms(DateTime)) %>% 
  filter(
    year(DateTime) == 2014,
    #!StationCode == "SDI", !StationCode == "RVB",
    !is.na(SpCnd)
  ) %>% 
  mutate(StationCode = factor(StationCode, levels = sta_order)) %>% 
  select(StationCode, DateTime, SpCnd)

# Define flow action duration for 2014
>>>>>>> 9456140a683c026eb0e35b4bfa9c6a6f786c1104
action_min <- as_datetime("2014-09-09 00:00:00")
action_max <- as_datetime("2014-09-24 00:00:00")

# Create multi-station plot
rtm_clean_spc_2014 %>% 
  ggplot(aes(x = DateTime, y = SpCnd)) +
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
  ylab("Specific Conductivity (uS/cm)") +
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

ggsave("SpCnd_2014.jpg", width = 7, height = 9.5, units = "in", dpi = 300)

<<<<<<< HEAD
# Define flow action duration for 2015
rtm_clean_spc_2015 <- rtm_clean_spc %>%
  filter(year(DateTime) == 2015)
=======
# Prepare data - 2015 Conductivity
rtm_clean_spc_2015 <- rtm_orig %>% 
  mutate(DateTime = ymd_hms(DateTime)) %>% 
  filter(
    year(DateTime) == 2015,
    #!StationCode == "SDI", !StationCode == "RVB",
    !is.na(SpCnd)
  ) %>% 
  mutate(StationCode = factor(StationCode, levels = sta_order)) %>% 
  select(StationCode, DateTime, SpCnd)

# Define flow action duration for 2015
>>>>>>> 9456140a683c026eb0e35b4bfa9c6a6f786c1104
action_min <- as_datetime("2015-08-21 00:00:00")
action_max <- as_datetime("2015-10-02 00:00:00")

# Create multi-station plot
rtm_clean_spc_2015 %>% 
  ggplot(aes(x = DateTime, y = SpCnd)) +
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
  ylab("Specific Conductivity (uS/cm)") +
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

ggsave("SpCnd_2015.jpg", width = 7, height = 9.5, units = "in", dpi = 300)

<<<<<<< HEAD
# Define flow action duration for 2016
rtm_clean_spc_2016 <- rtm_clean_spc %>%
  filter(year(DateTime) == 2016)
=======
# Prepare data - 2016 Conductivity
rtm_clean_spc_2016 <- rtm_orig %>% 
  mutate(DateTime = ymd_hms(DateTime)) %>% 
  filter(
    year(DateTime) == 2016,
    !is.na(SpCnd)
  ) %>% 
  mutate(StationCode = factor(StationCode, levels = sta_order)) %>% 
  select(StationCode, DateTime, SpCnd)

# Define flow action duration for 2016
>>>>>>> 9456140a683c026eb0e35b4bfa9c6a6f786c1104
action_min <- as_datetime("2016-07-14 00:00:00")
action_max <- as_datetime("2016-08-02 00:00:00")

# Create multi-station plot
rtm_clean_spc_2016 %>% 
  ggplot(aes(x = DateTime, y = SpCnd)) +
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
  ylab("Specific Conductivity (uS/cm)") +
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

ggsave("SpCnd_2016.jpg", width = 7, height = 9.5, units = "in", dpi = 300)

<<<<<<< HEAD
# Define flow action duration for 2017
rtm_clean_spc_2017 <- rtm_clean_spc %>%
  filter(year(DateTime) == 2017)
=======
# Prepare data - 2017 Conductivity
rtm_clean_spc_2017 <- rtm_orig %>% 
  mutate(DateTime = ymd_hms(DateTime)) %>% 
  filter(
    year(DateTime) == 2017,
    !is.na(SpCnd)
  ) %>% 
  mutate(StationCode = factor(StationCode, levels = sta_order)) %>% 
  select(StationCode, DateTime, SpCnd)

# Define flow action duration for 2017
>>>>>>> 9456140a683c026eb0e35b4bfa9c6a6f786c1104
action_min <- as_datetime("2017-08-29 00:00:00")
action_max <- as_datetime("2017-09-19 00:00:00")

# Create multi-station plot
rtm_clean_spc_2017 %>% 
  ggplot(aes(x = DateTime, y = SpCnd)) +
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
  ylab("Specific Conductivity (uS/cm)") +
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

ggsave("SpCnd_2017.jpg", width = 7, height = 9.5, units = "in", dpi = 300)

<<<<<<< HEAD
# Define flow action duration for 2018
rtm_clean_spc_2018 <- rtm_clean_spc %>%
  filter(year(DateTime) == 2018)
=======
# Prepare data - 2018 Conductivity
rtm_clean_spc_2018 <- rtm_orig %>% 
  mutate(DateTime = ymd_hms(DateTime)) %>% 
  filter(
    year(DateTime) == 2018,
    #!StationCode == "SDI",
    !is.na(SpCnd)
  ) %>% 
  mutate(StationCode = factor(StationCode, levels = sta_order)) %>% 
  select(StationCode, DateTime, SpCnd)

# Define flow action duration for 2018
>>>>>>> 9456140a683c026eb0e35b4bfa9c6a6f786c1104
action_min <- as_datetime("2018-08-28 00:00:00")
action_max <- as_datetime("2018-09-27 00:00:00")

# Create multi-station plot
rtm_clean_spc_2018 %>% 
  ggplot(aes(x = DateTime, y = SpCnd)) +
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
  ylab("Specific Conductivity (uS/cm)") +
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

ggsave("SpCnd_2018.jpg", width = 7, height = 9.5, units = "in", dpi = 300)

<<<<<<< HEAD
# Define flow action duration for 2019
rtm_clean_spc_2019 <- rtm_clean_spc %>%
  filter(year(DateTime) == 2019)
=======
# Prepare data - 2019 Conductivity
rtm_clean_spc_2019 <- rtm_orig %>% 
  mutate(DateTime = ymd_hms(DateTime)) %>% 
  filter(
    year(DateTime) == 2019,
    #!StationCode == "SDI",
    !is.na(SpCnd)
  ) %>% 
  mutate(StationCode = factor(StationCode, levels = sta_order)) %>% 
  select(StationCode, DateTime, SpCnd)

# Define flow action duration for 2019
>>>>>>> 9456140a683c026eb0e35b4bfa9c6a6f786c1104
action_min <- as_datetime("2019-08-26 00:00:00")
action_max <- as_datetime("2019-09-22 00:00:00")

# Create multi-station plot
rtm_clean_spc_2019 %>% 
  ggplot(aes(x = DateTime, y = SpCnd)) +
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
  ylab("Specific Conductivity (uS/cm)") +
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

ggsave("SpCnd_2019.jpg", width = 7, height = 9.5, units = "in", dpi = 300)
