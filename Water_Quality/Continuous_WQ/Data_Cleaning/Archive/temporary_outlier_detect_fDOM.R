# fDOM - draft outlier detection methods

library(tidyverse)
library(plotly)
library(ODWGtools)

source("global_ndfa_funcs.R")
source("Water_Quality/global_wq_funcs.R")
source("Water_Quality/Continuous_WQ/Data_Cleaning/rtm_test_outlier_detect_funcs.R")

# Import example data - STTD from 2017
fp_abs_rtm <- ndfa_abs_sp_path("WQ_Subteam/Processed_Data/Continuous/Filtered_Dates")

lib_orig <- import_rtm_data(file.path(fp_abs_rtm, "RTM_OUTPUT_LIB_formatted_filt.csv"), 11)

lib_fdom_2018 <- lib_orig %>% 
  mutate(DateTime = ymd_hms(DateTime)) %>% 
  filter(year(DateTime) == 2018) %>% 
  select(DateTime, fDOM) %>% 
  complete(DateTime = seq.POSIXt(min(DateTime), max(DateTime), by = "15 min"))

# Spike test - fixed thresholds c(7.5, 15)
lib_fdom_2018_spike <- lib_fdom_2018 %>% 
  mutate(qual_spike_fixed = rtqc_spike(fDOM, c(7.5, 15))) %>% 
  replace_na(list(qual_spike_fixed = "pass"))

p <- lib_fdom_2018_spike %>% 
  ggplot(aes(DateTime, fDOM, color = qual_spike_fixed)) +
  geom_point() +
  scale_color_manual(values = c("pass" = "black", "suspect" = "green", "fail" = "orange"))

ggplotly(p)

# % increase from prior value - use 50% for now, questionable though
lib_fdom_2018_perc_incr <- lib_fdom_2018 %>% 
  mutate(
    qual_incr = outlier_test_factor(
      case_when(
        is.na(fDOM) | is.na(lag(fDOM)) | is.na(lead(fDOM)) ~ NA_integer_,
        (fDOM - lag(fDOM))/lag(fDOM) > 0.5 ~ 4L,
        TRUE ~ 1L
      )
    )
  ) %>% 
  replace_na(list(qual_incr = "pass"))

p <- lib_fdom_2018_perc_incr %>% 
  ggplot(aes(DateTime, fDOM, color = qual_incr)) +
  geom_point() +
  scale_color_manual(values = c("pass" = "black", "suspect" = "green", "fail" = "orange"))

ggplotly(p)

# % decrease from prior value - use 50% for now
lib_fdom_2018_perc_decr <- lib_fdom_2018 %>% 
  mutate(
    qual_decr = outlier_test_factor(
      case_when(
        is.na(fDOM) | is.na(lag(fDOM)) | is.na(lead(fDOM)) ~ NA_integer_,
        (fDOM - lag(fDOM))/lag(fDOM) < -0.25 ~ 4L,
        TRUE ~ 1L
      )
    )
  ) %>% 
  replace_na(list(qual_decr = "pass"))

p <- lib_fdom_2018_perc_decr %>% 
  ggplot(aes(DateTime, fDOM, color = qual_decr)) +
  geom_point() +
  scale_color_manual(values = c("pass" = "black", "suspect" = "green", "fail" = "orange"))

ggplotly(p)

# Rate of change test - using 3 standard deviations, SD is of the prior 25-hour period
lib_fdom_2018_rate <- lib_fdom_2018 %>% 
  mutate(qual_rate = rtqc_rate(fDOM, 3, 100)) %>% 
  replace_na(list(qual_rate = "pass"))

p <- lib_fdom_2018_rate %>% 
  ggplot(aes(DateTime, fDOM, color = qual_rate)) +
  geom_point() +
  scale_color_manual(values = c("pass" = "black", "suspect" = "green", "fail" = "orange"))

ggplotly(p)

# majority scoring - 2 or more tests are suspect or fail
lib_fdom_2018_maj <- lib_fdom_2018 %>% 
  mutate(
    qual_spike_fixed = rtqc_spike(fDOM, c(7.5, 15)),
    qual_incr = outlier_test_factor(
      case_when(
        is.na(fDOM) | is.na(lag(fDOM)) | is.na(lead(fDOM)) ~ NA_integer_,
        (fDOM - lag(fDOM))/lag(fDOM) > 0.5 ~ 4L,
        TRUE ~ 1L
      )
    ),
    qual_decr = outlier_test_factor(
      case_when(
        is.na(fDOM) | is.na(lag(fDOM)) | is.na(lead(fDOM)) ~ NA_integer_,
        (fDOM - lag(fDOM))/lag(fDOM) < -0.25 ~ 4L,
        TRUE ~ 1L
      )
    ),
    qual_rate = rtqc_rate(fDOM, 3, 100)
  ) %>% 
  replace_na(
    list(
      qual_spike_fixed = "pass",
      qual_incr = "pass",
      qual_decr = "pass",
      qual_rate = "pass"
    )
  ) %>% 
  mutate(across(where(is.factor), ~if_else(.x == "pass", 0L, 1L))) %>% 
  mutate(
    qual_total = rowSums(across(starts_with("qual"))),
    qual_majority = outlier_test_factor(if_else(qual_total > 1, 4L, 1L))
  )

p <- lib_fdom_2018_maj %>% 
  ggplot(aes(DateTime, fDOM, color = qual_majority)) +
  geom_point() +
  scale_color_manual(values = c("pass" = "black", "suspect" = "green", "fail" = "orange"))

ggplotly(p)

