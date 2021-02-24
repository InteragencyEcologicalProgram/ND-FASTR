# Turbidity - draft outlier detection methods

library(tidyverse)
library(plotly)
library(ODWGtools)

source("global_ndfa_funcs.R")
source("Water_Quality/global_wq_funcs.R")
source("Water_Quality/Continuous_WQ/Data_Cleaning/rtm_test_outlier_detect_funcs.R")

# Import example data - STTD from 2017
fp_abs_rtm <- ndfa_abs_sp_path("WQ_Subteam/Processed_Data/Continuous/Filtered_Dates")

sttd_orig <- import_rtm_data(file.path(fp_abs_rtm, "RTM_OUTPUT_STTD_formatted_filt.csv"), 6)

sttd_turb_2017 <- sttd_orig %>% 
  mutate(DateTime = ymd_hms(DateTime)) %>% 
  filter(year(DateTime) == 2017) %>% 
  select(DateTime, Turbidity) %>% 
  complete(DateTime = seq.POSIXt(min(DateTime), max(DateTime), by = "15 min"))

# Spike test - fixed thresholds c(20, 40)
sttd_turb_2017_spike <- sttd_turb_2017 %>% 
  mutate(qual_spike_fixed = rtqc_spike(Turbidity, c(20, 40))) %>% 
  replace_na(list(qual_spike_fixed = "pass"))

p <- sttd_turb_2017_spike %>% 
  ggplot(aes(DateTime, Turbidity, color = qual_spike_fixed)) +
  geom_point() +
  scale_color_manual(values = c("pass" = "black", "suspect" = "green", "fail" = "orange"))

ggplotly(p)

# % increase from prior value - use 100% for now, questionable though
sttd_turb_2017_perc_incr <- sttd_turb_2017 %>% 
  mutate(
    qual_incr = outlier_test_factor(
      case_when(
        is.na(Turbidity) | is.na(lag(Turbidity)) | is.na(lead(Turbidity)) ~ NA_integer_,
        (Turbidity - lag(Turbidity))/lag(Turbidity) > 1 ~ 4L,
        TRUE ~ 1L
      )
    )
  ) %>% 
  replace_na(list(qual_incr = "pass"))

p <- sttd_turb_2017_perc_incr %>% 
  ggplot(aes(DateTime, Turbidity, color = qual_incr)) +
  geom_point() +
  scale_color_manual(values = c("pass" = "black", "suspect" = "green", "fail" = "orange"))

ggplotly(p)

# Rate of change test - using 3 standard deviations, SD is of the prior 25-hour period
sttd_turb_2017_rate <- sttd_turb_2017 %>% 
  mutate(qual_rate = rtqc_rate(Turbidity, 3, 100)) %>% 
  replace_na(list(qual_rate = "pass"))

p <- sttd_turb_2017_rate %>% 
  ggplot(aes(DateTime, Turbidity, color = qual_rate)) +
  geom_point() +
  scale_color_manual(values = c("pass" = "black", "suspect" = "green", "fail" = "orange"))

ggplotly(p)

# majority scoring - 2 or more tests are suspect or fail, value is greater than 50 NTU
sttd_turb_2017_maj <- sttd_turb_2017 %>% 
  mutate(
    qual_spike_fixed = rtqc_spike(Turbidity, c(20, 40)),
    qual_incr = outlier_test_factor(
      case_when(
        is.na(Turbidity) | is.na(lag(Turbidity)) | is.na(lead(Turbidity)) ~ NA_integer_,
        (Turbidity - lag(Turbidity))/lag(Turbidity) > 1 ~ 4L,
        TRUE ~ 1L
      )
    ),
    qual_rate = rtqc_rate(Turbidity, 3, 100)
  ) %>% 
  replace_na(
    list(
      qual_spike_fixed = "pass",
      qual_incr = "pass",
      qual_rate = "pass"
    )
  ) %>% 
  mutate(across(where(is.factor), ~if_else(.x == "pass", 0L, 1L))) %>% 
  mutate(
    qual_total = rowSums(across(starts_with("qual"))),
    qual_majority = outlier_test_factor(if_else(qual_total > 1, 4L, 1L)),
    qual_final = outlier_test_factor(if_else(Turbidity > 50 & qual_majority == "fail", 4L, 1L))
  )

p <- sttd_turb_2017_maj %>% 
  ggplot(aes(DateTime, Turbidity, color = qual_final)) +
  geom_point() +
  scale_color_manual(values = c("pass" = "black", "suspect" = "green", "fail" = "orange"))

ggplotly(p)

