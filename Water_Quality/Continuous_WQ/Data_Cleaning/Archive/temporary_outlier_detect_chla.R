# Chlorophyll - draft outlier detection methods

library(tidyverse)
library(plotly)
library(ODWGtools)

source("global_ndfa_funcs.R")
source("Water_Quality/global_wq_funcs.R")

# Import example data - I80 from 2018
fp_abs_rtm <- ndfa_abs_sp_path("WQ_Subteam/Processed_Data/Continuous/Filtered_Dates")

i80_orig <- import_rtm_data(file.path(fp_abs_rtm, "RTM_OUTPUT_I80_formatted_filt.csv"), 6)

i80_chl_2018 <- i80_orig %>% 
  mutate(DateTime = ymd_hms(DateTime)) %>% 
  filter(year(DateTime) == 2018) %>% 
  select(DateTime, Chla)

# Spike test - fixed thresholds c(15, 30)
i80_chl_2018_spike <- i80_chl_2018 %>% 
  mutate(qual_spike_fixed = rtqc_spike(Chla, c(15, 30))) %>% 
  replace_na(list(qual_spike_fixed = "pass"))

p <- i80_chl_2018_spike %>% 
  ggplot(aes(DateTime, Chla, color = qual_spike_fixed)) +
  geom_point() +
  scale_color_manual(values = c("pass" = "black", "suspect" = "green", "fail" = "orange"))

ggplotly(p)

# % increase from prior value - use 100% for now, questionable though
i80_chl_2018_perc_incr <- i80_chl_2018 %>% 
  mutate(
    qual_incr = outlier_test_factor(
      case_when(
        is.na(Chla) | is.na(lag(Chla)) | is.na(lead(Chla)) ~ NA_integer_,
        (Chla - lag(Chla))/lag(Chla) > 1 ~ 4L,
        TRUE ~ 1L
      )
    )
  ) %>% 
  replace_na(list(qual_incr = "pass"))

p <- i80_chl_2018_perc_incr %>% 
  ggplot(aes(DateTime, Chla, color = qual_incr)) +
  geom_point() +
  scale_color_manual(values = c("pass" = "black", "suspect" = "green", "fail" = "orange"))

ggplotly(p)

# Rate of change test - using 3 standard deviations, SD is of the prior 25-hour period
i80_chl_2018_rate <- i80_chl_2018 %>% 
  mutate(qual_rate = rtqc_rate(Chla, 3, 100)) %>% 
  replace_na(list(qual_rate = "pass"))

p <- i80_chl_2018_rate %>% 
  ggplot(aes(DateTime, Chla, color = qual_rate)) +
  geom_point() +
  scale_color_manual(values = c("pass" = "black", "suspect" = "green", "fail" = "orange"))

ggplotly(p)

# majority scoring - 2 or more tests are suspect or fail, value is greater than 30 ug/L
i80_chl_2018_maj <- i80_chl_2018 %>% 
  mutate(
    qual_spike_fixed = rtqc_spike(Chla, c(15, 30)),
    qual_incr = outlier_test_factor(
      case_when(
        is.na(Chla) | is.na(lag(Chla)) | is.na(lead(Chla)) ~ NA_integer_,
        (Chla - lag(Chla))/lag(Chla) > 1 ~ 4L,
        TRUE ~ 1L
      )
    ),
    qual_rate = rtqc_rate(Chla, 3, 100)
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
    qual_final = outlier_test_factor(if_else(Chla > 30 & qual_majority == "fail", 4L, 1L))
  )

p <- i80_chl_2018_maj %>% 
  ggplot(aes(DateTime, Chla, color = qual_final)) +
  geom_point() +
  scale_color_manual(values = c("pass" = "black", "suspect" = "green", "fail" = "orange"))

ggplotly(p)

