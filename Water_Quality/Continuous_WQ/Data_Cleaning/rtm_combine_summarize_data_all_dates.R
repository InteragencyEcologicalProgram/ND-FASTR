# NDFA Water Quality
# Purpose: 
  # 1) Combine all available continuous WQ data (not just the filtered dates)
  # 2) Summarize periods of record and sample counts of this data
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov


# 1. Global Code and Functions -----------------------------------------------

# Load packages
library(tidyverse)
library(lubridate)

# Source global NDFA functions
source("global_ndfa_funcs.R")
source("Water_Quality/global_wq_funcs.R")

# Define relative file path for processed continuous WQ data files saved on NDFA SharePoint
fp_rel_wq_proc_all <- "WQ_Subteam/Processed_Data/Continuous/All_Dates"

# Define relative file path for the file containing the final QA'ed and integrated continuous WQ data to be saved on NDFA SharePoint
# fp_rel_wq_qa_f <- "WQ_Subteam/Processed_Data/Continuous"

# Define absolute file paths
fp_abs_wq_proc_all <- ndfa_abs_sp_path(fp_rel_wq_proc_all)
# fp_abs_wq_qa_f <- ndfa_abs_sp_path(fp_rel_wq_qa_f)

# Set System Timezone as "Etc/GMT+8" (PST) to make it consistent with all data 
Sys.setenv(TZ = "Etc/GMT+8")


# 2. Import and Prepare Data ----------------------------------------------

# Create a vector of all file paths for the processed continuous WQ data
rtm_wq_proc_fp <- dir(fp_abs_wq_proc_all, full.names = TRUE)

# Remove the file with SDI data since we will need to import that separately 
  # (import_rtm_data doesn't work with it)
rtm_wq_proc_fp_f <- str_subset(rtm_wq_proc_fp, "_SDI_", negate = TRUE)

# Create a vector of the number of parameters within each station's dataset
n_params <- c(6, 11, 10, 7, 7, 7, 6, 8, 10, 7, 6, 6, 10)

# Import processed continuous WQ data into a dataframe
rtm_wq_proc_orig <- map2_dfr(rtm_wq_proc_fp_f, n_params, .f = import_rtm_data)

# Import processed continuous WQ data for SDI
sdi_orig <- read_csv(
  file.path(fp_abs_wq_proc_all, "RTM_OUTPUT_SDI_formatted_all.csv"),
  col_types = paste0("ccdd", str_dup("dc", 8))
)

# Add SDI data to all other continuous WQ data
rtm_wq_proc_orig <- bind_rows(rtm_wq_proc_orig, sdi_orig)

# Prepare data for summarizing
rtm_wq_proc_clean <- rtm_wq_proc_orig %>% 
  mutate(
    DateTime = ymd_hms(DateTime, tz = "Etc/GMT+8"),
    Date = date(DateTime),
    Year = year(DateTime)
  ) %>% 
  select(!c(ends_with("Qual"), starts_with("Flow"), Chla_RFU)) %>% 
  pivot_longer(
    cols = -c(starts_with("Date"), Year, StationCode),
    names_to = "Parameter",
    values_to = "Value"
  ) %>% 
  filter(!is.na(Value))


# 3. Summarize available data ---------------------------------------------

# Determine min and max dates and sample counts for each StationCode, Year, and Parameter combination
rtm_wq_proc_summ <- rtm_wq_proc_clean %>% 
  group_by(StationCode, Year, Parameter) %>% 
  summarize(
    min_date = min(Date),
    max_date = max(Date),
    n_samples = n()
  ) %>% 
  ungroup() %>%
  group_by(StationCode) %>% 
  complete(nesting(StationCode, Parameter), Year = seq.int(min(Year), max(Year), by = 1)) %>%
  ungroup()

# Determine all possible number of samples for each StationCode, Year, and Parameter combination
all_poss_samples <- rtm_wq_proc_clean %>% 
  group_by(StationCode, Year, Parameter) %>%
  complete(DateTime = seq.POSIXt(min(DateTime), max(DateTime), by = "15 min")) %>% 
  summarize(n_all = n()) %>% 
  ungroup()

# Join all possible number of samples to other summary statistics and calculate the number of 
  # missing time stamps and the percentage
rtm_wq_proc_summ_f <- rtm_wq_proc_summ %>% 
  left_join(all_poss_samples) %>% 
  mutate(
    n_missing = n_all - n_samples,
    perc_missing = n_missing/n_all
  ) %>% 
  select(-n_all) %>% 
  arrange(StationCode, Parameter, Year)

# Export summary data as a .csv file
rtm_wq_proc_summ_f %>% write_excel_csv("rtm_sample_summ_all_dates.csv", na = "")

