# NDFA Water Quality
# Purpose: 
  # 1) Summarize periods of record of all available continuous WQ data 
    # (not just the filtered dates). This is for the raw (not QA'ed) data.
  # 2) For just the few stations that were collected year-round, summarize periods of record,
    # sample counts, numbers of missing samples, and percent missing.
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

# Define absolute file path
fp_abs_wq_proc_all <- ndfa_abs_sp_path(fp_rel_wq_proc_all)

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


# 3. Summarize all available data ---------------------------------------------

# Determine periods of record for each StationCode, Year, and Parameter combination
rtm_wq_por <- rtm_wq_proc_clean %>% 
  group_by(StationCode, Year, Parameter) %>% 
  summarize(
    min_date = min(Date),
    max_date = max(Date)
  ) %>% 
  ungroup() %>%
  group_by(StationCode) %>% 
  complete(nesting(StationCode, Parameter), Year = seq.int(min(Year), max(Year), by = 1)) %>%
  ungroup() %>% 
  arrange(StationCode, Parameter, Year)

# Export period of record data as a .csv file
rtm_wq_por %>% write_excel_csv("rtm_por_all_dates.csv", na = "")


# 4. Summarize just the stations with year-round continuous records -----------

# Focus on the few stations with year-round continuous records
l_rtm_wq_cont_rec <- 
  list(
    "df_data" = rtm_wq_proc_clean, 
    "df_por" = rtm_wq_por
  ) %>% 
  map(
    ~filter(.x, StationCode %in% c("LIB", "LIS", "RVB", "SRH", "STTD")) %>% 
      filter(!(StationCode == "STTD" & Year < 2016))
  )

# Determine sample counts for each StationCode, Year, and Parameter combination for the
  # instantaneous 15-minute data
rtm_wq_counts15 <- l_rtm_wq_cont_rec$df_data %>% 
  count(StationCode, Year, Parameter, name = "n_samples")

# Determine number of days with samples within each StationCode, Year, and Parameter combination
rtm_wq_counts_day <- l_rtm_wq_cont_rec$df_data %>% 
  distinct(StationCode, Date, Year, Parameter) %>% 
  count(StationCode, Year, Parameter, name = "n_days")

# Add all possible 15-min time stamps for each StationCode and Parameter combination
  # for the entire period of record
rtm_wq_all_dt <- l_rtm_wq_cont_rec$df_data %>% 
  group_by(StationCode, Parameter) %>%
  complete(DateTime = seq.POSIXt(min(DateTime), max(DateTime), by = "15 min")) %>%
  ungroup() %>% 
  select(StationCode, Parameter, DateTime) %>% 
  mutate(
    Date = date(DateTime),
    Year = year(DateTime)
  )

# Determine all possible number of 15-min samples for each StationCode, Year, and 
  # Parameter combination
all_poss_samples15 <- rtm_wq_all_dt %>% 
  count(StationCode, Parameter, Year, name = "n_all")

# Determine all possible number of days for each StationCode, Year, and Parameter combination
all_poss_days <- rtm_wq_all_dt %>%
  distinct(StationCode, Date, Year, Parameter) %>% 
  count(StationCode, Parameter, Year, name = "n_all")

# Join all summary statistics for the 15-min data together and calculate the number of 
  # missing time stamps and the percentage
rtm_wq_summ15 <- l_rtm_wq_cont_rec$df_por %>% 
  left_join(rtm_wq_counts15) %>% 
  left_join(all_poss_samples15) %>% 
  mutate(
    n_missing = n_all - n_samples,
    perc_missing = n_missing/n_all
  ) %>% 
  select(-n_all) %>% 
  arrange(StationCode, Parameter, Year)

# Join all summary statistics for daily data together and calculate the number of 
  # missing days and the percentage
rtm_wq_summ_day <- l_rtm_wq_cont_rec$df_por %>% 
  left_join(rtm_wq_counts_day) %>% 
  left_join(all_poss_days) %>% 
  mutate(
    n_missing = n_all - n_days,
    perc_missing = n_missing/n_all
  ) %>% 
  select(-n_all) %>% 
  arrange(StationCode, Parameter, Year)

# Export data summaries as .csv files
rtm_wq_summ15 %>% write_excel_csv("rtm_sample_summ_all_dates_15min.csv", na = "")
rtm_wq_summ_day %>% write_excel_csv("rtm_sample_summ_all_dates_daily.csv", na = "")

