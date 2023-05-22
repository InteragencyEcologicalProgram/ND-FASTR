# NDFS Synthesis
# Purpose: Process the instantaneous and tidally-filtered flow data from the
  # Yolo Bypass near Lisbon (LIS) station from 2013-2022, and calculate daily
  # averages. Add the daily average tidally-filtered flow data collected at
  # Sacramento River at Freeport from NWIS for 2013-2022. This data was used in
  # figures and analyses for the NDFS contaminants and NDFS synthesis manuscripts.
# Author: Dave Bosworth
# Contacts: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
library(readxl)
library(dataRetrieval)
library(here)
library(conflicted)

# Declare package conflict preferences 
conflicts_prefer(dplyr::filter())

# Check if we are in the correct working directory
i_am("Water_Quality/Continuous_WQ/Data_Cleaning/LIS_SR_flow_clean_integrate_2013-2022.R")

# Set System Timezone as "Etc/GMT+8" (PST) to make it consistent with all data frames
Sys.setenv(TZ = "Etc/GMT+8")


# 1. Import Data ----------------------------------------------------------

# Import instantaneous and tidally-filtered flow data from LIS collected from
  # 2013-2022
df_lis_flow <- read_excel(
  here("Water_Quality/Continuous_WQ/data-raw/LIS_FlowData_2013_2022.xlsx"),
  sheet = "15 Min Net Flow",
  skip = 10
)

# Download daily average tidally-filtered flow data in cfs (#72137) for
  # Sacramento River at Freeport CA (11447650) using the `readNWISdv` function.
  # Download data for 2013-2022.
df_sac_net_flow_dv <- 
  readNWISdv(
    "11447650",
    "72137",
    startDate = "2013-01-01",
    endDate = "2022-12-31"
  ) %>% 
  as_tibble()
  

# 2. Clean Data -----------------------------------------------------------

# Prepare LIS flow data for averaging
df_lis_flow_c1 <- df_lis_flow %>% 
  transmute(
    DateTime = round_date(force_tz(`Date & Time (PST)`, tzone = "Etc/GMT+8"), unit = "15 minute"),
    Date = date(DateTime),
    LIS_InstFlow = `Flow (cfs)`,
    LIS_NetFlow = `Net Flow (cfs)`
  )

# Look for duplicated time stamps
df_lis_flow_c1 %>%
  count(DateTime) %>%
  filter(n > 1)
# No duplicated time stamps present in data set

# Inspect number of measurements per day for LIS instantaneous flow data
df_lis_inst_flow_num <- df_lis_flow_c1 %>% 
  drop_na(LIS_InstFlow) %>% 
  count(Date, name = "NumMeas")
  
df_lis_inst_flow_num %>% 
  count(NumMeas, name = "NumDays") %>% 
  arrange(desc(NumMeas)) %>% 
  print(n = 55)

# Inspect number of measurements per day for LIS net flow data
df_lis_net_flow_num <- df_lis_flow_c1 %>% 
  drop_na(LIS_NetFlow) %>% 
  count(Date, name = "NumMeas")

df_lis_net_flow_num %>% 
  count(NumMeas, name = "NumDays") %>% 
  arrange(desc(NumMeas)) %>% 
  print(n = 40)

# Both the instantaneous and net flow for LIS have days with large numbers of
  # missing values. We'll only calculate daily averages for days that have 10% or
  # fewer missing timestamps, which is 86-96 timestamps with values provided per
  # day. We have to separate the two types of flow data before filtering, and then
  # join them back together.
df_lis_inst_flow_c <- df_lis_flow_c1 %>% 
  select(-LIS_NetFlow) %>% 
  drop_na(LIS_InstFlow) %>% 
  group_by(Date) %>% 
  mutate(NumMeas = n()) %>% 
  ungroup() %>% 
  filter(NumMeas >= 86) %>% 
  select(-NumMeas)

df_lis_net_flow_c <- df_lis_flow_c1 %>% 
  select(-LIS_InstFlow) %>% 
  drop_na(LIS_NetFlow) %>% 
  group_by(Date) %>% 
  mutate(NumMeas = n()) %>% 
  ungroup() %>% 
  filter(NumMeas >= 86) %>% 
  select(-NumMeas)

df_lis_flow_c2 <- full_join(df_lis_inst_flow_c, df_lis_net_flow_c, by = join_by(DateTime, Date))

# Prepare Sacramento River at Freeport flow data to be joined to the LIS flow
  # data
df_sac_net_flow_dv_c <- df_sac_net_flow_dv %>% 
  select(
    Date, 
    SR_DailyAvgNetFlow = X_72137_00003
  )


# 3. Aggregate LIS values and join SR data --------------------------------

df_lis_flow_dv <- df_lis_flow_c2 %>% 
  # Calculate daily average flow values
  summarize(
    LIS_DailyAvgInstFlow = mean(LIS_InstFlow, na.rm = TRUE),
    LIS_DailyAvgNetFlow = mean(LIS_NetFlow, na.rm = TRUE),
    .by = Date
  ) %>% 
  # Convert Nan values to NA and round to 3 significant digits
  mutate(
    across(starts_with("LIS_"), ~ if_else(is.nan(.x), NA_real_, .x)),
    across(starts_with("LIS_"), ~ signif(.x, digits = 3))
  )

# Join Sacramento River at Freeport flow data to the LIS flow data
df_lis_sac_flow_dv <- 
  full_join(df_lis_flow_dv, df_sac_net_flow_dv_c, by = join_by(Date)) %>% 
  # Fill in missing dates with NA values
  complete(Date = seq.Date(ymd("2013-01-01"), ymd("2022-12-31"), by = "1 day")) %>% 
  arrange(Date)


# 4. Save and Export Data -------------------------------------------------

# Save final data set containing LIS and Sacramento River flow data for the NDFS
  # analyses as csv file for easier diffing
df_lis_sac_flow_dv %>% write_csv(here("Water_Quality/Data_Processed/LIS_SR_DailyAvgFlow_2013-2022.csv"))

# Save final data set containing LIS and Sacramento River flow data for the NDFS
  # analyses as an rds object in the GitHub repo for easier import
saveRDS(df_lis_sac_flow_dv, file = here("Water_Quality/Data_Processed/LIS_SR_DailyAvgFlow_2013-2022.rds"))

