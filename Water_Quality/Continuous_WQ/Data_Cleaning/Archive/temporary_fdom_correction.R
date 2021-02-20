
library(tidyverse)
library(readxl)
library(lubridate)
library(plotly)

# Source global NDFA functions
source("global_ndfa_funcs.R")
fp_rel_wq_proc_filt <- "WQ_Subteam/Processed_Data/Continuous/Filtered_Dates"
fp_abs_wq_proc_filt <- ndfa_abs_sp_path(fp_rel_wq_proc_filt)

sdi_orig <- read_csv(file.path(fp_abs_wq_proc_filt, "RTM_OUTPUT_SDI_formatted_filt.csv"), col_types = "ccdddcdcdcdcdcdcdcdc")

sdi_clean <- sdi_orig %>% 
  select(DateTime, WaterTemp, fDOM) %>% 
  mutate(DateTime = ymd_hms(DateTime)) %>% 
  filter(!is.na(fDOM)) %>% 
  mutate(fDOM_corr = fDOM/(1 + 0.008 * (WaterTemp - 20))) %>% 
  select(-WaterTemp) %>% 
  pivot_longer(cols = -DateTime, names_to = "Type", values_to = "value")

p <- sdi_clean %>% ggplot(aes(x = DateTime, y = value, color = Type)) + geom_line()
ggplotly(p)

