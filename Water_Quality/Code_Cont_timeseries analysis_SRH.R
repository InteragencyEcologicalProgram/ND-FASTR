# Continuous timeseries code
# 8/12/2020
# Sarah Perry, Traci Treleaven, Amanda Maguire

# import packages
library(ggplot2)
library(tidyverse)
library(lubridate)

# source functions
source('C:/Repositories/ND-FASTR/Water_Quality/Code_cont_timeseries analysis_func.R')


# --- Import Data ---
# define main FASTR filepath (assumes sync'd with Sharepoint)
fp_fastr <- 'California Department of Water Resources/Office of Water Quality and Estuarine Ecology - North Delta Flow Action/'

# define relative filepaths 
fp_rel_wq <- paste(fp_fastr,'WQ_Subteam/Processed_Data/Continuous/RTM_OUTPUT_SRH_formatted.csv', sep = '')
fp_rel_dates <- paste(fp_fastr,'Data Management/FlowDatesDesignations.csv', sep = '')

# define absolute filepaths
fp_abs_wq <- get_abs_path(fp_rel_wq)
fp_abs_dates <- get_abs_path(fp_rel_dates)

# read in data
df_wq <- read_csv(fp_abs_wq)
df_dates <- read_csv(fp_abs_dates)

# Pivot longer
df_wq_long <- df_wq %>% 
  select(!ends_with("_Qual")) %>% 
  pivot_longer(
    cols = -c(DateTime, StationCode),
    names_to = "Analyte",
    values_to = "Result")

# --- Clean Data In Order To Create Timeseries Plots w/ RLs ---
# create date/year columns (character type)
df_wq_long$Date <- format(strptime(as.character(df_wq_long$DateTime), '%Y/%m/%d'),'%m/%d/%Y')
df_wq_long$Year <- format(strptime(df_wq_long$DateTime, '%Y/%m/%d'),'%Y')
df_wq_long$Test <- as.POSIXct(df_wq_long$DateTime)


# add full analyte name column
df_wq_long <- add_analyte_names(df_wq_long)

# add in phase actions (will also convert 'Date' column to date type)
df_wq_long <- add_phase_actions(df_wq_long, df_dates)

# --- Prep for Graphs ---
# list of analytes/stations
analytes <- unique(df_wq_long$Analyte)
years <- unique(df_wq_long$Year)

# --- Create Timeseries --
# create plots
for (year in years){
  for (analyte in analytes){ 
    # create timeseries 
    p <- create_facet(df_wq_long[1:15])
    
    # print(p)    
  
    
    # save graphs
    fp_rel_save <- paste(fp_fastr,'WQ_Subteam/Raw_Plots/Continuous/time_series/',year,sep = '')
    fp_abs_save <- get_abs_path(fp_rel_save)

    ggsave(
      paste(fp_abs_save,'/Timeseries_',year,'_',analyte,'.png',sep = ''),
      p,
      width = 10,
      height = 10,
      unit = 'in'
    )
  break
    }
  break
}


