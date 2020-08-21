# Continuous timeseries code
# creates facet graphs for all stations/analytes
# 8/18/2020
# Sarah Perry, Traci Treleaven, Amanda Maguire

# import packages
library(ggplot2)
library(tidyverse)
library(lubridate)

# set wd
setwd('Water_Quality')

# source functions
source('Code_cont_timeseries analysis_func.R')

# --- Import Data ---
# define main FASTR filepath (assumes sync'd with Sharepoint)
fp_fastr <- 'California Department of Water Resources/Office of Water Quality and Estuarine Ecology - North Delta Flow Action/'

# define filepaths 
fp_rel_wq <- paste(fp_fastr,'WQ_Subteam/Processed_Data/Continuous/Combined_Files/combined_flow.csv', sep = '')
fp_abs_wq <- get_abs_path(fp_rel_wq)

# read in data
df_wq <- read_csv(
  fp_abs_wq,
  col_types = cols(
    .default = 'n',
    DateTime = 'T',
    Date = 'D',
    Year = 'c',
    PreFlowStart = 'c',
    PreFlowEnd = 'c',
    PostFlowStart = 'c',
    PostFlowEnd = 'c',
    ActionPhase = 'c',
    StationCode = 'c'
    )
  )

# remove extra cols
df_wq <- subset(df_wq, select = -c(WYType, FlowPulseType, NetFlowDays))

# pivot longer
df_wq_long <- df_wq %>% 
  select(!ends_with('_Qual')) %>% 
  pivot_longer(
    cols = -c(DateTime, StationCode, Date, ActionPhase, PreFlowStart, PreFlowEnd, PostFlowStart, PostFlowEnd, Year),
    names_to = 'Analyte',
    values_to = 'Result')

# --- Clean Data ---
# add full analyte name column
df_wq_long <- add_analyte_names(df_wq_long)

# add unit column
df_wq_long <- add_analyte_units(df_wq_long)

# add region column
df_wq_long <- add_region(df_wq_long)

# --- Prep for Graphs ---
# list of analytes/stations
analytes <- unique(df_wq_long$Analyte)
years <- unique(df_wq_long$Year)

# change station col to factor type and order it
stat_lvls <- c('RMB','RCS','WWT','RD22','DWT','I80','SHR','LIS','SRH','STTD','TOE','LIBCUT','SGG','BL5','LIB','RYI','SRV','RVB','SDI')
df_wq_long$StationCode <- factor(df_wq_long$StationCode, levels = stat_lvls)

# --- Create Timeseries --
# create plots
for (year in years){
  for (analyte in analytes){ 

    # create timeseries 
    p <- create_facet(df_wq_long)

    # save graphs
    fp_rel_save <- paste(fp_fastr,'WQ_Subteam/Raw_Plots/Continuous/time_series/',year,sep = '')
    fp_abs_save <- get_abs_path(fp_rel_save)

    ggsave(
      paste(fp_abs_save,'/Timeseries_',year,'_',analyte,'.png',sep = ''),
      p,
      width = 8,
      height = 12,
      unit = 'in'
    )
  }
}
