# FASTR - Discrete WQ Time Series Analysis
# purpose: time series analysis of discrete WQ data for FASTR
# author: Sarah Perry
# contact: seperry83@gmail.com

# import packages
library(ggplot2)
library(tidyverse)

# source functions
source('Water_Quality/Discrete_WQ/timeseries_analysis/d-wq_ts_station_plots_funcs.R')

# --- Import Data ---
# define main FASTR filepath (assumes sync'd with Sharepoint)
fp_fastr <- 'California Department of Water Resources/Office of Water Quality and Estuarine Ecology - North Delta Flow Action/'

# define relative filepaths 
fp_rel_wq <- paste0(fp_fastr,'WQ_Subteam/Processed_Data/Discrete/WQ_OUTPUT_Discrete_Lab_formatted.csv')
fp_rel_dates <- paste0(fp_fastr,'Data Management/FlowDatesDesignations.csv')
fp_rel_regions = paste0(fp_fastr, 'WQ_Subteam/Processed_Data/NDFA_WQ_Stations.csv')

# define absolute filepaths
fp_abs_wq <- get_abs_path(fp_rel_wq)
fp_abs_dates <- get_abs_path(fp_rel_dates)
fp_abs_regions <- get_abs_path(fp_rel_regions)

# read in data
df_wq <- read_csv(fp_abs_wq)
df_dates <- read_csv(fp_abs_dates)
df_regions <- read_csv(fp_abs_regions)

# --- Clean Data In Order To Create Timeseries Plots w/ RLs ---
# create date/year columns (character type)
df_wq$Date <- format(strptime(df_wq$DateTime, '%Y/%m/%d'),'%m/%d/%Y')
df_wq$Year <- format(strptime(df_wq$DateTime, '%Y/%m/%d'),'%Y')

df_wq <- merge(x = df_wq, y = df_regions[, c('StationCode', 'Region')], by = 'StationCode')

# add full analyte name column
df_wq <- add_analyte_names(df_wq)

# add in phase actions (will also convert 'Date' column to date type)
df_wq <- add_phase_actions(df_wq, df_dates)

# --- Convert NDs to NA --- 
df_wq$Result[df_wq$LabDetect == 'Non-detect'] <- NA

# --- Prep for Graphs ---
# list of analytes/stations
analytes <- unique(df_wq$Analyte)
years <- sort(unique(df_wq$Year))

# change station col to factor type and order it
stat_lvls <- c('RMB','RCS','WWT','RD22','DWT','I80','SHR','LIS','SRH','STTD','BL5','LIB','RYI','SRV','RVB','SDI')
df_wq$StationCode <- factor(df_wq$StationCode, levels = stat_lvls)

# --- Create Time Series --
# create plots
for (year in years){
  for (analyte in analytes) { 
    
    # create time series graphs
    p <- create_facet(df_wq)

    # save graphs
    fp_rel_save <- paste0(fp_fastr, 'WQ_Subteam/Plots/Discrete/Timeseries_Stations/', year)
    fp_abs_save <- get_abs_path(fp_rel_save)
    
    if (!length(p) == 0) {
      ggsave(
        paste(fp_abs_save,'/Timeseries_',year,'_',analyte,'.png',sep = ''),
        p,
        width = 8,
        height = 15,
        unit = 'in'
      )
    }
    break
  }
}
