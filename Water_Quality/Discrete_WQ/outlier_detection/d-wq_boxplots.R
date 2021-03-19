# FASTR - Discrete WQ Boxplots
# purpose: boxplots discrete WQ data for outlier detection
# author: Sarah Perry
# contact: seperry83@gmail.com 

# import packages
library(tidyverse)

# source functions
source('Water_Quality/Discrete_WQ/outlier_detection/d-wq_boxplots_funcs.R')

# --- Import Data ---
# define main FASTR filepath (assumes sync'd with Sharepoint)
fp_fastr <- 'California Department of Water Resources/North Delta Flow Action - Documents/'

# define relative filepaths 
fp_rel_wq <- paste(fp_fastr,'WQ_Subteam/Processed_Data/Discrete/WQ_INPUT_Discrete_Lab_2021-01-25.csv', sep = '')
fp_rel_dates <- paste(fp_fastr,'Data Management/FlowDatesDesignations_45days.csv', sep = '')
fp_rel_regions = paste0(fp_fastr, 'WQ_Subteam/Processed_Data/Discrete/Analysis/NDFA_map.csv')

# define absolute filepaths
fp_abs_wq <- get_abs_path(fp_rel_wq)
fp_abs_dates <- get_abs_path(fp_rel_dates)
fp_abs_regions <- get_abs_path(fp_rel_regions)

# read in data
df_wq <- read_csv(fp_abs_wq) 
df_dates <- read_csv(fp_abs_dates)
df_regions <- read_csv(fp_abs_regions)

# --- Clean Data In Order To Create Boxplots w/ RLs ---
# create date/year columns (character type)
df_wq$Date <- format(strptime(df_wq$DateTime, '%Y/%m/%d'),'%m/%d/%Y')
df_wq$Year <- as.numeric(format(strptime(df_wq$DateTime, '%Y/%m/%d'),'%Y'))
df_regions <- df_regions %>% rename(StationCode = Station)
df_wq <- merge(x = df_wq, y = df_regions[, c('StationCode', 'BroadRegion')], by = 'StationCode')
df_wq$Year <- as.character(df_wq$Year)

# add full analyte name column
df_wq <- add_analyte_names(df_wq)

# add in phase actions (will also convert 'Date' column to date type)
df_wq <- add_phase_actions(df_wq, df_dates)

#add 'full group' column (ie. station + year)
df_wq$FullGroup <- paste(df_wq$StationCode, df_wq$Year, df_wq$Analyte, sep = ',')
df_wq$Year <- as.double(df_wq$Year)

# change values in LabDetect column to boolean (censored = TRUE)
df_wq$LabDetect[df_wq$Result == '< RL'] <- 'Non-detect'
df_wq$LabDetect[df_wq$Result == '< MDL'] <- 'Non-detect'
df_wq$Result <- as.numeric(df_wq$Result)
df_wq$Result[df_wq$LabDetect == 'Non-detect'] <- NA
df_wq$LabDetect <- ifelse(df_wq$LabDetect == 'Non-detect', TRUE, FALSE)
df_wq$LabDetect[is.na(df_wq$LabDetect)] <- FALSE

# source functions
source('Water_Quality/Discrete_WQ/outlier_detection/d-wq_boxplots_funcs.R')

# --- Prep for Graphs ---
# list of analytes/stations
analytes <- unique(df_wq$Analyte)
stations <- unique(df_wq$StationCode)

# order ActionPhase column
df_wq$ActionPhase <- factor(df_wq$ActionPhase, levels = c('Pre', 'During', 'Post'))

# --- Create BoxPlot --
# create plots
for (analyte in analytes){ 
  # create boxplots (accounts for non-detects)
  bp <- cen_boxplt(df_wq)
  
  # save graphs
  fp_rel_save <- paste(fp_fastr,'WQ_Subteam/Plots/Discrete/Boxplots', sep = '')
  fp_abs_save <- get_abs_path(fp_rel_save)
  
  ggsave(
    paste0(fp_abs_save,'/Boxplots_year_',analyte,'.png'),
    bp,
    width = 10,
    height = 11,
    unit = 'in'
  )
}
