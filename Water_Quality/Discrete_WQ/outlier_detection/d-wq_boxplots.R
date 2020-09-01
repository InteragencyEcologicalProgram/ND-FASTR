# FASTR - Discrete WQ Boxplots
# purpose: boxplots discrete WQ data for outlier detection
# author: Sarah Perry
# contact: seperry83@gmail.com 

# import packages
library(tidyverse)

# source functions
source('d-wq_boxplots_funcs.R')

# --- Import Data ---
# define main FASTR filepath (assumes sync'd with Sharepoint)
fp_fastr <- 'California Department of Water Resources/Office of Water Quality and Estuarine Ecology - North Delta Flow Action/'

# define relative filepaths 
fp_rel_wq <- paste(fp_fastr,'WQ_Subteam/Processed_Data/Discrete/Discrete_Output_Lab.csv', sep = '')
fp_rel_dates <- paste(fp_fastr,'Data Management/FlowDatesDesignations.csv', sep = '')

# define absolute filepaths
fp_abs_wq <- get_abs_path(fp_rel_wq)
fp_abs_dates <- get_abs_path(fp_rel_dates)

# read in data
df_wq <- read_csv(fp_abs_wq) 
df_dates <- read_csv(fp_abs_dates)

# --- Clean Data In Order To Create Boxplots w/ RLs ---
# create date/year columns (character type)
df_wq$Date <- format(strptime(df_wq$DateTime, '%m/%d/%Y'),'%m/%d/%Y')
df_wq$Year <- as.numeric(format(strptime(df_wq$DateTime, '%m/%d/%Y'),'%Y'))

# add full analyte name column
df_wq <- add_analyte_names(df_wq)

# add in phase actions (will also convert 'Date' column to date type)
df_wq <- add_phase_actions(df_wq, df_dates)

#add 'full group' column (ie. station + year)
df_wq$FullGroup <- paste(df_wq$StationCode, df_wq$Year, df_wq$Analyte, sep = ',')

# change values in LabDetect column to boolean (censored = TRUE)
df_wq$LabDetect <- ifelse(df_wq$LabDetect == 'Non-detect', TRUE, FALSE)

# source functions
source('d-wq_boxplots_funcs.R')

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
  fp_rel_save <- paste(fp_fastr,'WQ_Subteam/Raw_Plots/Discrete/Boxplots', sep = '')
  fp_abs_save <- get_abs_path(fp_rel_save)
  
  ggsave(
    paste(fp_abs_save,'/Boxplots_',analyte,'.png',sep = ''),
    bp,
    width = 10,
    height = 11,
    unit = 'in'
  )
}
