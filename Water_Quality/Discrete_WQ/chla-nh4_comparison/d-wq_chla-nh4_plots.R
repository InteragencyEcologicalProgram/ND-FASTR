# FASTR - Chlorophyll and Ammonia Comparison
# purpose: qualitatively determine relationship b/w chla and NH3
# author: Sarah Perry
# contact: seperry83@gmail.com
 
# import packages
library(ggplot2)
library(tidyverse)

# source functions
source('Water_Quality/Discrete_WQ/chla-nh4_comparison/d-wq_chla-nh4_plots_funcs.R')

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

# --- Clean Data ---
# convert NDs to NA 
df_wq$Result<- as.numeric(df_wq$Result)
df_regions <- df_regions %>% rename(StationCode = Station)

# subset df_wq
df_wq <- merge(x = df_wq, y = df_regions[, c('StationCode', 'BroadRegion')], by = 'StationCode')
df_wq_long <- select(df_wq, c('StationCode','BroadRegion','DateTime','Result','Analyte'))
df_wq_long <- subset(df_wq_long, !is.na(BroadRegion))

# create date/year columns (character type)
df_wq_long$Date <- format(strptime(df_wq_long$DateTime, '%Y/%m/%d'),'%m/%d/%Y')
df_wq_long$Year <- format(strptime(df_wq_long$DateTime, '%Y/%m/%d'),'%Y')
df_wq_long <- subset(df_wq_long, Year > 2012)

# add full analyte name column  
# df_wq <- add_analyte_names(df_wq)

# add in phase actions (will also convert 'Date' column to date type)
df_wq_long <- add_phase_actions(df_wq_long, df_dates)

# convert to wide
df_wq_wide <- spread(df_wq_long, Analyte, Result)

# convert NH3 data to proper units
df_wq_wide$DisAmmonia_ugDl <- df_wq_wide$DisAmmonia * 100
df_wq_wide$DisAmmonia_umolL <- df_wq_wide$DisAmmonia_ugDl * 0.5872

# --- Prep for Graphs ---
# relevant variables
varis <- c('BroadRegion')
width <- c(10, 10)
height <- c(8, 14)

# order ActionPhase column
df_wq_wide$ActionPhase <- factor(df_wq_wide$ActionPhase, levels = c('Pre', 'During', 'Post'))
df_wq_wide$BroadRegion <- factor(df_wq_wide$BroadRegion, levels = c('Upstream','Downstream'))

# --- Create Plots --
for (i in seq(varis)) {
  # create plots
  p <- create_graph(df_wq_wide, vari)
  
  # save graphs
  fp_rel_save <- paste(fp_fastr,'WQ_Subteam/Plots/Discrete/Chla-NH4',sep = '')
  fp_abs_save <- get_abs_path(fp_rel_save)
  
  ggsave(
    paste(fp_abs_save,'/Chla-NH4_',varis[i],'.png',sep = ''),
    p,
    width = width[i],
    height = height[i],
    unit = 'in'
  )
}
