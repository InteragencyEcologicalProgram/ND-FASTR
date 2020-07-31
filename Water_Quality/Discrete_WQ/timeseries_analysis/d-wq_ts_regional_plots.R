# FASTR - Discrete WQ Timeseries Analysis
# purpose: timeseries analysis of discrete WQ data for FASTR
# author: Sarah Perry
# contact: seperry83@gmail.com

# import packages
library(zoo)
library(grid)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(RColorBrewer)

# source functions
source('d-wq_ts_regional_plots_funcs.R')

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

# --- Clean Data In Order To Create Timeseries Plots w/ RLs ---
# create date/year columns (character type)
df_wq$Date <- format(strptime(df_wq$DateTime, '%m/%d/%Y'),'%m/%d/%Y')
df_wq$Year <- format(strptime(df_wq$DateTime, '%m/%d/%Y'),'%Y')

# add full analyte name column
df_wq <- add_analyte_names(df_wq)

# add in phase actions (will also convert 'Date' column to date type)
df_wq <- add_phase_actions(df_wq, df_dates)

# --- Convert NDs to NA --- 
df_wq$Result[df_wq$LabDetect == 'Non-detect'] <- NA

# --- Prep for Graphs ---
# list of analytes/regions/years
analytes <- unique(df_wq$Analyte)
regions <- unique(df_wq$Region)
years <- unique(df_wq$Year)

# list of color palettes (to distinguish regions on facet graphs)
# based on ColorBrewer palettes
colors <- c('Reds', 'Blues', 'Greens', 'Purples', 'Oranges', 'Greys')

# --- Create Graphs ---
# create separate facet graphs for each year/analyte combo
for (year in years){
  for (analyte in analytes){
    # create plots to go on the facet graph
    plts <- mapply(
      region = regions,
      color_scheme = colors,
      FUN = function(x, region, color_scheme) create_plots(df_wq, region, color_scheme),
      SIMPLIFY = FALSE
    )
    
    # define relevant values for naming files
    analyte_full <- unique(df_wq$AnalyteFull[df_wq$Analyte == analyte])
    analyte_unit <- unique(df_wq$Units[df_wq$Analyte == analyte])
    
    # create common labels
    y_grob <- textGrob(analyte_unit, gp = gpar(fontsize = 15, fontface = 'bold'), rot = 90)
    x_grob <- textGrob('Date', gp = gpar(fontsize = 15, fontface = 'bold'))
    top_grob <- textGrob(paste(analyte_full,'-',year), gp=gpar(fontsize = 17, fontface = 'bold'))
    
    # create facet graph
    graph <- marrangeGrob(
      plts,
      ncol = 2,
      nrow = 3,
      top = top_grob,
      left = y_grob,
      bottom = x_grob)
    
    # filepath to save to
    fp_rel_save <- paste(fp_fastr,'WQ_Subteam/Raw_Plots/Discrete/Timeseries_Regional/',year, sep = '')
    fp_abs_save <- get_abs_path(fp_rel_save)
    
    # save plot
    ggsave(
      paste(fp_abs_save,'/Timeseries_',year,'_',analyte,'.png',sep = ''),
      graph,
      width = 8.5,
      height = 11,
      unit = 'in'
    )
  }
}