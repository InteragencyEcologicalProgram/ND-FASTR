# FASTR - Fuctions for Continuous WQ Timeseries Analysis
# purpose: functions for timeseries analysis of Coninuous WQ data for FASTR
# author: Sarah Perry
# contact: seperry83@gmail.com

# import packages
library(scales)
library(ggplot2)
library(tidyverse)

# --- Import Data from SharePoint ---
get_abs_path <- function(fp_rel){
  # define absolute filepath
  fp_abs <- normalizePath(file.path(Sys.getenv('USERPROFILE'), fp_rel))
  
  return(fp_abs)
}

# --- Add Full Analyte Names ---
add_analyte_names <- function(df){
  # add AnalyteFull column
  df <- df %>%
    mutate(
      AnalyteFull =
        case_when(
          Analyte == 'Chla' ~ 'Chlorophyll a',
          Analyte == 'SpCnd' ~ 'Specific Conductance',
          Analyte == 'WaterTemp' ~ 'Water Temperature',
          Analyte == 'Turbidity' ~ 'Turbidity',
          Analyte == 'pH' ~ 'pH',
          Analyte == 'DO' ~ 'Dissolved Oxygen',
          Analyte == 'Flow' ~ 'Flow',
          Analyte == 'FlowTF' ~ 'Flow TF',
          Analyte == 'Chla_RFU' ~ 'Chla RFU',
          Analyte == 'fDOM' ~ 'fDOM',
          Analyte == 'NitrateNitrite' ~ 'Nitrate Nitrite'
        )
    )
  return(df)
}

# --- Add Phase Actions ---
# adapted from Cat Pien's code
add_phase_actions <- function(df_wq, df_dates){
  # change 'Year' column to character
  df_dates$Year <- as.character(df_dates$Year)
  
  # combine the two dfs
  df_combined <- inner_join(df_wq, df_dates, by  = 'Year')

  # convert date columns to date type
  cols_date <- c('Date','PreFlowStart','PreFlowEnd','PostFlowStart','PostFlowEnd')

  df_combined[cols_date] <- lapply(df_combined[cols_date], as.Date, format = '%m/%d/%Y')

  # add ActionPhase column and remove non-NDFA data
  df_combined <- df_combined %>%
    mutate(
      ActionPhase =
        case_when(
          Date >= PreFlowStart & Date <= PreFlowEnd ~ 'Pre',
          Date >= PreFlowEnd & Date <= PostFlowStart ~ 'During',
          Date >= PostFlowStart & Date <= PostFlowEnd ~ 'Post'
        )
    ) %>%
    filter(!is.na(ActionPhase))
  
  return(df_combined)
}



# --- Blank Theme for Timeseries Graphs ---
blank_theme <- function(){
  theme_bw() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = 'black', size = 14, family = 'sans'),
      axis.text.x = element_text(angle = 45, vjust=0.5, margin = margin(t = 1)),
      strip.text.y = element_text(size = 14),
      axis.title = element_text(size = 18, face = 'bold'),
      plot.title = element_text(size = 20, hjust = 0.5, face = 'bold'),
      legend.position = 'none'
    )
}

# --- Create Facet Scatterplots  ---
create_facet <- function(df){
  # import blank theme
  blank_theme <- blank_theme()
  

  
  # filter df by analyte
  df_filt <-
    df %>%
    filter(
      Analyte == analyte,
      Year == year
    )

  
  # define flow action duration
  action_max <- paste(unique(df_filt$PostFlowStart), '00:00:00')
  action_max <- as.POSIXct(action_max, '%Y/%m/%d %H:%M:%S')
  action_min <- paste(unique(df_filt$PreFlowEnd), '23:45:00')
  action_min <- as.POSIXct(action_min, '%Y/%m/%d %H:%M:%S')
  
  # define relevant values for naming
  analyte_full <- unique(df_filt$AnalyteFull[df_filt$Analyte == analyte])
  # analyte_unit <- unique(df_filt$Units[df_filt$Analyte == analyte])
  
  # plot timeseries
  p <- ggplot() +
    facet_grid(StationCode ~ ., scales = 'free_y') +
    annotate('rect', xmin = action_min, xmax = action_max, ymin = -Inf, ymax = Inf, alpha = .08)
  # 
  # add timeseries data
  p <- p +
    # geom_point( # points
    #   df_filt,
    #   mapping = aes(x = DateTime, y = Result, group = StationCode, color = StationCode),
    #   size = 2
    # ) +
    geom_line( # line
      df_filt[complete.cases(df_filt),],
      mapping = aes(x = DateTime, y = Result, group = StationCode, color = StationCode),
      size = 1
    )
  
  # fix asthetics
  p <- p +
    blank_theme + # theme
    # scale_x_datetime(breaks = date_breaks("15 min")) +
    # xlab('Date') +
    # ylab(analyte_unit) +
    ggtitle(paste(analyte_full,'-',year))
  
  return(p)
}