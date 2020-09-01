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

# --- Add Analyte Units ---
add_analyte_units <- function(df){
  # add AnalyteFull column
  df <- df %>%
    mutate(
      Units =
        case_when(
          Analyte == 'Chla' ~ 'ug/L',
          Analyte == 'SpCnd' ~ 'uS/cm',
          Analyte == 'WaterTemp' ~ 'degrees C',
          Analyte == 'Turbidity' ~ 'NTU',
          Analyte == 'pH' ~ 'pH',
          Analyte == 'DO' ~ 'mg/L',
          Analyte == 'Flow' ~ 'cfs',
          Analyte == 'FlowTF' ~ 'cfs',
          Analyte == 'Chla_RFU' ~ 'RFU',
          Analyte == 'fDOM' ~ 'ug/L as QSE',
          Analyte == 'NitrateNitrite' ~ 'mg/L'
        )
    )
  return(df)
}

# --- Add Regions ---
add_region <- function(df){
  # add AnalyteFull column
  df <- df %>%
    mutate(
      Region =
        case_when(
          StationCode == 'BL5' ~ 'Cache Slough Complex',
          StationCode == 'LIB' ~ 'Cache Slough Complex',
          StationCode == 'LIBCUT' ~ 'Cache Slough Complex',
          StationCode == 'SGG' ~ 'Cache Slough Complex',
          StationCode == 'RYI' ~ 'Cache Slough Complex',
          StationCode == 'LIS' ~ 'Lower Yolo Bypass',
          StationCode == 'STTD' ~ 'Lower Yolo Bypass',
          StationCode == 'TOE' ~ 'Lower Yolo Bypass',
          StationCode == 'SHR' ~ 'Middle Sacramento River',
          StationCode == 'SRH' ~ 'Middle Sacramento River',
          StationCode == 'SRV' ~ 'Middle Sacramento River',
          StationCode == 'RVB' ~ 'Lower Sacramento River',
          StationCode == 'SDI' ~ 'Lower Sacramento River',
          StationCode == 'RCS' ~ 'Colusa Basin/Ridge Cut Slough',
          StationCode == 'RMB' ~ 'Colusa Basin/Ridge Cut Slough',
          StationCode == 'WWT' ~ 'Central Yolo Bypass',
          StationCode == 'DWT' ~ 'Central Yolo Bypass',
          StationCode == 'RD22' ~ 'Central Yolo Bypass',
          StationCode == 'I80' ~ 'Central Yolo Bypass',
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

# --- Check for all NA's per Station ---
check_station_nas <- function(df){
  # initiate empty vector
  remove_stations <- NULL
  
  # loop over stations
  for (station in unique(df$StationCode)) {
    df_w_nas <- df[df$StationCode == station,]
    all_na <- all(is.na(unique(df_w_nas$Result)))
    # determine stations w/ all NAs
    if (all_na) {
      remove_stations <- c(remove_stations, station)
    }
  
    # remove those stations
    df_no_nas <- filter(df, !StationCode %in% remove_stations)
  }
  return(df_no_nas)
}

# --- Blank Theme for Timeseries Graphs ---
blank_theme <- function(){
  theme_bw() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = 'black', size = 13.5, family = 'sans'),
      axis.text.x = element_text(angle = 45, vjust=0.5, margin = margin(t = 1)),
      strip.text.y = element_text(size = 14),
      axis.title = element_text(size = 18, face = 'bold'),
      plot.title = element_text(size = 20, hjust = 0.5, face = 'bold'),
      legend.position = 'top',
      legend.title = element_blank(),
      legend.box.margin = margin(-5,0,-5,0),
      legend.margin = margin(),
      legend.text = element_text(size = 13)
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
  
  # remove stations w/ NA values
  df_filt <- check_station_nas(df_filt)
  
  # check if all values are NA
  all_na <- all(is.na(unique(df_filt$Result)))
  
  if(all_na) {
  }
  else{
    # define flow action duration
    action_max <- paste(unique(df_filt$PostFlowStart), '00:00:00')
    action_max <- as.POSIXct(action_max, '%Y/%m/%d %H:%M:%S')
    action_min <- paste(unique(df_filt$PreFlowEnd), '23:45:00')
    action_min <- as.POSIXct(action_min, '%Y/%m/%d %H:%M:%S')
    
    # define relevant values for naming
    analyte_full <- unique(df_filt$AnalyteFull[df_filt$Analyte == analyte])
    analyte_unit <- unique(df_filt$Units[df_filt$Analyte == analyte])
    cmap_colors <- c('#999999', '#f781bf', '#B79F00', '#984ea3', '#377eb8', '#e41a1c') # #ffff33
    
    # plot timeseries
    p <- ggplot() +
      facet_grid(StationCode ~ ., scales = 'free_y', drop = TRUE) +
      annotate('rect', xmin = action_min, xmax = action_max, ymin = -Inf, ymax = Inf, alpha = .08)

    # 
    # add timeseries data
    p <- p +
      geom_line( # line
        df_filt,
        mapping = aes(x = DateTime, y = Result, group = StationCode, color = Region),
        size = 1
      )
    
    # fix asthetics
    p <- p +
      blank_theme + # theme
      # scale_x_datetime(breaks = date_breaks("15 min")) +
      scale_fill_manual(values = cmap_colors) +
      scale_color_manual(
        values = c('Cache Slough Complex' = cmap_colors[1],
                   'Central Yolo Bypass' = cmap_colors[2],
                   'Colusa Basin/Ridge Cut Slough' = cmap_colors[3],
                   'Lower Sacramento River' = cmap_colors[4],
                   'Lower Yolo Bypass' = cmap_colors[5],
                   'Middle Sacramento River' = cmap_colors[6]
        )
      ) +
      guides(color = guide_legend(ncol = 3, byrow = TRUE)) +
      xlab('Date') +
      ylab(analyte_unit) +
      ggtitle(paste(analyte_full,'-',year))
    
    return(p)
  }
}