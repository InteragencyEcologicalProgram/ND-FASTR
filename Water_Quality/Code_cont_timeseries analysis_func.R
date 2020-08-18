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
          Analyte == 'SpCnd' ~ 'Unit',
          Analyte == 'WaterTemp' ~ 'Unit',
          Analyte == 'Turbidity' ~ 'Unit',
          Analyte == 'pH' ~ 'pH',
          Analyte == 'DO' ~ 'Unit',
          Analyte == 'Flow' ~ 'Unit',
          Analyte == 'FlowTF' ~ 'Unit',
          Analyte == 'Chla_RFU' ~ 'RFU',
          Analyte == 'fDOM' ~ 'Unit',
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
          StationCode == 'SGG' ~ 'Cache Slough Complex', # check
          StationCode == 'RYI' ~ 'Cache Slough Complex',
          StationCode == 'LIS' ~ 'Lower Yolo Bypass',
          StationCode == 'STTD' ~ 'Lower Yolo Bypass',
          StationCode == 'SHR' ~ 'Middle Sacramento River',
          StationCode == 'SRH' ~ 'Middle Sacramento River',
          StationCode == 'SRV' ~ 'Middle Sacramento River',
          StationCode == 'RVB' ~ 'Lower Sacramento River',
          StationCode == 'SDI' ~ 'Lower Sacramento River',
          StationCode == 'TOE' ~ 'Lower Sacramento River', # check
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

  if(!analyte %in% unique(df_filt$Analyte)) {
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
        df_filt[complete.cases(df_filt['Result']),],
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
      guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
      xlab('Date') +
      ylab(analyte_unit) +
      ggtitle(paste(analyte_full,'-',year))
    
    return(p)
  }
}