# FASTR - Fuctions for Discrete WQ Timeseries Analysis
# purpose: functions for timeseries analysis of discrete WQ data for FASTR
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
          Analyte == 'DisAmmonia' ~ 'Dissolved Ammonia',
          Analyte == 'DisCalcium' ~ 'Dissolved Calcium',
          Analyte == 'DisChloride' ~ 'Dissolved Chloride',
          Analyte == 'Chla' ~ 'Chlorophyll a',
          Analyte == 'DisNitrateNitrite' ~ 'Dissolved Nitrate Nitrite',
          Analyte == 'DOC' ~ 'Dissolved Oraganic Carbon',
          Analyte == 'TOC' ~ 'Total Organic Carbon',
          Analyte == 'DON' ~ 'Dissolved Organic Nitrogen',
          Analyte == 'DOP' ~ 'Dissolved Organic Phosphate',
          Analyte == 'Pheo' ~ 'Pheophytin',
          Analyte == 'TOP' ~ 'Total Organic Phosphate',
          Analyte == 'DisSilica' ~ 'Dissolved Silica',
          Analyte == 'TSS' ~ 'Total Suspended Solids',
          Analyte == 'VSS' ~ 'Volatile Suspended Solids',
          Analyte == 'TDS' ~ 'Total Dissolved Solids',
          Analyte == 'TKN' ~ 'Total Kjeldahl Nitrogen'
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

# --- Create df for RL Segments ---
create_seg_df <- function(df){
  # subset out the ND data into own df (based on NA values in 'values' column)
  df_subset <- subset(df, is.na(df$Result))
  
  # check if any non-detect data
  any_data <- 'Non-detect' %in% df_subset$LabDetect
  
  if (any_data){
    # create new df for the vertical and horizontal segments
    df_seg <- data.frame(
      x_vert = df_subset$Date,
      xend_vert = df_subset$Date,
      y_vert = 0,
      yend_vert = df_subset$RptLimit,
      x_horz = df_subset$Date-2, # 2 is arbitrary, change this to change the length of the line
      xend_horz = df_subset$Date+2,
      y_horz = df_subset$RptLimit,
      yend_horz = df_subset$RptLimit,
      station = df_subset$StationCode,
      region = df_subset$Region
    )
    
    return(df_seg)
  }
  else{
    return(df_subset)
  }
  
}

# --- Blank Theme for Timeseries Graphs ---
blank_theme <- function(){
  theme_bw() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = 'black', size = 10, family = 'sans'),
      axis.text.x = element_text(angle = 45, vjust=0.5, margin = margin(t = 1)),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_text(size=12, hjust = 0.5),
      legend.position = 'top',
      legend.title = element_blank(),
      legend.box.margin=margin(-10,0,-10,0)
    )
}

# --- Create Plots for Facet Graph ---
create_plots <- function(df, region, color_scheme){
  # import blank theme
  blank_theme <- blank_theme()
  
  # filter df by analyte, year, and region
  df_filt <-
    df %>%
    filter(
      Analyte == analyte,
      Year == year,
      Region == region
    )

  # create segment (RL) df
  df_seg <- create_seg_df(df_filt)
  
  # define how many unique stations there are (for the color scheme)
  num_stats <- length(unique(df_filt$StationCode))
  
  # define color scheme
  colors = rev(brewer.pal(num_stats+2, color_scheme))
  
  # define x-lims
  date_max <- max(df$Date[df$Year == year])+3
  date_min <- min(df$Date[df$Year == year])-3
  
  # define flow action duration
  action_max <- unique(df_filt$PostFlowStart)
  action_min <- unique(df_filt$PreFlowEnd)
  
  # check if RL data exists
  RL_dat <- nrow(df_seg) > 0
  
  # plot timseries
  p <- ggplot() +
    annotate('rect', xmin = action_min, xmax = action_max, ymin = -Inf, ymax = Inf, alpha = .08)
  
  # add RL segments
  if (RL_dat) {
    p <- p +
      geom_segment( # vertical segment
        data = df_seg,
        mapping = aes(x = x_vert, xend = xend_vert, y = y_vert, yend = yend_vert, color = station),
        size = .8
      ) +
      geom_segment( # horizontal segment
        data = df_seg,
        mapping = aes(x = x_horz, xend = xend_horz, y = y_horz, yend = yend_horz, color = station),
        size = .8,
        lineend = 'square'
      )
  }
  
  # add timeseries data
  p <- p +
    geom_point( # points
      df_filt,
      mapping = aes(x = Date, y = Result, group = StationCode, color = StationCode),
      size = 3
    ) +
    geom_line( # line
      df_filt,
      mapping = aes(x = Date, y = Result, group = StationCode, color = StationCode),
      size = 1.3
    )
  
  # fix asthetics
  p <- p +
    blank_theme +
    scale_x_date(labels = date_format('%b'), breaks = pretty_breaks(10)) +
    xlim(date_min, date_max) +
    scale_color_manual(values=colors) +
    scale_fill_manual(values=colors) +
    ggtitle(region)
    
    return(p)
  }
