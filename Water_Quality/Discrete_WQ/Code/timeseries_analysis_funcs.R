# FASTR - Fuctions for Discrete WQ Timeseries Analysis
# purpose: functions for timeseries analysis of discrete WQ data for FASTR
# author: Sarah Perry
# contact: seperry83@gmail.com

# import packages
library(scales)
library(ggplot2)
library(tidyverse)

# --- Import Data from Sharepoint ---
read_in_data <- function(rel_fp){
  # define absolute filepath
  abs_fp <- normalizePath(file.path(Sys.getenv('USERPROFILE'), rel_fp))

  # read in data
  df <- read_csv(abs_fp)

  return(df)
}

# --- Add Phase Actions ---
# adapted from Cat Pien's code


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
    
  }else{
    return(df_subset)
  }
  
}

# --- Blank Theme for Timeseries Graphs ---
blank_theme <- function(){
  theme_bw() +
    theme(
      panel.border = element_blank(),
    # panel.grid.major = element_blank(),
    # panel.grid.major.x = element_blank() ,
    # panel.grid.minor = element_blank(),
    # axis.line = element_blank(),#line(color = 'black'),
    axis.text = element_text(color = 'black', size = 10, family = 'sans'),
    axis.text.x = element_text(angle = 90, vjust=0.5, margin = margin(t = 1)),
    # axis.title.x = element_blank(),#text(size = 14, family = 'sans'),
    # axis.title.y = element_blank(),#text(size = 14, family = 'sans'),
    # plot.title = element_text(size=13, face='bold', hjust = 0.5),
    # legend.position='top',
    legend.title = element_blank()
    )
}

# --- Plot Timeseries w/ RLs ---
plot_timeseries_RL <- function(df, region, color_scheme){

  # import blank theme
  blank_theme <- blank_theme()
  
  # filter df by analyte and year
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

  # check if RL data exists
  RL_dat <- nrow(df_seg) > 0
  # plot
  if (RL_dat) {
  p <- ggplot() +
      geom_point( # points
        df_filt,
        mapping = aes(x = Date, y = Result, group = StationCode, color = StationCode),
        size = 3
      ) +
      geom_line( # line
        df_filt,
        mapping = aes(x = Date, y = Result, group = StationCode, color = StationCode),
        size = 1.3
      ) +
    geom_segment( # vertical segment
      data = df_seg,
      mapping = aes(x = x_vert, xend = xend_vert, y = y_vert, yend = yend_vert, color = station),
      size = .8 ) +
    geom_segment( # horizontal segment
      data = df_seg,
      mapping = aes(x = x_horz, xend = xend_horz, y = y_horz, yend = yend_horz, color = station),
      size = .8,
      lineend = 'square'
      )
  
  p <- p +
    blank_theme +
    scale_x_date(labels = date_format('%b'), breaks = 'months') +
    scale_color_manual(values=colors) +
    scale_fill_manual(values=colors) +
    ggtitle(paste(analyte,region,year))

   return(p)
 }
}

# --- Plot Timeseries w/o RLs ---
plot_timeseries_no_RL <- function(df, region, color_scheme){
  
  # import blank theme
  blank_theme <- blank_theme()
  
  # filter df by analyte and year
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
  
  # check if RL data exists
  RL_dat <- nrow(df_seg) > 0
  
  # plot
  if (!RL_dat) {
    p <- ggplot() +
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
    
    p <- p +
      blank_theme +
      scale_x_date(labels = date_format('%b'), breaks = 'months') +
      scale_color_manual(values=colors) +
      scale_fill_manual(values=colors) +
      ggtitle(paste(analyte,region,year))
    
    return(p)
  }
}

# --- Create Facet Graph ---
create_plots <- function(df, region, color_scheme){
  # create plots with RL
  plts <- mapply(region = regions, color_scheme = colors, FUN = function(x, region, color_scheme) plot_timeseries_RL(df, region, color_scheme))
  
  # create plots w/o RL and concatenate
  plts <- c(plts, mapply(region = regions, color_scheme = colors, FUN = function(x, region, color_scheme) plot_timeseries_no_RL(df, region, color_scheme)))
  
  # remove null plots
  plts <- compact(plts)
  
  return(plts)
}
