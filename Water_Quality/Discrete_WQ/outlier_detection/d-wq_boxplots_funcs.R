# FASTR - Discrete WQ Boxplots Functions
# purpose: functions for boxplots discrete WQ data for outlier detection
# author: Sarah Perry
# contact: seperry83@gmail.com

# import functions
library(stats)
library(NADA)
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

# --- Calculate Outliers ---
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
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
      # panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text = element_text(color = 'black', size = 10, family = 'sans'),
      axis.text.x = element_text(angle = 45, vjust=0.5, margin = margin(t = 1)),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_text(size=12, hjust = 0.5),
      legend.position='top',
      legend.title = element_blank(),
      legend.box.margin=margin(-10,0,-10,0)
    )
}

# --- Calculate Data for Boxplot ---
# adapted from NADA function 'cenboxplot'
calc_boxplt_data <- function(obs, cen, group) {
  # change group to factor
  group_factor <- as.factor(group)
  
  # initalize empty vectors to populate
  data <- numeric()
  groups <- character()
  
  if(length(levels(group_factor)) > 0) { # if there's any data for any group
    
    # for each level of the group
    for (i in levels(group_factor)) { 
      
      # define the boolean where T = censored and F = uncensored for the group
      uncen_data <- cen[group == i]
      
      if (length(uncen_data) > 0) { # if there's any data
        
        if (all(uncen_data)) { # if it's all censored data
          # skip to next group
          next
        }
        else if (all(!uncen_data)) { # if all uncensored data
          # populate the vectors using all values
          data <- c(data, obs[group == i])
          grp <- rep(i, length(obs[group == i]))
          groups <- c(groups, grp)
        }
        else { # if mix between censored/uncensored data
          # populate the vectors using ROS model values
          mod <- suppressWarnings(cenros(obs[group == i], cen[group == i])$modeled)
          grp <- rep(i, length(mod))
          data <- c(data, mod)
          groups <- c(groups, grp)
        }
        
      }
    }
    # define df to return with all data/groups
    df <- data.frame(Data = data, FullGroup = groups)
    
    return(df)
  }
}


# --- Plot Boxplot ---
cen_boxplt <- function(df) {
  #import blank theme
  blank_theme <- blank_theme()
  
  # define range of years for plot
  df$Year <- as.numeric(df$Year)
  
  max_year <- max(unique(df$Year))
  min_year <- min(unique(df$Year))
  
  # filter df by analyte
  df_filt <-
    df %>%
    filter(
      Analyte == analyte,
      StationCode == station
    )
  
  # define vectors for cenfit function
  obs <- df_filt$Result
  cen <- df_filt$LabDetect
  group <- df_filt$FullGroup
  
  # calculate values to use in boxplot
  df_data <- calc_boxplt_data(obs, cen, group)

  # combine the filtered and data df's
  df_boxplt <- inner_join(df_filt, df_data, by  = 'FullGroup')
  
  # add boolean outlier column
  df_boxplt <- df_boxplt %>%
    group_by(Year) %>%
    mutate(Outlier = ifelse(is_outlier(Data), Data, as.numeric(NA)))

  # plot boxplots
  bp <- ggplot() +
    geom_boxplot(df_boxplt, mapping = aes(x = Year, y = Data, group = FullGroup)) +
    geom_point(df_boxplt, mapping = aes(x = Year, y = Outlier, group = FullGroup, fill = ActionPhase), size = 3, shape = 21)
  
  bp <- bp +
    blank_theme
    #scale_x_date(labels = date_format('%Y'), breaks = 'years') +
    #xlim(min_year, max_year)
  
  return(bp)
}
  