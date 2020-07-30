# FASTR - Discrete WQ Boxplots Functions
# purpose: functions for boxplots discrete WQ data for outlier detection
# author: Sarah Perry
# contact: seperry83@gmail.com

# import functions
library(ggplot2)

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

# --- Plot Boxplot ---
# adapted from NADA package
cen_boxplt <- function (obs, cen, group, log = TRUE, range = 0, ...) {
  # determine if log scaled is used
  if (log) {
    log = "y" 
  } else {
    log = ""
  }
  
  # determine if obs are grouped
  if (missing(group)) {
    ret = boxplot(cenros(obs, cen), log = log, range = range,
                  ...)
  } else {
    modeled = numeric()
    groups = character()
    
    for (i in levels(as.factor(group))) {
      # if censored data, perform ROS model
      if (length(cen[group == i]) > 1) {
        mod = suppressWarnings(cenros(obs[group == i], cen[group ==
                                                           i])$modeled)
        grp = rep(i, length(mod))
        modeled = c(modeled, mod)
        groups = c(groups, grp)
      }
      }
  }
  # plot boxplots
  if (length(cen[group == i]) > 1) {
    boxplot(modeled ~ as.factor(groups), log = log, range = range,
            ...)
    ret = data.frame(ros.model = modeled, group = groups)
  } else{
    boxplot(obs ~ as.factor(groups), log = log, range = range,
             ...)
    ret = data.frame(data = obs, group = groups)
  }
  abline(h = max(obs[cen]))
  invisible(ret)
}