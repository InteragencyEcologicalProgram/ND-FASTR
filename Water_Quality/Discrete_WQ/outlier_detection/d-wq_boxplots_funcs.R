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

cen_boxplt <- function (df) {
  
  # filter df by analyte
  df_filt <-
    df_wq %>%
    filter(
      Analyte == analyte,
      StationCode == station
    )
  
  # define vectors for cenfit function
  obs <- df$Result
  cen <- df$LabDetect
  group <- df$FullGroup
  
  
  data = numeric()
  groups = character()
  group_factor <- as.factor(group)
  
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
        data = c(data, obs[group == i])
        grp = rep(i, length(obs[group == i]))
        groups = c(groups, grp)
      }
      else { # if mix between censored/uncensored data
        # populate the vectors using ROS model values
        mod = suppressWarnings(cenros(obs[group == i], cen[group == i])$modeled)
        grp = rep(i, length(mod))
        data = c(data, mod)
        groups = c(groups, grp)
      }

      }
  }
      
      # plot boxplots
      # if (length(cen[group == i]) > 1) {   # if there's censored data
        # boxplot(data ~ as.factor(groups), log = log, range = range,
        #         ...)
        ret = data.frame(data = data, group = groups)
        # print(ret$group)
        bp <- ggplot(ret, aes(x=group, y=data, group=group)) +
          geom_boxplot(aes(fill=group))
        print(bp)
        # invisible(ret)
        # return(bp)
      # }
      # } else{ #if there isn't
      #   # print(i)
      #   ret = data.frame(ros.model = obs, group = groups)
      #   bp <- ggplot(ret, aes(x=group, y=ros.model, group=group)) +
      #     geom_boxplot(aes(fill=group))
      #   print(bp)
      #   # boxplot(obs ~ as.factor(groups), log = log, range = range,
      #   #         ...)
      #   #ret = data.frame(data = obs, group = groups)
      #   invisible(ret)
      #   return(bp)
      # }
    }
  }
}