# FASTR - Functions Chlorophyll and Ammonia Comparison
# purpose: functions for qualitatively determining relationship b/w chla and NH3
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

# --- Blank Theme for Timeseries Graphs ---
blank_theme <- function(){
  theme_bw() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = 'black', size = 14, family = 'sans'),
      axis.text.x = element_text(angle = 45, vjust=0.5, margin = margin(t = 1)),
      strip.text.x = element_text(size = 14),
      axis.title = element_text(size = 18, face = 'bold'),
      plot.title = element_text(size = 20, hjust = 0.5, face = 'bold'),
      legend.position = 'top',
      legend.box = 'vertical',
      legend.title = element_blank(),
      legend.title.align = 0,
      legend.box.margin = margin(-5,0,0,0),
      legend.spacing.y = unit(-0.2, 'cm'),
      legend.text = element_text(size = 13)
    )
}

# --- Create Facet Scatterplots  ---
create_graph <- function(df, vari){
  # import blank theme
  blank_theme <- blank_theme()
  
  # define relevant values
  if(varis[i] == 'BroadRegion') {
    fill <- 'BroadRegion'
    cmap_colors <- c('#999999', '#f781bf', '#B79F00', '#984ea3', '#377eb8', '#e41a1c')
  }
  else {
    fill <- 'Year'
  }

  # create base plot
  p <- ggplot() # +
  # facet_wrap(df[varis[i]][[1]] ~ ., ncol = 2, scales = 'free_y')

  # add scatter plot
  p <- p +
    geom_segment(data = df,
                 mapping = aes(x = 4, xend = 4, y = -Inf, yend = Inf),
                 color = '#2b2b2b',
                 linetype = 2,
                 size = 1) +
    geom_point(data = df,
             mapping = aes(x = DisAmmonia_umolL, y = Chla, fill = !!sym(fill), shape = ActionPhase),
             color = 'black',
             size = 4.5,
             stroke = 1.2
             ) 

  # fix asthetics
  if (varis[i] == 'Year') {
    p <- p +
      scale_fill_manual(values = cmap_colors)
  }
  
  p <- p +
    blank_theme +
    xlab(expression(bold(paste('NH'[4],' (\u03BCmol/L)',sep = '')))) +
    ylab(expression(bold(paste('Chlorophyll ', bolditalic('a'),' (\u03BCg/L)', sep = '')))) +
    xlim(0,20) +
    scale_shape_manual(values = c('During' = 21, 'Pre' = 22, 'Post' = 24)) +
    guides(fill = guide_legend(override.aes=list(shape=21))) +
    ggtitle(expression(bold(Chlorophyll~bolditalic(a)~vs.~NH[4])))
}