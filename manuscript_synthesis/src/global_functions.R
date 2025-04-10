# NDFS Synthesis Manuscript
# Purpose: Global functions to be used across all scripts for the NDFS Synthesis
  # manuscript
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
library(here)
library(fs)
library(rlang)
library(patchwork)

# Define absolute file path for NDFA SharePoint
  # Function creates a filepath to the NDFA SharePoint site
  # The optional fp_rel argument allows for more specific file paths beyond the NDFA SharePoint
    # root directory
ndfa_abs_sp_path <- function(fp_rel = NULL) {
  fp_fastr <- "California Department of Water Resources/North Delta Flow Action - Documents"
  
  if (is.null(fp_rel)) {
    fp_abs <- path(Sys.getenv('USERPROFILE'), fp_fastr)
  } else {
    fp_abs <- path(Sys.getenv('USERPROFILE'), fp_fastr, fp_rel)
  }
  
  return(fp_abs)
}

# Add Flow Action Periods - Before, During, After flow action
ndfa_action_periods <- function(df) {
  # Import dates for flow action periods
  df_act_dates_orig <- read_csv(
    here("manuscript_synthesis/data/raw/FlowDatesDesignations_45days.csv"), 
    show_col_types = FALSE
  )
  
  # Convert date columns in df_act_dates_orig to date type
  df_act_dates_clean <- df_act_dates_orig %>% mutate(across(starts_with(c("Pre", "Post")), mdy))
  
  # Add a Year variable to df if it doesn't already exist
  if (!"Year" %in% names(df)) {
    df <- df %>% mutate(Year = year(Date))
    message("Adding a Year variable to the dataframe.")
  }
  
  # Join flow action dates to df and add FlowActionPeriod variable
  df %>%  
    left_join(df_act_dates_clean, by  = "Year") %>% 
    mutate(
      FlowActionPeriod = case_when(
        Date >= PreFlowStart & Date <= PreFlowEnd ~ "Before",
        Date > PreFlowEnd & Date < PostFlowStart ~ "During",
        Date >= PostFlowStart & Date <= PostFlowEnd ~ "After"
      )
    ) %>% 
    # Remove some variables from df_act_dates_clean
    select(-c(WYType, FlowPulseType, NetFlowDays, starts_with(c("PreFlow", "PostFlow"))))
}

# Create diagnostic plots for linear models to check assumptions
plot_lm_diag <- function(df_data, param_var, model, ...) {
  
  df_data <- df_data %>%
    mutate(
      Residuals = residuals(model),
      Fitted = predict(model)
    )
  
  param_name <- as_name(ensym(param_var))
  
  plt_hist <- ggplot(df_data, aes(x = Residuals)) +
    geom_histogram(...) +
    labs(
      title = "Residual Histogram", 
      x = "Residuals"
    ) +
    theme_bw()
  
  plt_qq <- ggplot(df_data, aes(sample = Residuals)) + 
    labs(
      title = "Residual Probability Plot", 
      x = "Normal Quantiles", 
      y = "Residuals"
    ) + 
    stat_qq() + 
    stat_qq_line(linewidth = 1, color = 'red') + 
    theme_bw()
  
  plt_res_fit <- ggplot(df_data, aes(x = Fitted, y = Residuals)) +
    geom_point() +
    labs(
      title = "Residuals vs. Fitted Values", 
      x = (paste0("Predicted ", param_name)),
      y = "Residuals",
    ) +
    theme_bw()
  
  plt_obs_fit <- ggplot(df_data, aes(x = Fitted, y = {{ param_var}})) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    labs(
      title = "Observed vs. Fitted Values", 
      x = paste0("Predicted ", param_name),
      y = paste0("Observed ", param_name)
    ) +
    theme_bw()
  
  (plt_hist + plt_qq) / (plt_res_fit + plt_obs_fit)
}

