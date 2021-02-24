# NDFA Water Quality
# Purpose: Functions to be used for testing outlier detection methods and plotting their results
# Author: Dave Bosworth
# Contacts: David.Bosworth@water.ca.gov


# Load packages
library(ggplot2)
library(plotly)
library(scales)


# Convert real-time quality control labels to an ordered factor
outlier_test_factor <- function(x) {
  factor(x, c(1L, 3L, 4L), c("pass", "suspect", "fail"), ordered = TRUE)
}


# Create interactive plots to visualize results of outlier detection methods
outlier_test_plot <- function(df, qual_var, plot_title) {
  # Convert qual_var to enquo for tidy evaluation
  qual_var_enquo <- enquo(qual_var)
  
  # Create plot
  p <- df %>% 
    ggplot(aes(DateTime, value, color = !!qual_var_enquo)) +
    geom_point() +
    ggtitle(plot_title)
  
  ggplotly(p)
}

# Create ggplots to visualize final outlier detection procedures
outlier_final_plot <- function(df, param_var) {
  # Convert param_var to enquo for tidy evaluation
  param_var_enquo <- enquo(param_var)
  
  # Create plot
  df %>% 
    ggplot(aes(DateTime, !!param_var_enquo, color = qual_final)) +
    geom_point() +
    scale_color_manual(
      name = "Qual Final",
      values = c(
        "pass" = "black",
        "suspect" = "green",
        "fail" = "orange"
      )
    ) +
    scale_x_datetime(
      name = "Date",
      breaks = breaks_pretty(15),
      labels = label_date_short()
    )
}

