# NDFA Water Quality
# Purpose: Functions to be used for the exploratory data analysis of the contaminants data
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
library(scales)
library(rlang)
library(glue)


# Convert StationCode to a factor
conv_fact_sta_code <- function(df) {
  # Define factor order for StationCode
  sta_code_order <- c("RMB", "RCS", "RD22", "LIS", "STTD", "BL5", "RYI", "SHR")
  
  # Convert variable to factor
  df %>% mutate(StationCode = factor(StationCode, levels = sta_code_order))
}

# Convert SamplingEvent to a factor
conv_fact_se <- function(df, df_se) {
  # Only perform function if df is not NULL
  if (!is.null(df)) {
    # Define factor order for SamplingEvent
    se_order <- levels(df_se$SamplingEvent)
    df %>% mutate(SamplingEvent = factor(SamplingEvent, levels = se_order))
  }
}

# Create a vector of colors for the x-axis labels
  # not officially supported by ggplot2
def_xaxis_colors <- function(df) {
  df %>% 
    distinct(SamplingEvent, FlowActionPeriod) %>% 
    arrange(SamplingEvent) %>% 
    mutate(
      color = case_when(
        FlowActionPeriod == "Before" ~ "blue",
        FlowActionPeriod == "During" ~ "green",
        FlowActionPeriod == "After" ~ "black"
      )
    ) %>% 
    pull(color)
}

# Create barplot of contaminants data - all data types
create_barplot <- function(df, df_missing, df_se, fill_var, type = c("stack", "fill")) {
  
  # Define options for fill argument
  match.arg(type, c("stack", "fill"))
  
  # Convert fill_var to enquo and string for tidy evaluation
  fill_var_enquo <- enquo(fill_var)
  fill_var_text <- as_name(fill_var_enquo)
  
  # Create vector of colors for the x-axis labels
  xaxis_colors <- def_xaxis_colors(df_se)
  
  # Define legend title based on fill_var
  legend_title <- case_when(
    fill_var_text == "PesticideType" ~ "Pesticide Type:",
    fill_var_text == "PesticideClass" ~ "Pesticide Class:",
    fill_var_text == "Analyte" ~ "Contaminant:"
  )
  
  # Define y variable and y-axis label for stacked barplots based on fill_var
  if (str_detect(fill_var_text, "^Pest")) {
    y_var <- sym("TotalConc")
    y_label <- glue("Total Concentration ({df$Units[1]})")
  } else if (fill_var_text == "Analyte") {
    y_var <- sym("Result")
    y_label <- glue("Concentration ({df$Units[1]})")
  }
  
  # Create base plot
  p <- df %>% 
    ggplot(aes(SamplingEvent, !!y_var, fill = !!fill_var_enquo)) +
    facet_grid(rows = vars(StationCode)) +
    theme_light() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, color = xaxis_colors),
      strip.text = element_text(color = "black"),
      legend.position = "top"
    ) +
    scale_x_discrete(
      name = "Sampling Event",
      drop = FALSE
    ) +
    scale_fill_viridis_d(name = legend_title, option = "plasma") 
  
  # Make barplot either stacked or filled based on type argument
  if (type == "fill") {
    p <- p + 
      geom_col(position = "fill") +
      scale_y_continuous(name = "Percentage of Total Concentration", labels = label_percent())
  } else {
    p <- p + 
      geom_col() +
      scale_y_continuous(name = y_label, labels = label_comma())
  }
  
  # Add text labels for missing data if necessary
  if (!is.null(df_missing)) {
    p <- p +
      geom_text(
        aes(x = SamplingEvent, y = 0, label = txt_label, color = label_grp),
        data = df_missing,
        inherit.aes = FALSE,
        angle = 90,
        hjust = "left",
        size = 2.5
      ) +
      scale_color_manual(
        name = NULL,
        values = c("All below MDL" = "red", "Not sampled" = "black"),
        guide = "none"
      )
  }
  
  return(p)
}

