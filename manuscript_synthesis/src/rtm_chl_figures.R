# NDFS Synthesis Manuscript
# Purpose: Create time-series figures of the continuous chlorophyll data for the
  # NDFS Synthesis manuscript.
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov


# 1. Global Code and Functions -----------------------------------------------

# Load packages
library(tidyverse)
library(scales)
library(patchwork)
library(here)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())

# Function to add a grouping variable for flow pulse type - with or without
  # augmented pulse flow
grp_fp_type <- function(df) {
  # Define years with an augmented flow pulse
  yr_aug_fp <- c(2016, 2018, 2019)
  
  # Add grouping variable
  df %>% mutate(FlowPulseType = if_else(.data$Year %in% yr_aug_fp, "augmented", "not_augmented"))
}

# Function to create time-series plot of continuous chlorophyll data - defining
  # y-axis limits
plot_ts_chla <- function(df, df_fa, plt_title, y_max, n_breaks) {
  
  # Label y-axis if Region is Yolo Bypass
  y_label <- if (plt_title == "Yolo Bypass") expression(Chlorophyll~Fluoresence~(mu*g~L^{-1}))
  
  # Make legend span two rows if Region is Yolo Bypass
  n_row_legend <- if (plt_title == "Yolo Bypass") 2 else 1
  
  df %>% 
    ggplot(aes(x = Date, y = Chla, color = StationCode)) +
    geom_line() +
    facet_wrap(vars(Year), ncol = 1, scales = "free_x") +
    ggtitle(plt_title) +
    scale_x_date(
      name = "Date",
      breaks = breaks_pretty(10),
      labels = label_date_short(),
      expand = expansion(mult = 0.01)
    ) +
    scale_y_continuous(
      name = y_label,
      labels = label_comma(),
      limits = c(0, y_max),
      breaks = breaks_extended(n_breaks)
    ) +
    scale_color_viridis_d(
      name = "Station:", 
      option = "plasma", 
      end = 0.95
    ) +
    # Add shaded rectangles for the flow pulse periods
    geom_rect(
      aes(xmin = PreFlowEnd, xmax = PostFlowStart, ymin = -Inf, ymax = Inf),
      data = df_fa,
      inherit.aes = FALSE,
      alpha = 0.12,
      fill = "grey50"
    ) +
    theme_light() +
    theme(
      strip.text = element_text(color = "black"),
      legend.position = "top",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 7),
      legend.key.size = unit(0.6, "lines"),
      legend.margin = margin(2, 0, 0 ,0),
      plot.title = element_text(hjust = 0.5),
      panel.grid.minor = element_blank(),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 8)
    ) +
    guides(color = guide_legend(nrow = n_row_legend, byrow = TRUE))
}


# 2. Import Data -------------------------------------------------

# Define file path for processed chlorophyll data
fp_data <- here("manuscript_synthesis/data/processed")

# Import daily average chlorophyll data
df_chla <- readRDS(here("manuscript_synthesis/data/processed/chla_wt_daily_avg_2013-2019.rds"))

# Import continuous WQ data and dates of flow action periods
df_fa_dates <- read_csv(here("manuscript_synthesis/data/raw/FlowDatesDesignations_45days.csv"))


# 3. Prepare Data  --------------------------------------------------------

# Create a vector for the factor order of StationCode
sta_order <- c(
  "RCS",
  "RD22",
  "I80",
  "LIS",
  "STTD",
  "LIB",
  "RYI",
  "RVB"
)

# Prepare daily average chlorophyll data for time-series plots
df_chla_c <- df_chla %>% 
  select(-WaterTemp) %>% 
  drop_na(Chla) %>% 
  mutate(Year = year(Date)) %>% 
  # Fill in missing dates with NA values for geom_line to not interpolate data gaps
  group_by(StationCode, Year) %>% 
  complete(Date = seq.Date(min(Date), max(Date), by = "1 day")) %>%
  ungroup() %>% 
  mutate(
    # Add a grouping variable for Region and apply factor order for it
    Region = if_else(
      StationCode %in% c("RCS", "RD22", "I80", "LIS", "STTD"),
      "Yolo Bypass",
      "CSC and Lower Sac River"
    ),
    Region = factor(Region, levels = c("Yolo Bypass", "CSC and Lower Sac River")),
    # Apply factor order to StationCode
    StationCode = factor(StationCode, levels = sta_order)
  ) %>% 
  # Add a grouping variable for flow pulse type - with or without augmented pulse flow
  grp_fp_type()

# Create a tibble to define max values for the y-axes of the time-series plots -
  # grouped by FlowPulseType
df_ymax <- df_chla_c %>% 
  summarize(
    # Add a 5% buffer to ymax
    ymax = ceiling(max(Chla, na.rm = TRUE)) * 1.05,
    .by = FlowPulseType
  )

# Prepare dates of flow action periods to highlight the flow action periods for each year 
  # in the plots
df_fa_dates_c <- df_fa_dates %>% 
  select(Year, PreFlowEnd, PostFlowStart) %>% 
  # Exclude to 2013-2019
  filter(Year %in% 2013:2019) %>% 
  mutate(
    across(where(is.character), mdy),
    # add 1 day to PreFlowEnd so that the highlight for the flow action periods aligns correctly
    PreFlowEnd = PreFlowEnd + days(1)
  ) %>% 
  # Add a grouping variable for flow pulse type - with or without augmented pulse flow
  grp_fp_type()


# 4. Create Figures -------------------------------------------------------

# Nest data frame with flow action periods by FlowPulseType so data frames can
  # be joined to WQ data for plotting
ndf_fa_dates <- df_fa_dates_c %>% nest(.by = FlowPulseType, .key = "df_fa_dates")

# Create time-series figures of continuous chlorophyll data 
ndf_chla_plt <- df_chla_c %>% 
  arrange(FlowPulseType, Region) %>% 
  # First, create individual plots for each Region-FlowPulseType combination
  nest(.by = c(FlowPulseType, Region), .key = "df_data") %>% 
  list(ndf_fa_dates, df_ymax) %>% 
  reduce(left_join, by = join_by(FlowPulseType)) %>% 
  mutate(
    num_breaks = if_else(FlowPulseType == "augmented", 7, 5),
    plt_indiv = pmap(list(df_data, df_fa_dates, Region, ymax, num_breaks), plot_ts_chla)
  ) %>% 
  # Next, combine plots for each FlowPulseType
  select(FlowPulseType, plt_indiv) %>% 
  chop(plt_indiv) %>% 
  mutate(plt_comb = map(plt_indiv, wrap_plots))


# 5. Export Figures -------------------------------------------------------

# Define file path to export plots to
fp_ts_plt <- here("manuscript_synthesis/results/figures")

# Export figures for years with and without augmented flow pulses
pwalk(
  list(
    ndf_chla_plt$plt_comb,
    ndf_chla_plt$FlowPulseType,
    c(7.5, 8.3)
  ),
  ~ ggsave(
    plot = ..1,
    filename = paste0(fp_ts_plt, "/chl_ts_", ..2, "_fp.jpg"),
    width = 6.5, 
    height = ..3, 
    units = "in", 
    dpi = 300
  )
)

