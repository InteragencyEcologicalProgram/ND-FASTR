# NDFS Synthesis Manuscript
# Purpose: Create time-series figures of the continuous chlorophyll and specific
  # conductance data for the NDFS Synthesis manuscript.
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov


# 1. Global Code and Functions -----------------------------------------------

# Load packages
library(tidyverse)
library(scales)
library(here)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())


# 2. Import Data -------------------------------------------------

# Define root file path for data
fp_data <- here("manuscript_synthesis/data")

# Import daily average water quality data
df_wq <- readRDS(file.path(fp_data, "processed/wq_daily_avg_2013-2019.rds"))

# Import dates of flow action periods
df_fa_dates <- read_csv(file.path(fp_data, "raw/FlowDatesDesignations_45days.csv"))


# 3. Prepare Data  --------------------------------------------------------

# Create a vector for the factor order of StationCode
sta_order <- c("RD22", "STTD", "LIB", "RVB")

# Create vector of parameter names to be manipulated in later functions
wq_params <- c("Chla", "SpCnd")

# Prepare daily average chlorophyll and specific conductance data for
  # time-series plots
df_wq_c <- df_wq %>% 
  select(StationCode, Date, all_of(wq_params)) %>% 
  pivot_longer(
    cols = all_of(wq_params), 
    names_to = "Parameter", 
    values_to = "Value"
  ) %>% 
  drop_na(Value) %>% 
  mutate(Year = year(Date)) %>% 
  # Filter to only include representative stations for 4 habitat types (RD22,
    # STTD, LIB, RVB)
  filter(StationCode %in% sta_order) %>% 
  # Apply factor order to StationCode
  mutate(StationCode = factor(StationCode, levels = sta_order)) %>% 
  # Fill in missing dates with NA values for geom_line to not interpolate data gaps
  group_by(StationCode, Year, Parameter) %>% 
  complete(Date = seq.Date(min(Date), max(Date), by = "1 day")) %>%
  ungroup()

# Prepare dates of flow action periods to highlight the flow action periods for each year 
  # in the plots
df_fa_dates_c <- df_fa_dates %>% 
  select(Year, PreFlowEnd, PostFlowStart) %>% 
  # Exclude to 2013-2019
  filter(Year %in% 2013:2019) %>% 
  mutate(
    across(where(is.character), mdy),
    # add 1 day to PreFlowEnd so that the highlight for the flow action periods aligns correctly
    PreFlowEnd = PreFlowEnd + days(1),
    # Add a grouping variable for flow pulse type - with or without augmented pulse flow
    FlowPulseType = if_else(Year %in% c(2016, 2018, 2019), "augmented", "not_augmented")
  )


# 4. Create Figures -------------------------------------------------------

# Function to create time-series plot of daily average water quality data
plot_ts_wq <- function(df, param) {
  
  # Define y-axis label
  y_label <- if (param == "Chla") {
    expression(Chlorophyll~Fluoresence~(mu*g~L^{-1}))
  }  else if (param == "SpCnd") {
    expression(Specific~Conductance~(mu*S~cm^{-1}))
  }
  
  df %>% 
    ggplot(aes(x = Date, y = Value, color = StationCode)) +
    geom_line() +
    facet_wrap(vars(Year), ncol = 2, scales = "free") +
    scale_x_date(
      name = "Date",
      breaks = breaks_pretty(10),
      labels = label_date_short(),
      expand = expansion(mult = 0.01)
    ) +
    scale_y_continuous(
      name = y_label,
      labels = label_comma()
    ) +
    scale_color_viridis_d(
      name = "Station:", 
      option = "plasma",
      end = 0.9
    ) +
    # Add shaded rectangles for the flow pulse periods
    geom_rect(
      aes(
        xmin = PreFlowEnd,
        xmax = PostFlowStart,
        ymin = -Inf,
        ymax = Inf,
        fill = FlowPulseType
      ), 
      data = df_fa_dates_c,
      inherit.aes = FALSE,
      alpha = 0.12
    ) +
    scale_fill_manual(
      values = c(not_augmented = "gray40", augmented = "darkblue"), 
      guide = "none"
    ) +
    theme_light() +
    theme(
      strip.text = element_text(color = "black"),
      legend.position = "top",
      panel.grid.minor = element_blank()
    )
}

# Create time-series figures of daily average chlorophyll and specific
  # conductance data
ndf_wq_plt <- df_wq_c %>% 
  nest(.by = Parameter, .key = "df_data") %>% 
  mutate(plt_ts = map2(df_data, Parameter, plot_ts_wq))


# 5. Export Figures -------------------------------------------------------

# Define file path to export plots to
fp_ts_plt <- here("manuscript_synthesis/results/figures")

# Export figures of daily average chlorophyll and specific conductance data
walk2(
  ndf_wq_plt$plt_ts,
  ndf_wq_plt$Parameter,
  ~ ggsave(
    plot = .x,
    filename = paste0(fp_ts_plt, "/", .y, "_daily_avg_ts.jpg"),
    width = 6.5, 
    height = 8.25, 
    units = "in", 
    dpi = 300
  )
)

