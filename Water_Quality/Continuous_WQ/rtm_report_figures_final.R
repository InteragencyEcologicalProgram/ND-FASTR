# NDFA Water Quality
# Purpose: Create figures of the continuous water quality data for the NDFA Synthesis report:
  # 1) Time-series plots of managed and non-managed years
  # 2) Boxplots comparing years, flow pulse periods, and regions
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov


# 1. Global Code and Functions -----------------------------------------------

# Load packages
library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)
library(glue)

# Source functions
source("global_ndfa_funcs.R")
source("Water_Quality/global_wq_funcs.R")

# Internal function to define y-axis labels in plots
int_define_yaxis_lab <- function(param) {
  yaxis_lab <- case_when(
    param == "Chla" ~ "Chlorophyll (µg/L)",
    param == "DO" ~ "Dissolved Oxygen (mg/L)",
    param == "fDOM" ~ "fDOM (µg/L as QSE)",
    param == "NitrateNitrite" ~ "Nitrate + Nitrite (mg/L as N)",
    param == "pH" ~ "pH (pH units)",
    param == "SpCnd" ~ "Specific Conductance (µS/cm)",
    param == "Turbidity" ~ "Turbidity (FNU)",
    param == "WaterTemp" ~ "Water Temperature (degrees C)"
  )
  
  return(yaxis_lab)
}


# 2. Import Data -------------------------------------------------

# Define relative file path for file containing all QA'ed and cleaned continuous WQ data
fp_rel_rtm_data <- "WQ_Subteam/Processed_Data/Continuous/RTM_INPUT_all_2021-04-20.csv"

# Define relative file path for file containing dates of flow action periods
fp_rel_fa_dates <- "Data Management/FlowDatesDesignations_45days.csv"

# Define relative file path for files containing results of ANOVA models (F-tests and post-hoc)
fp_rel_anova <- "WQ_Subteam/Analysis_Results/CWQ"

# Define absolute file paths
fp_abs_rtm_data <- ndfa_abs_sp_path(fp_rel_rtm_data)
fp_abs_fa_dates <- ndfa_abs_sp_path(fp_rel_fa_dates)
fp_abs_anova <- ndfa_abs_sp_path(fp_rel_anova)

# Import continuous WQ data and dates of flow action periods
df_rtm_orig <- import_rtm_data(fp_abs_rtm_data, 10)
df_fa_dates_orig <- read_csv(fp_abs_fa_dates)

# Import results of ANOVA models
df_anova_f_test <- read_csv(file.path(fp_abs_anova, "cwq_aov_daily_avg_sea-none.csv"))
df_anova_mc_yr <- read_csv(file.path(fp_abs_anova, "cwq_emmeans_year_daily_avg_sea-none.csv"))
df_anova_mc_fa <- read_csv(file.path(fp_abs_anova, "cwq_emmeans_flowaction_daily_avg_sea-none.csv"))
df_anova_mc_region <- read_csv(file.path(fp_abs_anova, "cwq_emmeans_region_daily_avg_sea-none.csv"))


# 3. Clean WQ Data ---------------------------------------------------------

# Clean original continuous WQ data and calculate daily averages
df_rtm_clean <- df_rtm_orig %>% 
  mutate(
    DateTime = ymd_hms(DateTime),
    Date = date(DateTime),
    Year = year(DateTime)
  ) %>%
  # Don't include Flow, FlowTF, and all Qual variables
  select(-c(ends_with("_Qual"), starts_with("Flow"))) %>% 
  # Exclude SDI and SGG from the plots
  filter(!StationCode %in% c("SDI", "SGG")) %>% 
  # Pivot parameters in the long format
  pivot_longer(cols = where(is.numeric) & !Year, names_to = "Parameter", values_to = "Value") %>% 
  # Remove all NA values
  filter(!is.na(Value)) %>% 
  # Calculate daily averages
  group_by(Parameter, Year, StationCode, Date) %>% 
  summarize(Daily_avg = mean(Value)) %>% 
  ungroup()
  

# 4. Time-series Plots ----------------------------------------------------

# 4.1 Prepare Data for Plots ----------------------------------------------

# Create a vector for the factor order of StationCode
sta_order <- c(
  "RCS",
  "RD22",
  "I80",
  "LIS",
  "TOE",
  "STTD",
  "LIBCUT",
  "LIB",
  "RYI",
  "RVB",
  "SRH"
)

# Create a function to add grouping variables for FlowActionType - low vs. high pulse
add_FlowActionType <- function(df) {
  df %>% 
    mutate(
      # For the plots with all years of data
      FlowActionType = case_when(
        Year %in% c(2011, 2015) ~ "high pulse- low mag, long duration",
        Year %in% c(2012, 2016, 2018, 2019) ~ "high pulse- high mag, shorter duration",
        TRUE ~ "low pulse"
      ),
      # A more-simplified version for the fDOM and NitrateNitrite plots
      FlowActionType_s = if_else(str_detect(FlowActionType, "^high"), "high", "low")
    )
}

# Prepare daily average continuous WQ data for time-series plots
df_rtm_clean_ts <- df_rtm_clean %>% 
  # Fill in missing dates with NA values for geom_line to not interpolate data gaps
  group_by(Parameter, Year, StationCode) %>% 
  complete(Date = seq.Date(min(Date), max(Date), by = "1 day")) %>%
  ungroup() %>% 
  # Only keep stations in sta_order
  filter(StationCode %in% sta_order) %>% 
  mutate(
    # Add a grouping variable for Region and apply factor order for it
    Region = factor(
      if_else(
        StationCode %in% c("RCS", "RD22", "I80", "LIS", "TOE", "STTD"),
        "a_Upstream",
        "b_Downstream_Sac"
      )
    ),
    # Apply factor order to StationCode
    StationCode = factor(StationCode, levels = sta_order)
  ) %>% 
  # Add a grouping variable for FlowActionType - low vs. high
  add_FlowActionType()

# Prepare dates of flow action periods to highlight the flow action periods for each year 
  # in the plots and to define consistent x-axis limits
df_fa_dates_f <- df_fa_dates_orig %>% 
  select(Year, starts_with(c("Pre", "Post"))) %>% 
  mutate(across(where(is.character), mdy)) %>% 
  # add 1 day to PreFlowEnd so that the highlight for the flow action periods aligns correctly
  mutate(PreFlowEnd = PreFlowEnd + days(1)) %>% 
  # Add a grouping variable for FlowActionType - low vs. high
  add_FlowActionType()

# 4.2 Create Plot Functions -----------------------------------------------------

# Internal function to define titles for each time-series plot
int_define_ts_plot_title <- function(region) {
  plot_title <- if_else(
    region == "a_Upstream", 
    "Upstream Region", 
    "Downstream Region and Middle Sac River"
  )
  
  return(plot_title)
}

# Create a list for custom formatting of time-series plots
ts_cust_format <- list(
  theme_light(),
  theme(
    strip.text = element_text(color = "black"),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  ),
  scale_x_date(
    name = "Date",
    breaks = breaks_pretty(10),
    labels = label_date_short(),
    expand = expansion(mult = 0.01)
  ),
  scale_color_viridis_d(
    name = "Station:", 
    option = "plasma", 
    end = 0.95
  )
)

# Create function to add shaded rectangles for the flow action periods
add_fa_rect <- function(df) {
  geom_rect(
    aes(xmin = PreFlowEnd, xmax = PostFlowStart, ymin = -Inf, ymax = Inf),
    data = df,
    inherit.aes = FALSE,
    alpha = 0.12,
    fill = "grey50"
  )
}

# Create time-series plot of continuous WQ Data by Region
create_ts_plot_reg <- function(df, df_fa, param, region_cat, y_scale) {
  
  # define y-axis label
  y_lab <- int_define_yaxis_lab(param)
  
  # define plot label
  plot_title <- int_define_ts_plot_title(region_cat)
  
  # create base plot
  p <- df %>% 
    ggplot(aes(x = Date, y = Daily_avg, color = StationCode)) +
    geom_line() +
    ts_cust_format +
    ggtitle(plot_title) +
    add_fa_rect(df_fa)
  
  # only label y-axis for the plot on the left (Upstream Region)
  if (region_cat == "a_Upstream") {
    p <- p + scale_y_continuous(name = y_lab, labels = label_comma())
  } else {
    p <- p + scale_y_continuous(name = NULL, labels = label_comma())
  }
  
  # create facets with either fixed or free y-axis scales based on the y_scale argument
  if (y_scale == "free") {
    p <- p + facet_wrap(vars(Year), ncol = 1, scales = "free")
  } else {
    p <- p + facet_wrap(vars(Year), ncol = 1, scales = "free_x")
  }
  
  return(p)
}

# Create time-series plot of continuous WQ Data of all stations - for parameters that were
  # collected at a limited number of stations
create_ts_plot_all <- function(df, df_fa, param) {
  # define y-axis label
  y_lab <- int_define_yaxis_lab(param)
  
  # create plot
  p <- df %>% 
    ggplot(aes(x = Date, y = Daily_avg, color = StationCode)) +
    geom_line() +
    facet_wrap(
      vars(Year), 
      ncol = 2,
      scales = "free_x"
    ) +
    ts_cust_format +
    scale_y_continuous(
      name = y_lab, 
      labels = label_comma()
    ) +
    add_fa_rect(df_fa)
  
  return(p)
}

# 4.3 Create and Export Plots ---------------------------------------------

# Nest data frame with flow action periods by FlowActionType and FlowActionType_s so data frames 
  # can be joined to WQ data for plotting
df_fa_dates_f_nest <- df_fa_dates_f %>% 
  select(-FlowActionType_s) %>% 
  nest(df_fa_dates = -FlowActionType)

df_fa_dates_f_nest_s <- df_fa_dates_f %>% 
  filter(Year > 2012) %>% 
  select(-FlowActionType) %>% 
  nest(df_fa_dates = -FlowActionType_s)

# Create plots for common parameters (Chla, DO, pH, SpCnd, Turbidity, WaterTemp)
df_rtm_ts_plt <- df_rtm_clean_ts %>% 
  # keep only common parameters and stations
  filter(
    Parameter %in% c("Chla", "DO", "pH", "SpCnd", "Turbidity", "WaterTemp"),
    StationCode != "LIBCUT",
    # keep Turbidity data for TOE for 2014 and 2015 since Turbidity 
      # wasn't collected at STTD during those years
    !(StationCode == "TOE" & Parameter != "Turbidity"),
    !(StationCode == "TOE" & Year > 2015)
  ) %>% 
  mutate(StationCode = fct_drop(StationCode)) %>% 
  select(-FlowActionType_s) %>% 
  arrange(Parameter, FlowActionType, Region) %>% 
  nest(df_data = -c(Parameter, FlowActionType, Region)) %>% 
  left_join(df_fa_dates_f_nest) %>% 
  # create a y_scale variable to specify free or fixed y-axis scale
  mutate(
    y_scale = case_when(
      Parameter %in% c("DO", "pH", "WaterTemp") & str_detect(Region, "^a") ~ "free",
      Parameter %in% c("DO", "pH", "WaterTemp") & str_detect(Region, "^b") ~ "fixed",
      Parameter %in% c("Chla", "SpCnd", "Turbidity") ~ "free"
    )
  ) %>% 
  mutate(
    plt_indiv = pmap(
      list(df_data, df_fa_dates, Parameter, Region, y_scale), 
      .f = create_ts_plot_reg
    )
  ) %>% 
  select(Parameter, FlowActionType, plt_indiv) %>% 
  nest(plt_list = plt_indiv) %>% 
  mutate(plt_comb = map(plt_list, ~wrap_plots(pull(.x, plt_indiv))))

# Define file path to export plots to
fp_abs_ts_plt <- ndfa_abs_sp_path("WQ_Subteam/Plots/Continuous/Report/Time-series")

# Export plots for high pulse- high mag, shorter duration
df_rtm_ts_plt_high_mag_short_dur <- df_rtm_ts_plt %>% 
  filter(FlowActionType == "high pulse- high mag, shorter duration")

walk2(
  df_rtm_ts_plt_high_mag_short_dur$plt_comb,
  df_rtm_ts_plt_high_mag_short_dur$Parameter,
  ~ggsave(
    plot = .x,
    filename = paste0(fp_abs_ts_plt, "/", .y, "_ts_high_pulse_high_mag_short_dur.jpg"),
    width = 8.5, 
    height = 9.75, 
    units = "in", 
    dpi = 300
  )
)

# Export plots for high pulse- low mag, long duration
df_rtm_ts_plt_low_mag_long_dur <- df_rtm_ts_plt %>% 
  filter(FlowActionType == "high pulse- low mag, long duration")

walk2(
  df_rtm_ts_plt_low_mag_long_dur$plt_comb,
  df_rtm_ts_plt_low_mag_long_dur$Parameter,
  ~ggsave(
    plot = .x,
    filename = paste0(fp_abs_ts_plt, "/", .y, "_ts_high_pulse_low_mag_long_dur.jpg"),
    width = 8.5, 
    height = 7,
    units = "in", 
    dpi = 300
  )
)

# Slightly expand the width of the plot for specific conductance in 
  # high pulse- low mag, long duration years since a small section of the legend was cut off
ggsave(
  plot = df_rtm_ts_plt_low_mag_long_dur$plt_comb[[4]],
  filename = file.path(fp_abs_ts_plt, "SpCnd_ts_high_pulse_low_mag_long_dur.jpg"),
  width = 8.6, 
  height = 7, 
  units = "in", 
  dpi = 300
)

# Export plots for low pulse
df_rtm_ts_plt_low_pulse <- df_rtm_ts_plt %>% filter(FlowActionType == "low pulse")

walk2(
  df_rtm_ts_plt_low_pulse$plt_comb,
  df_rtm_ts_plt_low_pulse$Parameter,
  ~ggsave(
    plot = .x,
    filename = paste0(fp_abs_ts_plt, "/", .y, "_ts_low_pulse.jpg"),
    width = 8.5, 
    height = 9, 
    units = "in", 
    dpi = 300
  )
)

# Slightly expand the width of the plot for specific conductance in 
  # low pulse years since a small section of the legend was cut off
ggsave(
  plot = df_rtm_ts_plt_low_pulse$plt_comb[[4]],
  filename = file.path(fp_abs_ts_plt, "SpCnd_ts_low_pulse.jpg"),
  width = 8.6, 
  height = 9, 
  units = "in", 
  dpi = 300
)

# Create plots for fDOM and Nitrate + Nitrite:
df_rtm_ts_plt_nitr_fdom <- df_rtm_daily_avg %>% 
  filter(
    Parameter %in% c("fDOM", "NitrateNitrite"),
    Year > 2012
  ) %>%
  arrange(Parameter, FlowActionType_s) %>%
  select(-FlowActionType) %>% 
  nest(df_data = -c(Parameter, FlowActionType_s)) %>% 
  left_join(df_fa_dates_f_nest_s) %>% 
  mutate(
    plt = pmap(
      list(df_data, df_fa_dates, Parameter),
      .f = create_ts_plot_all
    )
  )

# Export fDOM and Nitrate + Nitrite time-series plots
pwalk(
  list(
    df_rtm_ts_plt_nitr_fdom$plt,
    df_rtm_ts_plt_nitr_fdom$Parameter,
    df_rtm_ts_plt_nitr_fdom$FlowActionType_s
  ),
  ~ggsave(
    plot = ..1,
    filename = paste0(fp_abs_ts_plt, "/", ..2, "_ts_", ..3, "_pulse.jpg"),
    width = 6.5, 
    height = 7, 
    units = "in", 
    dpi = 300
  )
)


# 5. Boxplots --------------------------------------------

# 5.1 Prepare Data for Plots ----------------------------------------------

# Prepare daily average data
df_rtm_clean_bp <- df_rtm_clean %>% 
  # Remove sites, parameters, and years we aren't including in the boxplots
  filter(
    StationCode != "SRH",
    !Parameter %in% c("fDOM", "NitrateNitrite", "WaterTemp"),
    Year >2012
  ) %>% 
  # Add variables for Region and Flow Pulse Period
  ndfa_action_periods() %>% 
  mutate(
    Region = if_else(
      StationCode %in% c("RMB", "RCS", "RD22", "I80", "LIS", "TOE", "STTD"),
      "Upstream",
      "Downstream"
    )
  ) %>% 
  # Log transform daily averages
  mutate(Daily_avg_log = log(Daily_avg)) %>% 
  # Convert Year, FlowActionPeriod, and Region to factors
  mutate(
    Year = factor(Year),
    FlowActionPeriod = factor(FlowActionPeriod, levels = c("Before", "During", "After")),
    Region = factor(Region, levels = c("Upstream", "Downstream"))
  )

# Prepare results of ANOVA models to add them to boxplots
# We will only indicate significant results of post-hoc tests for which the F-test for the 
  # main effect was significant
# Find significant main effects for each parameter
df_anova_f_test_signf <- df_anova_f_test %>% 
  filter(
    !str_detect(Variable, "^lag"),
    !is.na(Significance)
  ) %>% 
  select(Analyte, Variable)

# Create a list of vectors of the parameters with significant main effects for each effect
pull_signf_param <- function(df, main_effect) {
  df %>% 
    filter(Variable == main_effect) %>% 
    pull(Analyte)
}

signf_f_test_param <- 
  map2(
    rep(list(df_anova_f_test_signf), 3),
    c("Year", "FlowActionPeriod", "BroadRegion"),
    pull_signf_param
  ) %>% 
  set_names(c("yr", "fa", "region"))

# Pull out significant post-hoc contrasts for each main effect and parameter with 
  # a significant main effect
pull_signf_post_hoc <- function(df, signf_param) {
  df %>% 
    filter(
      Analyte %in% signf_param,
      str_detect(Significance, "\\*")
    )
}

format_signf_post_hoc <- function(df) {
  df %>% 
    separate(contrast, c("contrast1", "contrast2")) %>% 
    mutate(direction = if_else(estimate > 0, "higher", "lower")) %>% 
    select(Analyte, contrast1, contrast2, direction) %>% 
    arrange(Analyte, contrast1, contrast2)
}

signf_mc <- 
  map2(
    list(df_anova_mc_yr, df_anova_mc_fa, df_anova_mc_region),
    signf_f_test_param,
    pull_signf_post_hoc
  ) %>% 
  # Format data frames of significant post-hoc contrasts for easier assessment
  map(format_signf_post_hoc) %>% 
  set_names(c("yr", "fa", "region"))

# Prepare post-hoc results to be added as letters to the boxplots
# Function to create data frame of all factor levels for the main effect for each parameter with 
  # a significant main effect
pull_all_fact_levels <- function(df, main_effect, signf_param) {
  df %>% 
    distinct(Parameter, {{ main_effect }}) %>% 
    filter(Parameter %in% signf_param)
}

# Year:
#View(signf_mc$yr)
df_mc_yr <- df_rtm_clean_bp %>% 
  pull_all_fact_levels(Year, signf_f_test_param$yr) %>% 
  mutate(
    txt_label = case_when(
      Parameter == "Chla" & Year %in% c("2013", "2016") ~ "a",
      Parameter == "Chla" & Year == 2018 ~ "b",
      Parameter == "Chla" ~ "ab",
      Parameter == "DO" & Year == 2015 ~ "a",
      Parameter == "DO" & Year == 2019 ~ "b",
      Parameter == "DO" ~ "ab",
      Parameter == "pH" & Year %in% c("2015", "2017") ~ "a",
      Parameter == "pH" & Year == 2019 ~ "b",
      Parameter == "pH" ~ "ab",
      Parameter == "SpCnd" & Year == 2014 ~ "ab",
      Parameter == "SpCnd" & Year == 2015 ~ "a",
      Parameter == "SpCnd" & Year %in% c("2016", "2019") ~ "bc",
      Parameter == "SpCnd" & Year == 2017 ~ "c",
      Parameter == "SpCnd" ~ "abc",
      Parameter == "Turbidity" & Year %in% c("2013", "2015") ~ "ab",
      Parameter == "Turbidity" & Year %in% c("2014", "2016") ~ "a",
      Parameter == "Turbidity" & Year %in% c("2017", "2019") ~ "b",
      Parameter == "Turbidity" & Year == 2018 ~ "c"
    )
  )

# Flow Action Period:
#View(signf_mc$fa)
df_mc_fa <- df_rtm_clean_bp %>% 
  pull_all_fact_levels(FlowActionPeriod, signf_f_test_param$fa) %>% 
  mutate(
    txt_label = case_when(
      Parameter == "DO" & FlowActionPeriod %in% c("Before", "During") ~ "a",
      Parameter == "DO" & FlowActionPeriod == "After" ~ "b",
      Parameter == "SpCnd" & FlowActionPeriod %in% c("Before", "After") ~ "a",
      Parameter == "SpCnd" & FlowActionPeriod == "During" ~ "b",
      Parameter == "Turbidity" & FlowActionPeriod == "Before" ~ "a",
      Parameter == "Turbidity" & FlowActionPeriod %in% c("During", "After") ~ "b"
    )
  )

# Region:
#View(signf_mc$region)
df_mc_region <- df_rtm_clean_bp %>% 
  pull_all_fact_levels(Region, signf_f_test_param$region) %>% 
  mutate(txt_label = if_else(Region == "Upstream", "a", "b"))

# 5.2 Create Plot Functions -----------------------------------------------------

# Internal function to define x-axis labels for boxplots
int_define_box_xaxis_lab <- function(x_var) {
  xaxis_lab <- case_when(
    x_var == "Year" ~ "Year",
    x_var == "FlowActionPeriod" ~ "Flow Pulse Period",
    x_var == "Region" ~ "Region"
  )
  
  return(xaxis_lab)
}

# Internal function to define vertical position of post-hoc letters
int_define_ypos_mc_letters <- function(df) {
  max_y <- max(df$Daily_avg_log)
  min_y <- min(df$Daily_avg_log)
  add_ypos <- (max_y - min_y) * 0.1
  max_y + add_ypos
}

# Function to create boxplots for each parameter and main effect
create_boxplot <- function(df, df_mc_signf, param, grp_var) {
  
  # define x-axis and y-axis labels
  x_lab <- int_define_box_xaxis_lab(grp_var)
  y_lab <- glue("log({int_define_yaxis_lab(param)})")
  
  # convert grp_var to a symbol for tidy evaluation
  grp_var_sym <- sym(grp_var)
  
  # create base plot
  p <- df %>% 
    ggplot(aes(x = !!grp_var_sym, y = Daily_avg_log)) +
    geom_boxplot() +
    # add a symbol representing the mean of each group to the plot
    stat_summary( 
      fun = mean, 
      color = "red", 
      geom = "point", 
      shape = 8, 
      size = 2 
    ) +
    theme_light() +
    xlab(x_lab) +
    ylab(y_lab)
    
  # add letters to indicate significance of post-hoc tests for the parameters with significant
    # main effects
  if (!is.null(df_mc_signf)) {
    p <- p +
      geom_text(
        data = df_mc_signf,
        aes(
          y = int_define_ypos_mc_letters(df),
          label = txt_label
        ),
        size = 3.5
      )
  }
  
  return(p)
}

# 5.3 Create and Export Plots ---------------------------------------------

# Nest the post-hoc results by parameter in order to join to the rtm data
nest_mc_df <- function(df, var_name) {
  var_name_sym <- ensym(var_name)
  df %>% nest(!!var_name_sym := -Parameter)
}

df_mc_all <- 
  list(df_mc_yr, df_mc_fa, df_mc_region) %>% 
  map2(c("mc_yr", "mc_fa", "mc_region"), nest_mc_df) %>% 
  reduce(left_join)

# Create boxplots
df_rtm_boxplot <- df_rtm_clean_bp %>% 
  nest(df_data = -Parameter) %>%
  left_join(df_mc_all) %>% 
  mutate(
    plt_yr = pmap(
      list(df_data, mc_yr, Parameter),
      .f = create_boxplot, 
      grp_var = "Year"
    ),
    plt_region = pmap(
      list(df_data, mc_region, Parameter),
      .f = create_boxplot, 
      grp_var = "Region"
      ),
    plt_fa = pmap(
      list(df_data, mc_fa, Parameter),
      .f = create_boxplot, 
      grp_var = "FlowActionPeriod"
    ),
    plt_comb = pmap(
      list(plt_yr, plt_region, plt_fa), 
      ~ ..1 / (..2 + ..3) + plot_annotation(tag_levels = "A")
    )
  )

# Define file path to export plots to
fp_abs_boxplot <- ndfa_abs_sp_path("WQ_Subteam/Plots/Continuous/Report/Boxplot")

# Export boxplots
walk2(
  df_rtm_boxplot$plt_comb,
  df_rtm_boxplot$Parameter,
  ~ggsave(
    plot = .x,
    filename = paste0(fp_abs_boxplot, "/", .y, "_boxplot.jpg"),
    width = 6.5, 
    height = 8.25, 
    units = "in", 
    dpi = 300
  )
)

