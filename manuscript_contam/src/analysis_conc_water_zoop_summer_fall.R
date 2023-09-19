# NDFS Contaminants Manuscript
# Purpose: Run statistical analyses for the total pesticide concentration data
  # in water and zooplankton collected in summer-fall periods for the NDFS
  # contaminants manuscript. Exploratory analyses and figures are in:
  # ND-FASTR/manuscript_contam/notebooks/explore_contam_conc_water_zoop.Rmd
# Author: Dave Bosworth
# Contacts: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
library(car)
library(emmeans)
library(broom)
library(here)
library(conflicted)

# Declare package conflict preferences 
conflicts_prefer(dplyr::filter())

# Check if we are in the correct working directory
i_am("manuscript_contam/src/analysis_conc_water_zoop_summer_fall.R")


# 1. Import and Prepare Data ----------------------------------------------

# Define root directory for pesticide data:
fp_pest <- here("manuscript_contam/data/processed")

# Total Pesticide Concentration data:
df_conc_all <- read_rds(file.path(fp_pest, "contam_conc_water_zoop_2017-2019.rds"))

# Pesticide application data:
df_appl <- read_rds(file.path(fp_pest, "pesticide_use_daily_tot_2017-2020.rds"))

# Daily average flow data:
df_davg_flow <- read_rds(here("Water_Quality/Data_Processed/LIS_SR_DailyAvgFlow_2013-2022.rds"))

# Prepare the pesticide application data to be joined to the concentration data
  # by calculating monthly totals and standardizing to area of the watershed.
df_appl_c <- df_appl %>% 
  # Calculate monthly totals by region
  summarize(TotalAppl = sum(TotalApplication), .by = c(Region, Year, Month)) %>% 
  mutate(
    # Standardize total application to area (in square miles) of the watershed 
    TotalApplStd = if_else(
      Region == "Sacramento River", 
      TotalAppl/22526.79948,
      TotalAppl/4217.503529
    ),
    # Define station that each region is assigned to
    Station = case_match(
      Region,
      "Toe Drain" ~ "STTD", 
      "Sacramento River" ~ "SHR"
    )
  )

# Prepare the flow data to be joined to the concentration data using SR at
  # Freeport flow data for SHR and LIS flow data for STTD. Use Daily Average Net
  # flows for both.
df_davg_flow_c <- df_davg_flow %>% 
  select(-LIS_DailyAvgInstFlow) %>% 
  pivot_longer(
    cols = ends_with("NetFlow"),
    names_to = "Station",
    values_to = "DailyAvgNetFlow"
  ) %>% 
  mutate(
    Station = case_match(
      Station,
      "SR_DailyAvgNetFlow" ~ "SHR",
      "LIS_DailyAvgNetFlow" ~ "STTD"
    )
  )

# Join pesticide concentration, application, and flow data together, and
  # restrict the date ranges to summer-fall period (July 1 - Oct 31) for each year
df_conc_all_c <- df_conc_all %>% 
  mutate(
    Year = year(Date),
    Month = month(Date, label = TRUE)
  ) %>% 
  left_join(df_appl_c, by = join_by(Station, Year, Month)) %>% 
  left_join(df_davg_flow_c, by = join_by(Date, Station)) %>% 
  mutate(Month = as.numeric(Month)) %>%
  filter(Month %in% 7:10) %>% 
  select(Date, Year, Station, starts_with("TotalConc"), DailyAvgNetFlow, TotalApplStd)


# 2. Zooplankton Analyses -------------------------------------------------

# Prepare zooplankton concentration data for analysis
df_zoop <- df_conc_all_c %>% 
  select(-TotalConc_Water) %>% 
  drop_na() %>% 
  # Remove two samples with total concentrations equal to zero
  filter(TotalConc_Zoop > 0) %>% 
  mutate(
    # Add log-transformed and sqrt-transformed zooplankton concentrations
    TotalConc_Zoop_log = log(TotalConc_Zoop),
    TotalConc_Zoop_sqrt = sqrt(TotalConc_Zoop),
    # Convert year to character for categorical models
    Year = as.character(Year)
  )


# 2.1 Linear models with flow and application -----------------------------

# Run linear models separately for each station

# SHR: 
df_zoop_shr <- df_zoop %>% filter(Station == "SHR")

# Run model for SHR using log transformed zooplankton concentrations since the
  # model diagnostics looked best with these
m_zoop_q_appl_shr_log <- df_zoop_shr %>% 
  lm(TotalConc_Zoop_log ~ DailyAvgNetFlow + TotalApplStd, data = .)

# Convert summary table of model results to a tibble for export
df_m_zoop_q_appl_shr <- 
  tidy(summary(m_zoop_q_appl_shr_log)) %>% 
  mutate(
    Response = "log(Zooplankton)", 
    Station = "SHR", 
    .before = term
  )

# STTD:
df_zoop_sttd <- df_zoop %>% filter(Station == "STTD")

# Run model for STTD using square root transformed zooplankton concentrations
  # since the model diagnostics looked best with these
m_zoop_q_appl_sttd_sqrt <- df_zoop_sttd %>% 
  lm(TotalConc_Zoop_sqrt ~ DailyAvgNetFlow + TotalApplStd, data = .)

# Convert summary table of model results to a tibble for export
df_m_zoop_q_appl_sttd <- 
  tidy(summary(m_zoop_q_appl_sttd_sqrt)) %>% 
  mutate(
    Response = "sqrt(Zooplankton)", 
    Station = "STTD", 
    .before = term
  )


# 2.2 Linear model with station and year ----------------------------------

# Run model using log transformed zooplankton concentrations since the model
  # diagnostics looked best with these. Also run model without interaction between
  # Station and Year since it wasn't significant.
m_zoop_sta_yr_log <- df_zoop %>% lm(TotalConc_Zoop_log ~ Station + Year, data = .)

# Create ANOVA table for model and convert it to a tibble for export
# use type 2 sum of squares because of the unbalanced design
df_an_zoop_sta_yr <- 
  tidy(Anova(m_zoop_sta_yr_log, type = 2)) %>% 
  transmute(
    Response = "log(Zooplankton)",
    Parameter = term,
    Sum_Sq = sumsq,
    Df = df,
    F_value = statistic,
    p_value = p.value
  )

# The main effect of station is not significant meaning that total pesticide
  # concentrations in zooplankton don't differ between SHR and STTD. However, the
  # main effect of Year is significant, so we'll export the pairwise contrasts to
  # show how concentrations differ among the years. We'll also export the pairwise
  # contrasts for station since it is close to significant.

# Estimated marginal means for Year main effect
em_zoop_yr <- emmeans(m_zoop_sta_yr_log, specs = "Year")

# Estimated marginal means for Station main effect
em_zoop_sta <- emmeans(m_zoop_sta_yr_log, specs = "Station")

# Create table of contrasts and convert it to a tibble for export
df_em_zoop_sta_yr <- 
  bind_rows(
    tidy(pairs(em_zoop_yr)) %>% rename(p.value = adj.p.value), 
    tidy(pairs(em_zoop_sta))
  ) %>% 
  transmute(
    Response = "log(Zooplankton)", 
    Parameter = term,
    Contrast = contrast,
    Estimate = estimate,
    SE = std.error,
    Df = df,
    t_ratio = statistic,
    p_value = p.value
  ) 


# 3. Water Analyses -------------------------------------------------------

# Prepare water concentration data for analysis
df_water <- df_conc_all_c %>% 
  select(-TotalConc_Zoop) %>% 
  # only include 2019 since that was the only year water samples were collected
    # at STTD
  filter(Year == 2019) %>% 
  drop_na() %>% 
  # Add log-transformed water concentrations
  mutate(TotalConc_Water_log = log(TotalConc_Water))

# 3.1 Linear models with flow and application -----------------------------

# Run linear models separately for each station

# SHR: 
df_water_shr <- df_water %>% filter(Station == "SHR")

# Run model for SHR using log transformed water concentrations since the model
  # diagnostics looked best with these
m_water_q_appl_shr_log <- df_water_shr %>% 
  lm(TotalConc_Water_log ~ DailyAvgNetFlow + TotalApplStd, data = .)

# Convert summary table of model results to a tibble for export
df_m_water_q_appl_shr <- 
  tidy(summary(m_water_q_appl_shr_log)) %>% 
  mutate(
    Response = "log(Water)", 
    Station = "SHR", 
    .before = term
  )

# STTD:
df_water_sttd <- df_water %>% filter(Station == "STTD")

# Run model for STTD using original water concentrations since the model
  # diagnostics looked best with these
m_water_q_appl_sttd <- df_water_sttd %>% 
  lm(TotalConc_Water ~ DailyAvgNetFlow + TotalApplStd, data = .)

# Convert summary table of model results to a tibble for export
df_m_water_q_appl_sttd <- 
  tidy(summary(m_water_q_appl_sttd)) %>% 
  mutate(
    Response = "Water", 
    Station = "STTD", 
    .before = term
  )


# 3.2 Station comparison - t-test -----------------------------------------

# Compare total pesticide concentrations in water between the two stations, SHR
  # and STTD. Since samples were only collected at both stations in 2019, we'll
  # run a Welch's two-sample t-test to compare their means in 2019. We'll run the
  # test using natural log transformed values since they seem to be more normally
  # distributed and have relatively equal variances between SHR and STTD.
ttest_water_sta <- df_water %>% t.test(TotalConc_Water_log ~ Station, data = .)

# Convert results of t-test to a tibble for export
df_ttest_water_sta <- 
  tidy(ttest_water_sta) %>% 
  transmute(
    Response = "log(Water)",
    Parameter = "Station",
    Contrast = "SHR - STTD",
    Estimate = estimate,
    SE = ttest_water_sta$stderr,
    Df = parameter,
    t_statistic = statistic,
    p_value = p.value
  )


# 4. Export Results -------------------------------------------------------

# Define file path for results for the manuscript
fp_tables <- here("manuscript_contam/results/tables")

# Combine and export summary tables of flow-application models
bind_rows(df_m_zoop_q_appl_shr, df_m_zoop_q_appl_sttd, df_m_water_q_appl_shr, df_m_water_q_appl_sttd) %>% 
  transmute(
    Response,
    Station,
    Parameter = if_else(term == "(Intercept)", "Intercept", term),
    Estimate = estimate,
    SE = std.error,
    t_value = statistic,
    p_value = if_else(
      p.value < 0.001, 
      "< 0.001", 
      as.character(signif(p.value, digits = 4))
    )
  ) %>% 
  mutate(across(where(is.numeric), ~ formatC(signif(.x, 4), digits = 4, format = "fg", flag = "#"))) %>% 
  mutate(across(c("Estimate", "SE", "t_value", "p_value"), ~ paste0(.x, "##"))) %>% 
  write_csv(file.path(fp_tables, "flow_appl_model_summ_summer_fall.csv"))

# Export ANOVA results for the station-year analysis of the zooplankton concentrations
df_an_zoop_sta_yr %>% 
  mutate(
    across(
      c("Sum_Sq", "F_value", "p_value"), 
      ~ if_else(
        is.na(.x), 
        NA_character_, 
        paste0(formatC(signif(.x, 4), digits = 4, format = "fg", flag = "#"), "##")
      )
    )
  ) %>% 
  write_csv(file.path(fp_tables, "zoop_sta_yr_anova_summer_fall.csv"), na = "")

# Export table of Tukey pairwise contrasts for the station-year analysis of the
  # zooplankton concentrations
df_em_zoop_sta_yr %>% 
  mutate(
    across(
      c("Estimate", "SE", "t_ratio", "p_value"),
      ~ paste0(formatC(signif(.x, 4), digits = 4, format = "fg", flag = "#"), "##")
    )
  ) %>% 
  write_csv(file.path(fp_tables, "zoop_sta_yr_tukey_summer_fall.csv"))

# Export results of t-test comparing water concentrations between stations 
df_ttest_water_sta %>% 
  mutate(
    across(
      where(is.numeric),
      ~ paste0(formatC(signif(.x, 4), digits = 4, format = "fg", flag = "#"), "##")
    )
  ) %>% 
  write_csv(file.path(fp_tables, "water_sta_ttest_summer_fall.csv"))

