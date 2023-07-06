# NDFS Contaminants Manuscript
# Purpose: Process and integrate the total pesticide concentrations in
  # zooplankton and water samples collected from the Sacramento River and Toe
  # Drain in the summer-fall periods in 2017, 2018 and 2019. This data is used in
  # figures and analysis for the NDFS contaminants manuscript.
# Author: Dave Bosworth
# Contacts: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
library(readxl)
library(here)
library(conflicted)

# Declare package conflict preferences 
conflicts_prefer(dplyr::filter())

# Check if we are in the correct working directory
i_am("manuscript_contam/src/process_data_contam_conc.R")


# 1. Import Data ----------------------------------------------------------

# Import total pesticide concentration data for SHR and STTD
df_shr_conc <- read_excel(
  here("manuscript_contam/data/raw/DataforReportFigs_toDWR.xlsx"),
  sheet = "Figs 3&4",
  range = "A1:D37"
)

df_sttd_conc <- read_excel(
  here("manuscript_contam/data/raw/DataforReportFigs_toDWR.xlsx"),
  sheet = "Figs 3&4",
  range = "I1:L38"
)


# 2. Clean and Integrate Data ---------------------------------------------

# Prepare data for SHR and STTD to be combined
df_shr_conc_c <- df_shr_conc %>% 
  transmute(
    Date = date(Date),
    Station = "SHR",
    TotalConc_Zoop = `SHR Zoop Total Concentration ng/g`,
    TotalConc_Water = `SHR Water Total Concentration ng/l`
  ) %>% 
  # remove concentration data for 2020
  filter(year(Date) < 2020)

df_sttd_conc_c <- df_sttd_conc %>% 
  transmute(
    Date = date(Date),
    Station = "STTD",
    TotalConc_Zoop = `STTD Zoop Total Concentration ng/g`,
    TotalConc_Water = `STTD Water Total Concentration ng/l`
  ) %>% 
  # remove concentration data for 2020
  filter(year(Date) < 2020) %>% 
  # remove water concentration data for 2017-2018 since it's from LIS
  mutate(TotalConc_Water = if_else(year(Date) < 2019, NA_real_, TotalConc_Water))

# Combine data for SHR and STTD
df_conc_all <- bind_rows(df_shr_conc_c, df_sttd_conc_c)


# 3. Save and Export Data -------------------------------------------------

# Save final data set containing total pesticide concentration data for SHR and
  # STTD for the NDFS contaminants manuscript analyses as csv file for easier
  # diffing
df_conc_all %>% write_csv(here("manuscript_contam/data/processed/contam_conc_water_zoop_2017-2019.csv"))

# Save final data set containing total pesticide concentration data for SHR and
  # STTD for the NDFS contaminants manuscript analyses as an rds object in the
  # GitHub repo for easier import
saveRDS(df_conc_all, file = here("manuscript_contam/data/processed/contam_conc_water_zoop_2017-2019.rds"))

