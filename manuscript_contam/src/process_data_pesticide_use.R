# NDFS Contaminants Manuscript
# Purpose: Process the pesticide use data collected by the CA Department of
  # Pesticide Regulation for the Sacramento River and Toe Drain Regions in
  # 2017-2020. This data is used in figures and analysis for the NDFS contaminants
  # manuscript.
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
i_am("manuscript_contam/src/process_data_pesticide_use.R")


# 1. Import Data ----------------------------------------------------------

# Import pesticide use data for Sacramento River and Toe Drain
df_sr_use <- read_excel(
  here("manuscript_contam/data/raw/PesticideUse_2017-2020_ReducedtoUSGSAnalyteList.xlsx"),
  sheet = "Sacramento Reduced"
)

df_toe_use <- read_excel(
  here("manuscript_contam/data/raw/PesticideUse_2017-2020_ReducedtoUSGSAnalyteList.xlsx"),
  sheet = "ToeDrainReduced"
)

# Import lookup table for pesticide types and classes
df_contam_type <- read_csv(here("Water_Quality/Data_Processed/Contam_Types_and_Classes.csv"))


# 2. Clean and Integrate Data ---------------------------------------------

# Define rice pesticides
pest_rice <- c(
  "azoxystrobin",
  "carbaryl",
  "clomazone",
  "cyhalofop butyl",
  "methoxyfenozide",
  "penoxsulam",
  "propanil",
  "thiobencarb"
)

# Define Pyrethroid pesticides
pest_pyreth <- df_contam_type %>% 
  filter(PesticideClass == "Pyrethroid") %>% 
  pull(Analyte) %>% 
  str_remove("\\s\\(.+\\)")

# Convert vectors containing rice and Pyrethroid pesticides into regular
  # expressions
pest_rice_regex <- str_flatten(pest_rice, collapse = "|")
pest_pyreth_regex <- str_flatten(pest_pyreth, collapse = "|")

# Combine pesticide use data and prepare for summarizing
df_use_all <- 
  list(
    "Sacramento River" = df_sr_use,
    "Toe Drain" = df_toe_use
  ) %>% 
  bind_rows(.id = "Region") %>% 
  transmute(
    Region,
    Chemname,
    Date = date(applic_dt),
    Application = `Application (lbs)`
  ) %>% 
  # Assign pesticide class categories
  mutate(
    PesticideClass = case_when(
      str_detect(Chemname, regex(pest_rice_regex, ignore_case = TRUE)) ~ "Rice",
      str_detect(Chemname, regex(pest_pyreth_regex, ignore_case = TRUE)) ~ "Pyrethroid",
      TRUE ~ "Other"
    )
  ) %>% 
  drop_na(Application)
  
# Calculate daily totals for each Pesticide Class and Region
df_use_all_tot <- df_use_all %>% 
  summarize(
    TotalApplication = sum(Application),
    .by = c(Region, Date, PesticideClass)
  ) %>% 
  # Add month and year variables
  mutate(
    Month = month(Date, label = TRUE),
    Year = year(Date),
    .after = Date
  )

# Temporary plots of pesticide use data
library(scales)

# Daily Totals
df_use_all_tot %>% 
  mutate(
    PesticideClass = factor(PesticideClass, levels = c("Rice", "Pyrethroid", "Other")),
    DOY = yday(Date),
  ) %>% 
  ggplot(aes(x = DOY, y = TotalApplication, fill = PesticideClass)) +
  geom_col() +
  facet_grid(rows = vars(Year), cols = vars(Region)) +
  theme_bw() +
  scale_fill_viridis_d(
    name = "Pesticide Class", 
    option = "plasma", 
    end = 0.8
  ) +
  scale_y_continuous(
    name = "Total Application (lbs)",
    labels = label_comma()
  )

# Monthly Totals
df_use_all_tot %>% 
  mutate(PesticideClass = factor(PesticideClass, levels = c("Rice", "Pyrethroid", "Other"))) %>% 
  summarize(
    TotalApplication = sum(TotalApplication),
    .by = c(Region, Month, Year, PesticideClass)
  ) %>% 
  ggplot(aes(x = Month, y = TotalApplication, fill = PesticideClass)) +
  geom_col() +
  facet_grid(rows = vars(Year), cols = vars(Region)) +
  theme_bw() +
  scale_fill_viridis_d(
    name = "Pesticide Class", 
    option = "plasma", 
    end = 0.8
  ) +
  scale_y_continuous(
    name = "Total Application (lbs)",
    labels = label_comma()
  )

