# NDFS Contaminants Manuscript
# Purpose: Create figure and summary table of the 2017-2020 pesticide use data
  # for the Sacramento River and Yolo Bypass regions for the NDFS contaminants
  # manuscript. Exploratory figures are in:
  # ND-FASTR/manuscript_contam/notebooks/explore_pesticide_use.Rmd
# Author: Dave Bosworth
# Contacts: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
library(scales)
library(here)

# Check if we are in the correct working directory
i_am("manuscript_contam/src/pesticide_use_figure.R")

# Import daily total pesticide use data for 2017-2020
df_pest_use <- readRDS(here("manuscript_contam/data/processed/pesticide_use_daily_tot_2017-2020.rds"))

# Calculate monthly totals for each Region and Pesticide Class and prepare for figure
df_pest_use_c <- df_pest_use %>% 
  summarize(
    TotalApplication = sum(TotalApplication) / 2.205,
    .by = c(Region, Month, Year, PesticideClass)
  ) %>%
  mutate(PesticideClass = factor(PesticideClass, levels = c("Rice", "Pyrethroid", "Other")))

# Create figure of monthly totals by Region and Year
plt_pest_use <- df_pest_use_c %>% 
  ggplot(aes(x = Month, y = TotalApplication, fill = PesticideClass)) +
  geom_col() +
  facet_grid(rows = vars(Year), cols = vars(Region)) +
  scale_fill_viridis_d(
    name = "Pesticide Class", 
    option = "plasma", 
    end = 0.8
  ) +
  scale_y_continuous(
    name = "Total Application (kg)",
    labels = label_comma()
  ) +
  theme_bw() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

# Export figure
ggsave(
  filename = here("manuscript_contam/results/figures/pesticide_use_reg_yr.jpg"),
  plot = plt_pest_use,
  width = 6.5,
  height = 5.5,
  units = "in"
)

# Calculate annual pesticide application totals for each Region and Year
df_pest_use_summ <- df_pest_use %>% 
  summarize(
    TotalApplication = sum(TotalApplication) / 2.205,
    .by = c(Year, Region)
  )

# Export annual pesticide application totals
df_pest_use_summ %>% write_csv(file = here("manuscript_contam/results/tables/appl_totals.csv"))

