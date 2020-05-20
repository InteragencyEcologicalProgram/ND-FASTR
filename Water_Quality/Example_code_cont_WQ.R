# Example code for working with continuous water quality data
# Author: Dave Bosworth

# Load packages- install these packages if you haven't already using install.packages()
library(tidyverse)
library(readxl)
library(lubridate)


# Import Data -------------------------------------------------------------

# random extra code

# practice git hub 

# Define path on SharePoint site for data - this works if you have the SharePoint site synced
# to your computer
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/Office of Water Quality and Estuarine Ecology - North Delta Flow Action/Water Quality Subteam/Raw data and metadata"
  )
)

# 1. From Excel spreadsheet
  rd22 <- read_excel(
    path = paste0(sharepoint_path, "/DWR_NDFA_RD22_ContYSISondeData_2013_18.xlsx"),
    sheet = "RD22_2013"
  )

  # View structure of rd22
  glimpse(rd22)
  
  # View variable names of rd22
  names(rd22)
  
  # Look up information on a function
  ?read_excel
  
  # Import using a defined cell range
  rd22_v2 <- read_excel(
    path = paste0(sharepoint_path, "/DWR_NDFA_RD22_ContYSISondeData_2013_18.xlsx"),
    sheet = "RD22_2013",
    range = "A1:E7768"
  )
  
  glimpse(rd22_v2)
  names(rd22_v2)
  
# 2. From a csv file
  ?read_csv
  
  rvb <- read_csv(
    file = paste0(sharepoint_path, "/RTM_RAW_DWR RVB_2015-2017.csv"),
    skip = 4
  )
  
  glimpse(rvb)


# Dataframe manipulation --------------------------------------------------

# 1. Changing variable names
  ryi <- read_excel(
    path = paste0(sharepoint_path, "/RTM_RAW_USGS RYI_2011-2019.xlsx"),
    sheet = "2018_5350",
    range = "A94:N35130",
    col_names = FALSE
  )
  
  glimpse(ryi)
  
  # use names function
  names(ryi) <- c(
    "datetime",
    "mean_velo",
    "stage",
    "discharge_tf",
    "temperature",
    "spcond",
    "ph",
    "turbidity",
    "dissoxy_mgL",
    "discharge",
    "chlorophyll",
    "fdom",
    "salinity",
    "dissoxy_per_sat"
  )
  
  glimpse(ryi)
  
  # use rename function
  ryi <- ryi %>% rename(mean_velocity = mean_velo)
  glimpse(ryi)

# 2. Selecting and removing variables with the select function
  ryi %>% select(datetime, stage)
  
  ryi1 <- ryi %>% 
    select(datetime:discharge_tf)
  
  ryi1
  
  ryi %>% select(-datetime)
  ryi %>% select(-c(mean_velocity:discharge_tf))
  ryi %>% select(-c(mean_velocity:discharge_tf, discharge))

# 3. Changing variable types
  # Dates and times
  ryi_clean <- ryi %>% 
    mutate(datetime1 = mdy_hm(datetime))
  
  glimpse(ryi_clean)
    
  # Converting from character to numeric
  unique(ryi$turbidity)
  
  ryi_clean1 <- ryi_clean %>% 
    select(datetime1, turbidity) %>% 
    rename(datetime = datetime1) %>% 
    mutate(
      turb = str_trim(turbidity),
      turb1 = str_sub(turb, end = -2),
      turb2 = as.numeric(turb1)
    )
  
  glimpse(ryi_clean1)

  
# Plotting Data -----------------------------------------------------------

# Make a plot of cleaned up temperature data for ryi
ryi_clean1 %>% 
  ggplot(aes(x = datetime, y = turb2)) +
  geom_line()

# Remove missing data from plot
ryi_clean1 %>% 
  filter(!is.na(turb2)) %>% 
  ggplot(aes(x = datetime, y = turb2)) +
  geom_line()
  
