# Example code for working with discrete water quality data
# Author: Jenna Rinde

# Load packages- install these packages if you haven't already using install.packages()
library(tidyverse)
library(readxl)
library(lubridate)
library(dplyr)

# Import Data -------------------------------------------------------------

# Define path on SharePoint site for data - this works if you have the SharePoint site synced
# to your computer
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/Office of Water Quality and Estuarine Ecology - North Delta Flow Action/WQ_Subteam/Raw_Data/Discrete"
  )
)

# 1. Importing nutrient data from SharePoint


nuts <- read_excel(
  path = paste0(sharepoint_path, "/DWQ_nuts_raw.xlsx"), sheet = "WQData (11)"
)

head(nuts)

str(nuts)

#example of old code for different study for subsetting by station

CM40<- subset(CM, station== "CM40")
CM42<- subset(CM, station== "CM42")
CM43<- subset(CM, station== "CM43")
CM48<- subset(CM, station== "CM48")
RRI<- subset(CM, station== "CM42_RRI")
