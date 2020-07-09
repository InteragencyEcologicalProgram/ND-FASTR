# NDFA Water Quality
# Purpose: Code to import, clean, and export discrete laboratory water quality data from Bryte Lab
# Author: Jenna Rinde

# Load packages
library(tidyverse)
library(readxl)
library(lubridate)


# Import Data -------------------------------------------------------------

# Define path on SharePoint site for data - this works if you have the SharePoint site synced
  # to your computer
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/Office of Water Quality and Estuarine Ecology - North Delta Flow Action/WQ_Subteam/Raw_Data/Discrete/Raw_WDL_export_June2020"
  )
)

# Create a character vectors of all discrete lab data files in sharepoint_path
dis_lab_files <- dir(sharepoint_path, pattern = "\\.xlsx$", full.names = T)

# Define column types for data to be imported with read_excel
dis_lab_col_types <- c(
  rep("numeric", 2),
  rep("text", 3),
  "date",
  rep("text", 2),
  "numeric",
  rep("text", 8)
)

# Import and combine all of the discrete lab data
dis_lab_data_orig <- map_dfr(dis_lab_files, ~read_excel(.x, col_types = dis_lab_col_types))

# Clean up variable names in dis_lab_data_orig
names(dis_lab_data_orig) <- str_replace_all(names(dis_lab_data_orig), "[:space:]", "")

# Start to clean the discrete lab data
dis_lab_data_clean <- dis_lab_data_orig %>% 
  # Remove unnecessary Analytes
  filter(str_detect(Analyte, "^\\*No|^Field|^Spec|^Weath|Bromide$|Alkalinity$", negate = TRUE)) %>% 
  filter(Analyte != "Dissolved Total Kjeldahl Nitrogen") %>% 
  mutate(
    # Rename Analytes and Stations with NDFA standardized names
    Analyte = recode(
      Analyte,
      "Chlorophyll a" = "Chla",
      "Dissolved Ammonia" = "DisAmmonia",
      "Dissolved Calcium" = "DisCalcium",
      "Dissolved Chloride" = "DisChloride",
      "Dissolved Nitrate + Nitrite" = "DisNitrateNitrite",
      "Dissolved Organic Carbon" = "DOC",
      "Dissolved Organic Nitrogen" = "DON",
      "Dissolved Ortho-phosphate" = "DOP",
      "Dissolved Silica (SiO2)" = "DisSilica",
      "Pheophytin a" = "Pheo",
      "Total Dissolved Solids" = "TDS",
      "Total Kjeldahl Nitrogen" = "TKN",
      "Total Organic Carbon" = "TOC",
      "Total Phosphorus" = "TOP",
      "Total Suspended Solids" = "TSS",
      "Volatile Suspended Solids" = "VSS"       
    ),
    StationName = recode(
      StationName,
      "Below Base of Toe Drain in Prospect Slough (ID:47528)" = "BL5",
      "Cache Slough at Ryer Island (ID:47538)" = "RYI",
      "Davis Wastewater Discharge at Toe Drain (ID:47876)" = "DWT",
      "Liberty at Approx Cntr S End (ID:47539)" = "LIB",
      "Ridge Cut Slough at Knights Landing (ID:47535)" = "RCS",
      "Rominger Bridge at Colusa Basin Drain (ID:47867)" = "RMB",
      "Sacramento River @ Hood - C3A (ID:45916)" = "SRH",
      "Sacramento River @ Sherwood Harbor (ID:47116)" = "SHR",
      "Sacramento River at Decker Island (USGS) (ID:47092)" = "SDI",
      "Sacramento River at Rio Vista Bridge (ID:47536)" = "RVB",
      "Sacramento River at Vieira's (ID:47537)" = "SRV",
      "Toe Drain at County Road 22 (ID:47533)" = "RD22",
      "Toe Drain at Interstate 80 (ID:47532)" = "I80",
      "Toe Drain at Rotary Screw Trap (ID:47529)" = "STTD",
      "Woodland Wastewater Discharge at Toe Drain (ID:47873)" = "WWT",
      "Yolo Bypass Toe Drain Below Lisbon Weir (ID:145)" = "LIS"
    ),
    # Create a new variable to identify values below the Reporting Limit
    LabDetect = if_else(
      str_detect(Result, "<"),
      "Non-detect",
      "Detect"
    ),
    # Convert Result variable from character to numeric, use Reporting Limit values for <RL data
    Result = if_else(
      LabDetect == "Non-detect",
      RptLimit,
      signif(as.numeric(Result), 3)  #round to 3 significant digits
    )
  ) %>% 
  select(
    SampleCode,
    StationName, 
    CollectionDate, 
    Analyte, 
    Result, 
    LabDetect,
    RptLimit, 
    Units,
    Method,
    Depth,
    Purpose,
    ParentSample
  )

# Look for lab replicates
dis_lab_reps <- dis_lab_data_clean %>%
  count(StationName, CollectionDate, Analyte, Purpose) %>% 
  filter(n > 1) %>% 
  select(-n)

# Pull lab replicates from dis_lab_data_clean
dis_lab_rep_pairs <- inner_join(dis_lab_data_clean, dis_lab_reps)

# There are some lab replicates with different SampleCodes
test <- dis_lab_rep_pairs %>% 
  count(SampleCode, StationName, CollectionDate, Analyte, Purpose) %>% 
  filter(n == 1)
# DOC samples collected at SRH in 2019
# It would be good to remove the samples that were for the Lab's special DOC study

# Add a variable to define obs as either Rep1 or Rep2
dis_lab_reps_num <- dis_lab_rep_pairs %>%
  select(-c(SampleCode, Depth, ParentSample)) %>% 
  group_by(StationName, CollectionDate, Analyte, Purpose) %>% 
  mutate(Rep = row_number()) %>% 
  ungroup() %>% 
  filter(Rep != 3)

# Widen the Result variable to arrange Replicate Results side-by-side in same row
dis_lab_reps_r_wide <- dis_lab_reps_num %>%   
  pivot_wider(
    id_cols = -LabDetect,
    names_from = Rep, 
    values_from = Result,
    names_prefix = "Result_"
  )

# Widen the LabDetect variable to arrange Replicates side-by-side in same row
dis_lab_reps_ld_wide <- dis_lab_reps_num %>%   
  pivot_wider(
    id_cols = -Result,
    names_from = Rep, 
    values_from = LabDetect,
    names_prefix = "LabDetect_"
  )

# Join all lab rep dataframes back together
dis_lab_reps_wide <- dis_lab_rep_pairs %>% 
  select(-c(SampleCode, Result, LabDetect)) %>% 
  distinct() %>% 
  left_join(dis_lab_reps_r_wide) %>% 
  left_join(dis_lab_reps_ld_wide) %>% 
  mutate(RPD = round(abs(Result_1 - Result_2)/((Result_1 + Result_2)/2), 3))

# Export lab replicate data as a .csv file
dis_lab_reps_wide %>% write_excel_csv("Discrete_Lab_Data_Replicates.csv", na = "")

# Pull out field duplicates and calculate RPD's
# Separate date and time?
# Change column names

# Investigate collection depth



