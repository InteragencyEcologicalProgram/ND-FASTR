# NDFA Water Quality
# Purpose: Code to import, clean, and export discrete laboratory water quality data from Bryte Lab
# Author: Jenna Rinde, Dave Bosworth


# 1. Global Code ----------------------------------------------------------

# Load packages
library(tidyverse)
library(readxl)
library(lubridate)

# Source global WQ functions
source("Water_Quality/global_wq_funcs.R")

# Define main NDFA file path for WQ subteam (assumes synced with SharePoint)
fp_fastr <- "California Department of Water Resources/North Delta Flow Action - Documents/WQ_Subteam/"

# Define relative file paths for raw and processed discrete lab WQ data files
fp_rel_wq_raw <- paste0(fp_fastr, "Raw_Data/Discrete/Raw_WDL_export_June2020")
fp_rel_wq_proc <- paste0(fp_fastr, "Processed_Data/Discrete")

# Define absolute file paths
fp_abs_wq_raw <- get_abs_path(fp_rel_wq_raw)
fp_abs_wq_proc <- get_abs_path(fp_rel_wq_proc)

# Create a vector of object names to keep throughout the script
obj_keep <- c("fp_abs_wq_raw", "fp_abs_wq_proc", "obj_keep")

# Clean up working environment
rm(list = ls()[!(ls() %in% obj_keep)])


# 2. Import Data -------------------------------------------------------------

# Create a character vector of all raw discrete lab data files
dis_lab_files <- dir(fp_abs_wq_raw, pattern = "\\.xlsx$", full.names = T)

# Pull out "Discrete_WQ_2017_Aug-Oct.xlsx" file since it has a different data structure
dis_lab_files1 <- str_subset(dis_lab_files, "Discrete_WQ_2017", negate = TRUE)
dis_lab_files2 <- str_subset(dis_lab_files, "Discrete_WQ_2017")

# Define column types for data in dis_lab_files1 to be imported with read_excel
dis_lab_col_types <- c(
  rep("numeric", 2),
  rep("text", 3),
  "date",
  rep("text", 2),
  "numeric",
  rep("text", 8)
)

# Import and combine all of the raw discrete lab data in dis_lab_files1
dis_lab_data1_orig <- map_dfr(dis_lab_files1, ~read_excel(.x, col_types = dis_lab_col_types))

# Import the raw discrete lab data in dis_lab_files2
dis_lab_data2_orig <- read_excel(dis_lab_files2)


# 3. Clean Data --------------------------------------------------------------

# Clean up variable names in dis_lab_data1_orig and dis_lab_data2_orig
names(dis_lab_data1_orig) <- str_replace_all(names(dis_lab_data1_orig), "[:space:]", "")
names(dis_lab_data2_orig) <- str_replace_all(names(dis_lab_data2_orig), "[:space:]", "")

# Modify dis_lab_data1_orig and dis_lab_data2_orig so that they can be bound together
dis_lab_data1_clean <- dis_lab_data1_orig %>% 
  select(
    SampleCode,
    StationName, 
    CollectionDate, 
    Analyte, 
    Result, 
    RptLimit, 
    Units,
    Method,
    Purpose,
    ParentSample
  )

dis_lab_data2_clean <- dis_lab_data2_orig %>%
  select(
    SampleCode,
    StationName = LongStationName, 
    CollectionDate, 
    Analyte, 
    Result, 
    RptLimit, 
    Units,
    Method,
    Purpose = SampleType,
    ParentSample
  ) %>% 
  mutate(
    CollectionDate = mdy_hm(CollectionDate),
    RptLimit = as.numeric(RptLimit)
  )

# Bind dis_lab_data1_clean and dis_lab_data2_clean
dis_lab_data_clean <- bind_rows(dis_lab_data1_clean, dis_lab_data2_clean)

# Start to clean the discrete lab data
dis_lab_data_clean1 <- dis_lab_data_clean %>% 
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
    Purpose,
    ParentSample
  )


# 4. Lab Replicates ----------------------------------------------------------

# Remove the unusual lab replicates for DOC collected at SRH in 2019 
  # which have different Sample codes
srh_doc_19_orig <- dis_lab_data_clean %>% 
  filter(
    StationName == "SRH",
    Analyte == "DOC",
    year(CollectionDate) == 2019
  )

srh_doc_19_clean <- srh_doc_19_orig %>% 
  filter(
    !SampleCode %in% c(
      "E0119B0264",
      "E0219B0279",
      "E0319B0689",
      "E0419B0950",
      "E0519B1196",
      "E0619B1364"
    )
  )

dis_lab_data_clean1 <- 
  anti_join(dis_lab_data_clean, srh_doc_19_orig) %>% 
  bind_rows(srh_doc_19_clean)

# Pull out lab replicates
dis_lab_reps <- dis_lab_data_clean1 %>%
  count(SampleCode, Analyte) %>% 
  filter(n == 2) %>% 
  select(-n)

# Pull lab replicates from dis_lab_data_clean1
dis_lab_rep_pairs <- inner_join(dis_lab_data_clean1, dis_lab_reps)

# The Result and LabDetect variables are unique between lab replicate pairs, 
  # all other variables are identical

# Pull out Result and LabDetect variables and widen the replicate pairs in separate columns
dis_lab_reps_wide <- dis_lab_rep_pairs %>%
  select(SampleCode, Analyte, Result, LabDetect) %>% 
  # Add a variable to define obs as either Rep1 or Rep2
  group_by(SampleCode, Analyte) %>% 
  mutate(Rep = row_number()) %>% 
  ungroup() %>% 
  # Widen the Result and LabDetect variables
  pivot_wider(names_from = Rep, values_from = c(Result, LabDetect))

# Join lab replicate dataframes together and calculate the RPD of the replicate pairs
dis_lab_reps_f <- dis_lab_rep_pairs %>% 
  select(-c(Result, LabDetect)) %>% 
  distinct() %>% 
  left_join(dis_lab_reps_wide) %>% 
  mutate(RPD = round(abs(Result_1 - Result_2)/((Result_1 + Result_2)/2), 3))

# Are there any lab replicate pairs with RPD values greater than 25%?
dis_lab_reps_f %>% filter(RPD > 0.2)  
# yes, 12 pairs, all but one pair are close to the RL

# Export lab replicate data as a .csv file
dis_lab_reps_f %>% 
  select(
    SampleCode,
    StationName,
    CollectionDate,
    Analyte,
    Result_1,
    Result_2,
    LabDetect_1,
    LabDetect_2,
    RPD,
    RptLimit,
    Units,
    Method,
    Purpose, 
    ParentSample
  ) %>% 
  write_excel_csv(paste0(fp_abs_wq_proc, "/Discrete_Lab_Data_Replicates.csv"), na = "")

# Only include one of the two lab replicate pairs for the final dataset
dis_lab_reps_1r <- dis_lab_reps_f %>% 
  select(-c(Result_2, LabDetect_2, RPD)) %>% 
  rename(
    Result = Result_1,
    LabDetect = LabDetect_1
  )

dis_lab_data_clean2 <- dis_lab_data_clean1 %>% 
  anti_join(dis_lab_reps) %>% 
  bind_rows(dis_lab_reps_1r)

# Clean up working environment
obj_keep <- append(obj_keep, "dis_lab_data_clean2")
rm(list = ls()[!(ls() %in% obj_keep)])


# 5. Field Duplicates -----------------------------------------------------

# Pull out Field Duplicates
dis_field_dups <- dis_lab_data_clean2 %>% filter(Purpose != "Normal Sample")

# Pull out all parent samples from dis_lab_data_clean2 and join with Field Duplicate data
dis_field_dup_pairs <- 
  inner_join(
    dis_lab_data_clean2, 
    dis_field_dups,
    by = c("SampleCode" = "ParentSample", "Analyte"),
    suffix = c("_PS", "_FD")
 )

# Restructure dis_field_dup_pairs and calculate the RPD of the field duplicate pairs
dis_field_dup_pairs_f <- dis_field_dup_pairs %>% 
  mutate(RPD = round(abs(Result_PS - Result_FD)/((Result_PS + Result_FD)/2), 3)) %>% 
  rename(
    SampleCode_PS = SampleCode,
    StationName = StationName_PS,
    RptLimit = RptLimit_PS,
    Units = Units_PS,
    Method = Method_PS
  ) %>% 
  select(
    starts_with("Samp"),
    StationName,
    starts_with("Coll"),
    Analyte,
    starts_with("Res"),
    starts_with("LabD"),
    RPD,
    RptLimit,
    Units,
    Method
  )

# Are there any Field Duplicate pairs with RPD values greater than 25%?
dis_field_dup_pairs_f %>% filter(RPD > 0.2)  
# yes, 12 pairs, but values are small and the differences aren't alarming

# Export Field Duplicate data as a .csv file
dis_field_dup_pairs_f %>% write_excel_csv(paste0(fp_abs_wq_proc, "/Discrete_Lab_Data_Field_Dups.csv"), na = "")

# Only keep the Normal Samples for the final dataset
dis_lab_data_clean3 <- dis_lab_data_clean2 %>% filter(Purpose == "Normal Sample")

# Clean up working environment
obj_keep <- append(obj_keep, "dis_lab_data_clean3")
obj_keep <- discard(obj_keep, str_detect(obj_keep, "clean2$"))
rm(list = ls()[!(ls() %in% obj_keep)])


# 6. Final Cleaning and Export --------------------------------------------

# Rename and remove a few variables
dis_lab_data_clean3 <- dis_lab_data_clean3 %>%
  rename(
    StationCode = StationName,
    DateTime = CollectionDate
  ) %>%
  select(-c(Purpose, ParentSample))

# Export cleaned discrete lab data as a .csv file
dis_lab_data_clean3 %>% write_excel_csv(paste0(fp_abs_wq_proc, "/WQ_OUTPUT_Discrete_Lab_formatted.csv"), na = "")

