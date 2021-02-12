# NDFA Water Quality
# Purpose: Code to import, clean, and export discrete laboratory water quality data from Bryte Lab
# Author: Jenna Rinde, Dave Bosworth


# 1. Global Code ----------------------------------------------------------

# Load packages
library(tidyverse)
library(readxl)
library(lubridate)

# Source global WQ functions
source("global_ndfa_funcs.R")

# Define relative file paths for raw and processed discrete lab WQ data files
fp_rel_wq_raw <- "WQ_Subteam/Raw_Data/Discrete"
fp_rel_wq_proc <- "WQ_Subteam/Processed_Data/Discrete"

# Define absolute file paths
fp_abs_wq_raw <- ndfa_abs_sp_path(fp_rel_wq_raw)
fp_abs_wq_proc <- ndfa_abs_sp_path(fp_rel_wq_proc)

# Create a vector of object names to keep throughout the script
obj_keep <- c("fp_abs_wq_raw", "fp_abs_wq_proc", "obj_keep")

# Clean up working environment
rm(list = ls()[!(ls() %in% obj_keep)])


# 2. Import Data -------------------------------------------------------------

# Create a character vector of all raw discrete lab data files
dis_lab_files <- 
  dir(
    file.path(fp_abs_wq_raw, "Raw_WDL_export_June2020"),
    pattern = "\\.xlsx$", 
    full.names = T
  )

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

# Import J-flag Ammonia data
dis_lab_j_nh4 <-
  read_excel(
    file.path(fp_abs_wq_raw, "NDFA_Ammonia_to_MDL.xlsx"), 
    sheet = "NDFA_Ammonia to MDL"
  )


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
  ) %>% 
  # Remove samples for LIB station since they are already in dis_lab_data1_clean
  filter(!str_detect(StationName, "^Liberty"))
  
# Bind dis_lab_data1_clean and dis_lab_data2_clean
dis_lab_data_clean <- bind_rows(dis_lab_data1_clean, dis_lab_data2_clean)

# Start to clean the discrete lab data
dis_lab_data_clean1 <- dis_lab_data_clean %>% 
  # Remove unnecessary Analytes
  filter(str_detect(Analyte, "^\\*No|^Field|^Spec|^Weath|Bromide$|Alkalinity$", negate = TRUE)) %>% 
  filter(Analyte != "Dissolved Total Kjeldahl Nitrogen") %>% 
  # Remove Field Blank samples
  filter(!str_detect(Purpose, "^Blank")) %>% 
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
      "Dissolved ortho-Phosphate" = "DOP",
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
    StationName = str_replace(StationName, "[:space:]{1}\\(ID.+\\)", ""),
    StationName = recode(
      StationName,
      "Below Base of Toe Drain in Prospect Slough" = "BL5",
      "Cache Slough at Ryer Island" = "RYI",
      "Davis Wastewater Discharge at Toe Drain" = "DWT",
      "Liberty at Approx Cntr S End" = "LIB",
      "Ridge Cut Slough at Knights Landing" = "RCS",
      "Rominger Bridge at Colusa Basin Drain" = "RMB",
      "Sacramento River @ Hood - C3A" = "SRH",
      "Sacramento River @ Sherwood Harbor" = "SHR",
      "Sacramento River at Decker Island (USGS)" = "SDI",
      "Sacramento River at Rio Vista Bridge" = "RVB",
      "Sacramento River at Vieira's" = "SRV",
      "Toe Drain at County Road 22" = "RD22",
      "Toe Drain at Interstate 80" = "I80",
      "Toe Drain at Rotary Screw Trap" = "STTD",
      "Woodland Wastewater Discharge at Toe Drain" = "WWT",
      "Yolo Bypass Toe Drain Below Lisbon Weir" = "LIS"
    ),
    # Standardize the Units variable
    Units = if_else(Units == "ug/L", "µg/L", Units)
  ) %>% 
  select(
    SampleCode,
    StationCode = StationName, 
    DateTime = CollectionDate, 
    Analyte, 
    Result, 
    RL = RptLimit, 
    Units,
    Method,
    Purpose,
    ParentSample
  )

# Modify the J-flag Ammonia data frame so that it can be joined to the main data frame
dis_lab_j_nh4_clean <- dis_lab_j_nh4 %>% 
  select(
    SampleCode,
    Analyte,
    J_val = `J-value`,
    MDL
  ) %>% 
  filter(!is.na(J_val)) %>% 
  # Convert "<" values in J_val to "< MDL"
  mutate(J_val = if_else(J_val == "<", "< MDL", J_val))

# Inner join the J-flag Ammonia data to the main data frame
dis_lab_j_nh4_join <- inner_join(dis_lab_data_clean1, dis_lab_j_nh4_clean) %>% 
  # Remove duplicate records
  distinct() %>% 
  # Convert J_val variable to the Result variable
  select(-Result) %>% 
  rename(Result = J_val)

# Bind the J-flag Ammonia data back to the main data frame
dis_lab_data_clean2 <- dis_lab_data_clean1 %>% 
  anti_join(dis_lab_j_nh4_join, by = c("SampleCode", "Analyte")) %>% 
  bind_rows(dis_lab_j_nh4_join) %>% 
  mutate(
    # Create a new variable to identify values below the Reporting Limit and Method Detection Limit
    ResultQual = case_when(
      Result == "< MDL" ~ "< MDL",
      str_detect(Result, "<") ~ "< RL"
    ),
    # Convert Result variable from character to numeric, convert <RL and <MDL data to NA values
    Result = signif(as.numeric(Result), 3),  #round to 3 significant digits
    # Add "J- estimated" to ResultQual variable for values <RL but >MDL
    ResultQual = case_when(
      is.na(Result) ~ ResultQual,
      Result < RL ~ "J- estimated"
    )
  )

# Clean up working environment
obj_keep <- append(obj_keep, "dis_lab_data_clean2")
rm(list = ls()[!(ls() %in% obj_keep)])


# 4. Lab Replicates ----------------------------------------------------------

# Remove the unusual lab replicates for DOC collected at SRH in 2019 
  # which have different Sample codes
srh_doc_19_orig <- dis_lab_data_clean2 %>% 
  filter(
    StationCode == "SRH",
    Analyte == "DOC",
    year(DateTime) == 2019
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

dis_lab_data_clean3 <- 
  anti_join(dis_lab_data_clean2, srh_doc_19_orig) %>% 
  bind_rows(srh_doc_19_clean)

# Pull out lab replicates
dis_lab_reps <- dis_lab_data_clean3 %>%
  count(SampleCode, Analyte) %>% 
  filter(n == 2) %>% 
  select(-n)

# Pull lab replicates from dis_lab_data_clean3
dis_lab_rep_pairs <- inner_join(dis_lab_data_clean3, dis_lab_reps)

# The Result and ResultQual variables are unique between lab replicate pairs, 
  # all other variables are identical

# Pull out Result and ResultQual variables and widen the replicate pairs in separate columns
dis_lab_reps_wide <- dis_lab_rep_pairs %>%
  select(SampleCode, Analyte, Result, ResultQual) %>% 
  # Add a variable to define obs as either Rep1 or Rep2
  group_by(SampleCode, Analyte) %>% 
  mutate(Rep = row_number()) %>% 
  ungroup() %>% 
  # Widen the Result and ResultQual variables
  pivot_wider(names_from = Rep, values_from = c(Result, ResultQual))

# Join lab replicate dataframes together and calculate the RPD of the replicate pairs
dis_lab_reps_f <- dis_lab_rep_pairs %>% 
  select(-c(Result, ResultQual)) %>% 
  distinct() %>% 
  left_join(dis_lab_reps_wide) %>% 
  mutate(RPD = round(abs(Result_1 - Result_2)/((Result_1 + Result_2)/2), 3))

# Are there any lab replicate pairs with RPD values greater than 20%?
dis_lab_reps_f %>% filter(RPD > 0.2)  
# yes, 13 pairs, all but one pair are close to the RL

# Export lab replicate data as a .csv file
dis_lab_reps_f %>% 
  mutate(
    # Convert Result variables to character indicating <RL values as such
    Result_1 = case_when(
      ResultQual_1 == "< RL" ~ "< RL", 
      TRUE ~ as.character(Result_1)
    ),
    Result_2 = case_when(
      ResultQual_2 == "< RL" ~ "< RL", 
      TRUE ~ as.character(Result_2)
    )
  ) %>% 
  # Only keep "J- estimated" in ResultQual variables
  mutate(across(starts_with("ResultQual"), ~if_else(.x == "J- estimated", "J- estimated", NA_character_))) %>% 
  select(
    SampleCode,
    StationCode,
    DateTime,
    Analyte,
    Result_1,
    Result_2,
    ResultQual_1,
    ResultQual_2,
    RPD,
    RL,
    MDL,
    Units,
    Method,
    Purpose, 
    ParentSample
  ) %>% 
  write_excel_csv(file.path(fp_abs_wq_proc, "Discrete_Lab_Data_Replicates.csv"), na = "")

# Only include one of the two lab replicate pairs for the final dataset
dis_lab_reps_1r <- dis_lab_reps_f %>% 
  # Use Result_1 unless it is NA and Result_2 isn't
  mutate(
    Result = case_when(
      !is.na(Result_2) & is.na(Result_1) ~ Result_2,
      TRUE ~ Result_1
    ),
    ResultQual = case_when(
      !is.na(Result_2) & is.na(Result_1) ~ ResultQual_2,
      TRUE ~ ResultQual_1
    )
  ) %>% 
  select(-c(ends_with(c("_1", "_2")), RPD))

dis_lab_data_clean4 <- dis_lab_data_clean3 %>% 
  anti_join(dis_lab_reps) %>% 
  bind_rows(dis_lab_reps_1r)

# Clean up working environment
obj_keep <- str_subset(obj_keep, "dis_lab_data_clean2", negate = TRUE)
obj_keep <- append(obj_keep, "dis_lab_data_clean4")
rm(list = ls()[!(ls() %in% obj_keep)])


# 5. Field Duplicates -----------------------------------------------------

# Pull out Field Duplicates
dis_field_dups <- dis_lab_data_clean4 %>% filter(Purpose != "Normal Sample")

# Pull out all parent samples from dis_lab_data_clean3 and join with Field Duplicate data
dis_field_dup_pairs <- 
  inner_join(
    dis_lab_data_clean4, 
    dis_field_dups,
    by = c("SampleCode" = "ParentSample", "Analyte"),
    suffix = c("_PS", "_FD")
 )

# Restructure dis_field_dup_pairs and calculate the RPD of the field duplicate pairs
dis_field_dup_pairs_f <- dis_field_dup_pairs %>% 
  mutate(RPD = round(abs(Result_PS - Result_FD)/((Result_PS + Result_FD)/2), 3)) %>% 
  rename(
    SampleCode_PS = SampleCode,
    StationCode = StationCode_PS,
    RL = RL_PS,
    MDL = MDL_PS,
    Units = Units_PS,
    Method = Method_PS
  ) %>% 
  select(
    starts_with("Samp"),
    StationCode,
    starts_with("Date"),
    Analyte,
    starts_with("Result_"),
    starts_with("ResultQual_"),
    RPD,
    RL,
    MDL,
    Units,
    Method
  ) %>% 
  mutate(
    # Convert Result variables to character indicating <RL values as such
    Result_PS = case_when(
      ResultQual_PS == "< RL" ~ "< RL", 
      TRUE ~ as.character(Result_PS)
    ),
    Result_FD = case_when(
      ResultQual_FD == "< RL" ~ "< RL", 
      TRUE ~ as.character(Result_FD)
    )
  ) %>% 
  # Only keep "J- estimated" in ResultQual variables
  mutate(across(starts_with("ResultQual"), ~if_else(.x == "J- estimated", "J- estimated", NA_character_)))

# Are there any Field Duplicate pairs with RPD values greater than 20%?
dis_field_dup_pairs_f %>% filter(RPD > 0.2)  
# yes, 17 pairs, but values are small and the differences aren't alarming

# Export Field Duplicate data as a .csv file
dis_field_dup_pairs_f %>% write_excel_csv(file.path(fp_abs_wq_proc, "Discrete_Lab_Data_Field_Dups.csv"), na = "")

# Only keep the Normal Samples for the final dataset
dis_lab_data_clean5 <- dis_lab_data_clean4 %>% filter(Purpose == "Normal Sample")

# Clean up working environment
obj_keep <- str_subset(obj_keep, "dis_lab_data_clean4", negate = TRUE)
obj_keep <- append(obj_keep, "dis_lab_data_clean5")
rm(list = ls()[!(ls() %in% obj_keep)])


# 6. Final Cleaning and Export --------------------------------------------

# Make a few modifications to the final dataset
dis_lab_data_clean_f <- dis_lab_data_clean5 %>%
  mutate(
    # Convert Result variable to character indicating <MDL and <RL values as such
    Result = case_when(
      ResultQual == "< RL" ~ "< RL",
      ResultQual == "< MDL" ~ "< MDL",
      TRUE ~ as.character(Result)
    ),
    # Only keep "J- estimated" in ResultQual variable
    ResultQual = if_else(ResultQual == "J- estimated", "J- estimated", NA_character_)
  ) %>% 
  # Select variables to keep
  select(
    SampleCode:Result,
    ResultQual,
    RL,
    MDL,
    Units, 
    Method
  ) %>% 
  arrange(DateTime, Analyte)

# Export cleaned discrete lab data as a .csv file - 
# this is the final version of the dataset for the discrete lab data as of 1/25/2021
dis_lab_data_clean_f %>% write_excel_csv(file.path(fp_abs_wq_proc, "WQ_INPUT_Discrete_Lab_2021-01-25.csv"), na = "")

