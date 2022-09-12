## Extracting phyto data from EMP and AEU datasheets
## AEU's Yolo Bypass Phyto datasheets
## 9/7/2022
## Goal is to combine these datasets while retaining all data
## including GALD and Biovolume


library("tidyverse");packageVersion("tidyverse")
library("lubridate");packageVersion("lubridate")
library("janitor");packageVersion("janitor")

# Set working directory
setwd("./FASTR.phyto.final/")
getwd()

# Clean workspace
rm(list=ls())

#########################################
######## Importing Raw EMP Data #########
#########################################

# Import EMP data files
phyto_files_EMP <- dir(path = "data/EMP/csv", pattern = "\\.csv", full.names = T)

df_phyto_EMP <- map_dfr(phyto_files_EMP, ~read_csv(.x))

## Read in files with non-standard headers individually
df_Dec2021 <- read_csv("data/EMP/oddballs/December 2021.csv")
df_Nov2021 <- read_csv("data/EMP/oddballs/November 2021.csv")
df_Sep2013 <- read_csv("data/EMP/oddballs/September 2013.csv")
df_Nov2013 <- read_csv("data/EMP/oddballs/November 2013.csv")

## Combine like oddball dfs
df_phyto2013 <- bind_rows(df_Sep2013, df_Nov2013)
df_phyto2021 <- bind_rows(df_Dec2021, df_Nov2021)

## Remove individual dfs
rm(df_Dec2021)
rm(df_Nov2021)
rm(df_Nov2013)
rm(df_Sep2013)

## Rename headers to match standard BSA headers
## Oddballs actually have the "correct" name of Total Cells 
## rather than the incorrect "Number of cells per unit"

df_phyto2013 <- df_phyto2013 %>%
  rename("Number of cells per unit" = "Total Cells Counted")

df_phyto2021 <- df_phyto2021 %>%
  rename("Number of cells per unit" = "Total Number of Cells") %>%
  rename("Unit Abundance" = "Unit Abundance (# of Natural Units)")

## Combine oddball files with others
df_phyto_EMP <- bind_rows(df_phyto_EMP, df_phyto2013)
df_phyto_EMP <- bind_rows(df_phyto_EMP, df_phyto2021)

# Remove unneeded dfs
rm(df_phyto2013)
rm(df_phyto2021)

## Remove empty rows
df_phyto_EMP <- df_phyto_EMP %>% filter_all(any_vars(!is.na(.)))

## Correct GALD, which is imported into two separate columns
# Test to see if NAs are 'either/or' and that there aren't some rows with a 
# value in both GALD and GALD 1

sum(is.na(df_phyto_EMP$GALD)) # Total is 5880
sum(is.na(df_phyto_EMP$`GALD 1`)) # Total is 8072

# Sum of NAs is 13952 which is the same as the number of rows in the df.
# This shows that there aren't any rows with two values so that we can 
# Combine them without any issues.

# Move 
df_phyto_EMP <- df_phyto_EMP %>% relocate(`GALD 1`, .after = GALD)

# Combine both GALD columns
df_phyto_EMP <- df_phyto_EMP %>%
  rowwise() %>%
  mutate(GALD.Tot = sum(c_across(GALD:`GALD 1`), na.rm = TRUE))

# Remove old GALD columns and rename GALD.Tot
df_phyto_EMP <- df_phyto_EMP %>% 
  select(!(GALD:`GALD 1`)) %>%
  rename("GALD" = "GALD.Tot")

# Clean up column names
df_phyto_EMP <- df_phyto_EMP %>% clean_names(case = "big_camel")

df_phyto_EMP <- df_phyto_EMP %>% rename("GALD" = "Gald")

## Remove blank columns
df_phyto_EMP <- df_phyto_EMP %>% select_if(~ !all(is.na(.)))

## Remove columns that just have the method code "Phyto" as well as 
## pre-calculated organisms per mL
df_phyto_EMP <- df_phyto_EMP %>% select(SampleDate:Biovolume10,GALD)

## Add column to indicate what study it came from
df_phyto_EMP <- df_phyto_EMP %>% mutate(Study = "EMP", .after = StationCode)

#########################################
######## Importing Raw AEU Data #########
#########################################

# Import AEU data files (comment out when finished)
phyto_files_AEU <- dir(path = "data/csv", pattern = "\\.csv", full.names = T)
df_phyto_FASTR <- map_dfr(phyto_files_AEU, ~read_csv(.x))

## Remove empty rows
df_phyto_FASTR <- df_phyto_FASTR %>% filter_all(any_vars(!is.na(.)))

## Remove weird row with only a single zero in biovolume
df_phyto_FASTR <- df_phyto_FASTR %>% drop_na(MethodCode)

# Clean up column names
df_phyto_FASTR <- df_phyto_FASTR %>% clean_names(case = "big_camel")

## Filter out samples from other projects
# Read in station names with flags and merge
keepers <- read_csv("CSVs/station_names_flagged.csv")
df_phyto_FASTR <- left_join(df_phyto_FASTR, keepers)
rm(keepers)

## Remove data unrelated to project
df_phyto_FASTR <- df_phyto_FASTR %>% filter(Flag == "Keep")

## Remove flag column
df_phyto_FASTR <- df_phyto_FASTR %>% select(!(Flag))

## Confirm station IDs
unique(df_phyto_FASTR$StationCode) # Shows only 10 FASTR stations
table(df_phyto_FASTR$StationCode)

## Remove blank columns
df_phyto_FASTR <- df_phyto_FASTR %>% select_if(~ !all(is.na(.)))

# Remove individual measurment columns as they are only present in a small number of samples
df_phyto_FASTR <- df_phyto_FASTR %>% select(!(Dimension:DepthM))

# Remove secondary and tertiary GALD measurements as they don't vary much and 
# aren't present in EMP data
df_phyto_FASTR <- df_phyto_FASTR %>% select(!(Gald2:Gald3))

## Correct GALD, which is imported into two separate columns
# Test to see if NAs are 'either/or' and that there aren't some rows with a 
# value in both GALD and GALD 1

sum(is.na(df_phyto_FASTR$Gald)) # Total is 1791
sum(is.na(df_phyto_FASTR$Gald1)) # Total is 4535

# Sum of NAs is 6326 which is the same as the number of rows in the df.
# This shows that there aren't any rows with two values so that we can 
# Combine them without any issues.

# Move 
df_phyto_FASTR <- df_phyto_FASTR %>% relocate(Gald1, .after = Gald)

# Combine both GALD columns
df_phyto_FASTR <- df_phyto_FASTR %>%
  rowwise() %>%
  mutate(GALD.Tot = sum(c_across(Gald:Gald1), na.rm = TRUE))

# Check if rows have two NAs or no NAs in the GALD columns
# test <- df_phyto_FASTR %>%
#   mutate(GALD.Value = case_when(is.na(Gald) & is.na(Gald1) ~ "Fix",
#                                 TRUE ~ "Okay"))

# Remove old GALD columns and rename GALD.Tot
df_phyto_FASTR <- df_phyto_FASTR %>% 
  select(!(Gald:Gald1)) %>%
  rename("GALD" = "GALD.Tot")

## Remove MethodCode column that just says "Phyto" 
df_phyto_FASTR <- df_phyto_FASTR %>% select(!(MethodCode))

## Add column to indicate what study it came from
df_phyto_FASTR <- df_phyto_FASTR %>% mutate(Study = "FASTR", .after = StationCode)

#########################################
######## Combine Data & Analyze #########
#########################################

df_phyto <- bind_rows(df_phyto_EMP, df_phyto_FASTR)

## Average all 10 biovolume measurements for each taxon
df_phyto <- df_phyto %>% rowwise() %>% 
  mutate(BV.Avg = mean(c_across(Biovolume1:Biovolume10), na.rm = T)) %>% 
  select(!(Biovolume1:Biovolume10)) # Remove Individual Biovolume Columns

# Remove unneeded columns
df_phyto <- df_phyto %>% select(!c("BsaTin","DiatomSoftBody"))
df_phyto <- df_phyto %>% select(!(ColonyFilamentIndividualGroupCode:Shape))
df_phyto <- df_phyto %>% select(!(VolumeReceivedML:NumberOfFieldsCounted))

## Fix samples with missing times
## Just replace with 12:00:00 b/c aren't doing any time-based analyses
df_phyto <- df_phyto %>% replace_na(list(SampleTime = "12:00:00"))

## Get dates in the right format
## Some are 1/1/14 and others 1/1/2014
df_phyto$SampleDate <- mdy(df_phyto$SampleDate)

## Combine date and time column
df_phyto <- df_phyto %>% unite(DateTime, c("SampleDate","SampleTime"), sep = " ") #, remove = FALSE, na.rm = FALSE)

df_phyto$DateTime <- as_datetime(df_phyto$DateTime, 
                              tz = "US/Pacific",
                              format = c("%Y-%m-%d %H:%M:%OS"))

## Check for missing dates
df_phyto %>% filter(is.na(DateTime)) ## No missing dates

## Correct BSA header
df_phyto <- df_phyto %>% rename("TotalCells" = "NumberOfCellsPerUnit")

## Calculate Unit Density & Biovolume Density
df_phyto <- df_phyto %>%
  mutate(Units.per.mL = UnitAbundance * Factor) %>%
  mutate(BV.um3.per.mL= TotalCells * BV.Avg * Factor)

## Add column for year and month for highlighting data
df_phyto <- df_phyto %>% 
  mutate(Year = year(df_phyto$DateTime)) %>%
  mutate(Month = month(df_phyto$DateTime, label = T))

## Order month in calendar order rather than (default) alphabetical
df_phyto$Month = factor(df_phyto$Month, levels = month.abb)

## Reorder date/time columns
df_phyto <- df_phyto %>% 
  relocate(Year, .after = DateTime) %>% 
  relocate(Month, .after = DateTime)


# Convert years to factors for plotting
df_phyto$Year <- as.factor(df_phyto$Year)

#########################################
######## Data Cleaning #########
#########################################

## Fix EMP site names
df_phyto$StationCode <- gsub("EZ6 SAC","EZ6",df_phyto$StationCode)
df_phyto$StationCode <- gsub("EZ6SAC","EZ6",df_phyto$StationCode)
df_phyto$StationCode <- gsub("EZ6SJR","EZ6-SJR",df_phyto$StationCode)
df_phyto$StationCode <- gsub("EZ2SAC","EZ2",df_phyto$StationCode)
df_phyto$StationCode <- gsub("EZ2 SAC","EZ2",df_phyto$StationCode)
df_phyto$StationCode <- gsub("EZ2SJR","EZ2-SJR",df_phyto$StationCode)
df_phyto$StationCode <- gsub("EZ2 SJR","EZ2-SJR",df_phyto$StationCode)
df_phyto$StationCode <- gsub("EZ6 SJR","EZ6-SJR",df_phyto$StationCode)
df_phyto$StationCode <- gsub("D16-Twitchell","D16",df_phyto$StationCode)
df_phyto$StationCode <- gsub("D16-Twitchel","D16",df_phyto$StationCode)
df_phyto$StationCode <- gsub("D16 - Twitchell","D16",df_phyto$StationCode)
df_phyto$StationCode <- gsub("D16 Twitchell","D16",df_phyto$StationCode)
df_phyto$StationCode <- gsub("NZ328","NZ325",df_phyto$StationCode) ## Typo in August 2019
df_phyto$StationCode <- gsub("C3A-HOOD","C3A",df_phyto$StationCode)
df_phyto$StationCode <- gsub("C3A Hood","C3A",df_phyto$StationCode)
df_phyto$StationCode <- gsub("C3A- Hood","C3A",df_phyto$StationCode)
df_phyto$StationCode <- gsub("C3A-Hood","C3A",df_phyto$StationCode)
df_phyto$StationCode <- gsub("NZ542","NZS42",df_phyto$StationCode)
df_phyto$StationCode <- gsub("E26","EZ6",df_phyto$StationCode)
df_phyto$StationCode <- gsub("E22","EZ2",df_phyto$StationCode) # Typo in May 2018

## Fix AEU site names
df_phyto$StationCode <- gsub("SHER","SHR",df_phyto$StationCode)
df_phyto$StationCode <- gsub("BL-5","BL5",df_phyto$StationCode)

## Remove extra Microcystis tows at D19
df_phyto <- df_phyto %>% filter(StationCode != "D19 MC Tow")

## In Fall 2016, taxonomists began classifying the species 
## Chroococcus microscopicus as Eucapsis microscopica. This is one of the most
## dominant species in this samples, so all taxa previously classified as 
## C. microscopicus will be re-named E. microscopica

df_phyto <- df_phyto %>% 
  mutate(Taxon = case_when(Taxon == 'Chroococcus microscopicus' ~ 'Eucapsis microscopica',
                           TRUE ~ Taxon)) %>%
  mutate(Genus = case_when(Taxon == 'Eucapsis microscopica' ~ 'Eucapsis',
                           TRUE ~ Genus))

## The taxon Plagioselmis lacustris is inconsistently named, appearing sometimes as 
## Rhodomonas lacustris. Change to Rhodomonas lacustris to avoid confusion.

df_phyto <- df_phyto %>% 
  mutate(Taxon = case_when(Taxon == 'Plagioselmis lacustris' ~ 'Rhodomonas lacustris',
                           TRUE ~ Taxon)) %>%
  mutate(Genus = case_when(Taxon == 'Rhodomonas lacustris' ~ 'Rhodomonas',
                           TRUE ~ Genus))

## Correct the genus label for a Chlorella entry
df_phyto$Genus <- gsub("cf Chlorella","Chlorella",df_phyto$Genus)

## Units for Density (unit, biovolume) are in per mL, will convert to per L 
df_phyto <- df_phyto %>% mutate(across(Units.per.mL:BV.um3.per.mL, ~ .x * 1000,.keep = "unused"))

## Rename headers b/c units are now in L
df_phyto <- df_phyto %>% 
  rename("Units.per.L" = "Units.per.mL") %>%
  rename("BV.um3.per.L" = "BV.um3.per.mL")

#########################################
######### Adding Taxonomy Data ##########
#########################################

## Create list of all unique genera in the combined dataset and comment out once finished
# genera <- as_tibble_col(sort(unique(df_phyto$Genus)), column_name = "Genus")

## Read in previous list of taxa 
# taxa <- read_csv("CSVs/phyto_group_taxonomy.csv")
# genera <- left_join(genera, taxa)
# 
# write_csv(genera, file = "list_of_genera.csv")

# Read in CSV with manually-added WoRMS classification
taxa <- read_csv("CSVs/phyto_group_classification.csv")

df_phyto <- left_join(df_phyto, taxa)

## Check if any groups are missing
sum(is.na(df_phyto$Group)) # none missing 
rm(taxa)

## Save df to use for making plots
save(df_phyto, file = "RData/df_phyto.RData")


