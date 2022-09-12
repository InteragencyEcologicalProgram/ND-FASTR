## Extracting FASTR data from 
## AEU's Yolo Bypass Phyto datasheets
## 2/3/2022

library("tidyverse");packageVersion("tidyverse")
library("lubridate");packageVersion("lubridate")
library("janitor");packageVersion("janitor")

# Set working directory
setwd("./FASTR.phyto.final/")
getwd()

# Set visual theme in ggplot
theme_set(theme_bw())

# Clean workspace
rm(list=ls()) 

# Import EMP data files
phyto_files <- dir(path = "data/EMP/csv", pattern = "\\.csv", full.names = T)

phyto_all <- map_dfr(phyto_files, ~read_csv(.x))

## Read in files with non-standard headers individually
Dec2021 <- read_csv("data/EMP/oddballs/December 2021.csv")
Nov2021 <- read_csv("data/EMP/oddballs/November 2021.csv")
Sep2013 <- read_csv("data/EMP/oddballs/September 2013.csv")
Nov2013 <- read_csv("data/EMP/oddballs/November 2013.csv")

## Combine like oddball dfs
phyto2013 <- bind_rows(Sep2013, Nov2013)
phyto2021 <- bind_rows(Dec2021, Nov2021)

## Remove individual dfs
rm(Dec2021)
rm(Nov2021)
rm(Nov2013)
rm(Sep2013)

## Rename headers to match standard BSA headers
## Oddballs actually have the "correct" name of Total Cells 
## rather than the incorrect "Number of cells per unit"

phyto2013 <- phyto2013 %>%
  rename("Number of cells per unit" = "Total Cells Counted")

phyto2021 <- phyto2021 %>%
  rename("Number of cells per unit" = "Total Number of Cells") %>%
  rename("Unit Abundance" = "Unit Abundance (# of Natural Units)")

## Remove GALD measurements
phyto_all$GALD <- NULL
phyto_all$`GALD 1` <- NULL
phyto2021$`GALD 1` <- NULL
phyto2013$GALD <- NULL

## Combine oddball files with others
phyto_all <- bind_rows(phyto_all, phyto2013)
phyto_all <- bind_rows(phyto_all, phyto2021)

# Clean up column names
phyto <- phyto_all %>% clean_names(case = "big_camel")

## Remove pre-calculated Unit Density and blank columns
phyto <- phyto %>% select(MethodCode:Biovolume10)

## Remove empty rows
phyto <- phyto %>% filter_all(any_vars(!is.na(.)))

## Average all 10 biovolume measurements for each taxon
phyto <- phyto %>% rowwise() %>% mutate(BV.Avg = mean(c_across(Biovolume1:Biovolume10), na.rm = T))

# Remove Individual Biovolume Columns
phyto <- phyto %>% select(!(Biovolume1:Biovolume10))

# Remove unneeded columns
phyto <- phyto %>% select(!c("MethodCode","BsaTin","DiatomSoftBody","Synonym"))
phyto <- phyto %>% select(!(ColonyFilamentIndividualGroupCode:Shape))
phyto <- phyto %>% select(!(VolumeReceivedML:NumberOfFieldsCounted))

## Fix samples with missing times
## Just replace with 12:00:00 b/c aren't doing any time-based analyses
phyto <- phyto %>% replace_na(list(SampleTime = "12:00:00"))

## Get dates in the right format
## Some are 1/1/14 and others 1/1/2014
phyto$SampleDate <- mdy(phyto$SampleDate)

## Combine date and time column
phyto <- phyto %>% unite(DateTime, c("SampleDate","SampleTime"), sep = " ") #, remove = FALSE, na.rm = FALSE)

phyto$DateTime <- as_datetime(phyto$DateTime, 
                              tz = "US/Pacific",
                              format = c("%Y-%m-%d %H:%M:%OS"))

## Check for missing dates
phyto %>% filter(is.na(DateTime)) ## No missing dates

## Correct BSA header
phyto <- phyto %>% rename("TotalCells" = "NumberOfCellsPerUnit")

## Calculate Unit Density & Biovolume Density
phyto <- phyto %>%
  mutate(Units.per.mL = UnitAbundance * Factor) %>%
  mutate(BV.um3.per.mL= TotalCells * BV.Avg * Factor)

## Add column for year and month for highlighting data
phyto <- phyto %>% mutate(Year = year(phyto$DateTime))

phyto <- phyto %>% mutate(Month = month(phyto$DateTime, label = T))

phyto$Year <- as.factor(phyto$Year)

## Remove columns no longer needed
phyto <- phyto %>% 
  select(!(Species:BV.Avg)) %>% 
  select(!(Factor))

## List of stations 

#list(unique(phyto$StationCode))

## Fix site names
phyto$StationCode <- gsub("EZ6 SAC","EZ6",phyto$StationCode)
phyto$StationCode <- gsub("EZ6SAC","EZ6",phyto$StationCode)
phyto$StationCode <- gsub("EZ6SJR","EZ6-SJR",phyto$StationCode)
phyto$StationCode <- gsub("EZ2SAC","EZ2",phyto$StationCode)
phyto$StationCode <- gsub("EZ2 SAC","EZ2",phyto$StationCode)
phyto$StationCode <- gsub("EZ2SJR","EZ2-SJR",phyto$StationCode)
phyto$StationCode <- gsub("EZ2 SJR","EZ2-SJR",phyto$StationCode)
phyto$StationCode <- gsub("EZ6 SJR","EZ6-SJR",phyto$StationCode)
phyto$StationCode <- gsub("D16-Twitchell","D16",phyto$StationCode)
phyto$StationCode <- gsub("D16-Twitchel","D16",phyto$StationCode)
phyto$StationCode <- gsub("D16 - Twitchell","D16",phyto$StationCode)
phyto$StationCode <- gsub("D16 Twitchell","D16",phyto$StationCode)
phyto$StationCode <- gsub("NZ328","NZ325",phyto$StationCode) ## Typo in August 2019
phyto$StationCode <- gsub("C3A-HOOD","C3A",phyto$StationCode)
phyto$StationCode <- gsub("C3A Hood","C3A",phyto$StationCode)
phyto$StationCode <- gsub("C3A- Hood","C3A",phyto$StationCode)
phyto$StationCode <- gsub("C3A-Hood","C3A",phyto$StationCode)
phyto$StationCode <- gsub("NZ542","NZS42",phyto$StationCode)
phyto$StationCode <- gsub("E26","EZ6",phyto$StationCode)
phyto$StationCode <- gsub("E22","EZ2",phyto$StationCode) # Typo in May 2018

## Remove extra Microcystis tows at D19
phyto <- phyto %>% filter(StationCode != "D19 MC Tow")

## Print out all station IDs to flag which ones to remove
#all.stations <- sort(unique(phyto$StationCode))
#write(all.stations, file = "station_names.txt")

## Confirm station IDs
unique(phyto$StationCode)
table(phyto$StationCode)

## In Fall 2016, taxonomists began classifying the species 
## Chroococcus microscopicus as Eucapsis microscopica. This is one of the most
## dominant species in this samples, so all taxa previously classified as 
## C. microscopicus will be re-named E. microscopica

phyto <- phyto %>% 
  mutate(Taxon = case_when(Taxon == 'Chroococcus microscopicus' ~ 'Eucapsis microscopica',
                           TRUE ~ Taxon)) %>%
  mutate(Genus = case_when(Taxon == 'Eucapsis microscopica' ~ 'Eucapsis',
                           TRUE ~ Genus))

## The taxon Plagioselmis lacustris is inconsistently named, appearing sometimes as 
## Rhodomonas lacustris. Change to Rhodomonas lacustris to avoid confusion.

phyto <- phyto %>% 
  mutate(Taxon = case_when(Taxon == 'Plagioselmis lacustris' ~ 'Rhodomonas lacustris',
                           TRUE ~ Taxon)) %>%
  mutate(Genus = case_when(Taxon == 'Rhodomonas lacustris' ~ 'Rhodomonas',
                           TRUE ~ Genus))

## Correct the genus label for a Chlorella entry
phyto$Genus <- gsub("cf Chlorella","Chlorella",phyto$Genus)

sort(unique(phyto$Genus)) ## 238 unique genera

## Add column for year and month for highlighting data
phyto <- phyto %>% mutate(Year = year(phyto$DateTime))

phyto <- phyto %>% mutate(Month = month(phyto$DateTime, label = T))

## Order month in calendar order rather than (default) alphabetical
phyto$Month = factor(phyto$Month, levels = month.abb)

## Units for Density (unit, biovolume) are in per mL, will convert to per L 
phyto <- phyto %>% mutate(across(Units.per.mL:BV.um3.per.mL, ~ .x * 1000,.keep = "unused"))

## Rename headers b/c units are now in L
phyto <- phyto %>% 
  rename("Units.per.L" = "Units.per.mL") %>%
  rename("BV.um3.per.L" = "BV.um3.per.mL")

## Reorder columns
phyto <- phyto %>%
  #relocate(Group, .after = Genus) %>%
  relocate(Year, .after = DateTime) %>%
  relocate(Month, .after = DateTime)

## Summarize by genus
phyto_gen_EMP <- phyto %>%
  group_by(Year, Month, DateTime, StationCode, Genus) %>%
  summarize(across(Units.per.L:BV.um3.per.L, ~sum(.x, na.rm = TRUE))) %>%
  ungroup

## Read in Region data
regions <- read_csv("CSVs/station_regions_EMP.csv")

# Combine region data and phyto data
phyto_gen_EMP <- left_join(phyto_gen_EMP, regions)

# Reorder column
phyto_gen_EMP <- phyto_gen_EMP %>% relocate(Region, .after = StationCode)

# check if there are any NAs in Region after the join
table(is.na(phyto_gen_EMP$Region)) # no NAs

## Save data file
save(phyto_gen_EMP, file = "RData/phyto_gen_EMP.RData") 
