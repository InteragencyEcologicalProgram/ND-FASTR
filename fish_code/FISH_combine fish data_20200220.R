#Let's get the fish data organized.

library(tidyverse)
library(readxl)
library(lubridate)
library(readr)

#datasets to include:
#FMWT
#STN
#FRP
#USGS
#EDSM
#DJFMP

#add location infomrmation for stations that don't have any
stations = read.csv("FISH_MAN_AllIEPstations_20200220.csv", stringsAsFactors = F)

#First FMWT
#data origionally from: ftp://ftp.wildlife.ca.gov/TownetFallMidwaterTrawl/FMWT%20Data/FMWT%201967-2019%20Catch%20Matrix_updated.zip


FMWT <- read_excel("fish data/FMWT 1967-2019 Catch Matrix_updated.xlsx", 
                        sheet = "FlatFile", col_types = c("numeric", 
                               "date", "numeric", "text", "date", 
                               rep("numeric", 127)))

#need to make column headers match Yolo
#           "SampleID"      "Date"          "Year"          "Month"         "Time"          "DateTime"     
#"StationCode"   "MethodCode"    "GearID"        "ConditionCode" "VegCode"       "OrganismCode"  "CommonName"   
#"FishTagID"     "GeneticID"     "ForkLength"    "Count"         "Race"          "StageCode"     "WeatherCode"  
# "WaterTemp"     "Secchi"        "Conductivity"  "SpCnd"         "DO"            "pH"            "Turbidity"    
#"SubstrateCode" "Tide"          "SeineVol"      "TrapStatus"   

#record the previous names so we don't have to write out all the fish names again
oldnames = names(FMWT)

#re-assign new names
names(FMWT) = c("Year", "Date", "Survey", "StationCode", 
                "Time", "Index", "WaterTemp", 
                "Conductivity", "BottomConductivity", "Turbidity", "Secchi", "Depth",
                "VolumeSampled", "Tide", "Tow Direction", "WeatherCode", "Microcystis", "Wave",
                oldnames[19:132]) 

#Now flip it from wide to long using "pivot_longer" (from tidyr)
FMWT2 = pivot_longer(FMWT, 
                     cols = `Aequorea spp (Lens Jellyfish)`:`Yellowfin Goby`, #These are the columsn we want to pivot
                     names_to = "FMWT", values_to = "Count") #tell it what we want to name the new columns
#any column that isn't specified won't be pivoted. 



#Get rid of zeros, NAs, and the stations we don't care about. 
#also all data before 2000. We use the function "filter" from dplyr

FMWT3 = filter(FMWT2, Count != 0, !is.na(Count), Year >1999, 
               StationCode %in% c(795, 796, 797, 719, 723, 721, 705:717)) %>%
  
  #Now make a new column for "Survey" and "Method"
 mutate( Survey = "FMWT", MethodCode = "FMWT") %>%
  
  #join it to the station locations
 left_join( stations, by = c("Survey", "StationCode"))




#Now for EDSM
library(readr)
EDSM_KDTR <- read_csv("fish data/edi.415.1/EDSM_KDTR.csv", 
                      col_types = cols(Date = col_date(format = "%m/%d/%Y"), 
                                       Time = col_time(format = "%H:%M:%S")))


#filter it so its just the right regions
EDSM = filter(EDSM_KDTR, Region == "North", 
              CommonName != "Black Sea jellyfish")

#get rid of a few columns we don't need
EDSM = EDSM[,c(1,2,3,4,6,7,8,9,13,14,17, 19,20,21:30,32,33)]

#fix the names
oldnames = names(EDSM)
names(EDSM) = c("Region", "SubRegion", "Stratum", "StationCode", 
                "Date", 
                "Time", "Duration", "Tow","Latitude", "Longitude",
                "MethodCode","Tow Direction", "Depth", "Tide", "WeatherCode",
                "WaterTemp", "DO",
                "Conductivity", "Turbidity", "Secchi", 
                "VolumeSampled", "Debris", "DJFMP", "ForkLength", "Count")



#add something so you can tell it's EDSM
EDSM$Survey = "EDSM"

#Yolo bypass
YBFMP <- read_csv("fish data/FISH_RAW_YBFMP_WQ_Fish_2011_thru_2019_20200206.csv", 
                                                           col_types = cols(Date = col_date(format = "%Y-%m-%d"), 
                                                                            DateTime = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                                                            FishTagID = col_character(), GeneticID = col_character(), 
                                                                            SeineVol = col_number(), SpCnd = col_number(), 
                                                                            SubstrateCode = col_character(), 
                                                                            Tide = col_character(), Time = col_time(format = "%H:%M:%S"), 
                                                                            Turbidity = col_double(), VegCode = col_character(), 
                                                                            X1 = col_skip()))
#Filter it to post 2000
YBFMP = filter(YBFMP, Date > "2000-01-01")

#rename a few of hte columns to make it easier to join with the other data sets
YBFMP = rename(YBFMP, VolumeSampled = SeineVol, Yolo = CommonName)

#attach latitude and longitude
YBFMP2 = inner_join(YBFMP, stations, by = c("StationCode"))

#Now townet
TownetData <- read_excel("fish data/TownetData_1959-2019.xlsx",  
                         col_types = c("numeric","numeric", "date","text",
                                       rep("numeric", 96)),
                         sheet = "CatchPerTow")

#change the names to match Yolo
oldnames = names(TownetData)
names(TownetData) = c("Year", "Survey", "Date",
                      "StationCode", "Tow", "Index",
                      "VolumeSampled", "Secchi", "WaterTemp", "Conductivity",
                      "Depth", oldnames[12:100])

#pivot from wide to long
Townet = pivot_longer(TownetData, cols = `Age-0 Striped Bass`:`Yellowfin Goby`, 
                      names_to = "Townet", values_to = "Count")

#just the positive catches, only since 2000, and only stations near Yolo
Townet = filter(Townet, Count != 0, Year > 1999, 
                StationCode %in% c(706:800)) %>%
  #Add a varible for the survey name and the method code
  mutate(Survey = "Townet", MethodCode = "Townet") %>%
  left_join(stations, by = c("Survey", "StationCode"))

#now DJFMP (just the beach seines, since none of the tows are anywhere near)
DJFMP_BS <- read_csv("fish data/edi.244.3/1976-2018_DJFMP_beach_seine_fish_and_water_quality_data.csv", 
                     col_types = cols(Conductivity = col_number(), 
                                      DO = col_number(), EndMeter = col_skip(), 
                                      SampleDate = col_date(format = "%Y-%m-%d"), 
                                      SampleTime = col_time(format = "%H:%M:%S"), 
                                      Secchi = col_number(), SeineDepth = col_number(), 
                                      StartMeter = col_skip(), TotalMeter = col_skip(), 
                                      TowDirectionCode = col_number(), 
                                      TowNumber = col_number(), Turbidity = col_number(), 
                                      Volume = col_number()))

#Just the last twenty year, only region 2 (liberty Island and nearby)
DJFMP = filter(DJFMP_BS, SampleDate > "1999-12-31", RegionCode == 2)

#update the names
names(DJFMP) = c("Location", "RegionCode", "StationCode",
                 "Date",  "Time","MethodCode",
                 "GearConditionCode", "Weather", "DO",
                 "WaterTemp","Turbidity","Secchi",
                 "Conductivity", "Tow","TowDirectionCode",
                 "Duration", "Debris", "Disturbance", "AlternateSite",
                 "SeineLength", "SeineWidth", "Depth", "VolumeSampled",
                 "OrganismCode", "DJFMP", "MarkCode", "StageCode",
                 "Maturation", "FL", "Race", "Count")




#upload fish common names crosswalk
fishnamescrosswalk <- read_excel("FISH_MAN_namescrosswalk_20200122.xlsx")

#Use "merge" to add the standardized common names to the origional data set
Townet = merge(Townet, unique(fishnamescrosswalk[,c(1,5)]))

#Now make a new column for latitude and logitude
Townet2 = mutate(Townet, Latitude = NA, Longitude = NA) %>%
  
  #Subset just the columns we care about
  select(Date, Survey, StationCode, MethodCode, Count,
         Secchi, Conductivity, WaterTemp, 
         CommonName, Year, Tow, Depth, VolumeSampled, Latitude, Longitude)

#add the standardized names to FMWT
FMWT3 = merge(FMWT3, unique(fishnamescrosswalk[,c(1,3)]))

#add a new column and select the columns that are standard between data sets. 
FMWT4 = mutate(FMWT3, Tow = 1) %>%
  select(Date, Survey, StationCode, MethodCode, Count,
         Secchi, Conductivity, WaterTemp, 
         CommonName, Year, Tow, Depth, VolumeSampled, Latitude, Longitude)

#add standardized names to Yolo
Yolo = merge(YBFMP2, unique(fishnamescrosswalk[,c(1,2)]))

#add some new columns so we can merge them and select the standard columns.
Yolo2 = mutate(Yolo, Year = year(Date), 
               Tow = 1, 
               Depth = NA, 
               Survey = "Yolo") %>%
  select(Date, Survey, StationCode, MethodCode, Count,
         Secchi, Conductivity, WaterTemp, 
         CommonName, Year, Tow, Depth, VolumeSampled, Latitude, Longitude)

#do the same thing for EDSM
EDSM2 = merge(EDSM, unique(fishnamescrosswalk[,c(1,4)]))
EDSM3 = mutate(EDSM2, Year = year(Date), Survey = "EDSM") %>%
  select(Date, Survey, Year, StationCode, MethodCode, Count,
               Secchi, Conductivity, WaterTemp, 
               CommonName, Year, Tow, Depth, VolumeSampled, Latitude, Longitude)

#now do it for DJFMP
DJFMP2 = merge(DJFMP, unique(fishnamescrosswalk[,c(1,4)]))
DJFMP3 = mutate(DJFMP2, Year = year(Date), 
                Survey = "DJFMP") %>%
  left_join(stations, by = c("Survey", "StationCode")) %>%
  select(Date, Survey, Year, StationCode, MethodCode, Count,
         Secchi, Conductivity, WaterTemp, 
         CommonName, Year, Tow, Depth, VolumeSampled, Latitude, Longitude)

#Use "rbind" to put all the data sets together.
Allfishdata = rbind(Yolo2, EDSM3, Townet2, FMWT4, DJFMP3)

#Some of the data sets also had fish length information, so there are multiple rows
#for each species. We can use "group_by" and "summarize" to calculate the total catch
#for each sampling even.
AllfishdataSum = group_by(Allfishdata, Date, Survey, StationCode, 
                          MethodCode, Secchi, Conductivity,
                          WaterTemp, CommonName, Year, Tow, 
                          Depth, VolumeSampled, Latitude, Longitude) %>%
  summarize(totalCount = sum(Count))

#Export the final, integrated data set.
write.csv(AllfishdataSum, "FISH_MAN_allIEPsurveys_20200221.csv", row.names = F)
