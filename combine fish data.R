#Let's get the fish data organized.

library(tidyverse)
library(readxl)
library(lubridate)

#datasets to include:
#FMWT
#STN
#FRP
#USGS
#EDSM
#DJFMP

#First FMWT
#data origionally from: ftp://ftp.wildlife.ca.gov/TownetFallMidwaterTrawl/FMWT%20Data/FMWT%201967-2018%20Catch%20Matrix_updated.zip

#On 1/6, it was just data through 2018. 2019 data should be ready by 1/13


FMWT <- read_excel("fish data/FMWT 1967-2018 Catch Matrix_updated.xlsx", 
                        sheet = "FlatFile", col_types = c("numeric", 
                               "date", "numeric", "text", "date", rep("numeric", 123)))

#need to make column headers match Yolo
#SampleDate	SampleTime	StationCode	MethodCode	GearID	CommonName	GeneticallyConfirmed	
#GeneticID	Field_ID_CommonName	ForkLength	Count	FishSex	Race	
#MarkCode	CWTSample	FishTagID	StageCode	Dead	GearConditionCode	
#WeatherCode	WaterTemperature	Secchi	Conductivity	SpCnd	DO	pH	
#Turbidity	SubstrateCode	Tide	VolumeSeined	Latitude	Longitude

oldnames = names(FMWT)
names(FMWT) = c("Year", "SampleDate", "Survey", "StationCode", 
                "SampleTime", "Index", "WaterTemperature", 
                "Conductivity", "BottomConductivity", "Turbidity", "Secchi", "Depth",
                "VolumeSampled", "Tide", "Tow Direction", "WeatherCode", "Microcystis", "Wave",
                oldnames[19:128]) 

#Now flip it from wide to long
FMWT2 = pivot_longer(FMWT, cols = `Aequorea spp.`:`Yellowfin Goby`, names_to = "FMWT", values_to = "Count")

#Get rid of zeros, NAs, and the stations we don't care about. 
#also all data before 2000

FMWT3 = filter(FMWT2, Count != 0, !is.na(Count), Year >1999, 
               StationCode %in% c(795, 796, 797, 719, 723, 721, 705:717))
FMWT3$Survey = "FMWT"
FMWT3$MethodCode = "FMWT"

#Get rid of the jellyfish
#FMWT3 = filter(FMWT3, CommonName %in% c("Jellyfish (unid)", "Maeotias marginata"))


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
                "SampleDate", 
                "SampleTime", "Duration", "Tow","Latitude", "Longitude",
                "MethodCode","Tow Direction", "Depth", "Tide", "WeatherCode",
                "WaterTemperature", "DO",
                "Conductivity", "Turbidity", "Secchi", 
                "VolumeSampled", "Debris", "DJFMP", "ForkLength", "Count")



#add something so you can tell it's EDSM
EDSM$Survey = "EDSM"

#Yolo bypass
YBFMP <- read_csv("fish data/edi.233.2/YBFMP_Fish_Catch_and_Water_Quality.csv", 
                  col_types = cols(Conductivity = col_number(), 
                                   DO = col_number(), SampleDate = col_date(format = "%m/%d/%Y"), 
                                   SampleTime = col_time(format = "%H:%M"),
                                   FishTagID = col_character(),
                                   GeneticID = col_character(),
                                   Tide = col_character(),
                                   FishSex = col_character(),
                                   SpCnd = col_number(), Turbidity = col_number(), 
                                   VolumeSeined = col_number(), pH = col_number()))

#Filter it to post 2000
YBFMP = filter(YBFMP, SampleDate > "01/01/2000")
YBFMP = rename(YBFMP, VolumeSampled = VolumeSeined, Yolo = CommonName)

#Now townet
TownetData <- read_excel("fish data/TownetData_1959-2019.xlsx",  
                         col_types = c("numeric","numeric", "date","text",
                                       rep("numeric", 96)),
                         sheet = "CatchPerTow")
oldnames = names(TownetData)
names(TownetData) = c("Year", "Survey", "SampleDate",
                      "StationCode", "Tow", "Index",
                      "VolumeSampled", "Secchi", "WaterTemperature", "Conductivity",
                      "Depth", oldnames[12:100])
Townet = pivot_longer(TownetData, cols = `Age-0 Striped Bass`:`Yellowfin Goby`, 
                      names_to = "Townet", values_to = "Count")

Townet = filter(Townet, Count != 0, Year > 1999, StationCode %in% c(706:800))
Townet$Survey = "Townet"
Townet$MethodCode = "Townet"
fish = unique(Townet$Townet)
write.csv(fish, "fish.csv")


#upload fish common names crosswalk
fishnamescrosswalk <- read_excel("fishnamescrosswalk.xlsx")

Townet = merge(Townet, unique(fishnamescrosswalk[,c(1,5)]))
Townet2 = mutate(Townet, Latitude = NA, Longitude = NA) %>%
  select(SampleDate, Survey, StationCode, MethodCode, Count,
         Secchi, Conductivity, WaterTemperature, 
         CommonName, Year, Tow, Depth, VolumeSampled, Latitude, Longitude)

FMWT3 = merge(FMWT3, unique(fishnamescrosswalk[,c(1,3)]))
FMWT4 = mutate(FMWT3, Tow = 1, Latitude = NA, Longitude = NA) %>%
  select(SampleDate, Survey, StationCode, MethodCode, Count,
         Secchi, Conductivity, WaterTemperature, 
         CommonName, Year, Tow, Depth, VolumeSampled, Latitude, Longitude)

Yolo = merge(YBFMP, unique(fishnamescrosswalk[,c(1,2)]))
Yolo2 = mutate(Yolo, Year = year(SampleDate), Tow = 1, Depth = NA, Survey = "Yolo") %>%
  select(SampleDate, Survey, StationCode, MethodCode, Count,
         Secchi, Conductivity, WaterTemperature, 
         CommonName, Year, Tow, Depth, VolumeSampled, Latitude, Longitude)

EDSM2 = merge(EDSM, unique(fishnamescrosswalk[,c(1,4)]))
EDSM3 = mutate(EDSM2, Year = year(SampleDate), Survey = "EDSM") %>%
  select(SampleDate, Survey, Year, StationCode, MethodCode, Count,
               Secchi, Conductivity, WaterTemperature, 
               CommonName, Year, Tow, Depth, VolumeSampled, Latitude, Longitude)

Allfishdata = rbind(Yolo2, EDSM3, Townet2, FMWT4)
