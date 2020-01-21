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

#First FMWT
#data origionally from: ftp://ftp.wildlife.ca.gov/TownetFallMidwaterTrawl/FMWT%20Data/FMWT%201967-2019%20Catch%20Matrix_updated.zip


#at location infomrmatino for stations that don't have any
stations = read.csv("AllIEP.csv")


FMWT <- read_excel("fish data/FMWT 1967-2019 Catch Matrix_updated.xlsx", 
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
               StationCode %in% c(795, 796, 797, 719, 723, 721, 705:717)) %>%
 mutate( Survey = "FMWT", MethodCode = "FMWT") %>%
 left_join( stations, by = c("Survey", "StationCode"))


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

#change the names to match Yolo
oldnames = names(TownetData)
names(TownetData) = c("Year", "Survey", "SampleDate",
                      "StationCode", "Tow", "Index",
                      "VolumeSampled", "Secchi", "WaterTemperature", "Conductivity",
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
                 "SampleDate",  "SampleTime","MethodCode",
                 "GearConditionCode", "Weather", "DO",
                 "WaterTemperature","Turbidity","Secchi",
                 "Conductivity", "Tow","TowDirectionCode",
                 "Duration", "Debris", "Disturbance", "AlternateSite",
                 "SeineLength", "SeineWidth", "Depth", "VolumeSampled",
                 "OrganismCode", "DJFMP", "MarkCode", "StageCode",
                 "Maturation", "FL", "Race", "Count")




#upload fish common names crosswalk
fishnamescrosswalk <- read_excel("fishnamescrosswalk.xlsx")

Townet = merge(Townet, unique(fishnamescrosswalk[,c(1,5)]))
Townet2 = mutate(Townet, Latitude = NA, Longitude = NA) %>%
  select(SampleDate, Survey, StationCode, MethodCode, Count,
         Secchi, Conductivity, WaterTemperature, 
         CommonName, Year, Tow, Depth, VolumeSampled, Latitude, Longitude)

FMWT3 = merge(FMWT3, unique(fishnamescrosswalk[,c(1,3)]))
FMWT4 = mutate(FMWT3, Tow = 1) %>%
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

DJFMP2 = merge(DJFMP, unique(fishnamescrosswalk[,c(1,4)]))
DJFMP3 = mutate(DJFMP2, Year = year(SampleDate), 
                Survey = "DJFMP") %>%
  left_join(stations, by = c("Survey", "StationCode")) %>%
  select(SampleDate, Survey, Year, StationCode, MethodCode, Count,
         Secchi, Conductivity, WaterTemperature, 
         CommonName, Year, Tow, Depth, VolumeSampled, Latitude, Longitude)

Allfishdata = rbind(Yolo2, EDSM3, Townet2, FMWT4, DJFMP3)

#total catch by species
AllfishdataSum = group_by(Allfishdata, SampleDate, Survey, StationCode, MethodCode, Secchi, Conductivity,
                          WaterTemperature, CommonName, Year, Tow, Depth, VolumeSampled, Latitude, Longitude) %>%
  summarize(totalCount = sum(Count))

write.csv(AllfishdataSum, "Allfishdata.csv", row.names = F)
