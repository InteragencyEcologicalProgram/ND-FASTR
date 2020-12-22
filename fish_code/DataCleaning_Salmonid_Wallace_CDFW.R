library(tidyverse)
library(lubridate)
library(hms)

#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#
########## Importing / cleaning the data ##########
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#

#Download the data from the CDFW CalFish website: https://www.calfish.org/ProgramsData/ConservationandManagement/CentralValleyMonitoring/SacramentoValleyTributaryMonitoring/YoloandSutterBypasses-Monitoring/WallaceWeirandColusaBasinDrain.aspx and save to your working directory

#after downloading, I saved all the excel files as csv files for ease of importing

################ 2013-2014 Data ##################

names1314 <- c("StartDate", "StartTime", "StopDate", "StopTime", "HoursFished", "Flow", "Temp", "Turb", "Velo", "SalmonUnmarked", "SalmonMarked", "SteelheadUnmarked", "SteelheadMarked", "GreenSturgeon", "WhiteSturgeon", "Floy", "FloyColor", "FloyIDs", "Acoustic", "AcousticIDs", "Pit", "PitIDs", "Comments", "X")

df1314 <- read.csv("Data/WallaceWeir_Catch_data_2013-2014.csv", col.names = names1314, skip = 3)

#fixing columns
df1314$DO <- NA
df1314$X <- NULL

#fixing dates
df1314$StartDate <- mdy(df1314$StartDate)
df1314$StopDate <- mdy(df1314$StopDate)

#add flow action phase data
df1314$Phase <- ifelse(df1314$StartDate < as.Date('2013-10-03'), "Action", ifelse(df1314$StartDate > as.Date('2013-11-02'), "NonNDFA", "Post"))

#fixing times
df1314$StartTime <- format(strptime(df1314$StartTime, "%I:%M:%S %p"), "%H:%M")
df1314$StopTime <- format(strptime(df1314$StopTime, "%I:%M:%S %p"), "%H:%M")

############### 2014-2015 data ###################

names1415 <- c("StartDate", "StartTime", "StopDate", "StopTime", "HoursFished", "Flow", "Temp", "Turb", "Velo", "SalmonUnmarked", "SalmonMarked", "SteelheadUnmarked", "SteelheadMarked", "GreenSturgeon", "WhiteSturgeon", "Floy", "FloyColor", "FloyIDs", "Comments", "X", "X1", "X2", "X3", "X4")

df1415 <- read.csv("Data/WallaceWeir_Catch_data_2014-2015.csv", col.names = names1415, skip = 3)

#fixing columns
df1415$Acoustic <- NA
df1415$AcousticIDs <- NA
df1415$Pit <- NA
df1415$PitIDs <- NA
df1415$DO <- NA

df1415$X <- NULL
df1415$X1 <- NULL
df1415$X2 <- NULL
df1415$X3 <- NULL
df1415$X4 <- NULL

#fixing dates
df1415$StartDate <- mdy(df1415$StartDate)
df1415$StopDate <- mdy(df1415$StopDate)

#add flow action phase data
df1415$Phase <- ifelse(df1415$StartDate < as.Date('2014-09-24'), "Action", ifelse(df1415$StartDate > as.Date('2014-10-24'), "NonNDFA", "Post"))

################# 2015-2016 data #################

names1516 <- names1415

df1516 <- read.csv("Data/WallaceWeir_Catch_data_2015-2016.csv", col.names = names1516, skip = 3)

#fixing columns
df1516$Acoustic <- NA
df1516$AcousticIDs <- NA
df1516$Pit <- NA
df1516$PitIDs <- NA
df1516$DO <- NA

df1516$X <- NULL
df1516$X1 <- NULL
df1516$X2 <- NULL
df1516$X3 <- NULL
df1516$X4 <- NULL

#removing blank rows
df1516 <- df1516[-c(113:116), ]

#fixing dates
df1516$StartDate <- mdy(df1516$StartDate)
df1516$StopDate <- mdy(df1516$StopDate)

#add flow action phase data
df1516$Phase <- ifelse(df1516$StartDate < as.Date('2015-10-02'), "Action", ifelse(df1516$StartDate > as.Date('2015-11-01'), "NonNDFA", "Post"))

#fixing times
df1516$StartTime <- format(strptime(df1516$StartTime, "%I:%M:%S %p"), "%H:%M")
df1516$StopTime <- format(strptime(df1516$StopTime, "%I:%M:%S %p"), "%H:%M")

################## 2016-2017 data ################

names1617 <- names1415

df1617 <- read.csv("Data/WallaceWeir_catch_data_2016-2017.csv", col.names = names1617, skip = 3)

#fixing columns
df1617$Acoustic <- NA
df1617$AcousticIDs <- NA
df1617$Pit <- NA
df1617$PitIDs <- NA
df1617$DO <- NA

df1617$X <- NULL
df1617$X1 <- NULL
df1617$X2 <- NULL
df1617$X3 <- NULL
df1617$X4 <- NULL

#removing blank rows
df1617 <- df1617[-c(29:30), ]

#fixing dates
df1617$StartDate <- mdy(df1617$StartDate)
df1617$StopDate <- mdy(df1617$StopDate)

#add flow action phase data
df1617$Phase <- ifelse(df1617$StartDate < as.Date('2016-08-02'), "Action", ifelse(df1617$StartDate > as.Date('2016-09-01'), "NonNDFA", "Post"))

#fixing times
df1617$StartTime <- format(strptime(df1617$StartTime, "%I:%M:%S %p"), "%H:%M")
df1617$StopTime <- format(strptime(df1617$StopTime, "%I:%M:%S %p"), "%H:%M")

################ 2017-2018 data ##################

names1718 <- c("StartDate", "StartTime", "StopDate", "StopTime", "HoursFished", "Flow", "Temp", "Turb", "SalmonUnmarked", "SalmonMarked", "SteelheadUnmarked", "SteelheadMarked", "GreenSturgeon", "WhiteSturgeon", "Floy", "FloyColor", "FloyIDs", "Acoustic", "AcousticIDs", "Pit", "PitIDs", "Comments", "X")

df1718 <- read.csv("Data/ColusaBasin_Catch_Data_2017-2018.csv", col.names = names1718, skip = 3)

#fixing columns
df1718$DO <- NA
df1718$Velo <- NA
df1718$X <- NULL

#removing weird row
df1718 <- df1718[-c(141), ]

#fix weird value
df1718[226,4] <- as.character("8:30")

#fixing dates
df1718$StartDate <- mdy(df1718$StartDate)
df1718$StopDate <- mdy(df1718$StopDate)

#add flow action phase data
df1718$Phase <- ifelse(df1718$StartDate < as.Date('2017-09-19'), "Action", ifelse(df1718$StartDate > as.Date('2017-10-19'), "NonNDFA", "Post"))

################ 2018-2019 data ##################

names1819 <- c("StartDate", "StartTime", "StopDate", "StopTime", "HoursFished", "Flow", "Temp", "Turb", "SalmonUnmarked", "SalmonMarked", "SteelheadUnmarked", "SteelheadMarked", "GreenSturgeon", "WhiteSturgeon", "Floy", "FloyColor", "FloyIDs", "Acoustic", "AcousticIDs", "Pit", "PitIDs", "Comments")

df1819 <- read.csv("Data/WallaceWeir_Catch_Data_2018-2019.csv", col.names = names1819, skip = 3)

#fixing columns
df1819$DO <- NA
df1819$Velo <- NA

#fixing dates
df1819$StartDate <- mdy(df1819$StartDate)
df1819$StopDate <- mdy(df1819$StopDate)
df1819[31,3] <- as.Date("2018-09-26")
df1819[32,3] <- as.Date("2018-09-27")
df1819[33,3] <- as.Date("2018-09-28")
df1819[39,3] <- as.Date("2018-10-04")

#add flow action phase data
df1819$Phase <- ifelse(df1819$StartDate < as.Date('2018-09-27'), "Action", ifelse(df1819$StartDate > as.Date('2018-10-27'), "NonNDFA", "Post"))

################# 2019-2020 data #################

names1920 <- c("StartDate", "StartTime", "StopDate", "StopTime", "HoursFished", "Flow", "Temp", "Turb", "DO", "Velo", "SalmonUnmarked", "SalmonMarked", "SteelheadUnmarked", "SteelheadMarked", "GreenSturgeon", "WhiteSturgeon", "Floy", "FloyColor", "FloyIDs", "Acoustic", "AcousticIDs", "Pit", "PitIDs", "Comments", "X")

df1920 <- read.csv("Data/WallaceWeir_Catch_Data_2019-2020.csv", col.names = names1920, skip = 3)

#fixing columns
df1920$X <- NULL

#fixing dates
df1920$StartDate <- mdy(df1920$StartDate)
df1920$StopDate <- mdy(df1920$StopDate)
df1920[41,3] <- as.Date("2019-09-22")
df1920[42,3] <- as.Date("2019-09-23")

#add flow action phase data
df1920$Phase <- ifelse(df1920$StartDate < as.Date('2019-09-22'), "Action", ifelse(df1920$StartDate > as.Date('2019-11-30'), "NonNDFA", "Post"))



#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#
########### Binding the datasets ##################
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#

total <- rbind(df1314, df1415, df1516, df1617, df1718, df1819, df1920)

############ fixing the times #####################

#fixing StartTime
total$StartTime <- as.character(total$StartTime)
total$StartTime[total$StartTime == '-' | total$StartTime == 'Beach seine effort' | total$StartTime == "" | total$StartTime == "*" | total$StartTime == "**" | total$StartTime == "***" | total$StartTime == "n/a"] <- "0:00"
total$StartTime[is.na(total$StartTime)] <- "0:00"

#adding new StartDateTime column
total <- unite(total, StartDateTime, StartDate, StartTime, sep = " ")
total$StartDateTime <- ymd_hm(total$StartDateTime)

#fixing StopTime
total$StopTime <- as.character(total$StopTime)
total$StopTime[total$StopTime == '-' | total$StopTime == 'Beach seine effort' | total$StopTime == "" | total$StopTime == "*" | total$StopTime == "**" | total$StopTime == "***" | total$StopTime == "n/a" | total$StopTime == "N/A"] <- "0:00"
total$StopTime[is.na(total$StopTime)] <- "0:00"

#adding new StopDateTime column
total <- unite(total, StopDateTime, StopDate, StopTime, sep = " ")
total$StopDateTime <- ymd_hm(total$StopDateTime)

#changing HoursFished to numeric
total$HoursFished[total$HoursFished == '-' | total$HoursFished == 'Beach seine effort' | total$HoursFished == "" | total$HoursFished == "*" | total$HoursFished == "**" | total$HoursFished == "***" | total$HoursFished == "n/a" | total$HoursFished == "N/A"] <- "0"
total$HoursFished[is.na(total$HoursFished)] <- "0"
total$HoursFished <- as.numeric(total$HoursFished)


# change fish catch to numeric
total$SalmonUnmarked <- as.double(total$SalmonUnmarked)
total$SalmonMarked <- as.double(total$SalmonMarked)
total$SteelheadUnmarked <- as.double(total$SteelheadUnmarked)
total$SteelheadMarked <- as.double(total$SteelheadMarked)

total <- as_tibble(total)

#use gather to change data from wide to long
total.long <- gather(total, Species, Catch, SalmonMarked, SalmonUnmarked, SteelheadMarked, SteelheadUnmarked)
total.long$Catch[is.na(total.long$Catch)] <- "0"
glimpse(total.long)

#fix some things
total.long$Species <- as.factor(total.long$Species)
total.long$Phase <- as.factor(total.long$Phase)

#add CPUE
total.long$CPUE <- total.long$Catch/total.long$HoursFished

write.csv(total.long, file ="Salmonid_Wallace_CDFW.csv")