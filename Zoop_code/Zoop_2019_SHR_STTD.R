#2019 SHR and STTD zoop for contaminants manuscript

library(tidyverse)

#zooplankton data from Mallory and Nicole (biomass)
zoopNDFAv2.1<-read.csv("Zoop_code/zoop_NDFA_v2_copy.csv", stringsAsFactors = FALSE)
#data organization and cleanup

zoopNDFAv2.1$StationCode <- factor(zoopNDFAv2.1$StationCode)

zoopNDFAv2.1 <-  zoopNDFAv2.1%>% filter(Year=="2019")
zoopNDFAv3.1 <- zoopNDFAv2.1 
zoopNDFAv3.1$Year <- as.character(zoopNDFAv3.1$Year)


zoopNDFAv3.1 <- zoopNDFAv3.1 %>% filter(StationCode!="RMB")
zoopNDFAv3.1 <- zoopNDFAv3.1 %>% filter(StationCode!="RD22")
zoopNDFAv3.1 <- zoopNDFAv3.1 %>% filter(StationCode!="RCS")
zoopNDFAv3.1 <- zoopNDFAv3.1 %>% filter(StationCode!="I80")
zoopNDFAv3.1 <- zoopNDFAv3.1 %>% filter(StationCode!="BL5")
zoopNDFAv3.1 <- zoopNDFAv3.1 %>% filter(StationCode!="RYI")
zoopNDFAv3.1 <- zoopNDFAv3.1 %>% filter(StationCode!="LIB")
zoopNDFAv3.1 <- zoopNDFAv3.1 %>% filter(StationCode!="RVB")
zoopNDFAv3.1 <- zoopNDFAv3.1 %>% filter(StationCode!="LIS")

zoopNDFA4.1 <- zoopNDFAv3.1[,c("Year","Date","StationCode","Classification","Organism","CPUEZoop")] #new table with relevant columns

zoopNDFA4.1$StationCode <- as.character(zoopNDFA4.1$StationCode)
#write.csv(zoopNDFA4.1, file = "C:/Users/jadams/Documents/DES docs and forms/NDFA/Manuscript/ND-FASTR/Zoop_code/zoop_2019_SHR_STTD.csv")
