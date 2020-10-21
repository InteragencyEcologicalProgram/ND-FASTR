# FASTR exploratory Analyses
# Author: Laura Twardochleb
# Date created: 10/12/2020
# Purpose: ANOVAS/Regression explorations for FASTR

################# read in and clean data #######################################################################################################################

#clears the global environment (loaded packages, stored objects, etc.)
rm(list = ls())
library(tidyverse)
library(lubridate)
setwd("~/NDFA/2020/R_scripts")

load("~/NDFA/2020/R_scripts/FASTR_exploration.RData")


discrete<-read.csv("~/FASTR/WQ_OUTPUT_Discrete_Lab_formatted.csv", stringsAsFactors = FALSE)
distance<-read.csv("~/FASTR/NDFA_map.csv", stringsAsFactors = FALSE)
water_year<-read.csv("~/FASTR/FlowDatesDesignations.csv", stringsAsFactors = FALSE)
flow_magnitude<-read.csv("~/FASTR/flow_magnitude.csv", stringsAsFactors = FALSE, na.strings="")

#merge distance from source and discrete data

discrete2<-left_join(discrete, distance, by=c("StationCode"="ï..Station"))

#data cleaning in prep for analyses
#remove rows with only NA
discrete3<-discrete2[rowSums(is.na(discrete2)) != ncol(discrete2), ]

#remove sites we aren't including (not available for all years), create a column for year
remove<-c("DWT", "SRH", "SDI", "SHR", "SRV", "WWT")
discrete3$DateTime2<-discrete3$DateTime
discrete4<-filter(discrete3, !StationCode%in% remove)%>%separate(DateTime2, c("Month", "Day", "Year"), sep="/")%>%separate(Year, c("Year", "Time"), sep=" ")


#join with water year and flow_magnitude data sets
flow_magnitude$Year<-as.character(flow_magnitude$ï..Year)
discrete5<-left_join(discrete4, flow_magnitude)
water_year$Year<-as.character(water_year$Year)
discrete6<-left_join(discrete5, water_year)


#create factor for flow action period using PreFlowStart, PreFlowEnd, PostFlowStart, PostFlowEnd columns
discrete6$DateTime2<-as.Date(discrete6$DateTime, "%m/%d/%Y %H:%M")
discrete6$PreFlowStart<-as.Date(discrete6$PreFlowStart, "%m/%d/%Y")
discrete6$PreFlowEnd<-as.Date(discrete6$PreFlowEnd, "%m/%d/%Y")
discrete6$PostFlowStart<-as.Date(discrete6$PostFlowStart, "%m/%d/%Y")
discrete6$PostFlowEnd<-as.Date(discrete6$PostFlowEnd, "%m/%d/%Y")

discrete6$FlowPeriod<-if_else(discrete6$DateTime2<=discrete6$PreFlowEnd & discrete6$DateTime2>=discrete6$PreFlowStart, "Before", 
                             if_else(discrete6$DateTime2>=discrete6$PostFlowStart&discrete6$DateTime2<=discrete6$PostFlowEnd, "After", 
                                    if_else(discrete6$DateTime2>=discrete6$PreFlowEnd&discrete6$DateTime2<=discrete6$PostFlowStart, "During", discrete6$FlowPeriod)))

#all nas are before or after the monitoring period- so can remove
nas<-unique(discrete6$DateTime2[which(is.na(discrete6$FlowPeriod))])
#drop na values
discrete7<-discrete6[!is.na(discrete6$FlowPeriod),]

#subset to chla- discrete7 has cleaned nutrient data 
chla<-discrete7%>%filter(Analyte=="Chla")

#create different flow pulse types?


###################################### explore the data ###################################################################################################################
#plot flow metrics by flow pulse type
ggplot(data=chla, aes(x=FlowPulseType, y=Max.Daily.Ave.Net.Flow_cfs))+geom_point()
ggplot(data=chla, aes(x=FlowPulseType, y=Total.Average.Net.Volume.AF))+geom_point()
ggplot(data=chla, aes(x=FlowPulseType, y=NetFlowDays))+geom_point()

#plot chla across all sites by flow
plot(x=chla$Max.Daily.Ave.Net.Flow_cfs, y=chla$Result) #no obvious pattern but a little higher in middle of the range?
plot(x=chla$Total.Average.Net.Volume.AF, y=chla$Result) #no obvious pattern but a little higher in middle of the range?
plot(x=chla$NetFlowDays, y=chla$Result) #no obvious pattern but a little higher in middle of the range?

#plot chla by flow pulse type 
ggplot(data=chla, aes(x=FlowPulseType, y=Result))+geom_point() #highest in years with no flow pulse, lowest in years with construction
#plot chla by water year type
ggplot(data=chla, aes(x=WYType, y=Result))+geom_point() #no pattern

#plot chla at RVB by flow
plot(x=chla[chla$StationCode=="RVB",]$Max.Daily.Ave.Net.Flow_cfs, y=chla[chla$StationCode=="RVB",]$Result)
plot(x=chla[chla$StationCode=="RVB",]$Total.Average.Net.Volume.AF, y=chla[chla$StationCode=="RVB",]$Result) #seems to be higher at lower volume, or middle of the range?
plot(x=chla[chla$StationCode=="RVB",]$NetFlowDays, y=chla[chla$StationCode=="RVB",]$Result) #higher with fewer flow days

#plot chla in lower sac river (lower sites)
plot(x=chla[chla$Region=="LowerSac",]$Max.Daily.Ave.Net.Flow_cfs, y=chla[chla$Region=="LowerSac",]$Result)
plot(x=chla[chla$Region=="LowerSac",]$NetFlowDays, y=chla[chla$Region=="LowerSac",]$Result) #higher with fewer flow days

#plot chla in csc
plot(x=chla[chla$Region=="CacheSloughComplex",]$Max.Daily.Ave.Net.Flow_cfs, y=chla[chla$Region=="CacheSloughComplex",]$Result) #higher with higher flow
plot(x=chla[chla$Region=="CacheSloughComplex",]$NetFlowDays, y=chla[chla$Region=="CacheSloughComplex",]$Result) #higher with fewer flow days

#plot chla in loweryolo
plot(x=chla[chla$Region=="LowerYolo",]$Max.Daily.Ave.Net.Flow_cfs, y=chla[chla$Region=="LowerYolo",]$Result) #higher with higher flow
plot(x=chla[chla$Region=="LowerYolo",]$NetFlowDays, y=log(chla[chla$Region=="LowerYolo",]$Result) #higher with fewer flow days

#relationship between average volume and flow days
plot(x=chla$NetFlowDays, y=chla$Total.Average.Net.Volume.AF)
plot(x=chla$NetFlowDays, y=chla$Max.Daily.Ave.Net.Flow_cfs) #somewhat higher net flow for middle of the range with days

#plot chla by flow pulse type 
ggplot(data=chla, aes(x=FlowPulseType, y=Result))+geom_point() #highest in years with no flow pulse, lowest in years with construction
#plot chla by water year type
ggplot(data=chla, aes(x=WYType, y=Result))+geom_point() #no pattern


hist(chla$NetFlowDays)
hist(chla$Total.Average.Net.Volume.AF)
hist(chla$Max.Daily.Ave.Net.Flow_cfs)

#correlation of max ave daily net flow and net flow days
cor(chla$NetFlowDays, chla$Max.Daily.Ave.Net.Flow_cfs) #no strong correlation, can use both

#look at previous water year

#plot chla by managed, non-managed +, non-managed -: doesn't seem to be a good way to do this

#run ANOVA using regions on discrete chl-a, flow pulse type, and net flow days, max ave daily net flow

#three-factor anova with three-way interactions
chla4<-aov(log(chla$Result)~chla$FlowPulseType*chla$FlowPeriod*chla$Region)

summary(chla4)
plot(chla4)

lsmeans(chla4, list(pairwise~ FlowPulseType|Region|FlowPeriod)) #not enough power to compare all combinations


#three-factor anova with two-way interactions
#interactions b/w flow duration and other variables, so can't use ancova:re-run here
chla1<-aov(log(chla$Result)~chla$FlowPulseType*chla$FlowPeriod +chla$Region*chla$FlowPeriod+ chla$Region*chla$FlowPulseType)

summary(chla1)
plot(chla1)

lsmeans(chla1, list(pairwise~ FlowPulseType|Region|FlowPeriod)) 

######################## regressions using distance from source ###################################################################################################################################################
chla_reg<-lm(log(chla$Result)~I(chla$distance_between_km^2)+chla$FlowPeriod+chla$FlowPulseType+chla$distance_between_km*chla$FlowPeriod+chla$distance_between_km*chla$FlowPulseType+chla$NetFlowDays)
summary(chla_reg)
chla_reg[3]
#plot partial residuals
plot(x=I(chla$distance_between_km^2),y=chla_reg[2]$residuals)
step(chla_reg, direction = "both")

library(car)
crPlots(chla_reg)


chla_reg2<-lm(log(chla$Result)~chla$distance_between_km+chla$FlowPeriod+chla$FlowPulseType+chla$distance_between_km*chla$FlowPeriod+chla$distance_between_km*chla$FlowPulseType+chla$NetFlowDays)
summary(chla_reg2)
chla_reg[3]
#plot partial residuals
plot(x=chla$distance_between_km,y=chla_reg2[2]$residuals)
step(chla_reg, direction = "both")


save.image("FASTR_exploration.RData")
