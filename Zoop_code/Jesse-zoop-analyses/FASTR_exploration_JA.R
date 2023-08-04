#clear global environment
rm(list=ls(all=TRUE))

#Laura's exploratory code with zooplankton data
library(tidyverse)
library(lubridate)
library(cowplot)
library(lme4)
library(psych)
library(nlme)
library(lattice)
library(lmerTest)
library(visreg)
library(emmeans)
library(multcomp)
library(multcompView)

#setwd("C:/Users/jadams/Documents/DES docs and forms/NDFA/zoop/NEW Zoop Biomass/Zoop Biomass")

#zooplankton data from Mallory and Nicole (biomass)
zoopNDFAv2<-read.csv("Zoop_code/zoop_NDFA_v2.csv", stringsAsFactors = FALSE)

#change to factors and organize sample period levels
zoopNDFAv2$SamplePeriod <- factor(zoopNDFAv2$SamplePeriod,levels = c("Before","During","After"))
zoopNDFAv2$StationCode <- factor(zoopNDFAv2$StationCode)

#create regions for station groups
zoopNDFAv2$Region <- fct_collapse(zoopNDFAv2$StationCode,UpperYolo=c("RD22","I80"),
                                  MiddleSacRiver=c("SHR"),
                                  LowerYolo=c("LIS","STTD"),
                                  ColusaDrainRCS=c("RMB","RCS"),
                                  CacheSloughComplex=c("BL5","LIB"),
                                  LowerSac=c("RYI","RVB"))

#organize regions from north to south for facet plotting
zoopNDFAv2$Region <- factor(zoopNDFAv2$Region,levels = c("ColusaDrainRCS","UpperYolo","LowerYolo","CacheSloughComplex","MiddleSacRiver","LowerSac"))


#create regions for station groups
zoopNDFAv2$Regions2 <- fct_collapse(zoopNDFAv2$StationCode,Upstream=c("RCS","RD22","I80","LIS","STTD"),
                                                                    Downstream=c("BL5","LIB","RYI","RVB"))

#organize regions from north to south for facet plotting
zoopNDFAv2$Regions2 <- factor(zoopNDFAv2$Regions2,levels = c("Upstream","Downstream"))

#remove macrozooplankton (incomplete dataset and not targeted by our gear)
zoopNDFAv2 <- zoopNDFAv2%>%filter(Classification!="Macrozooplankton")
#read in data with additional flow parameters and create new data table joined with zoop data
flow_magnitude<-read.csv("flow_magnitude.csv", stringsAsFactors = FALSE, na.strings="",header = TRUE)
zoopNDFAv3 <- zoopNDFAv2 
zoopNDFAv3$Year <- as.character(zoopNDFAv3$Year)
flow_magnitude$Year<-as.character(flow_magnitude$ï..Year)
zoopNDFAv3<-left_join(flow_magnitude,zoopNDFAv3)
zoopNDFAv3 <-  zoopNDFAv3%>% filter(Year!="2011")
zoopNDFAv3 <-  zoopNDFAv3%>% filter(Year!="2012")
zoopNDFAv3 <- zoopNDFAv3 %>% filter(StationCode!="SHR")
zoopNDFAv3 <- zoopNDFAv3 %>% filter(StationCode!="RMB")

#write.csv(zoopNDFAv3, file = "C:/Users/jadams/Documents/DES docs and forms/NDFA/zoop/zoopNDFAv3.csv")


#######plot zoop across all sites by flow (these are based on Laura's prelim chlorophyl plots)##############

#note: 725 missing values (na) for BPUE are removed because there are no carbon weight estimates
nfdcz <- ggplot(data=zoopNDFAv3, aes(x=NetFlowDays, y=CPUEZoop))+geom_point() 
nfdb <- ggplot(data=zoopNDFAv3, aes(x=NetFlowDays, y=BPUE))+geom_point() 
nfdcz
nfdb
#plot zoop by flow pulse type 
fptcz <- ggplot(data=zoopNDFAv3, aes(x=FlowPulseType, y=CPUEZoop))+geom_point() 
fptb <- ggplot(data=zoopNDFAv3, aes(x=FlowPulseType, y=BPUE))+geom_point() 
fptcz
fptb
#plot zoop by water year type
wytcz <- ggplot(data=zoopNDFAv3, aes(x=WYType, y=CPUEZoop))+geom_point() +scale_x_discrete(limits=c("C","D","BN","W"))
wytb <- ggplot(data=zoopNDFAv3, aes(x=WYType, y=BPUE))+geom_point()+scale_x_discrete(limits=c("C","D","BN","W"))
wytcz
wytb

#view na's in BPUE (mostly cladocerans (Ilyocryptus and Macrothrix) and ostracods)
na.vals<-zoopNDFAv3[which(is.na(zoopNDFAv2$BPUE)),]

#view number of occurances of each net flow days value
table(zoopNDFAv3$NetFlowDays)

#plot all zoop by flow pulse type 

#CPUE histogram
zoopNDFAv3%>% ggplot(aes(log(CPUEZoop+1)))+geom_histogram() 

#flowpulsetype column
zoopNDFAv3%>% ggplot(aes(x=FlowPulseType, y=log(CPUEZoop+1)))+geom_col() 

#flowpulsetype jittered and boxplots with sampleperiod as an x axis
zoopNDFAv3 %>% ggplot(aes(x=FlowPulseType, y=log(CPUEZoop+1), colour=SamplePeriod))+geom_point(position = position_jitter())+scale_fill_viridis_c(alpha = 1, begin = 0.2, end = 1,direction = 1, option = "A", aesthetics = "fill")+scale_y_continuous(name=expression(paste("CPUE")), limits=c(),expand=c(0,0))

zoopNDFAv3%>%ggplot(aes(x=FlowPulseType, y=log(BPUE+1), colour=SamplePeriod))+geom_point(position = position_jitter())+scale_fill_viridis_c(alpha = 1, begin = 0.2, end = 1,direction = 1, option = "A", aesthetics = "fill") 

#####Boxplots of logCPUE and logBPUE for flowpulse type and sample period######

#BPUE
zoopNDFAv3 %>% ggplot(aes(x=interaction(FlowPulseType,SamplePeriod), y=log(BPUE+1)))+geom_boxplot(aes(fill=FlowPulseType),notch = TRUE)+scale_y_continuous(name=expression(paste("logBPUE")), limits=c(),expand=c(0,0))+
    scale_x_discrete(limits=c("NF.Before","CA.Before","MA-Ag.Before","MA-SR.Before", "NF.During","CA.During","MA-Ag.During","MA-SR.During","NF.After","CA.After","MA-Ag.After","MA-SR.After"))+
    annotate("rect", xmin = 4.5, xmax =8.5, ymin = 0, ymax = 15, alpha = .15, fill = "grey50")#+scale_fill_viridis_c(alpha = 1, begin = 0.2, end = 1,direction = 1, option = "A", aesthetics = "fill")

#CPUE
zoopNDFAv3%>% ggplot(aes(x=interaction(FlowPulseType,SamplePeriod), y=log(CPUEZoop+1)))+geom_boxplot(aes(fill=FlowPulseType),notch = TRUE)+scale_y_continuous(name=expression(paste("logCPUE")), limits=c(),expand=c(0,0))+
  scale_x_discrete(limits=c("NF.Before","CA.Before","MA-Ag.Before","MA-SR.Before", "NF.During","CA.During","MA-Ag.During","MA-SR.During","NF.After","CA.After","MA-Ag.After","MA-SR.After"))+
  annotate("rect", xmin = 4.5, xmax =8.5, ymin = 0, ymax = 15, alpha = .15, fill = "grey50")


#facet by region for flowpulsetype~sampleperiod
zoopNDFAv3%>% filter(Regions2!="ColusaDrainRCS") %>% ggplot(aes(x=interaction(FlowPulseType,SamplePeriod), y=log(CPUEZoop+1)))+geom_boxplot(aes(fill=FlowPulseType),notch = FALSE)+scale_y_continuous(name=expression(paste("logCPUE")), limits=c(),expand=c(0,0))+
  scale_x_discrete(limits=c("CA.Before","CA.During","CA.After","MA-Ag.Before","MA-Ag.During","MA-Ag.After","MA-SR.Before","MA-SR.During","MA-SR.After","NF.Before","NF.During","NF.After"))+
        facet_wrap(~ Regions2,ncol = 1)


#Boxplots of logCPUE and logBPUE for WY type and sample period

#BPUE
zoopNDFAv3 %>% ggplot(aes(x=interaction(WYType,SamplePeriod), y=log(BPUE+1)))+geom_boxplot(aes(fill=FlowPulseType),notch = TRUE)+scale_y_continuous(name=expression(paste("logBPUE")), limits=c(),expand=c(0,0))+
  scale_x_discrete(limits=c("C.Before","D.Before","BN.Before","W.Before", "C.During","D.During","BN.During","W.During","C.After","D.After","BN.After","W.After"))+
  annotate("rect", xmin = 4.5, xmax =8.5, ymin = 0, ymax = 15, alpha = .15, fill = "grey50")#+scale_fill_viridis_c(alpha = 1, begin = 0.2, end = 1,direction = 1, option = "A", aesthetics = "fill")

#CPUE
zoopNDFAv3 %>% ggplot(aes(x=interaction(WYType,SamplePeriod), y=log(CPUEZoop+1)))+geom_boxplot(aes(fill=FlowPulseType),notch = TRUE)+scale_y_continuous(name=expression(paste("logCPUE")), limits=c(),expand=c(0,0))+
  scale_x_discrete(limits=c("C.Before","D.Before","BN.Before","W.Before", "C.During","D.During","BN.During","W.During","C.After","D.After","BN.After","W.After"))+
  annotate("rect", xmin = 4.5, xmax =8.5, ymin = 0, ymax = 15, alpha = .15, fill = "grey50")#+scale_fill_viridis_c(alpha = 1, begin = 0.2, end = 1,direction = 1, option = "A", aesthetics = "fill")

#CPUE no fill
zoopNDFAv3 %>% ggplot(aes(x=interaction(WYType,SamplePeriod), y=log(CPUEZoop+1)))+geom_boxplot(aes(),notch = TRUE)+scale_y_continuous(name=expression(paste("logCPUE")), limits=c(),expand=c(0,0))+
  scale_x_discrete(limits=c("C.Before","D.Before","BN.Before","W.Before", "C.During","D.During","BN.During","W.During","C.After","D.After","BN.After","W.After"))+
  annotate("rect", xmin = 4.5, xmax =8.5, ymin = 0, ymax = 15, alpha = .15, fill = "grey50")

#Year~sampleperiod CPUE with flowpulsetype
zoopNDFAv3%>% ggplot(aes(x=interaction(Year,SamplePeriod), y=log(CPUEZoop+1)))+geom_boxplot(aes(fill=FlowPulseType),notch = TRUE)+scale_y_continuous(name=expression(paste("logCPUE")), limits=c(),expand=c(0,0))+
  scale_x_discrete(limits=c("2014.Before","2015.Before","2013.Before","2012.Before","2016.Before","2018.Before","2011.Before","2017.Before","2019.Before","2014.During","2015.During","2013.During","2012.During","2016.During","2018.During","2011.During","2017.During","2019.During","2014.After","2015.After","2013.After","2012.After","2016.After","2018.After","2011.After","2017.After","2019.After"))+
  annotate("rect", xmin = 9.5, xmax =18.5, ymin = 0, ymax = 15, alpha = .15, fill = "grey50")+
  annotate("text",x=1,y=5,label="C")+
  annotate("text",x=2,y=4,label="C")+
  annotate("text",x=3,y=5.3,label="D")+
  annotate("text",x=4,y=5.2,label="BN")+
  annotate("text",x=5,y=8,label="BN")+
  annotate("text",x=6,y=4,label="BN")+
  annotate("text",x=7,y=8.5,label="W")+
  annotate("text",x=8,y=5,label="W")+
  annotate("text",x=9,y=4.5,label="W")+
  annotate("text",x=10,y=5,label="C")+
  annotate("text",x=11,y=4.5,label="C")+
  annotate("text",x=12,y=6.6,label="D")+
  annotate("text",x=13,y=5.2,label="BN")+
  annotate("text",x=14,y=6.5,label="BN")+
  annotate("text",x=15,y=4.5,label="BN")+
  annotate("text",x=16,y=5.5,label="W")+
  annotate("text",x=17,y=4.5,label="W")+
  annotate("text",x=18,y=3.5,label="W")+
  annotate("text",x=19,y=5,label="C")+
  annotate("text",x=20,y=4.5,label="C")+
  annotate("text",x=21,y=4,label="D")+
  annotate("text",x=22,y=7,label="BN")+
  annotate("text",x=23,y=6,label="BN")+
  annotate("text",x=24,y=3.8,label="BN")+
  annotate("text",x=25,y=4.2,label="W")+
  annotate("text",x=26,y=3.5,label="W")+
  annotate("text",x=27,y=3.3,label="W")


#try different flow parameter (just for exploration but doesn't show any significant difference from flowpulsetype)

#netflowdays logCPUE with flowpulse type fill
zoopNDFAv3$NFD2 <- as.factor(zoopNDFAv3$NetFlowDays)

#facet wrap by sample period
zoopNDFAv3 %>%  ggplot(aes(x=NFD2, y=log(CPUEZoop+1)))+geom_boxplot(aes(fill=FlowPulseType),notch = TRUE)+scale_y_continuous(name=expression(paste("logCPUE")), limits=c(),expand=c(0,0))+ facet_wrap(~ SamplePeriod,ncol = 1)

#individual sample period plots
zoopNDFAv3 %>% filter(SamplePeriod=="Before") %>%  ggplot(aes(x=NFD2, y=log(CPUEZoop+1)))+geom_boxplot(aes(fill=FlowPulseType),notch = TRUE)+scale_y_continuous(name=expression(paste("logCPUE")), limits=c(),expand=c(0,0))

zoopNDFAv3 %>% filter(SamplePeriod=="During") %>%  ggplot(aes(x=NFD2, y=log(CPUEZoop+1)))+geom_boxplot(aes(fill=FlowPulseType),notch = TRUE)+scale_y_continuous(name=expression(paste("logCPUE")), limits=c(),expand=c(0,0))

zoopNDFAv3 %>% filter(SamplePeriod=="After") %>%  ggplot(aes(x=NFD2, y=log(CPUEZoop+1)))+geom_boxplot(aes(fill=FlowPulseType),notch = TRUE)+scale_y_continuous(name=expression(paste("logCPUE")), limits=c(),expand=c(0,0))

#max daily avg net flow, logCPUE with flowpulsetype fill
zoopNDFAv3$maxDANF <- as.factor(zoopNDFAv3$Max.Daily.Ave.Net.Flow_cfs)

#facet wrap by sample period
zoopNDFAv3 %>%  ggplot(aes(x=maxDANF, y=log(CPUEZoop+1)))+geom_boxplot(aes(fill=FlowPulseType),notch = TRUE)+scale_y_continuous(name=expression(paste("logCPUE")), limits=c(),expand=c(0,0))+ facet_wrap(~ SamplePeriod,ncol = 1)

#individual sample period plots
zoopNDFAv3 %>% filter(SamplePeriod=="Before") %>%  ggplot(aes(x=maxDANF, y=log(CPUEZoop+1)))+geom_boxplot(aes(fill=FlowPulseType),notch = TRUE)+scale_y_continuous(name=expression(paste("logCPUE")), limits=c(),expand=c(0,0))

zoopNDFAv3 %>% filter(SamplePeriod=="During") %>%  ggplot(aes(x=maxDANF, y=log(CPUEZoop+1)))+geom_boxplot(aes(fill=FlowPulseType),notch = TRUE)+scale_y_continuous(name=expression(paste("logCPUE")), limits=c(),expand=c(0,0))

zoopNDFAv3 %>% filter(SamplePeriod=="After") %>%  ggplot(aes(x=maxDANF, y=log(CPUEZoop+1)))+geom_boxplot(aes(fill=FlowPulseType),notch = TRUE)+scale_y_continuous(name=expression(paste("logCPUE")), limits=c(),expand=c(0,0))


#total average net volume, logCPUE with flowpulsetype fill
zoopNDFAv3$TNV_AF <- as.factor(zoopNDFAv3$Total.Average.Net.Volume.AF)

zoopNDFAv3 %>%  ggplot(aes(x=TNV_AF, y=log(CPUEZoop+1)))+geom_boxplot(aes(fill=FlowPulseType),notch = TRUE)+scale_y_continuous(name=expression(paste("logCPUE")), limits=c(),expand=c(0,0))+ facet_wrap(~ SamplePeriod,ncol = 1)


zoopNDFAv3 %>%filter(SamplePeriod=="After") %>%  ggplot(aes(x=TNV_AF, y=log(CPUEZoop+1)))+geom_boxplot(aes(fill=FlowPulseType),notch = TRUE)+scale_y_continuous(name=expression(paste("logCPUE")), limits=c(),expand=c(0,0))


######Important species######

#CPUE
#plots for individual taxonomic groups
a <- zoopNDFAv3 %>% filter(Species=="forbesi") %>% filter(Regions2!="na") %>% 
  ggplot(aes(x=interaction(FlowPulseType,SamplePeriod), y=log(CPUEZoop+1)))+geom_boxplot(aes(fill=FlowPulseType),notch = FALSE)+scale_y_continuous(name=expression(paste("P.forbesi CPUE")), limits=c(),expand=c(0,0))+
  scale_x_discrete(limits=c("NF.Before","CA.Before","MA-Ag.Before","MA-SR.Before", "NF.During","CA.During","MA-Ag.During","MA-SR.During","NF.After","CA.After","MA-Ag.After","MA-SR.After"))+
  annotate("rect", xmin = 4.5, xmax =8.5, ymin = 0, ymax = 15, alpha = .15, fill = "grey50")+
  facet_wrap(~ Regions2,ncol = 1)
a

b <- zoopNDFAv3 %>% filter(Species=="doerrii") %>%  filter(Regions2!="na") %>%
  ggplot(aes(x=interaction(FlowPulseType,SamplePeriod), y=log(CPUEZoop+1)))+geom_boxplot(aes(fill=FlowPulseType),notch = FALSE)+scale_y_continuous(name=expression(paste("S.doerrii CPUE")), limits=c(),expand=c(0,0))+
  scale_x_discrete(limits=c("NF.Before","CA.Before","MA-Ag.Before","MA-SR.Before", "NF.During","CA.During","MA-Ag.During","MA-SR.During","NF.After","CA.After","MA-Ag.After","MA-SR.After"))+
  annotate("rect", xmin = 4.5, xmax =8.5, ymin = 0, ymax = 15, alpha = .15, fill = "grey50")+
  facet_wrap(~ Regions2,ncol = 1)
b

c <- zoopNDFAv3 %>% filter(Classification=="Calanoids") %>%  filter(Regions2!="na") %>%
  ggplot(aes(x=interaction(FlowPulseType,SamplePeriod), y=log(CPUEZoop+1)))+geom_boxplot(aes(fill=FlowPulseType),notch = FALSE)+scale_y_continuous(name=expression(paste("All Calanoids CPUE")), limits=c(),expand=c(0,0))+
  scale_x_discrete(limits=c("NF.Before","CA.Before","MA-Ag.Before","MA-SR.Before", "NF.During","CA.During","MA-Ag.During","MA-SR.During","NF.After","CA.After","MA-Ag.After","MA-SR.After"))+
  annotate("rect", xmin = 4.5, xmax =8.5, ymin = 0, ymax = 15, alpha = .15, fill = "grey50")+
  facet_wrap(~ Regions2,ncol = 1)
c

d <- zoopNDFAv3 %>% filter(Classification=="Cyclopoids") %>%  filter(Regions2!="na") %>%
  ggplot(aes(x=interaction(FlowPulseType,SamplePeriod), y=log(CPUEZoop+1)))+geom_boxplot(aes(fill=FlowPulseType),notch = FALSE)+scale_y_continuous(name=expression(paste("All Cyclopoids CPUE")), limits=c(),expand=c(0,0))+
  scale_x_discrete(limits=c("NF.Before","CA.Before","MA-Ag.Before","MA-SR.Before", "NF.During","CA.During","MA-Ag.During","MA-SR.During","NF.After","CA.After","MA-Ag.After","MA-SR.After"))+
  annotate("rect", xmin = 4.5, xmax =8.5, ymin = 0, ymax = 15, alpha = .15, fill = "grey50")+
  facet_wrap(~ Regions2,ncol = 1)
d

e <- zoopNDFAv3 %>% filter(Genus=="Limnoithona") %>%  filter(Regions2!="na") %>%
  ggplot(aes(x=interaction(FlowPulseType,SamplePeriod), y=log(CPUEZoop+1)))+geom_boxplot(aes(fill=FlowPulseType),notch = FALSE)+scale_y_continuous(name=expression(paste("Limnoithona spp CPUE")), limits=c(),expand=c(0,0))+
  scale_x_discrete(limits=c("NF.Before","CA.Before","MA-Ag.Before","MA-SR.Before", "NF.During","CA.During","MA-Ag.During","MA-SR.During","NF.After","CA.After","MA-Ag.After","MA-SR.After"))+
  annotate("rect", xmin = 4.5, xmax =8.5, ymin = 0, ymax = 15, alpha = .15, fill = "grey50")+
  facet_wrap(~ Regions2,ncol = 1)
e

f <- zoopNDFAv3 %>% filter(Taxlife=="Copepoda nauplius") %>%  filter(Regions2!="na") %>%
  ggplot(aes(x=interaction(FlowPulseType,SamplePeriod), y=log(CPUEZoop+1)))+geom_boxplot(aes(fill=FlowPulseType),notch = FALSE)+scale_y_continuous(name=expression(paste("Copepod nauplii CPUE")), limits=c(),expand=c(0,0))+
  scale_x_discrete(limits=c("NF.Before","CA.Before","MA-Ag.Before","MA-SR.Before", "NF.During","CA.During","MA-Ag.During","MA-SR.During","NF.After","CA.After","MA-Ag.After","MA-SR.After"))+
  annotate("rect", xmin = 4.5, xmax =8.5, ymin = 0, ymax = 15, alpha = .15, fill = "grey50")+
  facet_wrap(~ Regions2,ncol = 1)
f

g <- zoopNDFAv3 %>% filter(Classification=="Harpacticoids") %>%  filter(Regions2!="na") %>%
  ggplot(aes(x=interaction(FlowPulseType,SamplePeriod), y=log(CPUEZoop+1)))+geom_boxplot(aes(fill=FlowPulseType),notch = FALSE)+scale_y_continuous(name=expression(paste("Harpacticoids CPUE")), limits=c(),expand=c(0,0))+
  scale_x_discrete(limits=c("NF.Before","CA.Before","MA-Ag.Before","MA-SR.Before", "NF.During","CA.During","MA-Ag.During","MA-SR.During","NF.After","CA.After","MA-Ag.After","MA-SR.After"))+
  annotate("rect", xmin = 4.5, xmax =8.5, ymin = 0, ymax = 15, alpha = .15, fill = "grey50")+
  facet_wrap(~ Regions2,ncol = 1)
g

h <- zoopNDFAv3 %>% filter(Classification=="Cladocera") %>%  filter(Regions2!="na") %>%
  ggplot(aes(x=interaction(FlowPulseType,SamplePeriod), y=log(CPUEZoop+1)))+geom_boxplot(aes(fill=FlowPulseType),notch = FALSE)+scale_y_continuous(name=expression(paste("Cladocera CPUE")), limits=c(),expand=c(0,0))+
  scale_x_discrete(limits=c("NF.Before","CA.Before","MA-Ag.Before","MA-SR.Before", "NF.During","CA.During","MA-Ag.During","MA-SR.During","NF.After","CA.After","MA-Ag.After","MA-SR.After"))+
  annotate("rect", xmin = 4.5, xmax =8.5, ymin = 0, ymax = 15, alpha = .15, fill = "grey50")+
  facet_wrap(~ Regions2,ncol = 1)
h

i <- zoopNDFAv3 %>% filter(Organism=="Rotifers") %>%  filter(Regions2!="na") %>%
  ggplot(aes(x=interaction(FlowPulseType,SamplePeriod), y=log(CPUEZoop+1)))+geom_boxplot(aes(fill=FlowPulseType),notch = FALSE)+scale_y_continuous(name=expression(paste("Rotifers CPUE")), limits=c(),expand=c(0,0))+
  scale_x_discrete(limits=c("NF.Before","CA.Before","MA-Ag.Before","MA-SR.Before", "NF.During","CA.During","MA-Ag.During","MA-SR.During","NF.After","CA.After","MA-Ag.After","MA-SR.After"))+
  annotate("rect", xmin = 4.5, xmax =8.5, ymin = 0, ymax = 15, alpha = .15, fill = "grey50")+
  facet_wrap(~ Regions2,ncol = 1)
i

j <- zoopNDFAv3 %>% filter(Organism=="Ostracods") %>%  filter(Regions2!="na") %>%
  ggplot(aes(x=interaction(FlowPulseType,SamplePeriod), y=log(CPUEZoop+1)))+geom_boxplot(aes(fill=FlowPulseType),notch = FALSE)+scale_y_continuous(name=expression(paste("Ostracods CPUE")), limits=c(),expand=c(0,0))+
  scale_x_discrete(limits=c("NF.Before","CA.Before","MA-Ag.Before","MA-SR.Before", "NF.During","CA.During","MA-Ag.During","MA-SR.During","NF.After","CA.After","MA-Ag.After","MA-SR.After"))+
  annotate("rect", xmin = 4.5, xmax =8.5, ymin = 0, ymax = 15, alpha = .15, fill = "grey50")+
  facet_wrap(~ Regions2,ncol = 1)
j

#use cowplot to align stacked plots
aligned2 <- align_plots(a,b,c,d,e,f,g,h,i,j,align = "v")
plot_grid(a,b,c,d,e,f,g,h,i,j,ncol = 5,align = "v")

##########ANOVA lme4#############

zoopNDFA4 <- zoopNDFAv3[,c("Year","Date","SamplePeriod","Regions2","StationCode","CPUEZoop")] #new table with relevant columns
zoopNDFA4$SamplePeriod <- as.character(zoopNDFA4$SamplePeriod)
zoopNDFA4$Regions2 <- as.character(zoopNDFA4$Regions2)
zoopNDFA4$StationCode <- as.character(zoopNDFA4$StationCode)

zoopNDFA4$scaleCPUE = scale(zoopNDFA4$CPUE) #may need to rescale data for certain analyses

zoopNDFA4 <- zoopNDFA4 %>% group_by(Date,StationCode,Year,Regions2,SamplePeriod) %>% 
  summarise(cpue=sum(CPUEZoop),
            scaled=sum(scaleCPUE))

zoopNDFA6 <- zoopNDFA4%>% group_by(Year,StationCode,SamplePeriod) %>% #check number of samples taken at each station or for each region - group by stationcode shows only sttd in 2013 and ...
  summarise(n=length(cpue)) #...accordingly, grouping by region2 shows only upstream samples in 2013, thus, need to drop 2013 from analyses

#write.csv(zoopNDFA4, file = "C:/Users/jadams/Documents/DES docs and forms/NDFA/zoop/zoopNDFA4.csv")

hist(log(zoopNDFA4$cpue)) #log transformed CPUE data are normally distributed

zoopNDFA5=zoopNDFA4 %>% filter(Year==2017) #subset one year of data for simplified model tests

model1 <- lmer(log(cpue) ~ SamplePeriod+Regions2+(1|StationCode),data = zoopNDFA5,REML = FALSE)  #try one year first with simplified model (no interactions, simple error structure)
summary(model1)# this worked
coef(model1)

visreg(model1, xvar = "Year", by = "SamplePeriod")
visreg(model1, xvar = "SamplePeriod", by = "Year")
visreg(model1, xvar = "Regions2", by = "SamplePeriod")
visreg(model1, xvar = "Year", by = "Regions2")
plot(model1)

table(zoopNDFA5$SamplePeriod)
table(zoopNDFA5$Regions2)
table(zoopNDFA5$StationCode)


#try with additive model- this runs
model2 <- lmer(log(cpue) ~ SamplePeriod+Regions2+Year+(1|StationCode),data = zoopNDFA4,REML = FALSE) 
summary(model2)

library(car)
library(emmeans)
Anova(model2)
emmeans(model2, pairwise~Year, adjust = "tukey")

#interactive model with no region- this runs, year significant
model3 <- aov(log(cpue) ~ SamplePeriod*Year,data = zoopNDFA4) 
summary(model3)
TukeyHSD(model3)
#THis is not great because you are using type I insteadof type III SS

#try with two-way interactive model- not enough replication
model4 <- lmer(log(cpue) ~ Regions2*Year+Year*SamplePeriod+SamplePeriod*Regions2+(1|StationCode),data = zoopNDFA4,REML = FALSE) 
summary(model4)
Anova(model4, type = "III")
emmeans(model4, pairwise~Year, adjust = "tukey")
emmeans(model4, pairwise~Year|Regions2, adjust = "tukey")

#try with additive, two-way interaction hierarchical model (this is the original one we wanted, with stationcode nested within region) - not enough replication-does not converge
model5 <- lmer(log(cpue) ~ Regions2*Year+Year*SamplePeriod+SamplePeriod*Regions2+(Regions2|StationCode),data = zoopNDFA4,REML = FALSE) 
summary(model5)

table(zoopNDFA5$SamplePeriod)
table(zoopNDFA5$Regions2)
table(zoopNDFA5$StationCode)


#test 1 year dataset (2017) without year as a fixed effect using different models
model6 <- glmer.nb(log(round(cpue)) ~ SamplePeriod*Regions2+(1|StationCode),data = zoopNDFA5)
summary(model6)

model7 <- lme(log(cpue) ~ SamplePeriod+Regions2+(1|StationCode),data = zoopNDFA5)
summary(model7)

m1 <- lme(cpue ~ SamplePeriod+Regions2,random=~Regions2|StationCode,data=zoopNDFA5)

m2<-aov(CPUEZoop~Year*SamplePeriod, data = zoopNDFA5)
summary(m2)

m3<-aov(log(cpue)~SamplePeriod+Regions2+Error(1|StationCode), data = zoopNDFA5)

#---Remove 2013 and run analyses again-------
zoopNDFA7=zoopNDFA4 %>% filter(Year!=2013)

model1.1 <- lmer(log(cpue) ~ SamplePeriod+Regions2+(1|StationCode),data = zoopNDFA7,REML = FALSE)  
summary(model1.1)
Anova(model1.1)

#try with additive model- this runs
model2.1 <- lmer(log(cpue) ~ SamplePeriod+Regions2+Year+(1|StationCode),data = zoopNDFA7,REML = FALSE) #additive model with stationcode as a random effect - this works but not if stationcode is nested within only regions2 (i.e., regions2|stationcode)
summary(model2.1)

#interactive model with no region- this runs, year significant
model3.1 <- aov(log(cpue) ~ SamplePeriod*Year,data = zoopNDFA7) 
summary(model3.1)
TukeyHSD(model3.1)

#switch the order fixed effect terms are specified to account for unbalanced design - shows same significance, so test is valid (see Hector et al, 2010)
#interactive model with no region- this runs, year significant
model3.2 <- aov(log(cpue) ~ Year*SamplePeriod,data = zoopNDFA7) 
summary(model3.2)
TukeyHSD(model3.2)

#Two-way interactive model and station code as a random effect
model4.1 <- lmer(log(cpue) ~ Regions2*Year+Year*SamplePeriod+SamplePeriod*Regions2+(1|StationCode),data = zoopNDFA7,REML = FALSE) 
summary(model4.1)
anova(model4.1) #this runs- year, regions:year, year:sampleperiod, regions:sampleperiod are all significant
emmeans(model4.1, pairwise ~ Year|SamplePeriod)

#Try the same as 4.1 but with a different random effect structure 
model4.2 <- lmer(log(cpue) ~ Regions2*Year+Year*SamplePeriod+SamplePeriod*Regions2+(1|Regions2/StationCode),data = zoopNDFA7,REML = FALSE) 
summary(model4.2)
anova(model4.2) #results look the same as 4.1 (according to Rosie, this is because the stations are all unique names): year, regions:year, year:sampleperiod, regions:sampleperiod are all significant

#try with additive, two-way interaction hierarchical model with station code nested within region (this is the original one we wanted) 
model5.1 <- lmer(log(cpue) ~ Regions2*Year+Year*SamplePeriod+SamplePeriod*Regions2+(Regions2|StationCode),data = zoopNDFA4,REML = FALSE) 
summary(model5.1) # not enough replication: does not converge

#Laura helped with the following post hoc test
lsmeans(model4.2, ~Regions2|Year,adjust="sidak")#post hoc test on region and year (significant from anova) shows no significant differences of individual contrasts within a year but significant differences between years
lsmeans(model4.2, ~Year|Regions2,adjust="sidak")#post hoc test on region and year (significant from anova) shows no significant differences of individual contrasts within a year but significant differences between years

#Brittany helped with the following post hoc tests
L1=lsmeans(model4.2,~Regions2|Year,adjust="Tukey")
cld(L1,Letters=letters)
lsmeans(model4.1, "Year")
anova(model4.1)
L1.1 = emmeans(model4.1, pairwise ~ Regions2|Year, adjust = "sidak" )
L1.1

L1.1 = emmeans(model4.1, pairwise ~ Year, adjust = "sidak" )
L1.1


L2=lsmeans(model4.1,~Year|Regions2,adjust="Tukey")
cld(L2,Letters = letters,alpha = .05)

L3=lsmeans(model4.1,~Year|SamplePeriod,adjust="Tukey")
cld(L3,Letters = letters,alpha = .05)

L4=lsmeans(model4.2,~SamplePeriod|Year,adjust="Tukey")
cld(L4,Letters = letters,alpha = .05)

L5=lsmeans(model4.2,~SamplePeriod|Regions2|Year,adjust="Tukey")
cld(L5,Letters = letters,alpha = .05)

L6=lsmeans(model4.2,~Year|Regions2|SamplePeriod,adjust="Tukey")
cld(L6,Letters = letters,alpha = .05)

AIC(model1.1,model2.1,model3.1,model4.1,model4.2) #AIC model selection--models 4.1 and 4.2 are the best (4.1 is better but they're very similar)

#Try the same post hoc tests above with bonferroni adjustment

#Laura helped with the following post hoc test
lsmeans(model4.2, ~Regions2|Year,adjust="bonferroni")#post hoc test on region and year (significant from anova) shows no significant differences of individual contrasts within a year but significant differences between years

#Brittany helped with the following post hoc tests
S1=lsmeans(model4.2,~Regions2|Year,adjust="bonferroni")
cld(S1,Letters=letters)

S2=lsmeans(model4.2,~Year|Regions2,adjust="bonferroni")
cld(S2,Letters = letters,alpha = .05)

S3=lsmeans(model4.2,~Year|SamplePeriod,adjust="bonferroni")
cld(S3,Letters = letters,alpha = .05)

S4=lsmeans(model4.2,~SamplePeriod|Year,adjust="bonferroni")
cld(S4,Letters = letters,alpha = .05)

S5=lsmeans(model4.2,~SamplePeriod|Regions2|Year,adjust="bonferroni")
cld(S5,Letters = letters,alpha = .05)

S6=lsmeans(model4.2,~Year|Regions2|SamplePeriod,adjust="bonferroni")
cld(S6,Letters = letters,alpha = .05)

visreg(model4.2)

visreg(model4.2, xvar = "Year", by = "SamplePeriod")
visreg(model4.2, xvar = "SamplePeriod", by = "Year")
visreg(model4.2, xvar = "Regions2", by = "SamplePeriod")
visreg(model4.2, xvar = "Year", by = "Regions2", alpha = .5,  type = "conditional")
visplot(model4.2)


model6 <- glmer.nb(log(round(cpue)) ~ SamplePeriod*Regions2+(1|StationCode),data = zoopNDFA5)
summary(model6)

model7 <- lme(log(cpue) ~ SamplePeriod+Regions2+(1|StationCode),data = zoopNDFA5)


m1 <- lme(cpue ~ SamplePeriod+Regions2,random=~Regions2|StationCode,data=zoopNDFA5)

m2<-aov(CPUEZoop~Year*SamplePeriod, data = zoopNDFA5)
summary(m2)

m3<-aov(log(cpue)~SamplePeriod+Regions2+Error(1|StationCode), data = zoopNDFA5)

######boxplot for UBR with just SR and AG, in order before, during, after######

#filter to remove CA and NF flowpulsetypes and put flow period in order
#boxplots with period and flowpulsetype interaction on x axis (adjust limits to see boxes or extreme values)
zoopNDFAv2sub <- zoopNDFAv2 %>% filter(FlowPulseType=="MA-Ag" | FlowPulseType=="MA-SR")
ggplot(data=zoopNDFAv2sub, aes(x=interaction(FlowPulseType,SamplePeriod), y=BPUE))+geom_boxplot(notch = TRUE,outlier.shape = NA,na.rm = TRUE)+scale_y_continuous(name=expression(paste("CPUE")), limits=c(0,1000),expand=c(0,0))+scale_x_discrete(limits=c("MA-Ag.Before","MA-SR.Before","MA-Ag.During","MA-SR.During","MA-Ag.After","MA-SR.After"))

