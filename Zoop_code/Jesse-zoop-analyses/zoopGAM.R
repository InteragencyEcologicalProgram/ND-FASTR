#GAM analysis for FASTR zooplankton

library(tidyverse)
library(lubridate)
library(scales)
library(knitr)
library(mgcv)
library(lme4)
library(car)
library(emmeans)
library(gratia)
library(here)
library(forcats)
# Source functions
source(here("global_ndfa_funcs.R"))
source(here("Water_Quality/global_wq_funcs.R"))

#zooplankton data from Mallory and Nicole (biomass)
zoopNDFAv2<-read.csv("Zoop_code/zoop_NDFA_v2.csv", stringsAsFactors = FALSE)
#data organization and cleanup
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
glimpse(zoopNDFAv2)

#read in data with additional flow parameters and create new data table joined with zoop data
flow_magnitude<-read.csv("Zoop_code/flow_magnitude.csv", stringsAsFactors = FALSE, na.strings="",header = TRUE)
flow_dates<-read.csv("Zoop_code/FlowDatesDesignations.csv", stringsAsFactors = FALSE, na.strings="",header = TRUE)
flow_dates$PreFlowStart <- format(as.Date(flow_dates$PreFlowStart, format = "%m/%d/%Y"), "%Y-%m-%d")
flow_dates$PreFlowEnd <- format(as.Date(flow_dates$PreFlowEnd, format = "%m/%d/%Y"), "%Y-%m-%d")
flow_dates$PostFlowStart <- format(as.Date(flow_dates$PostFlowStart, format = "%m/%d/%Y"), "%Y-%m-%d")
flow_dates$PostFlowEnd <- format(as.Date(flow_dates$PostFlowEnd, format = "%m/%d/%Y"), "%Y-%m-%d")
flow_dates <- flow_dates %>% filter(Year!="2011")
flow_dates <- flow_dates %>% filter(Year!="2012")
flow_dates <- flow_dates %>% filter(Year!="2013")

zoopNDFAv3 <- zoopNDFAv2 
zoopNDFAv3$Year <- as.character(zoopNDFAv3$Year)
flow_magnitude$Year<-as.character(flow_magnitude$Year)

zoopNDFAv3<-left_join(flow_magnitude,zoopNDFAv3)

#Remove years 2011 and 2012 with incomplete sampling and remove Sherwood (outside study area) and Rominger Bridge (too few samples) 
zoopNDFAv3 <-  zoopNDFAv3%>% filter(Year!="2011")
zoopNDFAv3 <-  zoopNDFAv3%>% filter(Year!="2012")
zoopNDFAv3 <- zoopNDFAv3 %>% filter(StationCode!="SHR")
zoopNDFAv3 <- zoopNDFAv3 %>% filter(StationCode!="RMB")

zoopNDFA4 <- zoopNDFAv3[,c("Year","Date","SamplePeriod","Region","Regions2","StationCode","CPUEZoop")] #new table with relevant columns
zoopNDFA4$SamplePeriod <- as.character(zoopNDFA4$SamplePeriod)
zoopNDFA4$Regions2 <- as.character(zoopNDFA4$Regions2)
zoopNDFA4$StationCode <- as.character(zoopNDFA4$StationCode)



zoopNDFA4$scaleCPUE = scale(zoopNDFA4$CPUE) #may need to rescale data for certain analyses

#NOTE: added classification for individual taxa group analysis, but you need to remove this for the original total zoop analysis

zoopNDFA4 <- zoopNDFA4 %>% group_by(Date,StationCode,Year, Regions2,SamplePeriod) %>% 
  summarise(cpue=sum(CPUEZoop),
            scaled=sum(scaleCPUE))

#2013 removed because only STTD sampled
zoopNDFA7=zoopNDFA4 %>% filter(Year!=2013) # this dataset excludes 2011-2013 and groups total CPUE biomass for each sample

#remove data with bad flowmeter data affecting CPUE
zoopNDFA7 <- zoopNDFA7 %>%  filter(Date!="2016-07-07" | StationCode!="RVB")
zoopNDFA7 <- zoopNDFA7 %>% filter(Date!="2016-01-06" | StationCode!="STTD")

zoopNDFA7$SamplePeriod <- factor(zoopNDFA7$SamplePeriod,levels=c("Before","During","After"))


#the following is the model and post hoc from the original report#
#######Two-way interactive model and station code as a random effect--THIS IS THE MODEL we ultimately chose#####
model4.1 <- lmer(log(cpue) ~ Regions2*Year+Year*SamplePeriod+SamplePeriod*Regions2+(1|StationCode),data = zoopNDFA7,REML = TRUE) 
summary(model4.1)
model4.1
modtab <- Anova(model4.1, type = 3, test.statistic = "F") #this runs- year, regions:year, year:sampleperiod, regions:sampleperiod are all significant
kable(modtab)
summary(modtab)

#####Post-hoc with emmeans Sidak method - THIS IS THE POST HOC USED######
#use emmeans instead to get p value
lmer_emm <- emmeans(model4.1, specs = pairwise ~Regions2:Year,adjust="sidak")#post hoc test on region and year (significant from anova) shows no significant differences of individual contrasts within a year but significant differences between years
phoc <- print(test(lmer_emm)$contrasts)
kable(phoc)

####GAM analysis####

#first some preliminary sample counts to assess study design

#counts by station, year and sample period
zoopNDFA7 %>% 
  group_by(Year, SamplePeriod, StationCode) %>% summarise(n = n()) %>%  
  arrange(StationCode) %>% 
  pivot_wider(names_from = StationCode, values_from = n) %>% 
  arrange(Year, SamplePeriod) %>% 
  kable()

#counts by region, year and sample period
zoopNDFA7 %>% 
  group_by(Year, SamplePeriod, Regions2) %>% summarise(n = n()) %>%
  pivot_wider(names_from = Regions2, values_from = n) %>% 
  kable()

#boxplots by year and region

zoopNDFA7 %>% 
  ggplot(aes(x = SamplePeriod, y = cpue, fill = SamplePeriod)) +
  geom_boxplot() +
  facet_grid(rows = vars(Year), cols = vars(Regions2)) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides = "l")

#boxplots by station and region

zoopNDFA7 %>%
  filter(Regions2 == "Upstream") %>% 
  ggplot(aes(x = SamplePeriod, y = cpue, fill = SamplePeriod)) +
  geom_boxplot() +
  facet_grid(rows = vars(Year), cols = vars(StationCode)) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides = "l")

zoopNDFA7 %>%
  filter(Regions2 == "Downstream") %>% 
  ggplot(aes(x = SamplePeriod, y = cpue, fill = SamplePeriod)) +
  geom_boxplot() +
  facet_grid(rows = vars(Year), cols = vars(StationCode)) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides = "l")

#GAM smooth plots
zoopNDFA8 <- zoopNDFA7
zoopNDFA8$DOY <- yday(zoopNDFA8$Date)
zoopNDFA8$logcpue <- zoopNDFA8$cpue
zoopNDFA8$logcpue=log10(zoopNDFA8$cpue)
zoopNDFA8$StationCode <- factor(zoopNDFA8$StationCode)
zoopNDFA8$Regions2 <- factor(zoopNDFA8$Regions2)
zoopNDFA8$Year <- factor(zoopNDFA8$Year)


flow_dates$DOYpreEND <- yday(flow_dates$PreFlowEnd)
flow_dates$DOYpostSTART <- yday(flow_dates$PostFlowStart)

zoopNDFA8 %>% 
  ggplot(aes(x = DOY, y = cpue, color = Regions2)) +
  geom_smooth() +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  facet_wrap(vars(Year), scales = "free_y") +
  geom_rect(
    data = flow_dates,
    aes( 
      xmin = DOYpreEND, 
      xmax = DOYpostSTART, 
      ymin = -Inf, 
      ymax = Inf
    ),
    inherit.aes = FALSE,
    alpha = 0.2,
    fill = "brown"
  ) +
   theme_bw()


zoopNDFA8 %>% 
  ggplot(aes(x = DOY, y = logcpue)) +
  geom_point(size = 1, alpha = 0.2) +
  geom_smooth() +
  theme_bw()

zoopNDFA8 %>% 
  ggplot(aes(x = DOY, y = logcpue, color = Regions2)) +
  geom_point(size = 1, alpha = 0.2) +
  geom_smooth() +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  theme_bw()

#model4.1 <- lmer(log(cpue) ~ Regions2*Year+Year*SamplePeriod+SamplePeriod*Regions2+(1|StationCode),data = zoopNDFA7,REML = TRUE)

m_cpue_gam <- gam(
  logcpue ~ (Year+SamplePeriod+Regions2)^2 + s(DOY, k=20) + s(StationCode, bs = "re"), 
  data = zoopNDFA8,
  method = "REML"
)

summary(m_cpue_gam)

gam.check(m_cpue_gam)

plot(m_cpue_gam, pages=1, residuals=TRUE)
#check out geom_quasirandom from the ggbeeswarm package