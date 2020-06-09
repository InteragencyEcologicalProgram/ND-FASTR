rm(list=ls())#clear global environment
setwd("~/R/NDFA")
#load(file = "NDFA_clam.RData")

library(EnvStats)
library(tidyverse)
library(ggplot2)
library(psych)

d = read_csv("NDFA_corbicula2.csv")

#windows()#creates blank template to run plots to

#Data exploration and visualization#########

#all variables
#?pairs.panels
pairs.panels(d)
pairs.panels(d[,-2])

#summary table
dsum = d %>% 
  group_by(Region) %>% 
  
  summarize(
    meanAFDM = mean(AFDM, n = n()),
    meanCHLA = mean(ChlA, n = n()),
    meanTEMP = mean(Temp, n = n()),
    meanPh = mean(PH, n = n()),
    meanDO = mean(DO, n = n ()), 
    meanSC = mean(SC, n = n ()),
    meanDens = mean(Density, n = n ()),
    meanTurb = mean(Turb, n = n ()),
    meanDepth = mean(Depth, n = n ()),
    meanFR = mean(FR, n = n()),
          )

#########column plot with two y axes##############
#two axes first attempt didn't work so well
dsum$Region <- factor(dsum$Region,levels = c("Toe_Drain", "Stair_Step", "Prospect_Slough", "Liberty_Island","Lindsey_Slough", "Cache_Slough", "Deep_Water_Ship_Channel", "Ryer_Island", "Rio_Vista"), labels = c("TD", "SS", "PS", "LIB", "LS", "CS", "DWS", "RYI", "RVB"))
testplot <- ggplot(dsum, aes(x=Region, y = meanFR)) 
  testplot + geom_col()
  testplot <-  ggplot(dsum, aes(x = Region, y=meanFR)) + geom_col()
testplot2 <- testplot


testplot2 + geom_col(aes(x=Region, y=meanAFDM))+
  scale_y_continuous(sec.axis = sec_axis(~.*5, name = "meanAFDM"))

test3 = ggplot(dsum, aes(x = Region, y = meanAFDM)) + 
  geom_col()+
  scale_y_continuous(sec.axis = sec_axis(~.*107, name = "meanFR"))
test3


dsum2 = mutate(dsum, meanFR2 = meanFR/107) %>%
  pivot_longer(cols = c(meanAFDM, meanFR2), names_to = "variable", values_to= "value")

test3 = ggplot(dsum2, aes(x = Region, y = value)) + 
  geom_col(aes(fill = variable), position = "dodge")+
  scale_y_continuous(sec.axis = sec_axis(~.*107, name = "meanFR"))
test3



#the following is modified code from online
# plot showing biomass 
p <- ggplot(dsum, aes(x = Region))
p <- p + geom_col(aes(y = meanAFDM), fill = "blue")+ geom_col(aes(y = meanAFDM), fill = "red")
p
p <- ggplot(dsum, aes(x = Region))
p <- p + geom_col(aes(y = meanAFDM), fill = "red")
p
# adding the filt rate data, transformed to match roughly the range of the temperature
p <- p + geom_col(aes(y = meanFR, colour = "meanFR"), fill = "blue")
p
# adding the secondary axis, following the example in the help file ?scale_y_continuous

p <- p + scale_y_continuous(sec.axis = sec_axis(trans = NULL,name = waiver(), breaks = waiver(),labels = waiver(),guide = waiver()))
# modifying colours and theme options
p <- p + scale_colour_manual(values = c("blue", "red"))
p <- p + labs(y = "mean AFDM",
              x = "Region",
              colour = "Parameter")
p <- p + theme(legend.position = c(0.1,0.8))
p

##############more exploration###########
#subset by year
dplot19 <- filter(d,Year == 2019)
dplot14 <- filter(d,Year == 2014)

ggplot(dplot19, aes(x = Density) +
  geom_histogram(binwidth = 30))

ggplot(dplot14, aes(x = Density, group=Year)) +
  geom_histogram(binwidth = 30)

#par(mfrow=c(2,2))
hist(log(dplot14$AFDM))
hist(log(dplot19$AFDM))

hist(dplot14$AFDM)
hist(dplot19$AFDM)
#?hist

mplot <- ggplot(data = d, aes(x=Year, y=AFDM, group=Year, ylab(AFDM (g/m2))))

#box and dot plots side by side
mplot +
  geom_boxplot(aes(x = as.numeric(Year) + .2, group = Year), width = .25) +
  geom_dotplot(aes(x = as.numeric(Year) - .2, group = Year),
  binaxis = "y",
  binwidth = .5,
  stackdir = "center") +
  scale_x_continuous(breaks = c(2014,2019),labels = c(2014, 2019))

#violin with box plot inside
mplot +
  geom_violin(width = 2.5) +
  geom_boxplot(width = .25, fill = "grey", outlier.color = NA) +
  stat_summary(fun.y = median, geom = "point", fill = "white", shape = 21, size = 3)+ 
  scale_x_continuous(breaks = c(2014,2019),labels = c(2014, 2019))


#?geom_boxplot

#vignette("ggplot2-specs")

#?geom_qq

#QQ plots of individual years
#log transform 2014 AFDM and 2019 Density:
ggplot(dplot14, aes(sample = Density)) +
  geom_qq()+
  geom_qq_line()
  
ggplot(dplot19, aes(sample = log(Density))) +
  geom_qq()+
  geom_qq_line()

ggplot(dplot14, aes(sample = log(AFDM))) +
  geom_qq()+
  geom_qq_line()

ggplot(dplot19, aes(sample = AFDM)) +
  geom_qq()+
  geom_qq_line()

#log transformation better for aggregated year
ggplot(d, aes(sample = log(AFDM))) +
  geom_qq()+
  geom_qq_line()

ggplot(d, aes(sample = log(Density))) +
  geom_qq()+
  geom_qq_line()

#boxplots
par(mfrow=c(3,3))
boxplot(Depth ~ Year, data = d)
boxplot(Turb ~ Year, data = d)
boxplot(AFDM ~ Year, data = d, ylab = 'AFDM (g/m2)', varwidth = TRUE)
boxplot(Density ~ Year, data = d)
boxplot(ChlA ~ Year, data = d)
boxplot(PH ~ Year, data = d)
boxplot(DO ~ Year, data = d)
boxplot(Temp ~ Year, data = d)
boxplot(SC ~ Year, data = d)
par(mfrow=c(1,1))

#?boxplot

#omit RVB to see out-sized effect
noRVB = d[-c(4,8),]
  
par(mfrow=c(3,3))
boxplot(Depth ~ Year, data = noRVB)
boxplot(Turb ~ Year, data = noRVB)
boxplot(AFDM ~ Year, data = noRVB)
boxplot(Density ~ Year, data = noRVB)
boxplot(ChlA ~ Year, data = noRVB)
boxplot(PH ~ Year, data = noRVB)
boxplot(DO ~ Year, data = noRVB)
boxplot(Temp ~ Year, data = noRVB)
boxplot(SC ~ Year, data = noRVB)
par(mfrow=c(1,1))
#boxplot(Density ~ Year, data = d, plot=FALSE)

ggplot(data = d)+
  stat_summary(mapping = aes(x=Year, y=AFDM),
               fun.ymin = min,
               fun.ymax = max,
               fun.y = median)+
  scale_x_continuous(breaks = c(2014,2019))



#Exploratory Analysis####

##if there's no difference in mean biomass and density between years,
##then what's the biomass in 2014 at just the 2019 sites versus all sites? 
##are they different or the same? what're the grazing rates in 2019...same or different?
##and, are there environmental variables with a strong relationship to biomass in 2014?
##if so, do those relationships occur in the 2019 data too?

#AFDM/Year boxplot
par(mfrow=c(1,2))
boxplot(AFDM ~ Year, data = d, ylab = 'AFDM (g/m2)', varwidth = TRUE)
boxplot(Density ~ Year, data = d, ylab = 'Density (clams/m2)', varwidth = TRUE)

#AFDM/Year box plot
mplot +
  geom_boxplot(width = .25, fill = "grey", outlier.color = NA) +
  stat_summary(fun.y = mean, geom = "point", fill = "white", shape = 21, size = 3)+ 
  scale_x_continuous(breaks = c(2014,2019),labels = c(2014, 2019))

#shapiro-wilk test for normality
shapiro.test(d$AFDM) #not normally distributed
shapiro.test(log(d$AFDM)) #the log transformed data are normally distributed

#combined dataset with full 2014
d2Yr = read_csv("NDFA_corb_2Yr.csv")
dYRsum = d2Yr %>% 
  group_by(Year) %>% 
  
  summarize(
    meanAFDM = mean(AFDM, n = n()),
    meanCHLA = mean(ChlA, n = n()),
    meanTEMP = mean(Temp, n = n()),
    meanPh = mean(PH, n = n()),
    meanDO = mean(DO, n = n ()), 
    meanSC = mean(SC, n = n ()),
    meanDens = mean(Density, n = n ()),
    meanTurb = mean(Turb, n = n ()),
    meanDepth = mean(Depth, n = n ()),
  )


#ANOVA on log-transformed data
logAFDM.mod1 = aov(log(AFDM) ~ Year, data = d)
summary(logAFDM.mod1)
hist(resid(logAFDM.mod1))
ggplot(logAFDM.mod1, aes(sample = resid(logAFDM.mod1))) +
  geom_qq()+
  geom_qq_line()

##2014 analysis######

d14 = read_csv("NDFA_corb_2014.csv")

#Add 1 to AFDM for log-transforming data with zeroes
d14$x <- NA 
d14$x <- d14$AFDM+1

#Try removing DWSC and Rio Vista
d14mod <- subset(d14, !(Region=='Rio_Vista'))
d14mod2 <- subset(d14mod, !(Region=='Deep_Water_Ship_Channel'))

boxcox(d14$x, objective.name = "Shapiro-Wilk")

boxplot(AFDM~Region, data = d14)
violin(AFDM~Year, data = d2Yr)

d2Yrplot <- ggplot(data = d2Yr, aes(x=Year, y=AFDM, group=Year, ylab(AFDM (g/m2))))
d2Yrplot +
  geom_violin(aes(x = as.numeric(Year) + .2, group = Year), width = .25) +
  geom_dotplot(aes(x = as.numeric(Year) - .2, group = Year),
               binaxis = "y",
               binwidth = .5,
               stackdir = "center") +
  scale_x_continuous(breaks = c(2014,2019),labels = c(2014, 2019))

hist((1/sqrt(d14$x)))

shapiro.test((1/sqrt(d14$AFDM+1)))#add 1 so there are no non-zeroes, but why isn't it normal?

#ANOVA 2014 data
logAFDM14.mod =aov(log(AFDM+1) ~ Region, data = d14)
summary(logAFDM14.mod)
hist(resid(logAFDM14.mod))
ggplot(logAFDM14.mod, aes(sample = resid(logAFDM14.mod))) +
  geom_qq()+
  geom_qq_line()

#better transformation using inverse sqrt
invrtAFDM14.mod =aov((1/sqrt(x)) ~ Region, data = d14)
summary(invrtAFDM14.mod)
hist(resid(invrtAFDM14.mod))
ggplot(invrtAFDM14.mod, aes(sample = resid(invrtAFDM14.mod))) +
  geom_qq()+
  geom_qq_line()

invrtAFDM14.mod =aov((1/sqrt(AFDM+1)) ~ Region, data = d14mod2)
summary(invrtAFDM14.mod)
hist(resid(invrtAFDM14.mod))
ggplot(invrtAFDM14.mod, aes(sample = resid(invrtAFDM14.mod))) +
  geom_qq()+
  geom_qq_line()

#Compare mean of 2014 with mean of 2019
#T-test
t.test((log(d2Yr$AFDM+1))~d2Yr$Year)
hist(log(d2Yr$AFDM))
boxcox(d2Yr$AFDM+1, objective.name = "shapiro-wilk")

#####Final data analyses used#####

#AFDM~Year at matched 2014/2019 sites
par(mfrow=c(1,1))
dAFDM.mod=aov(log(AFDM)~Year, data = d)
summary(dAFDM.mod)
hist(resid(dAFDM.mod))
ggplot(dAFDM.mod, aes(sample = resid(dAFDM.mod))) +
  geom_qq()+
  geom_qq_line()

#Density~Year at matched 2014/2019 sites
dDens.mod=aov(log(Density)~Year, data = d)
summary(dDens.mod)
hist(resid(dDens.mod))
ggplot(dDens.mod, aes(sample = resid(dDens.mod))) +
  geom_qq()+
  geom_qq_line()

#North Delta is the same per 2014 v 2019, so pool data and check region differences
#Biomass~Region from pooled 2014/2019 all data
AFDM2Yr.mod=aov(1/sqrt(AFDM+1)~Region, data = d2Yr)
summary(AFDM2Yr.mod)
hist(resid(AFDM2Yr.mod))
ggplot(AFDM2Yr.mod, aes(sample = resid(AFDM2Yr.mod))) +
  geom_qq()+
  geom_qq_line()

#Density~Region from pooled 2014/2019 all data
Dens2Yr.mod=aov(Density~Region, data = d2Yr)
summary(Dens2Yr.mod)
hist(resid(Dens2Yr.mod))
ggplot(Dens2Yr.mod, aes(sample = resid(Dens2Yr.mod))) +
  geom_qq()+
  geom_qq_line()

###########test#########

p <- ggplot(dsum, aes(x = Region))
p <- p + geom_bar
p

save.image("NDFA_clam.RData")
