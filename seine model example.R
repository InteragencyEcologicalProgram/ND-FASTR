#example fish GLM and multivariate analyses

#first let's load a few libraries

#data manipulation package
library(tidyverse)
#multivariate analysis package
library(vegan)
#package with test for overdispersion
library(AER)

#negative binomial package
library(MASS)


#load some YBFMP data
yolo = read.csv("fish data/FISH_RAW_YBFMP_WQ_Fish_2011_thru_2019_20200206.csv")

#make sure it read in correctly
str(yolo)

#let's change the date and filter it so it's just beach seines
yolo = mutate(yolo, Date = as.Date(Date, format = "%m/%d/%Y"))

yolo = filter(yolo, MethodCode == "BSEIN", Year >2009, Year < 2020)


#Calculate catch/volume
yolo = mutate(yolo, CPUE = Count/SeineVol)

#make a version of the "year" variable that is a factor instead of a continuous variable. 

yolo = mutate(yolo, Year2 = as.factor(Year))

#####################################################################################3
#now let's do some linear models.

#Let's start with total catch

#To calculate total catch, Group by Sample ID and sum.

yolotot = group_by(yolo, SampleID, Date, Year, Year2, StationCode, SeineVol) %>%
  summarise(CPUE = sum(CPUE, na.rm = T), Count = sum(Count, na.rm = T))

#quick histogram of catch
ggplot(yolotot) + geom_histogram(aes(x = CPUE), bins = 100)

#OK, so that's definitely not normally distributed. We're going to need a poisson model

#basic poisson model
#I just rounded CPUE to make it work. THere are better ways, but that is fast and easy.
m1 = glm(round(CPUE)~ StationCode + Year, family = "poisson", data = yolotot)
summary(m1)

#whenever all the coefficients are highly significant, I am skeptakle. let's test for overdispersion
dispersiontest(m1)

#OK, super, highly overdisperssed. LEt's try a negative binomial model
m2 = glm.nb(round(CPUE)~ StationCode + Year, data = yolotot)
summary(m2)
#That looks better

library(visreg)
visreg(m2)

#try it with year as a factor instead
m3 = glm.nb(round(CPUE)~ StationCode + Year2, data = yolotot)
summary(m3)

visreg(m3)

###############################################################################
#now let's go multivariate

#This takes a long time to run with all the data, so I"ll just do a subset
yoloX = filter(yolo, Year %in% c(2017, 2018, 2019))

#First set up a community matrix

Commat = pivot_wider(yoloX,id_cols = c(SampleID, Date, Year, Year2, Month, StationCode),
                     names_from = OrganismCode, values_from = CPUE, values_fill = 0,
                     values_fn = sum) %>%
  dplyr::select(-NONE, -`NA`)

#it doesn't like empty rows, so we need to get rid of all the "no catch" rows
Commat2 = Commat[which(rowSums(Commat[,7:44]) != 0),]

#Create a relative composition matrix from that
RelCom = Commat2[,7:44]/rowSums(Commat2[,7:44])

#now run a PERMANOVA
p1 = adonis(RelCom~StationCode, data = Commat2)
p1

#this is telling us that Station is important, but only explains 13% of the vairance

#try adding year
p2 = adonis(RelCom~StationCode + Year2, data = Commat2)
p2

#now do a ANOSIM

a1 = with(Commat2, anosim(RelCom, StationCode))

#SIMPER is similar to ANOSIM, but computed differently
s1 = with(Commat2, simper(RelCom, StationCode))
s1
