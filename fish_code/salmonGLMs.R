#let's see what we can do with Jeff's salmonid data

#First load some librarys

library(tidyverse)

#import the data

salmon = read.csv("YB_Salmonid_catch.csv", stringsAsFactors = F)

#let's see if that read in right
str(salmon)

#fix a few of the columns so they are the right data type
salmon = mutate(salmon, 
                
                WYtype = factor(WYtype, levels = c(1:5), 
                labels = c("CD", "D", "BN", "AN", "W")),
                WYtype.2yrs= factor(WYtype, levels = c(1:5), 
                                    labels = c("CD", "D", "BN", "AN", "W")) )

#Now let's graph annual catch versus transport time from two years ago.

#First calculate total catch by year and species
#I use "group_by" and "Summarize" to calculate summary stats.
salmonsum = group_by(salmon, Year, Species, Flow.Action..Y.N.) %>%
  summarize(catch = length(FL), MeanTrans = mean(MeanTrans), Trans.2yrs = mean(Trans.2yrs), Trans.3yrs = mean(Trans.3yrs), Trans.4yrs = mean(Trans.4yrs),
            WYtype = first(WYtype), WYtype.2yrs = first(WYtype.2yrs), WYtype.3yrs = first(WYtype.3yrs), WYtype.4yrs = first(WYtype.4yrs))

#Now do a quick histogram, to see if the data are normally distributed
ggplot(salmonsum, aes(x = catch, fill = Species)) +geom_histogram() + 
  facet_grid(~Species, scales = "free_x")

#Nope. Definitely not normal. Probably need to do a transformation before stats.
#Also, not enough RBT to do much with

#Now a scatter plot
ggplot(salmonsum, aes(x = catch, y = MeanTrans)) +geom_point() + 
  facet_grid(~Species, scales = "free_x")


#Let's try log-transforming both transport difference and catch
#to see if it looks a little clearer.
ggplot(salmonsum, aes(x = log(catch), y = log(MeanTrans))) +geom_point() + 
  facet_grid(~Species, scales = "free_x")

#copying Rosie's code for addt'l views: transport-2years

#And a scatter plot
ggplot(salmonsum, aes(x = catch, y = Trans.2yrs)) +geom_point() + 
  facet_grid(~Species, scales = "free_x")

#log transformed
ggplot(salmonsum, aes(x = log(catch), y = log(Trans.2yrs))) +geom_point() + 
  facet_grid(~Species, scales = "free_x")

#Now transport-3years

ggplot(salmonsum, aes(x = catch, y = Trans.3yrs)) +geom_point() + 
  facet_grid(~Species, scales = "free_x")

#log transformed
ggplot(salmonsum, aes(x = log(catch), y = log(Trans.3yrs))) +geom_point() + 
  facet_grid(~Species, scales = "free_x")

#Now transport-4years

ggplot(salmonsum, aes(x = catch, y = Trans.4yrs)) +geom_point() + 
  facet_grid(~Species, scales = "free_x")

#log transformed
ggplot(salmonsum, aes(x = log(catch), y = log(Trans.4yrs))) +geom_point() + 
  facet_grid(~Species, scales = "free_x")

#Meh.

#Now let's do a general linear model. Because we have count data,
#and they are not normally distributed, we will want to use either the poisson
#or the negative binomial distribution (dipending on dispersion), rather
#than the  normal distribution.

#let's just use the chinook though, and take out the steelhead
salonly = filter(salmonsum, Species == "CHN")

m1 = glm(catch ~ MeanTrans, family = "poisson", data = salonly)
summary(m1)

#check the diagnostic plots
plot(m1)


#check for overdispersion
library(AER)

dispersiontest(m1)
#dispersion is much, much greater than 1. We've got a problem.

#we've got some outliers that are really throwing things off. (2014, 2015, and 2017)
#We could take them out and re-run the model, or try again with a different distribution

#Checking other transport years (-2,3,4 years)
m1a = glm(catch ~ Trans.2yrs, family = "poisson", data = salonly)
summary(m1a)

#check the diagnostic plots
plot(m1a)


#check for overdispersion

dispersiontest(m1a)
#Same - dispersion much greater than 1
#Now trans - 3 years
m1b = glm(catch ~ Trans.3yrs, family = "poisson", data = salonly)
summary(m1b)

#check the diagnostic plots
plot(m1b)


#check for overdispersion
library(AER)

dispersiontest(m1b)
#Same as the other transport versus years
#Lastly - trans-4years
m1c = glm(catch ~ Trans.4yrs, family = "poisson", data = salonly)
summary(m1c)

#check the diagnostic plots
plot(m1c)


#check for overdispersion

dispersiontest(m1c)
# Much higher than 1 again.

#let's try a quasipoisson first
m2 = glm(catch ~ MeanTrans, family = "quasipoisson", data = salonly)
summary(m2)
plot(m2)
#better, but 2015 and 2017 are still throwing us off
#Trying trans - (2, 3, 4) years
m2a = glm(catch ~ Trans.2yrs, family = "quasipoisson", data = salonly)
summary(m2a)
plot(m2a)
#Eh.
m2b = glm(catch ~ Trans.3yrs, family = "quasipoisson", data = salonly)
summary(m2b)
plot(m2b)
#Nah
m2c = glm(catch ~ Trans.4yrs, family = "quasipoisson", data = salonly)
summary(m2c)
plot(m2c)
#zzzzzz

#let's try a negaitve binomial.
library(MASS)
m3 = glm.nb(catch ~ MeanTrans, data = salonly)
#Gross I don't know why it did that.

#Let's try just taking the outliers off and see what happens
salonly2 = filter(salonly, Year != 2014 & Year != 2015 & Year != 2017)

m4 = glm(catch~ MeanTrans, family = "poisson", data = salonly2)
summary(m4)
plot(m4)

#that's much better, but we should also check for overdispersion.
dispersiontest(m4)
#dispersion is still greater than 1, but much better

#now a quasipoisson again
m5 = glm(catch~ MeanTrans, family = "quasipoisson", data = salonly2)
summary(m5)
plot(m5)

#This model is significant, and fits all the assumptions
#and diagnostic tests, but it actually shows a negative relationship
#between catch and transport distance. Overall, I'm not buying it.

#visualize the model
library(visreg)
visreg(m5)

#Now for transport-2yrs

m5a = glm(catch~ Trans.2yrs + WYtype, family = "quasipoisson", data = salonly)
summary(m5a)
plot(m5a)

#visualize the model
visreg(m5a)

#Now trans-3 years

m5b = glm(catch~ Trans.3yrs + WYtype, family = "quasipoisson", data = salonly)
summary(m5b)
plot(m5b)

m5b = glm(catch~ Trans.3yrs, family = "quasipoisson", data = salonly)
summary(m5b2)
visreg(m5b2)
plot(m5b2)
visreg(m5b)

#Now trans-4 years

m5c = glm(catch~ Trans.4yrs + WYtype, family = "quasipoisson", data = salonly2)
summary(m5c)
plot(m5c)

visreg(m5c)

###################################################################################

#Let's see what happens if we put in some other variables.

mm = glm(catch~Trans.2yrs + WYtype, family = "quasipoisson", salonly2)
summary(mm)


mm = glm(catch~ Trans.3yrs + WYtype.3yrs, 
         family = "quasipoisson", salonly)
summary(mm)
plot(mm)

salonly3 = filter(salonly, Trans.3yrs < 200 & catch <50)
m5c = glm(catch~ Trans.3yrs, family = "quasipoisson", data = salonly3)
summary(m5c)
plot(m5c)

#################################################################################

#go back to the origional and look at action period
salmon$date = as.Date(paste(salmon$Year, salmon$Month, salmon$Day), format = "%Y %m %d")
s1 = ggplot(salmon, aes(x = date, color = Flow.Action..Y.N.))
s1 + geom_bar()

m5c = glm(catch~ Flow.Action..Y.N., family = "quasipoisson", data = salonly)
summary(m5c)
plot(m5c)
visreg(m5c)
