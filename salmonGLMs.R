#let's see what we can do with Jeff's salmonid data

#First load some librarys

library(tidyverse)

#import the data

salmon = read.csv("Salmonid_catch_exporatory data.csv", stringsAsFactors = F)

#let's see if that read in right
str(salmon)

#fix a few of the columns so they are the right data type
salmon = mutate(salmon, 
                SampleTime = as.POSIXct(strptime(SampleTime, 
                                                         format = "%I:%M:%S %p")),
                WYtype = factor(WYtype, levels = c(1:5), 
                labels = c("CD", "D", "BD", "AN", "W")),
                WYtype.2yrs= factor(WYtype, levels = c(1:5), 
                                    labels = c("CD", "D", "BD", "AN", "W")) )

#Now let's graph annual catch versus transport time from two years ago.

#First calculate total catch by year and species
#I use "group_by" and "Summarize" to calculate summary stats.
salmonsum = group_by(salmon, Year, Species) %>%
  summarize(catch = length(FL), MeanTrans = mean(MeanTrans), 
            WYtype = first(WYtype), WYtype.2yrs = first(WYtype.2yrs))

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

#let's try a quasipoisson first
m2 = glm(catch ~ MeanTrans, family = "quasipoisson", data = salonly)
summary(m2)
plot(m2)
#better, but 2015 and 2017 are still throwing us off

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
