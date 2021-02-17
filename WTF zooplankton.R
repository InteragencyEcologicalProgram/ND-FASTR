#what is wrong with all this code?

library(tidyverse)
library(lme4)
library(pscl)

zoops = read.csv("zoopNDFA4.csv")

table(zoops$Regions2, zoops$Year)

ggplot(salmon, aes(x=all.catch, y = flow.avg)) + geom_point() + geom_smooth()

m1 = zeroinfl(all.catch ~flow.avg + Week + Year, data = salmon, dist = "negbin", offset = all.hours)


salmon = read.csv("flow.catch.csv")
salmon$Year = as.factor(salmon$Year)
salmon$YN = NA
salmon$YN[which(salmon$all.catch == 0)] = 0
salmon$YN[which(salmon$all.catch >= 1)] = 1

m2 = glm(YN~flow.avg + Week + Year, data = salmon, family = "binomial")
summary(m2)
visreg(m2, type = "contrast")
