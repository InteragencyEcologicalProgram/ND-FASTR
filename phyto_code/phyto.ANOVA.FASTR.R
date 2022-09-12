## Calculate ANOVAs for FASTR
##

library("tidyverse");packageVersion("tidyverse")
library("ggpubr");packageVersion("ggpubr")
library("rstatix");packageVersion("rstatix")
library("car");packageVersion("car")
library("visreg");packageVersion("visreg")
library("emmeans");packageVersion("emmeans")

rm(list=ls()) #cleans workspace
theme_set(theme_bw())

# Set working directory
setwd("./FASTR.phyto.final/")
getwd()

## Load biovolume density data at group and genus level
load("RData/phyto.gen.RData")
load("RData/phyto.sum.RData")

## Change Year category to factor
phyto.sum$Year <- as.factor(phyto.sum$Year)
phyto.sum$FlowPulseType <- as.factor(phyto.sum$FlowPulseType)

## Create QQ Plot to check for normality
ggqqplot(log10(phyto.sum$Total.BV.per.L))

## View histogram to check for normality
hist(log10(phyto.sum$Total.BV.per.L))

## Run Shapiro-Wilks test to check whether data is normally distributed
shapiro.test(log10(phyto.sum$Total.BV.per.L))

## Run Levene test to compare variance
phyto.sum %>% levene_test(log10(Total.BV.per.L)~FlowPulseType) # p = 0.901
phyto.sum %>% levene_test(log10(Total.BV.per.L)~Year) # p = 0.980
phyto.sum %>% levene_test(log10(Total.BV.per.L)~Region) # p = 0.0230

## Run 3-way ANOVA
phyto.sum.aov <- phyto.sum %>% anova_test(log10(Total.BV.per.L) ~ Region*Year*ActionPhase)
phyto.sum.aov

## Rosie Code
L1 <- lm(log10(Total.BV.per.L)~ Region*Year + Region*ActionPhase + ActionPhase*Year, data = phyto.sum)

summary(L1)

Anova(L1, type = 2)

visreg(L1)

visreg.AP.Region <- visreg(L1, xvar = "ActionPhase", by = "Year", gg = T)

visreg.AP.Region +
  facet_wrap(Year ~ ., ncol = 3, dir = "h")

ggsave(path="plots",
       filename = "visreg.AP.Region.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=3.5,
       width=7, 
       dpi="print")

visreg(L1, xvar = "Region", by = "Year")
visreg(L1, xvar = "Region", by = "ActionPhase")
visreg(L1, xvar = "ActionPhase", by = "Region")


emmeans(L1, pairwise ~ Region:ActionPhase)
emmeans(L1, pairwise ~ ActionPhase)
emmeans(L1, pairwise ~ Year:ActionPhase)