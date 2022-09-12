## Calculate Standard Error for Total Phytoplankton Biomass
## FASTR Synthesis Project
## 6/8/2022 TMF

#library("tidyverse");packageVersion("tidyverse")
library("Rmisc");packageVersion("Rmisc")

# Set working directory
setwd("./FASTR.phyto.final/")
getwd()

# Clean workspace
rm(list=ls()) 

## Load total biovolume data for FASTR project
load("RData/phyto.sum.RData")
load("RData/phyto.grp.BV.RData")

## Calculate standard error for total phyto BV
phyto.grp.sum.error <- summarySE(phyto.sum, 
                         measurevar="Total.BV.per.L", 
                         groupvars=c("Year","StationCode","ActionPhase"))

## Calculate standard error for group-level phyto BV
phyto.grp.error <- summarySE(phyto.grp.BV, 
                                 measurevar="BV.um3.per.L", 
                                 groupvars=c("Year","ActionPhase","Region","Group"))

## Save RData File
save(phyto.grp.sum.error, file = "RData/phyto.grp.sum.error.RData")

## Save as CSV file
#write.csv(phyto.grp.error, file = "phyto.grp.error.csv")
