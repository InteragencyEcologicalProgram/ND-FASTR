#Calcluate Biomass

library(tidyverse)
library(readxl)

#Import the data
NDzoops = read_excel("Zoop_code/LT_zoop_RAW.xlsx", na = "NA")

#Combine taxon and life stage
NDzoops = mutate(NDzoops, Taxlife = paste(TaxonName, LifeStage))

#import list of biomass conversions
#This will give the approximate carbon weight, in micrograms,
#of each taxon based on literature values for the average weight of the
#taxon or its closest relative. These are the same values used by EMP and
#other monitoring programs in the SFE.
biomass = read.csv("Zoop_code/Biomass.csv")

NDzoops = merge(NDzoops, biomass[,c(3,4)])

NDzoops = mutate(NDzoops, BPUE = CarbonWeight*CPUEZoop)
