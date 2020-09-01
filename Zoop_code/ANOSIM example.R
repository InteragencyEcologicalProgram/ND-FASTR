#set working directory
setwd("C:/Users/mbedwell/OneDrive - California Department of Water Resources/info for studies/Fall Flow Study/synthesis/ANOSIM example")

#call packages
library(tidyverse)
library(vegan)

#bring in data
diet_complete3<- read_csv("diet_complete3.csv")


#flip dataframe and perform nmds and ANOSIM on taxa

##using info from: https://jkzorz.github.io/2019/06/06/NMDS.html and https://jkzorz.github.io/2019/06/11/ANOSIM-test.html

#reorder the "location" factor
diet_complete3$Location = factor(diet_complete3$Location, levels = c("RVR", "DWSC"))

#make a new variable that combines location and mesh
#we'll make "location" into a number, RVR = 1, DWSC = 2
diet_complete3$l.m.c = paste(as.numeric(diet_complete3$Location), diet_complete3$Cage, diet_complete3$Mesh)

#remove the old values
diet_complete3$Location <- NULL
diet_complete3$Mesh <- NULL
diet_complete3$Cage <- NULL

#flip data frame so that Prey.Taxa is the column names, mean.count is the value, and location.mesh are the row names
all.flip <- spread(data=diet_complete3, key = Prey.Taxa, value= mean.count)

#convert "l.m.c" back to "location and "mesh" and "cage"
all.flip$Location <- factor(substr(all.flip$l.m.c, 1,1), levels = c(1,2), 
                            labels = c("RVR", "DWSC"))
all.flip$Cage <- substr(all.flip$l.m.c, 3, 4)
all.flip$Mesh <- substr(all.flip$l.m.c, 5, 9)

#extract columns with abundance info
diet = all.flip[2:53]

#change to matrix for vegan
m.diet <- as.matrix(diet)

#run metaMDS command - use bray as the distance measure (Bray-Curtis takes into account speices presence/absence as well as abundance - other distance measures often only account for presence/absence)
nmds <- metaMDS(m.diet, distance = "bray")
plot(nmds)

treat=c(rep("RVR",3),rep("DWSC",3))
ordiplot(nmds, type="n")
ordihull(nmds,groups=treat,draw="polygon",col="grey90",label=F)
orditorp(nmds,display="species",col="red",air=0.01)
orditorp(nmds,display="sites",col=c(rep("green",5),rep("blue",5)),
         air=0.01,cex=1.25)

#extract NMDS scores (x and y coordinates)
data.scores = as.data.frame(scores(nmds))

#add columns to data frame 
data.scores$Location = all.flip$Location
data.scores$Mesh = all.flip$Mesh
data.scores$Cage = all.flip$Cage

#plot
ggplot(data.scores, aes(x=NMDS1, y=NMDS2)) +
  geom_point(size=5, aes(shape=Location, colour=Mesh))

#ANOSIM
ano <- anosim(m.diet, all.flip$Mesh, distance="bray")
ano #more seperation within mesh types than between them - would probably be better to break site out prior to testing

ano <- anosim(m.diet, all.flip$Location, distance="bray")
ano #high seperation between locations

#"The ANOSIM statistic compares the mean of ranked dissimilarities between groups to the mean of ranked dissimilarities within groups. An R value close to "1.0" suggests dissimilarity between groups while an R value close to "0" suggests an even distribution of high and low ranks within and between groups. R values below "0" suggest that dissimilarities are greater within groups than between groups. See Clarke and Gorley (2001) for a guide to interpreting ANOSIM R values."

#by site -----------------------------------
#split into RV and DWSC
split<- split(all.flip, all.flip$Location)
RVR <- split[["RVR"]]
DWSC <- split[["DWSC"]]

#remove location columns
RVR$Location <- NULL
DWSC$Location <- NULL

#extract columns with abundance info
diet.RVR = RVR[2:53]
diet.DWSC = DWSC[2:53]

#change to matrix for vegan
r.diet <- as.matrix(diet.RVR)
d.diet <- as.matrix(diet.DWSC)

#run metaMDS
r.nmds <- metaMDS(r.diet, distance = "bray")
plot(r.nmds)

d.nmds <- metaMDS(d.diet, distance = "bray")
plot(d.nmds)

#extract NMDS scores (x and y coordinates)
r.data.scores = as.data.frame(scores(r.nmds))

d.data.scores = as.data.frame(scores(d.nmds))

#add columns to data frame 
r.data.scores$Mesh = RVR$Mesh
r.data.scores$Cage = RVR$Cage

d.data.scores$Mesh = DWSC$Mesh
d.data.scores$Cage = DWSC$Cage

#plot
ggplot(r.data.scores, aes(x=NMDS1, y=NMDS2)) +
  geom_point(size=5, aes(shape=Mesh, colour=Cage))

ggplot(d.data.scores, aes(x=NMDS1, y=NMDS2)) +
  geom_point(size=5, aes(shape=Mesh, colour=Cage))

#ANOSIM
r.ano <- anosim(r.diet, RVR$Mesh, distance="bray")
r.ano #low seperation between mesh types

d.ano <- anosim(d.diet, DWSC$Mesh, distance="bray")
d.ano #low seperation between mesh types

