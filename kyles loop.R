
#data generation
#create a data frame of n = 30 values of 10 levels in 3 groups
#vector of 10 levels aka "stations"
x <- rep(seq(1:10),times = 3)

#response variable "y" aka "counts"
y <- x+rnorm(30)

#three group levels aka "years"
grp <- rep(1:3,each=10)

#data frame them together
dat_ex <- data.frame(cbind(x,y,grp))

#create a data frame of a second set of grouped data with "z" a random variable close to 1, aka "volume"
# group levels are 1-3 here too
z <- rnorm(3,1,0.1)
grp2 <- seq(1:3) 
dat_2 <- data.frame(cbind(grp2,z))

#single iteration of what I want a loop to do:

#step 1: split the data set into groups: "i" & "not i", i = group 1 in this iteration 
dat_i <- dat_ex[dat_ex$grp == "1",]
dat_not_i <- dat_ex[dat_ex$grp != "1",] 

#sum the response variable by level of x for data in groups other than "i" (i = 1, not_i = 2,3)
agg_dat_not_i <- aggregate(data=dat_not_i,y~x,FUN = "sum")

#modify the summed y variable by dividing by the sum of z for the 
#corresponding groups from the second data set (grp2 groups 2 & 3)

agg_dat_not_i$y2 <- agg_dat_not_i$y/sum(dat_2$z[which(dat_2$grp2 != 1)])

#do a linear regression on the tranformed data by the levels in x
lm1 <- lm(data=agg_dat_not_i,y2~x)
summary(lm1)

#modify the y variable in group i with the corresponding value for group i
dat_i$y_prime <- dat_i$y/dat_2$z[which(dat_2$grp2 == "1")]

#add the fitted values generate from the linear regression to the 
#corresponding rows in the index data frame

dat_i$fitted <- fitted.values(lm1)

#create a vector of the differences of the y' values from the index data set with the fitted data set
dat_i$delta <- dat_i$y_prime-dat_i$fitted

#sum the absolute value of the deviations as a lack-of-fit index
grp1_dev<-sum(abs(dat_i$delta))

#once this algorithm is loopified, 
#I'd like the generated value to be added to a vector with a value for each group level

#######################################################################
#Rosei's test
input = unique(dat_ex$grp)
output = 1:length(input)

for(i in 1:length(output)) {
  
  #step 1: split the data set into groups: "i" & "not i", i = group 1 in this iteration 
  dat_i <- dat_ex[dat_ex$grp == input[i],]
  dat_not_i <- dat_ex[dat_ex$grp != input[i],] 
  
  #sum the response variable by level of x for data in groups other than "i" (i = 1, not_i = 2,3)
  agg_dat_not_i <- aggregate(data=dat_not_i,y~x,FUN = "sum")
  
  #modify the summed y variable by dividing by the sum of z for the 
  #corresponding groups from the second data set (grp2 groups 2 & 3)
  
  agg_dat_not_i$y2 <- agg_dat_not_i$y/sum(dat_2$z[which(dat_2$grp2 != input[i])])
  
  #do a linear regression on the tranformed data by the levels in x
  lm1 <- lm(data=agg_dat_not_i,y2~x)
  summary(lm1)
  
  #modify the y variable in group i with the corresponding value for group i
  dat_i$y_prime <- dat_i$y/dat_2$z[which(dat_2$grp2 == input[i])]
  
  #add the fitted values generate from the linear regression to the 
  #corresponding rows in the index data frame
  
  dat_i$fitted <- fitted.values(lm1)
  
  #create a vector of the differences of the y' values from the index data set with the fitted data set
  dat_i$delta <- dat_i$y_prime-dat_i$fitted
  
  #sum the absolute value of the deviations as a lack-of-fit index
  output[i]<-sum(abs(dat_i$delta))
  
}
