# April 5th, 2022.
# Analysis script for the foreground motion experiment - CLEANING DATA PART. 


# load libraries in bulk
x<-c("ggpubr", "ggplot2", "multcomp", "pastecs", "tidyr","dplyr", "ggiraph", "ggiraphExtra", "plyr", 
     "covreg", "plot3D", "Hmisc", "corrplot", "psych", "tidyverse", "hrbrthemes", "viridis", "gapminder",
     "ggExtra", "scatterplot3d", "reshape2", "rlang", "plyr", "data.table")

require(x)
lapply(x, require, character.only = TRUE)

# locate the data and import it
setwd("/Users/gorkem.er/Desktop/21Projects/Single_FG_Motion")
d = read.csv("fgmdata_Ready.csv",header=TRUE, quote="\"") 


# IMPORTANT: order the list by trial_participant_id before new id# allocation. Because
# we registered the data simultaneously  



#### Add subject IDs starting at "1" ####
# e.g., subIDs will be o1, o2, etc.

fgmdata <- fgmdata[order(fgmdata$sub),]

sub = fgmdata$sub[1]
number = 1



for (i in 1:nrow(fgmdata)) {
  if (fgmdata$sub[i] != sub) {
    print(i)
    number = number+1
    sub = fgmdata$sub[i]
  }
  fgmdata$id[i] = paste("o",number, sep = "")
}



#### RT ####

# plot a histogram of RTs with mean line
h<-ggplot(fgmdata, aes(x=rt)) + 
  geom_histogram(color="black", fill="gray") +
  geom_vline(aes(xintercept=mean(rt)),
             color="blue", linetype="dashed", size=1) +
  xlim(-1,15000) +
  ggtitle("RTs")
h

range(fgmdata$rt) # some of the longer rts are absurd
outlierRT = mean(fgmdata$rt)+2.5*(sd(fgmdata$rt)) # identify outlier rt cutoff
outlierRT

#### Data Clean with LM ####

#cannot do it because not everyone has same-same trials
#check with this aggregated function

test <- aggregate(pairAR_sameCondition~id, data.Identicals, sum)

# find the conditions where pairs had the identical AR 
singSlopes= array(data = NA,dim=c(length(unique(data$id)),3)) #empty 74x3 array
nS = length(unique(data$id)) #n iterations for the loop below. Should be 74 unique ids
data_onlyEqualPairs <- subset(data, data$pairAR_sameCondition==1)

#then look for regression btw response AR (DV) and cued AR (IV) for each id
for (s in 1:nS) {
  id = s
  name = paste("o",id,sep = "") 
  sing.Data_slopes <- subset(data_onlyEqualPairs, data_onlyEqualPairs$id==name) #pull up data from one observer. 
  res <- lm(sing.Data_slopes$responseAR ~ sing.Data_slopes$cuedAR, data = sing.Data_slopes) #run correl on that person's data
  singSlopes[s,1] = name #add correlation results to array
  singSlopes[s,2] = res$coefficients[2]              #adding the beta coefficient (the slope) of the regression
  singSlopes[s,3] = summary(res)$coefficients[,4][2] #adding the p-values of the regression coefficients
}

# plot slope coefficients and p-values
plot(singSlopes[,2], singSlopes[,3], main="Slope Coefficient values: Individuals",
     xlab="Slope Coeff", ylab="P-value", pch=19)

# Add individual slopes results to main data
for (i in 1:nrow(data)) {
  #find the row in correlation array with current number is subject
  id = data$id[i]
  r = which(singSlopes == id)
  data$slopeCoeff[i] <- singSlopes[r,2]
  data$slopePval[i] <- singSlopes[r,3]
}
subset(data, data$id=="o63") #check one person's data against correlation array

# New data set containing only Participants with significant reg slop btw response & cuedAR
data.Clean_lm <- subset(data, data$slopeCoeff>0.2) ## 61/72 stays

cleanScatter <- ggplot(distBtwShapesPerId, aes(x = as.numeric(row.names(distBtwShapesPerId)), y = distBtwShapes)) + 
  geom_point(shape=19, size=3, alpha=0.1, show.legend = FALSE) +
  geom_smooth(data = distBtwShapesPerId, method=lm, color = "red") +
  ylim(-.7,.7) +
  labs(x="Cued aspect ratio", y = "Reported aspect ratio") +
  ggtitle("Cleaned data")
cleanScatter + geom_density_2d(color="blue", alpha=0.2)

unique(data.Clean_lm$id) #this is the resultant id list
plot(data.Clean_lm$distBtwShapes) #I also included exclusion list by distBtwShapes


#subset the data using only the people who had appropriate inter shape distance
data.Clean_lm_distShapeExcl <- subset(data.Clean_lm,data.Clean_lm$distBtwShapes >= 200 & data.Clean_lm$distBtwShapes <= 600)

# get the ids with the appropriate inter-shape distances
length(unique(data.Clean_lm_distShapeExcl$id))

plot(peopleWithCorrDist$distBtwShapes)
#42/72 remained after 1) cleaning & 2) shapeDist elimination

# output the csv externally 
write.csv(data, "untouchedData.csv", row.names = FALSE)
write.csv(data.Clean_lm,"fgm_cleanData_lm.csv", row.names = FALSE)

