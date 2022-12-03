summlibrary("scatterplot3d") # load
x<-c("ggpubr", "ggplot2", "multcomp", "ggpubr", "pastecs", "tidyr","dplyr", "ggiraph", "ggiraphExtra", "plyr", 
     "covreg", "plot3D", "Hmisc", "corrplot", "psych", "tidyverse", "hrbrthemes", "viridis", "gapminder",
     "ggplot2", "ggExtra", "ggMarginal", "scatterplot3d" )

require(x)
lapply(x, require, character.only = TRUE)

### Load libraries ###
# data formatting
library(reshape2); library(rlang); library(data.table); library(plyr)
# statistics
library(psych); library(pastecs)
# regression
library(lme4); library(car); library(ggpubr)
# graphing
library(ggplot2); library(Hmisc)
# data wrangling
library(dplyr)
library("reshape2")

##### LOCATING THE DATA ##### 
##setwd("~/Desktop/Motion_finalData")
d = read.csv("data/Motion_finalData_01.csv",header=TRUE, quote="\"") 
d = subset(d, !(d$trial_participant_id==57746 | d$trial_participant_id==141906))# remove id 57746, 141906, because it is me, running tests, I found out about that because it has so few trials 
#d$distBtwShapesPerId <- aggregate(distBtwShape2~trial_participant_id, d, mean)

d$distBtwShapes <- ifelse(d$aperture_configuration==0, abs(d$aperture1_center_y - d$aperture2_center_y), abs(d$aperture1_center_x - d$aperture2_center_x) )#this is when organization is vertical, getting y values

# check distance between shapes per id #
distBtwShapesPerId <- aggregate(distBtwShapes~trial_participant_id, d, mean)
#find people who have less than 400 and higher than 500 distance between shapes
peopleWithCorrDist <- subset(distBtwShapesPerId,distBtwShapesPerId$distBtwShapes >= 350 & distBtwShapesPerId$distBtwShapes <= 522)
peopleWithLongerDist <- subset(distBtwShapesPerId,distBtwShapesPerId$distBtwShapes<=349 | distBtwShapesPerId$distBtwShapes>= 523)

#agg <- aggregate(list(d$trial_num), by = list(d$trial_participant_id), sum)


data <- d[c("rt","aperture_configuration", "cueType","ellipse1_move_direction",
            "ellipse2_move_direction", "selected_ellipse_logAR", "cued_ellipse_logAR" ,"uncued_ellipse_logAR",
            "coherence_level",
            "trial_num","round_number","trial_participant_id", "distBtwShapes")]

# change column names
nms <- c("rt", "shape_org", "cued_ellipse", "e1_motion_dir", "e2_motion_dir", "responseAR","cuedAR", "uncuedAR", "coherence", "trial_number", "round_number", "subNum", "distBtwShapes")
setnames(data, nms)

# reorder columns for ease of eyeballing the data 
data <- data[,c("subNum","distBtwShapes","trial_number", "cuedAR", "uncuedAR", "responseAR","rt","shape_org", "cued_ellipse", "e1_motion_dir", "e2_motion_dir", "round_number", "coherence")]

# remove na in r - remove rows - na.omit function / option
data <- na.omit(data)

#making characters numeric
data <- mutate_all(data, function(x) as.numeric(as.character(x)))


# What specific direction is cued shape moving?
# cued motion direction [cued_motDir]: 0, 90, 180, 270                
# populate cued_motion direction column -- use "cued_ellipse" and e1/e2 motion directions
data$cued_motion_dir <- ifelse(data$cued_ellipse == 1, data$e1_motion_dir, data$e2_motion_dir)

# re-code global_org so that "1" indicates vertical organization
data$pairGlobalOrg <- (data$shape_org-1)*-1

# Are the two shapes moving in the same direction?
# motion direction same? [motDir_same]: 1 or 0                        
# (here, they must both be 90, or both 180, etc.)
# populate motion direction "same" column 
data$pairMotSame <- ifelse(data$e1_motion_dir == data$e2_motion_dir, 1, 0)

# What's the difference between the response and the cued shape?
# responseAR-cuedAR
# pos value = "tall" error
data$responseError <- data$responseAR-data$cuedAR

# Are the two shapes in the same category of aspect ratio? i.e., both tall?
data$sameARCat <- ifelse(data$cuedAR < -0.00000048  & data$uncuedAR < -0.00000048 | data$cuedAR > -0.00000048 & data$uncuedAR > -0.00000048 | data$cuedAR == data$uncuedAR, 1, 0)

#tag identical pairs 
data$pairAR_sameCondition <- ifelse(data$cuedAR == data$uncuedAR, 1, 0)


#### Add subject IDs starting at "1" ####
# e.g., subIDs will be o1, o2, etc.

data <- data[order(data$subNum),]

sub = data$subNum[1]
number = 1

# IMPORTANT: order the list by trial_participant_id before new id# allocation. Because
# we registered the data simultaneously  

for (i in 1:nrow(data)) {
  if (data$subNum[i] != sub) {
    print(i)
    number = number+1
    sub = data$subNum[i]
  }
  data$id[i] = paste("o",number, sep = "")
}


#### RT ####

# histogram of rts with mean line
h<-ggplot(data, aes(x=rt)) + 
  geom_histogram(color="black", fill="gray") +
  geom_vline(aes(xintercept=mean(rt)),
             color="blue", linetype="dashed", size=1) +
  xlim(-1,15000) +
  ggtitle("RTs")
h

# histogram of rts with mean line
h<-ggplot(d, aes(x=rt)) + 
  geom_histogram(color="black", fill="gray") +
  geom_vline(aes(xintercept=mean(rt)),
             color="blue", linetype="dashed", size=1) +
  xlim(-1,15000) +
  ggtitle("RTs")
h

range(data$rt) # some of the longer rts are absurd
outlierRT = mean(data$rt)+2.5*(sd(data$rt)) # identify outlier rt cutoff


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

