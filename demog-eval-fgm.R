# script for evaluting demographics of bgm experiment
# last edit/CREATION: Late Agu 2022
# gorkemer

# last edit: Late Agu 2022

#### 0- load libraries

# gorkemer
# 5 August 
# analyze on the cleaned data (output of the prepareData_01.R)
# extras: #colnames(group_mean) <- c("Mean Response AR", "CuedAR", "id")
### Load libraries ###
library(reshape2); library(rlang); library(data.table); library(plyr) # data formatting
library(psych); library(pastecs) # statistics
library(lme4); library(car); library(ggpubr); library(corrplot); library(magrittr) # regression
library(ggplot2); library(Hmisc) # graphing
library(dplyr) # data wrangling
library("reshape2");library("plot3D")library(extrafont)library("scatterplot3d")
library(fitdistrplus); library(MASS); library(survival); library(npsurv) ; library(lsei)
library(actuar)library(statmod)
library(Hmisc)library(broom)library(quickpsy)library(tidyverse)

# clear the workspace
rm(list=ls()) 

# set wd if needed setwd('~/Desktop/background-motion/Data Analysis'), check with getwd()
setwd('~/Desktop/21Projects/Single_FG_Motion')
#### 0- Select Data ####

d = read.csv('demographics-fgm.csv', header=TRUE) ## same-different response test run without random motion and now it is just L-R
d2 = read.csv('demographics-fgm2.csv', header=TRUE) 
#remove width or height with value of 2414
#17952, 19101, 19198, 19210 are empty participants. The reall participant number is:
data = d2
#change some column names
colnames(data)[which(names(data) == "Q4")] <- "age"
ageRange <- na.omit(as.numeric(data$age))
#get the age range
range(ageRange)
colnames(data)[which(names(data) == "Q5")] <- "gender"
fgm_demog <- data[,c("gender", "id")] #to-be used for the manuscript
head(fgm_demog)
fgm_demog <- fgm_demog[-1,] # remove first column-row from the table. 
fgm_demog$gender <- as.factor(fgm_demog$gender) 
# get the first row a duplicated ids
head(fgm_demog)
fgm_demog <- subset(fgm_demog,duplicated(fgm_demog$id)|!duplicated(fgm_demog$id,fromLast=TRUE))
fgm_demog <- subset(fgm_demog,duplicated(fgm_demog$id)|!duplicated(fgm_demog$id,fromLast=TRUE))
fgm_demog <- subset(fgm_demog,duplicated(fgm_demog$id)|!duplicated(fgm_demog$id,fromLast=TRUE))
fgm_demog <- subset(fgm_demog,duplicated(fgm_demog$id)|!duplicated(fgm_demog$id,fromLast=TRUE))
fgm_demog <- subset(fgm_demog,duplicated(fgm_demog$id)|!duplicated(fgm_demog$id,fromLast=TRUE))
head(fgm_demog)
#### copied from bgm_analyse.R script ####
# load bgm data 
fgmdata = read.csv('fgmdata.csv', header = TRUE)
number_of_sub <- unique(fgmdata$sub)
# iterate all and get the beta coefficient
fgmdata.globalTrialN <- data.frame(matrix(ncol = 1, nrow = length(number_of_sub)))
for (s in 1:length(number_of_sub)){
  print(s)
  tmpdata <- fgmdata[fgmdata$sub == number_of_sub[s],]
  fgmdata.globalTrialN[s,1] <- nrow(tmpdata)# check the n of each participant
  fgmdata.globalTrialN[s,2] <- number_of_sub[s]
}
colnames(fgmdata.globalTrialN) <- c("trialN","id")
head(fgmdata.globalTrialN)
incompletedPeople_global <- fgmdata.globalTrialN$id[fgmdata.globalTrialN$trialN<241]
plot(fgmdata.globalTrialN[,2], fgmdata.globalTrialN[,1])
abline(h = 241)
incompletedPeople_global # equals to incompletedPeople in fgmdata, nice! 
fgmdata <- fgmdata[!( (fgmdata$sub %in% incompletedPeople_global)),]
fgmdata <- fgmdata[!(fgmdata$sub == 18667),] # because 0.53 corr\
length(unique(fgmdata$sub))
#matching demog and data used in the manuscript
#bgm_demog <- bgm_demog[-which(d$id == ""),]
# there is cell with empty id value, removing it
fgm_demog$id <- as.numeric(fgm_demog$id)
for (i in 1:length(unique(fgm_demog$id))){
  data_id  <- unique(fgmdata$sub)[i]
  demog_id <- unique(fgm_demog$id)[i]
  print(data_id)
  print(demog_id)
  #search this id in the data and if there is one, put check
  fgm_demog[i,"exists_in_data"] = ifelse(demog_id %in% as.numeric(unique(fgmdata$sub)), "exists", "dontExists")
}
head(fgm_demog)
table(fgm_demog$exists_in_data)
# counting blank people to "prefer not to say"
table(fgm_demog$gender)
table(fgm_demog$gender[1:69]) # counting this
