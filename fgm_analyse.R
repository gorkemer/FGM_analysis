# April 5th, 2022.
# Analysis script for the foreground motion experiment. 
# I've revised the script of SingleFGMot_01_prepare.R script that sits locally on 
# my computer. This is a revised version for the manuscript I'm writing. 
# gorkemer
# April 5th, 2022
# This script aims to analyse the foreground motion experiment and it uses the 
# untouched data file (output of the fgm_prepareData.R)

# clear the workspace
rm(list=ls()) 

# load libraries in bulk
x<-c("ggpubr", "ggplot2", "multcomp", "pastecs", "tidyr","dplyr", "ggiraph", "ggiraphExtra", "plyr", 
     "covreg", "plot3D", "Hmisc", "corrplot", "psych", "tidyverse", "hrbrthemes", "viridis", "gapminder",
     "ggExtra", "scatterplot3d", "reshape2", "rlang", "plyr", "data.table", "lme4", "magrittr", "fitdistrplus",
     "gridExtra", "statmod", "dotwhisker")

require(x)
lapply(x, require, character.only = TRUE)

#remove scientific notation in the entire R session
options(scipen = 100)

# loading data
setwd('~/Desktop/21Projects/Single_FG_Motion')
fgmdata = read.csv('fgmdata.csv', header = TRUE)

# some relevant functions
trunc <- function(x, ..., prec = 0) base::trunc(x * 10^prec, ...) / 10^prec;
# sourcing my own functions
source("~/Documents/GitHub/ger_R_functions/plot_functions.R")


# run a multiple linear regression models on response error

fullModel <- lm(fgmdata$responseError ~ fgmdata$cuedAR + fgmdata$uncuedAR
                * fgmdata$sameDirection1S0D * fgmdata$global_org * fgmdata$coherence)
summary(fullModel)
anova(fullModel)

# aggregate by sub then run the same analysis again

fgmdata.agg <- aggregate(responseError ~ cuedAR + uncuedAR + 
                           sameDirection1S0D + global_org + sub, mean, data = fgmdata)

subject_IDs <- unique(fgmdata.agg$sub)

singleSub <- fgmdata.agg$sub[fgmdata.agg$sub == subject_IDs[1]]

# run the regression model again
fullModel.sub <- lm(fgmdata.agg $responseError ~ fgmdata.agg $cuedAR + fgmdata.agg $uncuedAR
                * fgmdata.agg $sameDirection1S0D * fgmdata.agg $global_org)
summary(fullModel.sub)
anova(fullModel.sub)



#########
  
data <- fgmdata # for now

#box plot
p0<- ggplot(data, aes(uncuedCat, respError, group = as.factor(uncuedCat))) 
p0 + geom_jitter(shape=16, position=position_jitter(0.2), alpha = 0.02) + geom_boxplot()

# Change box plot colors by groups
ggplot(data, aes(x=uncuedCat, y=respError, fill = as.factor(sameMotion))) +
  geom_boxplot() + geom_jitter(shape=16, position=position_jitter(0.2), alpha = 0.02)
#geom_boxplot(position=position_dodge(1))# Change the position

# grouped boxplot
data$uncuedCat = as.character(data$uncuedCat)
data$sameMotion = as.character(data$sameMotion)
ggplot(data, aes(x=uncuedCat, y=respError, fill=sameMotion)) + 
  geom_boxplot()





lmCat <- lm(respError ~ uncuedCatLabel * sameMotion, data)
summary(lmCat)
anova(lmCat)

p1<- plotREF(data, data$uncuedAR, data$respError, as.factor(data$sameMotion)) + facet_wrap(~data$uncuedCat)
p1 + facet_grid(data$globalOrg) + facet_wrap(data$cuedMotDir)


# response AR - raw DV analysis # 
lmCat <- lm(respError ~ uncuedAR * sameMotion, data)
summary(lmCat)
anova(lmCat)


# aggregate by 
data_agg <- aggregate(respError ~ uncuedAR + sameMotion + globalOrg, data, mean)
p2<- ggplot(data_agg, aes(uncuedAR, respError)) + geom_point(aes(colour=sameMotion), shape=15, size=1.5) 
p2

p2<- plotREF(data, data$uncuedAR, data$respError, as.factor(data$sameMotion)) + facet_grid(~data$uncuedCatLabel)
p2

p2 +   stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x) +facet_wrap(data$globalOrg)


# substract each response error of motion same 1) and 2) for each uncuedAR. That
# way I can get the influence of same on the response error for each uncuedAR

for (i in 1:nrow(data_agg)){
  
}

motionSame <- subset(data_agg, data_agg$sameMotion==1)
motionDiff <- subset(data_agg, data_agg$sameMotion==0)
## do it for bgm ##
motionAll <- cbind(motionSame, motionDiff)

test <- motionSame- motionDiff
motionSame$test2 <- motionSame$respError - motionDiff$respError

p2<- ggplot(data_agg, aes(uncuedAR, respError)) + geom_point(aes(colour=sameMotion), shape=15, size=1.5) + facet_wrap(~sameMotion) + facet_grid(~globalOrg)
p2 + geom_bar(stat="identity")
p2 + geom_col(aes(fill = respError < 0), position = "identity") + scale_fill_manual(guide = FALSE, breaks = c(TRUE, FALSE), values=c("gray", "darkred"))



