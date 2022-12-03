# gorkemer
# 15 August 
# analyze on the cleaned data (output of the prepareData_01.R)
# extras: #colnames(group_mean) <- c("Mean Response AR", "CuedAR", "id")
### Load libraries ###

library(reshape2); library(rlang); library(data.table); library(plyr) # data formatting
library(psych); library(pastecs) # statistics
library(lme4); library(car); library(ggpubr); library(corrplot); library(magrittr) # regression
library(ggplot2); library(Hmisc) # graphing
library(dplyr) # data wrangling
library("reshape2");
library(fitdistrplus); library(MASS); library(survival); library(npsurv) ; library(lsei)
library(gridExtra)
library(extrafont)
font_import("Trebuchet MS")
library(actuar)
library(statmod)
library(dotwhisker)

source("../plot_functions.R")

rm(list=ls()) # CLEAR THE WORKSPACE

trunc <- function(x, ..., prec = 0) base::trunc(x * 10^prec, ...) / 10^prec;

#remove scientific notation in the entire R session
options(scipen = 100)

setwd('~/Desktop/21Projects/Single_FG_Motion')

data = read.csv('motion_RA.csv', header = TRUE)

data$uncuedCat = ifelse(data$uncuedAR < -0.00000048, -1, ifelse(data$uncuedAR==-0.00000048, 0, 1))
data$uncuedCatLabel = ifelse(data$uncuedAR < -0.00000048, "flat", ifelse(data$uncuedAR==-0.00000048, "circle", "tall"))


# 13 Agu 22
cols = rgb(red = 1, green = 0, blue = 0)
plot(data$responseAR, data$cuedAR, main= "Response AR as a function of cued AR (paired with same)", 
     ylab = "Response AR", xlab = "Cued AR",
     col = alpha(cols, 0.005), pch=16)

#do it per participant
number_of_sub <- unique(data$trial_participant_id)
test <- aggregate(data$responseAR ~ data$trial_participant_id + data$cuedAR + data$sameMotion, data = data, mean)
ggplot(test, aes(`data$cuedAR`, `data$responseAR`)) + geom_point(aes(colour= as.factor(`data$trial_participant_id`)), shape=15, size=1.5) 

#do it per participant per average
number_of_sub <- unique(data$trial_participant_id)
test <- aggregate(data$responseAR ~ data$cuedAR + data$sameMotion, data = data, mean)
ggplot(test, aes(`data$cuedAR`, `data$responseAR`)) + geom_point(aes(colour= as.factor(`data$sameMotion`)), shape=15, size=1.5) 

stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x) +facet_wrap(data$globalOrg)


# do it for taller & flatter uncued
test <- aggregate(data$responseAR ~ data$cuedAR + data$sameMotion + data$uncuedAR, data = data, mean)
ggplot(test[test$`data$uncuedAR`>0,], aes(`data$cuedAR`, `data$responseAR`)) + geom_point(aes(colour= as.factor(`data$sameMotion`)), shape=15, size=1.5) 
ggplot(test[test$`data$uncuedAR`<0,], aes(`data$cuedAR`, `data$responseAR`)) + geom_point(aes(colour= as.factor(`data$sameMotion`)), shape=15, size=1.5) 
ggplot(test[test$`data$uncuedAR`== test$`data$cuedAR`,], aes(`data$cuedAR`, `data$responseAR`)) + geom_point(aes(colour= as.factor(`data$sameMotion`)), shape=15, size=1.5) 

#get the mean
mean(test$`data$responseAR`[test$`data$uncuedAR`>0 & test$`data$sameMotion` == 1])
mean(test$`data$responseAR`[test$`data$uncuedAR`>0 & test$`data$sameMotion` == 0])

mean(test$`data$responseAR`[test$`data$uncuedAR`<0 & test$`data$sameMotion` == 1])
mean(test$`data$responseAR`[test$`data$uncuedAR`<0 & test$`data$sameMotion` == 0])

#check the RE
data$cuedCat <- ifelse(data$cuedAR > -0.00000048, 1, ifelse(data$cuedAR == data$uncuedAR, 0, -1) )
data$catChange = ifelse(data$uncuedCat != data$cuedCat, 1, 0)
table(data$catChange)
test <- aggregate(data$respError ~ data$arDiff + data$sameMotion + data$catChange, data = data, mean)
#category change'i ele al. if cuedCat != uncued, catChange = 1, otherwise = 0

# run the plot on catChange == 0 only
ggplot(test[test$`data$arDiff`>0 & test$`data$catChange` == 0,], aes(`data$arDiff`, `data$respError`)) + geom_point(aes(colour= as.factor(`data$sameMotion`)), shape=15, size=1.5) 
ggplot(test[test$`data$arDiff`>0 & test$`data$catChange` == 1,], aes(`data$arDiff`, `data$respError`)) + geom_point(aes(colour= as.factor(`data$sameMotion`)), shape=15, size=1.5) 

ggplot(test[test$`data$arDiff`>0,], aes(`data$arDiff`, `data$respError`)) + geom_point(aes(colour= as.factor(`data$sameMotion`)), shape=15, size=1.5) 
ggplot(test[test$`data$arDiff`== 0,], aes(`data$arDiff`, `data$respError`)) + geom_point(aes(colour= as.factor(`data$sameMotion`)), shape=15, size=1.5) 

#color by grouping
test <- aggregate(data$responseAR ~ data$trial_participant_id + data$cuedAR + data$sameMotion, data = data, mean)
ggplot(test, type="l", aes(`data$cuedAR`, `data$responseAR`)) + geom_point(aes(colour= as.factor(`data$sameMotion`)), shape=15, size=1.5, alpha = 1/60) 

#only identical-pairs
test <- aggregate(data$responseAR ~ data$cuedAR + data$sameMotion + data$sameAR, data = data, mean)
ggplot(test[test$`data$sameAR`==1,], aes(`data$cuedAR`, `data$responseAR`, color = as.factor(`data$sameMotion`), group = as.factor(`data$sameMotion`))) + geom_point(aes(colour= as.factor(`data$sameMotion`)), shape=15, size=1.5, alpha = 30/60) +
  geom_line()

# taller uncued
test <- aggregate(data$responseAR ~  data$sameMotion + data$sameAR + data$uncuedAR , data = data, mean)
ggplot(test[test$`data$uncuedAR`>0 & test$`data$sameAR` == 1,], aes(`data$uncuedAR`, `data$responseAR`, color = as.factor(`data$sameMotion`), group = as.factor(`data$sameMotion`))) + geom_point(aes(colour= as.factor(`data$sameMotion`)), shape=15, size=1.5, alpha = 30/60) +
  geom_line()


# diff uncued
test <- aggregate(data$responseAR ~  data$sameMotion + data$sameAR + data$uncuedAR , data = data, mean)
ggplot(test[test$`data$uncuedAR`>0,], aes(`data$uncuedAR`, `data$responseAR`, color = as.factor(`data$sameMotion`), group = as.factor(`data$sameMotion`))) + geom_point(aes(colour= as.factor(`data$sameMotion`)), shape=15, size=1.5, alpha = 30/60) +
  geom_line()

data = data[data$trial_participant_id]
plot(data$responseAR, data$cuedAR, main= "Response AR as a function of cued AR (paired with same)", 
     ylab = "Response AR", xlab = "Cued AR",
     col = alpha(cols, 0.005), pch=16, data = data)


#1- is motion same attracts the judgment? I'm going to flip them to the same side
data$uncuedAR_norm <- abs(data$uncuedAR)
table(data$uncuedAR_norm)
data$respError_norm <- abs(data$respError)
table(data$respError_norm)
data$cuedAR_norm <- abs(data$cuedAR)
table(data$cuedAR_norm)

plot(x= data$uncuedAR_norm, y = data$respError_norm, col = alpha(cols, 0.1))
ggplot(data, aes(uncuedAR_norm, respError_norm, color = as.factor(sameMotion), group = as.factor(sameMotion))) + geom_point(aes(colour= as.factor(sameMotion)), shape=15, size=1.5, alpha = 2/60) +
  geom_smooth(method='lm', formula= y~x) + facet_grid(~globalOrg + data$sameARCat)+
  geom_jitter(alpha = 1/60)+
  coord_cartesian(ylim=c(0, 0.3))

# start
number_of_param <- 4
temp_fgmdata = data.frame(matrix(ncol = number_of_param, nrow = 1))
colnames(temp_fgmdata) <- c('-1', '0', '1', 'id')
colnames(temp_fgmdata)
colnames(data_wide)

# removing 141906, this person had no uncuedCat of 0
data<- data[!(data$trial_participant_id==141906),]
remove <- c(141906)
number_of_sub <- number_of_sub [! number_of_sub %in% remove]

for (i in 1:length(number_of_sub)){
  print(number_of_sub[i])
  tmpdata <- data[data$trial_participant_id== number_of_sub[i],]
  for (r in 1:nrow(tmpdata)){
    sub_data <- aggregate(responseAR ~ sameMotion + uncuedCat, data = tmpdata, mean)
  }
  #sub_data$id <- number_of_sub[i]
  sub_data_rs <- reshape(sub_data, idvar = "uncuedCat", timevar = "sameMotion", direction = "wide")
  sub_data_rs$diff <- sub_data_rs[,"responseAR.1"] - sub_data_rs[,"responseAR.0"] # find the difference, test of motion same
  sub_data_rs <- sub_data_rs[,c("uncuedCat","diff")] # only take the uncuedCat and diff
  data_wide = spread(sub_data_rs, uncuedCat, diff)
  data_wide$id <- number_of_sub[i]
  temp_fgmdata <- rbind(temp_fgmdata,data_wide)
  data_wide <- NA
}
#removing first row
temp_fgmdata <- temp_fgmdata[-1,]
mean(temp_fgmdata$`1`)
mean(temp_fgmdata$`-1`)

plot(temp_fgmdata$`0` ~ id, data = temp_fgmdata)
abline(0,0)

plot(temp_fgmdata$`1` ~ id, data = temp_fgmdata)
abline(0,0)

plot(temp_fgmdata$`-1` ~ id, data = temp_fgmdata)
abline(0,0)


test <- melt(temp_fgmdata, measure.vars = c("-1","0","1"), variable.name = "grouping", value.name = "value")
head(test)

head(temp_fgmdata)
#last: make the absolute value
test_2 <- test[test$grouping==1 | test$grouping==3,]

test$grouping <- as.numeric(test$grouping)
ggplot(test[test$grouping==1 | test$grouping==3,] %>% mutate(x = jitter(grouping), y = jitter(value)), 
       aes(x = grouping, y = value, col = as.factor(id))) + 
  geom_point() + 
  geom_line()

abs(mean(test[test$grouping==1]$value)) + abs(mean(test[test$grouping==3]$value))
# change is 0.01458395 / 0.47 = around 3% :D 

mean(test[test$grouping==3]$value, na.rm = TRUE) 
mean(test[test$grouping==2]$value, na.rm = TRUE) 

ggplot(test, aes(x=id, y=value)) + 
  geom_point(aes(col=grouping), size=3) +
  geom_point() + 
  geom_line(aes(col=grouping))+
  facet_grid(~globalOrg)

library(ggplot2)
ggplot(test) + 
  geom_jitter(aes(x=grouping, y=value, color = as.factor(id)))+
  geom_line(aes(x=grouping, y=value, color = as.factor(id))) + 
  geom_point() + 
  geom_line()


##### long to wide using spread() function of tidyr package
sub_data_rs <- sub_data_rs[,c(1,4)]
library(tidyr)
data_wide = spread(sub_data_rs, uncuedCat, diff)
data_wide





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



