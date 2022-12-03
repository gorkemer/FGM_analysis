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
library(Rmisc)
library(tidyr)
#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/ggpubr")
library(ggpubr)
library(nlme)
library(lmerTest)

#remove scientific notation in the entire R session
options(scipen = 100)
source("../plot_functions.R")

rm(list=ls()) # CLEAR THE WORKSPACE

trunc <- function(x, ..., prec = 0) base::trunc(x * 10^prec, ...) / 10^prec;
setwd('~/Desktop/21Projects/Single_FG_Motion')

fgmdata = read.csv('motion_RA.csv', header = TRUE)
# exp details
# removing, 141906 does not have a circle uncuedCat trial because that person only had 30 trials, nrow(fgmdata[fgmdata$trial_participant_id == 141906,])
fgmdata <- fgmdata[!(fgmdata$trial_participant_id==141906),]
number_of_sub <- unique(fgmdata$trial_participant_id)
cols = rgb(red = 1, green = 0, blue = 0)
fgmdata$sameMotion <- as.factor(fgmdata$sameMotion)
fgmdata$globalOrg <- as.factor(fgmdata$globalOrg)
fgmdata$cuedMotDir <- as.factor(fgmdata$cuedMotDir)
fgmdata$trial_participant_id <- as.factor(fgmdata$trial_participant_id)
# new meta variables
fgmdata$cuedAR <- round(fgmdata$cuedAR, digits = 2)
table(fgmdata$cuedAR)
fgmdata$uncuedAR <- round(fgmdata$uncuedAR, digits = 2)
fgmdata$responseAR <- round(fgmdata$responseAR, digits = 2)
table(fgmdata$responseAR)
fgmdata$uncuedCat = ifelse(fgmdata$uncuedAR < 0, -1, ifelse(fgmdata$uncuedAR==-0, 0, 1))
fgmdata$uncuedCat <- as.factor(fgmdata$uncuedCat)
fgmdata$cuedCat = ifelse(fgmdata$cuedAR < 0, -1, ifelse(fgmdata$cuedAR==-0, 0, 1))
table(fgmdata$uncuedCat)
table(fgmdata$cuedCat)
fgmdata$respAcc <- ifelse( (fgmdata$responseAR > 0 & fgmdata$cuedCat == 1)  | (fgmdata$responseAR < 0 & fgmdata$cuedCat == -1) | (fgmdata$responseAR == 0 & fgmdata$cuedCat == 0), 1, 0)
table(fgmdata$respAcc)
fgmdata$globalMotion <- as.factor(ifelse(fgmdata$cuedMotDir == 90 | fgmdata$cuedMotDir == 270, 1, -1))

# check all Ps regression line
ggplot(fgmdata, aes(x = cuedAR, y = responseAR)) + 
  geom_point(alpha=1/120)+
  geom_smooth(method = "lm", se=TRUE,  alpha=0.2) +
  labs(x="(<-flatter)   cued AR   (taller->)", y = "(<-flatter) ResponseAR (taller->)") +
  labs(title="", subtitle=" ")+
  theme_classic() + facet_wrap(~trial_participant_id, ncol = 6)
# iterate all and get the beta coefficient
fgmdata.cleaning <- data.frame(matrix(ncol = 8, nrow = length(number_of_sub)))
colnames(fgmdata.cleaning) <- c("reg_p", "reg_beta", "meanRT", "corr","corr_p", "respAcc","trialN","id")
for (s in 1:length(number_of_sub)){
  tmpdata <- fgmdata[fgmdata$trial_participant_id == number_of_sub[s],]
  lm_sub <- lm(formula = responseAR ~ cuedAR, data = tmpdata)
  fgmdata.cleaning[s,1] <- round(summary(lm_sub)$coefficients[2,4], digits = 3) #p-value
  fgmdata.cleaning[s,2] <- round(summary(lm_sub)$coefficients[2,1], digits= 3) #estimate
  fgmdata.cleaning[s,3] <- round(mean(tmpdata$rt), digits = 0)/1000
  fgmdata.cleaning[s,4] <- cor.test(tmpdata$cuedAR, tmpdata$responseAR, method = "pearson")$estimate
  fgmdata.cleaning[s,5] <- round(cor.test(tmpdata$cuedAR, tmpdata$responseAR, method = "pearson")$p.value, digits = 2)
  fgmdata.cleaning[s,6] <- mean(tmpdata$respAcc)
  fgmdata.cleaning[s,7] <- nrow(tmpdata)# check the n of each participant
  fgmdata.cleaning[s,8] <- number_of_sub[s]
}
head(fgmdata.cleaning)
plot(fgmdata.cleaning$reg_beta, fgmdata.cleaning$reg_p)
plot(fgmdata.cleaning$meanRT)
plot(fgmdata.cleaning$corr, fgmdata.cleaning$respAcc) # correlation and accuracy appears to be linked
below40P<-fgmdata.cleaning$id[fgmdata.cleaning$respAcc <= 0.40] # 3/74 showed below 0.40 accuracy
below40P
below50P<-fgmdata.cleaning$id[fgmdata.cleaning$respAcc <= 0.50] # 3/74 showed below 0.40 accuracy
below50P
testToBeCleaned = below50P[12]
fgmdata.cleaning[fgmdata.cleaning$id == testToBeCleaned,]
plot(fgmdata$cuedAR[fgmdata$trial_participant_id == testToBeCleaned], fgmdata$responseAR[fgmdata$trial_participant_id == testToBeCleaned])
# those below40P all show bad performance in all metrics (e.g. regression beta, p-value, correlation, reaction time). 
incompletedPeople <- fgmdata.cleaning$id[fgmdata.cleaning$trialN<241]
incompletedPeople
#### CLEANED DATA ####
fgmdata <- fgmdata[!( (fgmdata$trial_participant_id %in% below50P)),]
fgmdata <- fgmdata[!( (fgmdata$trial_participant_id %in% incompletedPeople)),]
# check
incompletedPeople[1:length(incompletedPeople)] %in% unique(fgmdata$trial_participant_id) 
# clear relationship between CUED vs RESPONSE
plot(fgmdata$responseAR, fgmdata$cuedAR, main= "Response AR as a function of cued AR (paired with same)", 
     ylab = "Response AR", xlab = "Cued AR",
     col = alpha(cols, 0.005), pch=16)
# cleaning based on regression line
res <- cor.test(fgmdata$cuedAR, fgmdata$responseAR, 
                method = "pearson")
res # overall 0.66 correlation
# calculate aspect-ratio-repulsion-index
aggregate(responseAR~cuedAR + sameAR + trial_participant_id, fgmdata, mean)
findSameAR_trials <- summarySE(fgmdata, measurevar="responseAR", groupvars=c("cuedAR", "sameAR", "trial_participant_id"))
sameAR_trials <- findSameAR_trials[findSameAR_trials$sameAR==1,]
sameAR_trials #so many less-same trials per individuals
par(mfrow = c(2,2))
plot(findSameAR_trials$cuedAR[findSameAR_trials$sameAR==1], findSameAR_trials$responseAR[findSameAR_trials$sameAR==1])
abline(coef = c(0,1),col="red", lwd=3, lty=2)
plot(findSameAR_trials$cuedAR[findSameAR_trials$sameAR==0], findSameAR_trials$responseAR[findSameAR_trials$sameAR==0])
abline(coef = c(0,1),col="red", lwd=3, lty=2)
#uncuedAR
findSameAR_trials_uncued <- summarySE(fgmdata, measurevar="responseAR", groupvars=c("uncuedAR", "sameAR", "trial_participant_id"))
plot(findSameAR_trials_uncued$uncuedAR[findSameAR_trials_uncued$sameAR==1], findSameAR_trials_uncued$responseAR[findSameAR_trials_uncued$sameAR==1])
abline(coef = c(0,1),col="red", lwd=3, lty=2)
plot(findSameAR_trials_uncued$uncuedAR[findSameAR_trials_uncued$sameAR==0], findSameAR_trials_uncued$responseAR[findSameAR_trials_uncued$sameAR==0])
abline(coef = c(0,1),col="red", lwd=3, lty=2)
par(mfrow = c(1,1))
#response error by uncued Cat
tmpdata <- summarySE(fgmdata, measurevar="responseAR", groupvars=c("uncuedCat", "trial_participant_id", "sameMotion"))
head(tmpdata)
plot(tmpdata$uncuedCat, tmpdata$responseAR)
tmpdata$trial_participant_id <- as.factor(tmpdata$trial_participant_id)
ggplot(tmpdata[tmpdata$uncuedCat==-1 | tmpdata$uncuedCat == 1,], aes(uncuedCat, responseAR, color = trial_participant_id, group = trial_participant_id)) + geom_point(aes(color = trial_participant_id),shape=15, size=1.5, alpha = 30/60) +
  geom_line() + facet_grid(~sameMotion) # not really legible
#get the difference from same to notsame
head(tmpdata)
tmpdata2 <- aggregate(responseAR ~ uncuedCat + sameMotion + trial_participant_id, data = fgmdata, mean)
head(tmpdata2)
require(dplyr)
tmpdata3 <- tmpdata2 %>%
  pivot_wider(
    names_from = c(sameMotion, uncuedCat),
    values_from = c(responseAR)
  )
colnames(tmpdata3) <- c("id", "NS_uncued_flat", "NS_uncued_circle", "NS_uncued_tall","S_uncued_flat","S_uncued_circle", "S_uncued_tall")
head(tmpdata3)
#find the difference in flat and tall (diff btw SAME - NS)
tmpdata3$diff_tall <- tmpdata3$S_uncued_tall - tmpdata3$NS_uncued_tall
tmpdata3$diff_flat <- tmpdata3$S_uncued_flat - tmpdata3$NS_uncued_flat
tmpdata3$diff_circle <- tmpdata3$S_uncued_circle - tmpdata3$NS_uncued_circle
head(tmpdata3)
par(mfrow = c(1,3)) # plotting diff_tall and diff_flat and diff_circle
plot(tmpdata3$diff_flat, ylim = c(-0.4, 0.4))
abline(c(0,0), lty = 2)
plot(tmpdata3$diff_circle, ylim = c(-0.4, 0.4))
abline(c(0,0), lty = 2)
plot(tmpdata3$diff_tall, ylim = c(-0.4, 0.4))
abline(c(0,0), lty = 2)
# Mean +/- standard deviation
tmpdata <- aggregate(respError~uncuedCat + trial_participant_id, fgmdata, mean)
ggerrorplot(tmpdata, x = "uncuedCat", y = "respError", 
            desc_stat = "mean_sd")
ggerrorplot(tmpdata, x = "uncuedCat", y = "respError", 
            desc_stat = "mean_sd", color = "black",
            add = "jitter", add.params = list(color = "darkgray"))
# Specify the comparisons you want
my_comparisons <- list( c("-1", "1"), c("0", "1"), c("-1", "0") )
ggerrorplot(tmpdata, x = "uncuedCat", y = "respError", 
            desc_stat = "mean_sd", color = "black",
            add = "jitter", add.params = list(color = "darkgray"))+
  stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 1)                  # Add global p-value
# DOING IT WITH PAIRED = TRUE
my_comparisons <- list( c("-1", "1"), c("0", "1"), c("-1", "0") )
ggerrorplot(tmpdata, x = "uncuedCat", y = "respError", 
            desc_stat = "mean_sd", color = "black",
            add = "jitter", add.params = list(color = "darkgray"))+
  stat_compare_means(paired = TRUE, comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 1)                  # Add global p-value
# Color by grouping variable "sameMotion"
tmpdata <- aggregate(respError~ uncuedCat + trial_participant_id + sameMotion, fgmdata, mean)
tmpdata$sameMotion <- as.factor(tmpdata$sameMotion)
tmpdata$uncuedCat <- as.factor(tmpdata$uncuedCat)
ggerrorplot(tmpdata, x = "uncuedCat", y = "respError", 
            desc_stat = "mean_sd", 
            color = "sameMotion", palette = "jco",
            position = position_dodge(0.3))
# Basic line plots of means +/- se with jittered points
ggline(tmpdata, x = "uncuedCat", y = "respError", color = "sameMotion",
       add = c("mean_se", "jitter")) # INTERESTING! I see a difference especially at tall and circle, not at flat (similar to Tim's finding)
ggbarplot(tmpdata, x = "uncuedCat", y = "respError", 
          add = c("mean_se", "jitter"),
          color = "sameMotion", palette = "jco",
          position = position_dodge(0.8)) # ADD THIS
ggline(tmpdata, x = "uncuedCat", y = "respError",
       add = c("mean_se", "jitter"),
       color = "sameMotion", palette = "jco")
# flipping x-axis and grouping
ggbarplot(tmpdata, x = "sameMotion", y = "respError", 
          add = c("mean_se", "jitter"),
          color = "uncuedCat", palette = "jco",
          position = position_dodge(0.8))
ggline(tmpdata, x = "sameMotion", y = "respError", 
       add = c("mean_se", "jitter"),
       color = "uncuedCat", palette = "jco")
# compare tall and circle
my_comparisons = list(c("0","1"))
ggline(tmpdata, x = "sameMotion", y = "respError",
       add = c("mean_se", "jitter"),
       color = "uncuedCat", palette = "jco")+
      stat_compare_means(paired = TRUE, comparisons = my_comparisons)+
      stat_compare_means(label.y = 0.5)+ # ADD THIS, works for both TALL and Circle, tiny effect though!
      facet_grid(~uncuedCat)
# checking anova results
ANOVA <- aov(respError ~ sameMotion*uncuedCat + Error(as.factor(trial_participant_id)/(sameMotion*uncuedCat)), data=tmpdata)
summary(ANOVA) # YES! YES! YES! Pretty neat interaction (no effect of uncued cat but only interacts if motion is the same)
# let's confirm this with lmer
tmpdata$trial_participant_id <- as.factor(tmpdata$trial_participant_id)
lmm <- lmer(respError ~ sameMotion * uncuedCat + ( uncuedCat | trial_participant_id), fgmdata)
summary(lmm) #simdi biraz az efekt goruyorum, hatta kayboluyor olabilir,  bunun cleaning ile ilgisi var mi?
# uncuedAR
lmm <- lmer(respError ~ sameMotion * uncuedAR + ( sameMotion*uncuedAR | trial_participant_id), fgmdata)
summary(lmm) # now I see a clear interaction, even when controlling for random effects of uncuedAR for each participant
# I also like this graph
ggbarplot(tmpdata, x = "sameMotion", y = "respError", 
          add = c("mean_se", "jitter"),
          color = "sameMotion", palette = "jco",
          position = position_dodge(0.8))+ 
          stat_compare_means(paired = TRUE, comparisons = my_comparisons)+
          stat_compare_means(label.y = 1)+
          facet_grid(~uncuedCat) # ADD this
# doing the same with fgm data
table(fgmdata$sameMotion, fgmdata$uncuedCat)
ggline(fgmdata, x = "sameMotion", y = "respError",
       add = c("mean_se", "jitter"),
       color = "uncuedCat", palette = "jco")+
  stat_compare_means(paired= TRUE, comparisons = my_comparisons)+
  stat_compare_means(label.y = 1)+ # tall is significant! circle is marginal
  facet_grid(~uncuedCat) # facet_grid(~uncuedCat+globalOrg) global org eklenince, between'de daha cok averaging var.
# ttest
t.test(tmpdata$respError[tmpdata$uncuedCat==1], mu = 0, alternative = "greater") #“two.sided” (default), “greater” or “less”.
# tall -> response error is greater than 0
t.test(tmpdata$respError[tmpdata$uncuedCat==1 & tmpdata$sameMotion==1], tmpdata$respError[tmpdata$uncuedCat==1 & tmpdata$sameMotion==0], paired = TRUE) 
# hmm this gives out 0.12, tall-group (-0.002) != tall-ungroup (0.01) (p=0.12)
t.test(tmpdata$respError[tmpdata$uncuedCat==0 & tmpdata$sameMotion==1], tmpdata$respError[tmpdata$uncuedCat==0 & tmpdata$sameMotion==0], paired = TRUE) 
# circle is significant, 0.03, marginal, significant with cleaner data
# GLOBAL ORG, this has to have a role in the integration - 
tmpdata <- aggregate(respError~uncuedCat + trial_participant_id + sameMotion + globalOrg, fgmdata, mean)
tmpdata$sameMotion<- factor(tmpdata$sameMotion, levels = c(0:1), labels= c("Non-grouped", "Grouped"))
tmpdata$globalOrg<- factor(tmpdata$globalOrg, levels = c(0:1), labels= c("between", "within"))
tmpdata$trial_participant_id <- as.factor(tmpdata$trial_participant_id)
tmpdata$uncuedCat <- factor(tmpdata$uncuedCat, levels = c(-1,0,1), labels= c("Flat", "Circle", "Tall"))
tmpdata$respError <- round(tmpdata$respError, digits =3)
ggline(tmpdata, x = "sameMotion", y = "respError", 
       add = c("mean_se", "jitter"),
       color = "uncuedCat", palette = "jco")+facet_grid(~globalOrg) # between has more noise though
# stats
my_comparisons <- list(c("Non-grouped", "Grouped"))
table(tmpdata$sameMotion,tmpdata$uncuedCat)
ggline(tmpdata, x = "sameMotion", y = "respError", 
       add = c("mean_se", "jitter"),
       color = "uncuedCat", palette = "jco")+facet_grid(~uncuedCat+globalOrg)+
      stat_compare_means(paired = TRUE, comparisons = my_comparisons)+ 
      stat_compare_means(label.y = 1) # nice! nice! nice! more averaging in between (interesting)
# checking anova results
ANOVA <- aov(respError ~ sameMotion*globalOrg*uncuedCat + Error(trial_participant_id/(sameMotion*globalOrg*uncuedCat)), data=tmpdata)
summary(ANOVA) # not sure but there might not be an interaction
lmm <- lmer(respError ~ 1  + sameMotion*uncuedCat*globalOrg + (sameMotion|trial_participant_id), tmpdata)
summary(lmm)
anova(lmm)
# Okay, I guess there is no interaction :/ 
lmm <- lmer(respError ~ 1  + sameMotion*globalOrg + (globalOrg|trial_participant_id), fgmdata)
summary(lmm)
# trying other relationships
lmm <- lmer(respError ~ 1  + as.factor(sameMotion)*as.factor(uncuedCat) + (as.factor(sameMotion)*as.factor(uncuedCat)|trial_participant_id), fgmdata)
summary(lmm) # I see a 0.01 effect on tall
m <- lmer(respError ~ as.factor(sameMotion)*as.factor(uncuedCat) + (1 + as.factor(sameMotion) | trial_participant_id), data=fgmdata, REML=F)
summary(m)
anova(m) # I see a .06 p value interaction
# continuous variable
m <- lmer(respError ~ as.factor(sameMotion)*uncuedAR + (1 + as.factor(sameMotion) | trial_participant_id), data=fgmdata, REML=F)
summary(m) # again with this regression model I see an effect of uncued x same motion -> response error
# globalOrg
m <- lmer(respError ~ as.factor(sameMotion)*uncuedAR*as.factor(globalOrg) + (1 + as.factor(sameMotion)*as.factor(globalOrg) | trial_participant_id), data=fgmdata, REML=F)
summary(m) # main effect of global org and attraction to uncued in the same motion condition, random effects'deki faktorleri azaltirsam daha da net cikiyor sonuclar.
# back to simple ANOVA
ANOVA <- aov(respError ~ as.factor(sameMotion)*as.factor(uncuedCat) + Error(as.factor(trial_participant_id)/(as.factor(sameMotion))), data=tmpdata)
summary(ANOVA)
# fgmdata - same results
ANOVA <- aov(respError ~ sameMotion*uncuedCat + Error(as.factor(trial_participant_id)/(sameMotion)), data=fgmdata)
summary(ANOVA) # again I see an interaction, need to do post-hoc comparison?
# coherence
ANOVA <- aov(respError ~ sameMotion*uncuedCat*as.factor(coherence_level) + Error(as.factor(trial_participant_id)/(sameMotion)), data=fgmdata)
summary(ANOVA) # coherence didnt do much
ANOVA <- aov(respError ~ sameMotion*uncuedCat*globalOrg + Error(as.factor(trial_participant_id)/(sameMotion)), data=fgmdata)
summary(ANOVA) # again main effect of global org, but no 3-way interaction
# global org with regression again
m <- lmer(respError ~ as.factor(sameMotion)*as.factor(uncuedCat)*as.factor(globalOrg) + (1 + as.factor(sameMotion)*as.factor(globalOrg) | trial_participant_id), data=fgmdata, REML=F)
summary(m) # again same results, tiny effect but significant (0.02) falan bir efekt
# CHANGE OF TOPIC - globalMotion!
tmpdata <- aggregate(respError~uncuedCat + trial_participant_id + sameMotion + globalOrg + globalMotion, fgmdata, mean)
tmpdata$sameMotion<- factor(tmpdata$sameMotion, levels = c(0:1), labels= c("Non-grouped", "Grouped"))
tmpdata$globalOrg<- factor(tmpdata$globalOrg, levels = c(0:1), labels= c("between", "within"))
tmpdata$uncuedCat <- factor(tmpdata$uncuedCat, levels = c(-1,0,1), labels= c("Flat", "Circle", "Tall"))
tmpdata$trial_participant_id <- as.factor(tmpdata$trial_participant_id)
my_comparisons <- list(c("-1", "1"))
ggline(tmpdata, x = "globalMotion", y = "respError",
       add = c("mean_se", "jitter"),
       color = "uncuedCat", palette = "jco")+
  stat_compare_means(comparisons = my_comparisons)+
  stat_compare_means(label.y = 1)+ 
  facet_grid(~sameMotion+globalOrg) # globalMotion doesn't seem to matter much
# END # 
# homework: 9.15.22: uncuedCat vs sameMotion interaction'i goruyorum, ozellikle circle oldugunda ve tall oldugunda, bunun
# uzerine yapistirip gec.
# tim's previous graph replication
tmpdata <- aggregate(respError~ uncuedAR + trial_participant_id + sameMotion, fgmdata, mean)
head(tmpdata)
tmpdata$sameMotion <- as.factor(tmpdata$sameMotion)
tmpdata$uncuedAR <- as.factor(abs(tmpdata$uncuedAR))
tmpdata$respError <- abs(round(tmpdata$respError, digits=3))
table(tmpdata$uncuedAR)
my_comparisons <- list(c("0.28", "0.32"))
ggline(tmpdata, x = "uncuedAR", y = "respError",
       add = c("mean_se", "jitter"),
       color = "sameMotion")+
  stat_compare_means(paired = TRUE, comparisons = my_comparisons)+
  stat_compare_means(label.y = 0.5)+
  facet_grid(~sameMotion)

# RECAP!
tmpdata <- aggregate(responseAR ~ trial_participant_id + uncuedCat + sameMotion, fgmdata, mean)
table(tmpdata$uncuedCat)
table(tmpdata$sameMotion)
my_comparisons <- list(c("0", "1"))
ggline(tmpdata, x = "sameMotion", y = "responseAR",
       add = c("mean_se", "jitter"),
       color = "uncuedCat")+
      stat_compare_means(paired = TRUE, comparisons = my_comparisons)+
      stat_compare_means(label.y = 0.5)+
      facet_grid(~uncuedCat) #ok, almost a significant finding there (.086), (uncleaned daha kotu)
#how much a circle moves around
tmpdata <- aggregate(responseAR ~ trial_participant_id + uncuedCat + sameMotion + globalOrg, fgmdata[fgmdata$cuedAR==0,], mean)
tmpdata <- tmpdata[!(tmpdata$uncuedCat == 0),]
table(tmpdata$uncuedCat)
table(tmpdata$sameMotion)
my_comparisons <- list(c("-1", "1"))
ggline(tmpdata, x = "uncuedCat", y = "responseAR",
       add = c("mean_se", "jitter"),
       color = "sameMotion")+
  stat_compare_means(paired = TRUE, comparisons = my_comparisons)+
  stat_compare_means(label.y = 0.5)+
  facet_grid(~sameMotion) #ok, almost a significant finding there (.086) (.053) with global org, (uncleaned daha kotu)
completedPeople <- data.frame(table(tmpdata$trial_participant_id))
completedPeople
newPeople <- completedPeople$Var1[completedPeople$Freq>=8]
length(newPeople)
tmpdata <- tmpdata[tmpdata$trial_participant_id %in% newPeople,]
#how much a -0.5 moves around
tmpdata <- aggregate(responseAR ~ trial_participant_id + uncuedCat + sameMotion, fgmdata[fgmdata$cuedAR==-0.05,], mean)
tmpdata <- tmpdata[!(tmpdata$uncuedCat == 0),]
table(tmpdata$uncuedCat)
table(tmpdata$sameMotion)
my_comparisons <- list(c("-1", "1"))
ggline(tmpdata, x = "uncuedCat", y = "responseAR",
       add = c("mean_se", "jitter"),
       color = "sameMotion")+
  stat_compare_means(paired = TRUE, comparisons = my_comparisons)+
  stat_compare_means(label.y = 0.5)+
  facet_grid(~sameMotion) #ok, almost a significant finding there (.086), (uncleaned daha kotu)
completedPeople <- data.frame(table(tmpdata$trial_participant_id))
completedPeople
newPeople <- completedPeople$Var1[completedPeople$Freq>=4]
length(newPeople)
tmpdata <- tmpdata[tmpdata$trial_participant_id %in% newPeople,]



### OLD CHAPTER BELOW #####

# add stats
my_comparisons <- list( c("-1", "1"), c("0", "1"), c("-1", "0") )
ggline(tmpdata, x = "uncuedCat", y = "respError", color = "sameMotion",
       add = c("mean_se", "jitter"))+ # INTERESTING! I see a difference especially at tall and circle, not at flat (similar to Tim's finding)
  stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 1)                  # Add global p-value


test <- aggregate(fgmdata$responseAR ~ fgmdata$trial_participant_id + fgmdata$cuedAR + fgmdata$sameMotion, data = fgmdata, mean)
ggplot(test, aes(`fgmdata$cuedAR`, `fgmdata$responseAR`)) + geom_point(aes(colour= as.factor(`fgmdata$trial_participant_id`)), shape=15, size=1.5) 

#do it per participant per average
number_of_sub <- unique(fgmdata$trial_participant_id)
test <- aggregate(fgmdata$responseAR ~ fgmdata$cuedAR + fgmdata$sameMotion, data = fgmdata, mean)
colnames(test) <- c("cuedAR", "motion", "response")
colnames(test)
ggplot(test, aes(cuedAR, response)) + geom_point(aes(colour= as.factor(motion)), shape=15, size=1.5)

# Calculates mean, sd, se and IC
my_sum <- fgmdata %>%
  group_by(cuedAR, trial_participant_id) %>%
  summarise( 
    n=n(),
    mean=mean(responseAR),
    sd=sd(responseAR)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

plot(my_sum$cuedAR, my_sum$mean)


# Standard deviation
ggplot(my_sum) +
  geom_bar( aes(x=cuedAR, y=mean), stat="identity", fill="forestgreen", alpha=0.1) +
  geom_errorbar( aes(x=cuedAR, ymin=mean-sd, ymax=mean+sd), width=0.1, colour="orange", alpha=0.1, size=0.5) +
  ggtitle("using standard deviation")

# Standard Error
ggplot(my_sum) +
  geom_bar( aes(x=cuedAR, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=cuedAR, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("using standard error")

# Confidence Interval
ggplot(my_sum) +
  geom_bar( aes(x=cuedAR, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=cuedAR, ymin=mean-ic, ymax=mean+ic), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("using confidence interval")

# summarySE provides the standard deviation, standard error of the
# mean, and a (default 95%) confidence interval # "trial_participant_id"
tgc <- summarySE(fgmdata, measurevar="responseAR", groupvars=c("cuedAR", "sameMotion", "trial_participant_id"))
head(tgc)

# basic t-test
ttest = t.test(tgc$responseAR[tgc$sameMotion==1], tgc$responseAR[tgc$sameMotion==0])
ttest #two group means are not different
plot(tgc$responseAR[tgc$sameMotion==1], tgc$responseAR[tgc$sameMotion==0])
abline(c(0,1)) #actually I see a lot of diff btw 1 and 0. 

# Standard error of the mean
ggplot(tgc, aes(x=uncuedAR, y=respError, colour=as.factor(sameMotion))) + 
  geom_errorbar(aes(ymin=respError-se, ymax=respError+se), width=.1) +
  geom_line() +
  geom_point()

# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0) # move them .05 to the left and right

ggplot(tgc, aes(x=uncuedAR, y=respError, colour=as.factor(sameMotion))) + 
  geom_errorbar(aes(ymin=respError-se, ymax=respError+se), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd)


# Use 95% confidence interval instead of SEM
ggplot(tgc, aes(x=abs(uncuedAR), y=abs(respError), colour=as.factor(sameMotion))) + 
  geom_errorbar(aes(ymin=respError-ci, ymax=respError+ci), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd) #+ facet_grid(~globalOrg)

# Black error bars - notice the mapping of 'group=supp' -- without it, the error
# bars won't be dodged!
ggplot(tgc, aes(x=cuedAR, y=responseAR, colour=as.factor(sameMotion), group=as.factor(sameMotion))) + 
  geom_errorbar(aes(ymin=responseAR-ci, ymax=responseAR+ci), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3)


# get the difference of motion
head(tgc)




data <- fgmdata
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



