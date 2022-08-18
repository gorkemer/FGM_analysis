# April 5th, 2022.
# Analysis script for the foreground motion experiment. 
# I've revised the script of SingleFGMot_01_prepare.R script that sits locally on 
# my computer. This is a revised version for the manuscript I'm writing. 
# gorkemer
# April 5th, 2022
# This script aims to analyse the foreground motion experiment and it uses the 
# untouched data file (output of the fgm_prepareData.R)

# last edit: Agu 16th, 22

# clear the workspace
rm(list=ls()) 

# load libraries in bulk
x<-c("ggpubr", "ggplot2", "multcomp", "pastecs", "tidyr","dplyr", "ggiraph", "ggiraphExtra", "plyr", 
     "covreg", "Hmisc", "corrplot", "psych", "tidyverse", "hrbrthemes", "viridis", "gapminder",
     "ggExtra", "scatterplot3d", "reshape2", "rlang", "plyr", "data.table", "lme4", "magrittr", "fitdistrplus",
     "gridExtra", "statmod", "dotwhisker", "lmerTest", "nlme", "GGally")
#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/ggpubr")
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
# exp details
number_of_sub <- unique(fgmdata$sub)
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
fgmdata$globalMotion <- as.factor(ifelse(fgmdata$cued_motion_dir == 90 | fgmdata$cued_motion_dir == 270, 1, -1))

# check all Ps regression line
ggplot(fgmdata, aes(x = cuedAR, y = responseAR)) + 
  geom_point(alpha=1/120)+
  geom_smooth(method = "lm", se=TRUE,  alpha=0.2) +
  labs(x="(<-flatter)   cued AR   (taller->)", y = "(<-flatter) ResponseAR (taller->)") +
  labs(title="", subtitle=" ")+
  theme_classic() + facet_wrap(~sub, ncol = 6)
# iterate all and get the beta coefficient
fgmdata.cleaning <- data.frame(matrix(ncol = 8, nrow = length(number_of_sub)))
colnames(fgmdata.cleaning) <- c("reg_p", "reg_beta", "meanRT", "corr","corr_p", "respAcc","trialN","id")
for (s in 1:length(number_of_sub)){
  tmpdata <- fgmdata[fgmdata$sub == number_of_sub[s],]
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
plot(fgmdata$cuedAR[fgmdata$sub == testToBeCleaned], fgmdata$responseAR[fgmdata$sub == testToBeCleaned])
# those below40P all show bad performance in all metrics (e.g. regression beta, p-value, correlation, reaction time). 
incompletedPeople <- fgmdata.cleaning$id[fgmdata.cleaning$trialN<241]
incompletedPeople
#### CLEANED DATA ####
#fgmdata <- fgmdata[!( (fgmdata$sub %in% below50P)),]
fgmdata <- fgmdata[!( (fgmdata$sub %in% incompletedPeople)),]
# check
incompletedPeople[1:length(incompletedPeople)] %in% unique(fgmdata$sub) 
below50P[1:length(below50P)] %in% unique(fgmdata$sub) # many of the below50 people are included in the analysis
# clear relationship between CUED vs RESPONSE
cols = rgb(red = 1, green = 0, blue = 0)
plot(fgmdata$responseAR, fgmdata$cuedAR, main= "Response AR as a function of cued AR (paired with same)", 
     ylab = "Response AR", xlab = "Cued AR")
# cleaning based on regression line
res <- cor.test(fgmdata$cuedAR, fgmdata$responseAR, 
                method = "pearson")
res # overall 0.66 correlation
# calculate aspect-ratio-repulsion-index
aggregate(responseAR~cuedAR + identicalShapesI1D0 + sub, fgmdata, mean)
findSameAR_trials <- summarySE(fgmdata, measurevar="responseAR", groupvars=c("cuedAR", "identicalShapesI1D0", "sub"))
sameAR_trials <- findSameAR_trials[findSameAR_trials$identicalShapesI1D0==1,]
sameAR_trials #so many less-same trials per individuals
par(mfrow = c(2,2))
plot(findSameAR_trials$cuedAR[findSameAR_trials$identicalShapesI1D0==1], findSameAR_trials$responseAR[findSameAR_trials$identicalShapesI1D0==1], xlab = "Cued AR", ylab = "Response AR", main = "Same-AR Trials")
abline(coef = c(0,1),col="red", lwd=3, lty=2)
plot(findSameAR_trials$cuedAR[findSameAR_trials$identicalShapesI1D0==0], findSameAR_trials$responseAR[findSameAR_trials$identicalShapesI1D0==0], xlab = "Cued AR", ylab = "Response AR",main = "Different-AR Trials", ylim = c(-0.6, 0.6))
abline(coef = c(0,1),col="red", lwd=3, lty=2)
#uncuedAR
findSameAR_trials_uncued <- summarySE(fgmdata, measurevar="responseAR", groupvars=c("uncuedAR", "identicalShapesI1D0", "sub"))
plot(findSameAR_trials_uncued$uncuedAR[findSameAR_trials_uncued$identicalShapesI1D0==1], findSameAR_trials_uncued$responseAR[findSameAR_trials_uncued$identicalShapesI1D0==1], xlab = "Uncued AR", ylab = "Response AR", main = "Same-AR Trials")
abline(coef = c(0,1),col="red", lwd=3, lty=2)
plot(findSameAR_trials_uncued$uncuedAR[findSameAR_trials_uncued$identicalShapesI1D0==0], findSameAR_trials_uncued$responseAR[findSameAR_trials_uncued$identicalShapesI1D0==0], xlab = "Uncued AR", ylab = "Response AR", main = "Different-AR Trials", ylim = c(-0.6, 0.6))
abline(coef = c(0,1),col="red", lwd=3, lty=2)
abline(coef = c(0,0),col="red", lwd=3, lty=2)
par(mfrow = c(1,1))
#response error by uncued Cat
tmpdata <- summarySE(fgmdata, measurevar="responseAR", groupvars=c("uncuedCat", "sub", "sameDirection1S0D"))
head(tmpdata)
plot(tmpdata$uncuedCat, tmpdata$responseAR)
tmpdata$sub <- as.factor(tmpdata$sub)
ggplot(tmpdata[tmpdata$uncuedCat==-1 | tmpdata$uncuedCat == 1,], aes(uncuedCat, responseAR, color = sub, group = sub)) + geom_point(aes(color = sub),shape=15, size=1.5, alpha = 30/60) +
  geom_line() + facet_grid(~sameDirection1S0D) # not really legible
#get the difference from same to notsame
head(tmpdata)
tmpdata2 <- aggregate(responseAR ~ uncuedCat + sameDirection1S0D + sub, data = fgmdata, mean)
head(tmpdata2)
require(dplyr)
tmpdata3 <- tmpdata2 %>%
  pivot_wider(
    names_from = c(sameDirection1S0D, uncuedCat),
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
# order the level of sub for x-axis
tmpdata3 <- tmpdata3[order(tmpdata3$diff_flat),]
plot(tmpdata3$diff_flat, ylim = c(-0.4, 0.4), xlab = "sub", ylab = "Grouping Effect (diff) when uncued is FLAT")
abline(c(0,0), lty = 2)
tmpdata3 <- tmpdata3[order(tmpdata3$diff_circle),]
plot(tmpdata3$diff_circle, ylim = c(-0.4, 0.4), xlab = "sub", ylab = "Grouping Effect (diff) when uncued is CIRCLE")
abline(c(0,0), lty = 2)
tmpdata3 <- tmpdata3[order(tmpdata3$diff_tall),]
plot(tmpdata3$diff_tall, ylim = c(-0.4, 0.4), xlab = "sub", ylab = "Grouping Effect (diff) when uncued is TALL")
abline(c(0,0), lty = 2)
# Mean +/- standard deviation
tmpdata <- aggregate(responseError~uncuedCat + sub, fgmdata, mean)
ggerrorplot(tmpdata, x = "uncuedCat", y = "responseError", 
            desc_stat = "mean_sd")
ggerrorplot(tmpdata, x = "uncuedCat", y = "responseError", 
            desc_stat = "mean_sd", color = "black",
            add = "jitter", add.params = list(color = "darkgray"))
# Specify the comparisons you want
my_comparisons <- list( c("-1", "1"), c("0", "1"), c("-1", "0") )
ggerrorplot(tmpdata, x = "uncuedCat", y = "responseError", 
            desc_stat = "mean_sd", color = "black",
            add = "jitter", add.params = list(color = "darkgray"))+
  stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 1)                  # Add global p-value
# DOING IT WITH PAIRED = TRUE
my_comparisons <- list( c("-1", "1"), c("0", "1"), c("-1", "0") )
ggerrorplot(tmpdata, x = "uncuedCat", y = "responseError", 
            desc_stat = "mean_sd", color = "black",
            add = "jitter", add.params = list(color = "darkgray"))+
  stat_compare_means(paired = TRUE, comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 1)                  # Add global p-value
# Color by grouping variable "sameDirection1S0D"
tmpdata <- aggregate(responseError~ uncuedCat + sub + sameDirection1S0D, fgmdata, mean)
tmpdata$sameDirection1S0D <- as.factor(tmpdata$sameDirection1S0D)
tmpdata$uncuedCat <- as.factor(tmpdata$uncuedCat)
ggerrorplot(tmpdata, x = "uncuedCat", y = "responseError", 
            desc_stat = "mean_sd", 
            color = "sameDirection1S0D", palette = "jco",
            position = position_dodge(0.3))
# Basic line plots of means +/- se with jittered points
ggline(tmpdata, x = "uncuedCat", y = "responseError", color = "sameDirection1S0D",
       add = c("mean_se", "jitter")) # INTERESTING! I see a difference especially at tall and circle, not at flat (similar to Tim's finding)
ggbarplot(tmpdata, x = "uncuedCat", y = "responseError", 
          add = c("mean_se", "jitter"),
          color = "sameDirection1S0D", palette = "jco",
          position = position_dodge(0.8)) # ADD THIS
ggline(tmpdata, x = "uncuedCat", y = "responseError",
       add = c("mean_se", "jitter"),
       color = "sameDirection1S0D", palette = "jco")
# flipping x-axis and grouping
ggbarplot(tmpdata, x = "sameDirection1S0D", y = "responseError", 
          add = c("mean_se", "jitter"),
          color = "uncuedCat", palette = "jco",
          position = position_dodge(0.8))
ggline(tmpdata, x = "sameDirection1S0D", y = "responseError", 
       add = c("mean_se", "jitter"),
       color = "uncuedCat", palette = "jco")
# compare tall and circle
my_comparisons = list(c("0","1"))
ggline(tmpdata, x = "sameDirection1S0D", y = "responseError",
       add = c("mean_se", "jitter"),
       color = "uncuedCat", palette = "jco")+
  stat_compare_means(paired = TRUE, comparisons = my_comparisons)+
  stat_compare_means(label.y = 0.5)+ # ADD THIS, works for both TALL and Circle, tiny effect though!
  facet_grid(~uncuedCat)
# checking anova results
ANOVA <- aov(responseError ~ sameDirection1S0D*uncuedCat + Error(as.factor(sub)/(sameDirection1S0D*uncuedCat)), data=tmpdata)
summary(ANOVA) # YES! YES! YES! Pretty neat interaction (no effect of uncued cat but only interacts if motion is the same), #57 people
# let's confirm this with lmer
tmpdata$sub <- as.factor(tmpdata$sub)
lmm <- lmer(responseError ~ sameDirection1S0D * uncuedCat + ( uncuedCat | sub), fgmdata)
summary(lmm) # categorical uncued Cat koymama gerek yok burda, continuous girebilirim, giriyorum altta.
# uncuedAR
lmm <- lmer(responseError ~ sameDirection1S0D * uncuedAR + ( sameDirection1S0D*uncuedAR | sub), fgmdata)
summary(lmm) # now I see a clear interaction, even when controlling for random effects of uncuedAR for each participant, around 2.4 percent change
# I also like this graph
my_comparisons = list(c("0","1"))
ggbarplot(tmpdata[!(tmpdata$uncuedCat==0),], x = "sameDirection1S0D", y = "responseError", 
          add = c("mean_se", "jitter"),
          color = "sameDirection1S0D", palette = "jco",
          position = position_dodge(0.8))+ 
  stat_compare_means(paired = TRUE, comparisons = my_comparisons)+
  stat_compare_means(label.y = 0.3)+
  facet_grid(~uncuedCat) # ADD this
# doing the same with fgm data
table(fgmdata$sameDirection1S0D, fgmdata$uncuedCat)
ggline(fgmdata, x = "sameDirection1S0D", y = "responseError",
       add = c("mean_se", "jitter"),
       color = "uncuedCat", palette = "jco")+
  stat_compare_means(paired= TRUE, comparisons = my_comparisons)+
  stat_compare_means(label.y = 1)+ # tall is significant! circle is marginal
  facet_grid(~uncuedCat) # facet_grid(~uncuedCat+global_org) global org eklenince, between'de daha cok averaging var.
# ttest
t.test(tmpdata$responseError[tmpdata$uncuedCat==1], mu = 0, alternative = "greater") #“two.sided” (default), “greater” or “less”.
# tall -> response error is greater than 0
#Homogeneity of variance
var(tmpdata$responseError[tmpdata$uncuedCat==1 & tmpdata$sameDirection1S0D==1])
var(tmpdata$responseError[tmpdata$uncuedCat==1 & tmpdata$sameDirection1S0D==0]) # equal?
t.test(tmpdata[tmpdata$sameDirection1S0D==1,"responseError"], tmpdata[tmpdata$sameDirection1S0D==0,"responseError"], paired = TRUE) 
# response errors are significantly different between group vs ungroup
ggline(tmpdata, x = "sameDirection1S0D", y = "responseError",
       add = c("mean_se", "jitter"))+
  stat_compare_means(paired= TRUE, comparisons = my_comparisons)+
  stat_compare_means(label.y = 1)
# okay this means that having same direction introduces biases (more errors on responses)
# now grouping test, FLAT
t.test(tmpdata[tmpdata$sameDirection1S0D==0 & tmpdata$uncuedCat==-1,"responseError"], 
       tmpdata[tmpdata$sameDirection1S0D==1 & tmpdata$uncuedCat==-1,"responseError"], 
       paired = TRUE)  # flat not different
t.test(tmpdata[tmpdata$sameDirection1S0D==0 & tmpdata$uncuedCat==1,"responseError"], 
       tmpdata[tmpdata$sameDirection1S0D==1 & tmpdata$uncuedCat==1,"responseError"], 
       paired = TRUE)  # tall not different significantly, but there is a trend
# hmm this gives out 0.12, tall-group (-0.002) != tall-ungroup (0.01) (p=0.12)
tmpdata <- aggregate(responseAR~ uncuedCat + sub + sameDirection1S0D, fgmdata, mean)
t.test(tmpdata$responseError[tmpdata$uncuedCat==0 & tmpdata$sameDirection1S0D==1], 
       tmpdata$responseError[tmpdata$uncuedCat==0 & tmpdata$sameDirection1S0D==0], paired = TRUE) 
# circle is significant, 0.03, marginal, significant with cleaner data
# testing on RESPONSE AR
t.test(tmpdata[tmpdata$sameDirection1S0D==0 & tmpdata$uncuedCat==1,"responseAR"], 
       tmpdata[tmpdata$sameDirection1S0D==1 & tmpdata$uncuedCat==1,"responseAR"], 
       paired = TRUE) # responses got taller with uncued tall and grouping (0.07)
# these t-tests are not great because we lose the magnitude of uncued cat (tall-so-tall etc)
# GLOBAL ORG, this has to have a role in the integration - 
tmpdata <- aggregate(responseError~uncuedCat + sub + sameDirection1S0D + global_org, fgmdata, mean)
tmpdata$sameDirection1S0D<- factor(tmpdata$sameDirection1S0D, levels = c(0:1), labels= c("Non-grouped", "Grouped"))
tmpdata$global_org<- factor(tmpdata$global_org, levels = c(0:1), labels= c("between", "within"))
tmpdata$sub <- as.factor(tmpdata$sub)
tmpdata$uncuedCat <- factor(tmpdata$uncuedCat, levels = c(-1,0,1), labels= c("Flat", "Circle", "Tall"))
tmpdata$responseError <- round(tmpdata$responseError, digits =3)
ggline(tmpdata, x = "sameDirection1S0D", y = "responseError", 
       add = c("mean_se", "jitter"),
       color = "uncuedCat", palette = "jco")+facet_grid(~global_org) # between has more noise though
# stats
my_comparisons <- list(c("Non-grouped", "Grouped"))
table(tmpdata$sameDirection1S0D,tmpdata$uncuedCat)
ggline(tmpdata, x = "sameDirection1S0D", y = "responseError", 
       add = c("mean_se", "jitter"),
       color = "uncuedCat", palette = "jco")+facet_grid(~uncuedCat+global_org)+
  stat_compare_means(paired = TRUE, comparisons = my_comparisons)+ 
  stat_compare_means(label.y = 1) # nice! nice! nice! more averaging in between (interesting)
ANOVA <- aov(responseError ~ sameDirection1S0D*global_org*uncuedCat + Error(sub/(sameDirection1S0D*global_org)), data=tmpdata)
summary(ANOVA) # not sure but there might not be an interaction
lmm <- lmer(responseError ~ 1  + sameDirection1S0D*uncuedCat*global_org + (sameDirection1S0D|sub), tmpdata)
summary(lmm)
anova(lmm)# Okay, I guess there is no interaction with global organization :/ 
# checking anova results, on fgmdata
#lmm <- lmer(responseError ~ 1  + sameDirection1S0D*global_org*uncuedAR + (global_org*sameDirection1S0D|sub), fgmdata)
#summary(lmm) # again I confirm this: 1) sig interaction of uncuedAR and grouping, 2) main effect of global org
#lmm <- lmer(responseError ~ 1  + sameDirection1S0D*global_org*uncuedAR + (global_org*sameDirection1S0D*uncuedAR|sub), fgmdata)
#summary(lmm) # added uncuedAR to the random effects
# continuous variable
m <- lmer(responseError ~ as.factor(sameDirection1S0D)*uncuedAR + (1 + as.factor(sameDirection1S0D) | sub), data=fgmdata, REML=F)
summary(m) # wow I see a main effect of uncuedAR, repulsion when they are not grouped, ADD THIS
# global_org
m <- lmer(responseError ~ as.factor(sameDirection1S0D)*uncuedAR*as.factor(global_org) + (1 + as.factor(sameDirection1S0D)*as.factor(global_org) | sub), data=fgmdata, REML=F)
summary(m) # 1) main effect of global org and 2) attraction to uncued in the same motion condition
# again same findings! in fact uncued repulsion is almost there. 
# fgmdata - same results
ANOVA <- aov(responseError ~ sameDirection1S0D*uncuedCat + Error(as.factor(sub)/(sameDirection1S0D)), data=fgmdata)
summary(ANOVA) # again I see an interaction yeah? within section shows 0.06?
# coherence
ANOVA <- aov(responseError ~ sameDirection1S0D*uncuedCat*as.factor(fgmdata$coherence) + Error(as.factor(sub)/(sameDirection1S0D)), data=fgmdata)
summary(ANOVA) # coherence didnt do much
ANOVA <- aov(responseError ~ sameDirection1S0D*uncuedCat*global_org + Error(as.factor(sub)/(sameDirection1S0D)), data=fgmdata)
summary(ANOVA) # again main effect of global org, but no 3-way interaction
# global org with regression again
m <- lmer(responseError ~ as.factor(sameDirection1S0D)*as.factor(uncuedCat)*as.factor(global_org) + (1 + as.factor(sameDirection1S0D)*as.factor(global_org) | sub), data=fgmdata, REML=F)
summary(m) # again same results, tiny effect but significant (0.02) falan bir efekt
# CHANGE OF TOPIC - globalMotion!
tmpdata <- aggregate(responseError~uncuedCat + sub + sameDirection1S0D + global_org + globalMotion, fgmdata, mean)
tmpdata$sameDirection1S0D<- factor(tmpdata$sameDirection1S0D, levels = c(0:1), labels= c("Non-grouped", "Grouped"))
tmpdata$global_org<- factor(tmpdata$global_org, levels = c(0:1), labels= c("between", "within"))
tmpdata$uncuedCat <- factor(tmpdata$uncuedCat, levels = c(-1,0,1), labels= c("Flat", "Circle", "Tall"))
tmpdata$sub <- as.factor(tmpdata$sub)
my_comparisons <- list(c("-1", "1"))
ggline(tmpdata, x = "globalMotion", y = "responseError",
       add = c("mean_se", "jitter"),
       color = "uncuedCat", palette = "jco")+
  stat_compare_means(comparisons = my_comparisons)+
  stat_compare_means(label.y = 1)+ 
  facet_grid(~sameDirection1S0D+global_org) # globalMotion might make this taller, yeah
# global motion anova
ANOVA <- aov(responseError ~ as.factor(sameDirection1S0D)*uncuedCat*as.factor(fgmdata$coherence)*as.factor(globalMotion) + Error(as.factor(sub)/(as.factor(sameDirection1S0D))), data=fgmdata)
summary(ANOVA)
#### END #### 
#### final ####
tmpdata <- aggregate(responseError~ uncuedCat + sub + sameDirection1S0D, fgmdata, mean)
tmpdata$sameDirection1S0D <- as.factor(tmpdata$sameDirection1S0D)
tmpdata$uncuedCat <- as.factor(tmpdata$uncuedCat)
my_comparisons <- list(c("0", "1"))
ggbarplot(tmpdata[!(tmpdata$uncuedCat==0),], x = "sameDirection1S0D", y = "responseError", 
          add = c("mean_se", "jitter"),
          color = "sameDirection1S0D", palette = "jco",
          position = position_dodge(0.8))+ 
  stat_compare_means(paired = TRUE, comparisons = my_comparisons)+
  stat_compare_means(label.y = 0.3)+
  facet_grid(~uncuedCat)
ggline(tmpdata[!(tmpdata$uncuedCat==0),], x = "sameDirection1S0D", y = "responseError", 
       add = c("mean_se", "jitter"),
       color = "uncuedCat", palette = "jco")+ 
  stat_compare_means(paired = TRUE, comparisons = my_comparisons)+
  stat_compare_means(label.y = 0.3)+
  facet_grid(~uncuedCat)
t.test(tmpdata$responseError[tmpdata$uncuedCat==1 & tmpdata$sameDirection1S0D==1], 
       tmpdata$responseError[tmpdata$uncuedCat==1 & tmpdata$sameDirection1S0D==0], paired = TRUE, alternative = "greater") 
t.test(tmpdata$responseError[tmpdata$uncuedCat==-1 & tmpdata$sameDirection1S0D==1], 
       tmpdata$responseError[tmpdata$uncuedCat==-1 & tmpdata$sameDirection1S0D==0], paired = TRUE, var.equal = TRUE, alternative = "less") 
ggpaired(tmpdata[!(tmpdata$uncuedCat==0),], x = "sameDirection1S0D", y = "responseError",
         color = "uncuedCat", line.color = "gray", line.size = 0.4,
         palette = "jco")+
  stat_compare_means(paired = TRUE)+facet_grid(~uncuedCat)
#real g
realG <- ggpaired(tmpdata[!(tmpdata$uncuedCat==0),], x = "sameDirection1S0D", y = "responseError",
         color = "sameDirection1S0D", line.color = "gray", line.size = 0.4, position = position_dodge(0.5))+
         stat_compare_means(paired = TRUE, label.y = 0.15, comparisons = my_comparisons)+
         facet_grid(~uncuedCat)+
         scale_color_grey(start = 0.0, end = 0.5)+
         coord_cartesian(ylim = c(-0.15, 0.15))+
        stat_boxplot(notch = FALSE, outlier.shape=8)+
        geom_point(shape=16, position=position_jitter(0.01))+
        stat_summary(fun.y=mean, shape=25, size=0.4, col = "darkred", fill="red")
        #stat_boxplot(width = 0.5, notch = FALSE, outlier.shape=8) # geom ='errorbar' #         stat_summary(fun.y=mean, geom="circle", shape=23, size=4)+
summary(realG)
realG
realG$layers <- realG$layers[-3]
realG
realG$layers <- realG$layers[-2]
realG
# 
ggline(tmpdata[!(tmpdata$uncuedCat==0),], x = "sameDirection1S0D", y = "responseError", 
       add = c("mean_se", "jitter"),
       color = "uncuedCat", palette = "jco")+ 
  stat_compare_means(paired = TRUE, comparisons = my_comparisons)+
  stat_compare_means(label.y = 0.3)
ggpairs(tmpdata)+theme_bw()
ggpairs(tmpdata[!(tmpdata$uncuedCat==0),], columns = 1:4, ggplot2::aes(colour=sameDirection1S0D))
compare_means(responseError ~ sameDirection1S0D, data = tmpdata[(tmpdata$uncuedCat==1),], paired = TRUE, alternative = "greater", method = "t.test")
compare_means(responseError ~ sameDirection1S0D, data = tmpdata[(tmpdata$uncuedCat==1),], paired = TRUE, alternative = "greater", method = "wilcox.test")
compare_means(responseError ~ sameDirection1S0D, data = tmpdata[(tmpdata$uncuedCat==-1),], paired = TRUE, alternative = "less")
compare_means(responseError ~ sameDirection1S0D, data = tmpdata, paired = TRUE, alternative = "greater", method = "kruskal.test")
compare_means(responseError ~ sameDirection1S0D, data = tmpdata, paired = TRUE, alternative = "greater", method = "anova")
# Subset weight data before treatment
ungrouped <- subset(tmpdata[(tmpdata$uncuedCat==1),],  sameDirection1S0D == "0", responseError,
                 drop = TRUE)
# subset weight data after treatment
grouped <- subset(tmpdata[(tmpdata$uncuedCat==1),],  sameDirection1S0D == "1", responseError,
                    drop = TRUE)
# Plot paired data
library(PairedData)
pd <- paired(ungrouped, grouped)
plot(pd, type = "profile") + theme_bw()
ungrouped <- subset(tmpdata[(tmpdata$uncuedCat==-1),],  sameDirection1S0D == "0", responseError,
                    drop = TRUE)
# subset weight data after treatment
grouped <- subset(tmpdata[(tmpdata$uncuedCat==-1),],  sameDirection1S0D == "1", responseError,
                  drop = TRUE)
# check assumptions of t-test paired
# our sample size is large enough (more than 30), 
# no need to check whether the differences of the pairs follow a normal distribution Central Limit Theorem
# Shapiro-Wilk normality test for the differences
d <- with(tmpdata[(tmpdata$uncuedCat==-1),], 
          responseError[sameDirection1S0D == "0"] - responseError[sameDirection1S0D == "1"])
shapiro.test(d) # => FLAT -> p-value = 0.1305 # we assume the normality
d <- with(tmpdata[(tmpdata$uncuedCat==1),], 
          responseError[sameDirection1S0D == "0"] - responseError[sameDirection1S0D == "1"])
shapiro.test(d) # => TALL -> p-value = 0.003 # we do not assume the normality, need to 
# do Wilcoxon:  non-normality is less important in large samples (at least in respect of significance level, though power might still be an issue if you need to find small effects
#Zimmerman and Zumbo (1993)[1] suggest a Welch-t-test on the ranks which they say performs better that the Wilcoxon-Mann-Whitney in cases where the variances are unequal.
ggqqplot(d) #almost all the points fall approximately along this reference line, we can assume normality.
x <- tmpdata$responseError[tmpdata$uncuedCat==1 & tmpdata$sameDirection1S0D==1]
y <- tmpdata$responseError[tmpdata$uncuedCat==1 & tmpdata$sameDirection1S0D==0]
wilcox.test(x, y, paired = TRUE, 
                   alternative = "greater")
t.test(x,y, paired = TRUE, alternative = "greater")
# doing mixed effects models
fit.fgm <- lmer(responseError ~ (1 | sub), data = fgmdata)
summary(fit.fgm)
par(mfrow = c(1, 2))
qqnorm(ranef(fit.fgm)$sub[,"(Intercept)"], 
       main = "Random effects")
qqnorm(resid(fit.fgm), main = "Residuals")
par(mfrow = c(1, 1))
with(tmpdata, interaction.plot(x.factor = uncuedCat, 
                               trace.factor = sub, 
                               response = responseError))
fit.fgm <- lmer(responseError ~ (1 | sub) + (1 | uncuedCat) + 
                      (1 | sub:uncuedCat), data = tmpdata)
summary(fit.fgm) # hot much of a variance is going on
fit.fgm <- lmer(responseError ~ sameDirection1S0D*uncuedCat + (1 | sub) + 
                       (1 | sub:sameDirection1S0D) + (1 | sub:uncuedCat), data = tmpdata)
summary(fit.fgm)
anova(fit.fgm) # anova for the fixed effects
# probably this is driven by circle condition. I might not need to run an ANOVA actually since there 
# also I'm moving away from uncuedCat analysis
tmpdata <- aggregate(responseError~ uncuedAR + sub + sameDirection1S0D, fgmdata, mean)
ggplot(tmpdata, aes(x = uncuedAR, y = responseError, color=as.factor(sameDirection1S0D))) + 
  geom_point(size=2, shape=23, alpha = 1/2)+
  geom_smooth(method=lm, aes(fill=as.factor(sameDirection1S0D)), fullrange=TRUE)+
  theme_classic()
summary(lmer(responseError ~ uncuedAR * sameDirection1S0D + (1 | sub) + 
       (1 | sub:sameDirection1S0D) + (1 | sub:uncuedAR), data = tmpdata))
# get each people's beta estimate
number_of_sub <- unique(tmpdata$sub)
fgmdata.indv_beta <- data.frame(matrix(ncol = 2, nrow = length(number_of_sub)))
# array(NA, dim= c(length(subject_IDs), 3))
for (r in 1:length(number_of_sub)){
  tmpdata_sub <- tmpdata[tmpdata$sub==number_of_sub[r],]
  #run a regression model on individual sub
  lm_sub <- lm(responseError ~ uncuedAR * sameDirection1S0D, data = tmpdata_sub)
  lm_beta <- summary(lm_sub)$coefficients[4]
  fgmdata.indv_beta[r,1] <- lm_beta
  fgmdata.indv_beta[r,2] = number_of_sub[r]
}
head(fgmdata.indv_beta)
plot(fgmdata.indv_beta$X1)
#doing it with simple regression motion seperately
tmpdata <- aggregate(responseError~ uncuedAR + sub + sameDirection1S0D, fgmdata, mean)
fgmdata.indv_beta <- data.frame(matrix(ncol = 3, nrow = length(number_of_sub)))
for (r in 1:length(number_of_sub)){ 
  tmpdata_sub <- tmpdata[tmpdata$sub==number_of_sub[r],]
  #run a regression model on individual sub
  lm_sub_diff <- lm(responseError ~ uncuedAR, data = tmpdata_sub[tmpdata_sub$sameDirection1S0D==0,])
  lm_beta_diff <- summary(lm_sub_diff)$coefficients[2]
  lm_sub_same <- lm(responseError ~ uncuedAR, data = tmpdata_sub[tmpdata_sub$sameDirection1S0D==1,])
  lm_beta_same <- summary(lm_sub_same)$coefficients[2]
  fgmdata.indv_beta[r,1] <- lm_beta_diff
  fgmdata.indv_beta[r,2] <- lm_beta_same
  fgmdata.indv_beta[r,3] = number_of_sub[r]
}
head(fgmdata.indv_beta)
plot(fgmdata.indv_beta$X1, fgmdata.indv_beta$X2)
abline(c(0,1)) # more points lie left of the abline, same has higher response errors
#long to wide format
meltData <- melt(fgmdata.indv_beta[1:2])
head(meltData)
p <- ggplot(meltData, aes(factor(variable), value)) 
p + geom_boxplot() + facet_wrap(~variable, scale="free")+ theme_classic() +
  geom_point()
my_comparisons = list(c("X1","X2"))
ggpaired(meltData, x = "variable", y = "value", line.color = "gray", 
                 line.size = 0.2, position = position_dodge(0.5))+
  stat_compare_means(paired = TRUE, label.y = 0.35, comparisons = my_comparisons)
gglinePlot <- ggline(meltData, x = "variable", y = "value", 
       add = c("mean_ci", "jitter"), palette = "jco")+ 
  stat_compare_means(paired = TRUE, comparisons = my_comparisons)+
  stat_compare_means(label.y = 0.3)
gglinePlot
summary(gglinePlot)
compare_means(value ~ variable, data = meltData, paired = TRUE,  method = "t.test")# alternative = "greater", method = "t.test"
ggerrorplot(meltData, x = "variable", y = "value", 
            desc_stat = "mean_ci", color = "black",
            add = "jitter", add.params = list(color = "darkgray"))+
  stat_compare_means(comparisons = my_comparisons, paired = TRUE)+
  stat_compare_means(label.y = 0.4)
#another good graph
ggline(meltData, x = "variable", y = "value",
       add = c("mean_ci", "jitter"), add.params = list(color = "darkgray"))+
  stat_compare_means(paired= TRUE, comparisons = my_comparisons)+
  stat_compare_means(label.y = 1) #stat_summary(fun.y=mean, shape=25, size=0.4, col = "darkred", fill="red")
ANOVA <- aov(responseError ~ sameDirection1S0D*uncuedCat + Error(as.factor(sub)/(sameDirection1S0D)), data=fgmdata)
summary(ANOVA) #
# now moving into plotting results, run this script first, then use the meltdata for plotting and/or use the same scripts here
# end of final # 
# how about cuedAR on regression model? #
lmm <- lmer(responseError ~ 1  + sameDirection1S0D*uncuedAR + (sameDirection1S0D|sub), tmpdata)
summary(lmm)
tmpdata <- aggregate(responseError~ cuedAR + uncuedAR + sub + sameDirection1S0D, fgmdata, mean)
lmm <- lmer(responseError ~ 1  + cuedAR+sameDirection1S0D*uncuedAR + (sameDirection1S0D|sub), tmpdata)
summary(lmm)
# oky, having cued on the tmpdata but no on the model leads to interesting results
# and, having cued on the tmpdata AND on the model reaffirms the initial analysis. 











# homework: 9.15.22: uncuedCat vs sameDirection1S0D interaction'i goruyorum, ozellikle circle oldugunda ve tall oldugunda, bunun
# uzerine yapistirip gec.
# tim's previous graph replication
tmpdata <- aggregate(responseError~ uncuedAR + sub + sameDirection1S0D, fgmdata, mean)
head(tmpdata)
tmpdata$sameDirection1S0D <- as.factor(tmpdata$sameDirection1S0D)
tmpdata$uncuedAR <- as.factor(abs(tmpdata$uncuedAR))
tmpdata$responseError <- abs(round(tmpdata$responseError, digits=3))
table(tmpdata$uncuedAR)
my_comparisons <- list(c("0.28", "0.32"))
ggline(tmpdata, x = "uncuedAR", y = "responseError",
       add = c("mean_se", "jitter"),
       color = "sameDirection1S0D")+
  stat_compare_means(paired = TRUE, comparisons = my_comparisons)+
  stat_compare_means(label.y = 0.5)+
  facet_grid(~sameDirection1S0D)

# RECAP!
tmpdata <- aggregate(responseAR ~ sub + uncuedCat + sameDirection1S0D, fgmdata, mean)
table(tmpdata$uncuedCat)
table(tmpdata$sameDirection1S0D)
my_comparisons <- list(c("0", "1"))
ggline(tmpdata, x = "sameDirection1S0D", y = "responseAR",
       add = c("mean_se", "jitter"),
       color = "uncuedCat")+
  stat_compare_means(paired = TRUE, comparisons = my_comparisons)+
  stat_compare_means(label.y = 0.5)+
  facet_grid(~uncuedCat) #ok, almost a significant finding there (.086), (uncleaned daha kotu)
#how much a circle moves around
tmpdata <- aggregate(responseAR ~ sub + uncuedCat + sameDirection1S0D + global_org, fgmdata[fgmdata$cuedAR==0,], mean)
tmpdata <- tmpdata[!(tmpdata$uncuedCat == 0),]
table(tmpdata$uncuedCat)
table(tmpdata$sameDirection1S0D)
my_comparisons <- list(c("-1", "1"))
ggline(tmpdata, x = "uncuedCat", y = "responseAR",
       add = c("mean_se", "jitter"),
       color = "sameDirection1S0D")+
  stat_compare_means(paired = TRUE, comparisons = my_comparisons)+
  stat_compare_means(label.y = 0.5)+
  facet_grid(~sameDirection1S0D) #ok, almost a significant finding there (.086) (.053) with global org, (uncleaned daha kotu)
completedPeople <- data.frame(table(tmpdata$sub))
completedPeople
newPeople <- completedPeople$Var1[completedPeople$Freq>=8]
length(newPeople)
tmpdata <- tmpdata[tmpdata$sub %in% newPeople,]
#how much a -0.5 moves around
tmpdata <- aggregate(responseAR ~ sub + uncuedCat + sameDirection1S0D, fgmdata[fgmdata$cuedAR==-0.05,], mean)
tmpdata <- tmpdata[!(tmpdata$uncuedCat == 0),]
table(tmpdata$uncuedCat)
table(tmpdata$sameDirection1S0D)
my_comparisons <- list(c("-1", "1"))
ggline(tmpdata, x = "uncuedCat", y = "responseAR",
       add = c("mean_se", "jitter"),
       color = "sameDirection1S0D")+
  stat_compare_means(paired = TRUE, comparisons = my_comparisons)+
  stat_compare_means(label.y = 0.5)+
  facet_grid(~sameDirection1S0D) #ok, almost a significant finding there (.086), (uncleaned daha kotu)
completedPeople <- data.frame(table(tmpdata$sub))
completedPeople
newPeople <- completedPeople$Var1[completedPeople$Freq>=4]
length(newPeople)
tmpdata <- tmpdata[tmpdata$sub %in% newPeople,]


# previous response error analyses that I decided not to pursue anymore # 16 Agu 22
# what is the mean response error per subject?
mean_RE_perMotionSub <- array(NA, dim= c(length(subject_IDs), 3))

for (s in 1:length(subject_IDs)){

  tmpdata <- fgmdata[fgmdata$sub == subject_IDs[s],]
  mean_RE_perMotionSub[s,1] <- subject_IDs[s]
  mean_RE_perMotionSub[s,2] <- mean(tmpdata$responseError[tmpdata$sameDirection1S0D == 0])
  mean_RE_perMotionSub[s,3] <- mean(tmpdata$responseError[tmpdata$sameDirection1S0D == 1])
}
mean_RE_perMotionSub <- data.frame(mean_RE_perMotionSub)
colnames(mean_RE_perMotionSub) <- c("id", "RE_D", "RE_S")

# t-test to see if means of response error for similar and dissimilar is different

RE_perSimilarity <- t.test(mean_RE_perMotionSub$RE_D, mean_RE_perMotionSub$RE_S, paired = T)
RE_perSimilarity # mean difference is neglible, so no significant difference.

plot(mean_RE_perMotionSub$RE_S, mean_RE_perMotionSub$RE_D)
lines(c(-1,1),c(-1,1))#draw an identifier line, not really different per condition
# this is expected because mean of cued AR and mean of uncued AR is equal
mean(fgmdata$cuedAR)
mean(fgmdata$uncuedAR)

# lets define our target matrix
fgmdata.target <- array(NA, dim = c(length(subject_IDs),7))
colnames(fgmdata.target) <- c("id", "responseError", "uncuedAR", "cuedAR",
                              "sameDirection1S0D", "global_org", "coherence")
fgmdata.target[,1] <- subject_IDs
head(fgmdata.target)

# aggregate by sub then run the same analysis again
fgmdata.agg <- aggregate(responseError ~ cuedAR + uncuedAR + 
                           sameDirection1S0D + global_org + sub + sameCatS1D0, mean, data = fgmdata)


# QUESTION: should I run the analysis by the aggregated data? How do I account for the
# variances within subjects
# run a multiple (and mixed) linear regression models on response error

library(lme4)
RE_mixed = lmer(responseError ~ cuedAR * uncuedAR * sameDirection1S0D + (1 | sub), data = fgmdata)
summary(RE_mixed) # variance is close to zero, data is too large to use mixed effects regression modeling

# normal multiple regression model

m1 <- lm(responseError ~ cuedAR, data = fgmdata)
summary(m1)

m2 <- lm(responseError ~ cuedAR + uncuedAR * sameDirection1S0D, data = fgmdata)
summary(m2)
#anova(m2)

names(m2$coefficients) <- c('Intercept','Cued AR','Uncued AR', "Same Direction", "Uncued AR x Same Direction")
summary(m2)

m3 <- lm(responseError ~ cuedAR + uncuedAR * sameDirection1S0D *global_org, data = fgmdata)
summary(m3)
anova(m3)

m4 <- lm(responseError ~ cuedAR + uncuedAR * sameDirection1S0D *global_org *sameCatS1D0, data = fgmdata)
summary(m4)
anova(m4)

# DV: Response AR

m1 <- lm(responseAR ~ cuedAR + uncuedAR * sameDirection1S0D, data = fgmdata)
summary(m1)
names(m1$coefficients) <- c('Intercept','Cued AR','Uncued AR', "Same Direction", "Uncued AR x Same Direction")
summary(m1)


# adding whisker plots

library(dotwhisker)
#plot lm results
dwplot(list(m1, m2, m3), vline = geom_vline(
  xintercept = 0,
  colour = "grey40",
  linetype = 2,
),
vars_order = c("uncuedAR:sameDirection1S0D:global_org", "sameDirection1S0D:global_org", "uncuedAR:global_org", "global_org","uncuedAR:sameDirection1S0D", "sameDirection1S0D", "uncuedAR",  "cuedAR" ),
model_order = c("Model 3", "Model 2", "Model 1")
) + theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, face="bold")
  ) + xlab("Regression Coefficients\n") + 
  coord_flip() + 
  scale_color_manual(name="",values=c("gray10", "blue", "darkviolet")) + theme_bw()  #darkviolet

ggsave(filename = "dotwhisker_bgm.png",  width = 5, height = 5, units = "in", device='png', dpi=700) 

