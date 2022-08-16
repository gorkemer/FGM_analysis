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
     "covreg", "Hmisc", "corrplot", "psych", "tidyverse", "hrbrthemes", "viridis", "gapminder",
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

# number of subjects
subject_IDs <- unique(fgmdata$sub)

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

