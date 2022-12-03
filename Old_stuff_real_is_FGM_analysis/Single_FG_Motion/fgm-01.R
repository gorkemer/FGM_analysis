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

library(extrafont)
font_import("Trebuchet MS")
library(actuar)
library(statmod)

#theme_set(theme_gray(base_size = 18))

#remove scientific notation in the entire R session
options(scipen = 100)

# set wd if needed setwd('~/Desktop/background-motion/Data Analysis'), check with getwd()
setwd('~/Desktop/VPEC_Projects/Single_FG_Motion')
#data = read.csv("fgm_cleanData_lm.csv", header = TRUE)
#data = read.csv('fgm_cleanData_cor.csv', header=TRUE)
data = read.csv('untouchedData.csv', header = TRUE)

#subset identical pairs
data.identicals <- subset(data, data$pairAR_sameCondition==1)
#data.identicals <- arrange(data.identicals, id, cuedAR, desc(pairAR_sameCondition), desc(responseAR))
data.differents <- subset(data, data$pairAR_sameCondition==0)

#get the aggregated response to cuedAR *add for running for the whole set
data_agg_same <- aggregate(responseAR ~ cuedAR + id, data = data.identicals, mean)
data_agg_diff <- aggregate(responseAR ~ cuedAR + id, data = data.differents, mean)
data_agg_same_all <-  aggregate(responseAR~cuedAR, data.identicals, mean)
data_agg_diff_all <-  aggregate(responseAR~cuedAR, data.differents, mean)

data_SameDiff_difference <- data_agg_diff_all - data_agg_same_all # plain difference between agg resp to different vs identical shapes
t.test(data_SameDiff_difference$responseAR, mu = 0) # difference is not different than 0 

#create normBySame_responseAR
data.differents$normed_responseAR <- NA
data.differents$responseToIdentical <- NA
data.differents$selectedCuedARofIdentical <- NA

for (i in 1:nrow(data_agg_same_all)) { # get the response to the same

  # get the id of a given trial
  #selectedID <- data_agg_same$id[i]
  selectedCuedAR <- data_agg_same_all$cuedAR[i]
  selectedResponseARtoSame <- data_agg_same_all$responseAR[i] #this is the response to the identical pair

  #look through the data and match the cuedAR with the data
  for (a in 1:nrow(data.differents)){
    if (selectedCuedAR == data.differents$cuedAR[a]){ #&& selectedID == data.differents$id[a]){ #find the cuedAR and id couplings and substract response to identical from that response on that trial
      data.differents$normed_responseAR[a] <- data.differents$responseAR[a] - selectedResponseARtoSame #positive values indicate deviation from the cuedAR towards "taller" responses
      data.differents$responseToIdentical[a] <- selectedResponseARtoSame
      data.differents$selectedCuedARofIdentical[a]<- selectedCuedAR #this should match with the trial's cuedAR
    }
  }
}

##using norm responsesget the aggregated response to cuedAR *add for running for the whole set
data_agg_same <- aggregate(responseAR ~ cuedAR + id, data = data.identicals, mean)
data_agg_diff_norm <- aggregate(normed_responseAR ~ cuedAR + id, data = data.differents, mean)
data_agg_same_all <-  aggregate(responseAR~cuedAR, data.identicals, mean)
data_agg_diff_all_norm <-  aggregate(normed_responseAR~cuedAR, data.differents, mean)

data_SameDiff_difference_norm <- data_agg_diff_all_norm - data_agg_same_all # plain difference between agg resp to different vs identical shapes
t.test(data_SameDiff_difference_norm$normed_responseAR, mu = 0) # again, difference is not different than 0, but the difference actually increased
# with the normalization. 

# run basic model
lm_main <- lm(normed_responseAR ~  uncuedAR * pairMotSame, data.differents)
summary(lm_main) #Anova(lm_main)

lm_main <- lm(normed_responseAR ~  uncuedAR * sameARCat * pairMotSame * pairGlobalOrg, data.differents)
summary(lm_main) #Anova(lm_main)
### we see globalOrg effect. And, interestingly
# and this relationship reversed at the within-hemifield as if globaOrg affects some cuedAR further. 

# next: look seperately for tall, flat, and equal shapes. 

lm_second <- lm(normed_responseAR ~ uncuedAR * pairMotSame  * sameARCat, data.differents)
summary(lm_second) #same results as the basic model. No averaging, interaction btw globalOrg & cuedAR only. 
# !!! with the unclean data results are close to significance! uncued has repulsive influence, but when they are close
# together at the within hemifield, beta turns into positive, meaning averaging is occuring.
# !!! and an effect of pairMotSame! increase in uncuedAR, when motion is the same, increases the response!!!!!!!!!! 
## THIS IS DONE!!! #### The only issue is uncued have repulsive influence as a main effect, and this is uncleaned data. 
# next: I can find out about the distance between shapes, I know it varied by monitor. I can just cut by that. First I can look 
# at how it affected the results, of course. 

lm_main <- lm(normed_responseAR ~  cuedAR * uncuedAR * pairMotSame, data.differents)
summary(lm_main) # again, pairMotSame x uncuedAR interacts but p = 0.06. (0.04 with 41/72 people) This is when it is uncleaned. I might do alternative (non-cor) cleaning. 


## also I can look at 1) how pairMotSame influences SD of the responses, 2) how sameARCat plays a role? 
#sameARCat
lm_second <- lm(normed_responseAR ~ cuedAR * uncuedAR * sameARCat * pairMotSame * pairGlobalOrg, data.differents)
summary(lm_second) # wow so much interaction, not sure how to interpret this. It looks like when they have the same AR category
# increase in uncuedAR has higher impact on the cuedAR (facilitatory impact).

# coherence?
data.differents$coherenceLevel <- ifelse(data.differents$coherence==0.8, 1, 0)
lm_second <- lm(normed_responseAR ~ cuedAR * uncuedAR * pairMotSame * pairGlobalOrg * coherenceLevel, data.differents)
summary(lm_second) #slight interaction with globalOrg, higher coherence facilitated the effect of globalOrg. 
lm_main <- lm(normed_responseAR ~  cuedAR * uncuedAR * pairMotSame * coherenceLevel, data.differents)
summary(lm_main) # no influence of coherence
