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

source("plot_functions.R")

# 1- confirm people did the task cued vs response
# 2- do the stim underestimation -> identical trials reg cued vs response
# 3- do the main regression analyses and check if global organization flips the mechanism

#remove scientific notation in the entire R session
options(scipen = 100)

setwd('~/Desktop/21Projects/Single_FG_Motion')

data = read.csv('motion_RA.csv', header = TRUE)

data.identicals = subset(data, data$sameAR==1)
data.differents = subset(data, data$sameAR==0)
data$motionThreeLevels = ifelse(data$e1MotDir == data$e2MotDir, 0, 
                                ifelse((data$e1MotDir == 180 & data$e2MotDir == 0) | (data$e1MotDir == 90 & data$e2MotDir == 270),"diverge", "converge" ))

#get the aggregated response to cuedAR *add for running for the whole set
data_agg_same <- aggregate(responseAR ~ cuedAR + id, data = data.identicals, mean)
data_agg_diff <- aggregate(responseAR ~ cuedAR + id, data = data.differents, mean)
data_agg_same_all <-  aggregate(responseAR~cuedAR, data.identicals, mean)
data_agg_diff_all <-  aggregate(responseAR~cuedAR, data.differents, mean)

data_SameDiff_difference <- data_agg_diff_all - data_agg_same_all # plain difference between agg resp to different vs identical shapes
t.test(data_SameDiff_difference$responseAR, mu = 0) # difference is not different than 0 

# run check model
lm_check <- lm(responseAR ~  cuedAR, data)
summary(lm_check) #Anova(lm_main)

#underestimation score
lm_SameSame <- lm(responseAR~ cuedAR, data = data.identicals)
summary(lm_SameSame)
#predict the underestimation amount
coef(lm_SameSame)[1] + 0.50*coef(lm_SameSame)[2] # at 0.50, what is the y axis

#### Simulate Responses ####
for (i in 1:nrow(data)) { # for each trial in data,
  data$simResp_middle[i] = runif(1, -.2, .2) #create a random response from the middle of the response dist
  data$simResp_underest[i] = data$cuedAR[i]*.5 + runif(1, -.05, .05) #or underestimate the cuedAR, then add some noise
  data$simResp_underest_withRealCoef[i] = coef(lm_SameSame)[1] + data$cuedAR[i]*coef(lm_SameSame)[2] #or underestimate the cuedAR, then add some noise
}


#run a regression analysis on arDiff and response 
initial_model <- lm(responseAR~ arDiff, data = data)
summary(initial_model)

#which of the two explains the data better. Here I control for the underestimation component
controlled_model <- lm(responseAR~ arDiff + simResp_underest_withRealCoef, data = data)
summary(controlled_model)

#now I add the motion direction component
motion_model <- lm(responseAR~ arDiff * sameMotion + simResp_underest_withRealCoef, data = data)
summary(motion_model)

#now I add the global organization component
globalOrg_model <- lm(responseAR~ arDiff * sameMotion * globalOrg * cuedMotDir + simResp_underest_withRealCoef, data = data)
summary(globalOrg_model)

#now I add same AR Cat component
sameARCat_model <- lm(responseAR~ arDiff * sameMotion * sameARCat + simResp_underest_withRealCoef, data = data)
summary(sameARCat_model)

#total model
total_model <- lm(responseAR~ arDiff * sameMotion * sameARCat * globalOrg + simResp_underest_withRealCoef, data = data)
summary(total_model)

#### model comparisons #### 
# the model with the lowest AIC and BIC score is preferred.
AIC(globalOrg_model)  # underestimation controlled arDiff & motion: 5.2k (5.3k with only underestimation), uncontrolled score: 9.8k
BIC(main_model)

#first we see.
data$responseAR_normedWithStim <- data$responseAR - data$simResp_underest_withRealCoef

scatterPlot <- ggplot(data, aes(x = arDiff, y = responseAR - simResp_underest_withRealCoef )) + 
  geom_point(alpha=0.01, aes(colour = factor(sameMotion)), show.legend = FALSE) + #shape=11, size=0.3, 
  geom_density_2d(color="gray", alpha=0.3) + 
  #geom_jitter(size = 0.1, alpha=1/60)+
  #geom_point(aes(shape = factor(pairMotSame))) +
  #geom_point(aes(color = factor(pairMotSame))) +
  #geom_smooth(data = data, method=lm, color = "red") +
  coord_cartesian(ylim=c(-0.5, 0.5))+
  theme(legend.key.size = unit(0.2, "cm")) + 
  geom_segment(aes(x=-1,xend=1,y=0,yend=0), linetype="dotted")+ 
  geom_smooth(method = "lm", 
              aes(color = factor(sameMotion))) +
  scale_color_manual(name="Motion Direction",
                     labels=c("Unshared","Shared")
                     ,values=c("red",
                               "black"))+
  labs(x="(<-flatter)   Uncued - Cued   (taller->)", y = "(<-flatter) Response (normed) AR (taller->)") +
  labs(title="", subtitle=" ") +
  theme_classic()

# + scale_y_continuous(breaks = seq(-0.05, 0.05, by = 0.05))

label_globalOrg = c( "0" =  "Between Hemifield / Flat", "1" = "Within Hemifield / Tall") 
#tiff("test.png", units="in", width=7, height=5, res=300)
scatterPlot + facet_wrap(~globalOrg, labeller = labeller(globalOrg = label_globalOrg)) + facet_grid(~sameARCat)
#dev.off()


p1 <- plot_globalOrg(data, data$arDiff, data$responseAR, data$simResp_underest_withRealCoef, data$sameMotion ) #+  facet_grid(~sameARCat)


grid.arrange(p1, scatterPlot, nrow = 1)

#### OLD SCRIPT BELOW ##### 

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
