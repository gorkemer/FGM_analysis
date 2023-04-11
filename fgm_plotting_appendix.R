# April 12th, 2022.
# Plotting script for the foreground motion experiment. 
# Same/similar to bgm_plotting.R script. Only doing it for the fgm plots
# plots to be used for the manuscript
# good representative participant: 939045
# last update: 12/15/22
# clear the workspace
rm(list=ls()) 
# load libraries in bulk
x<-c("ggpubr", "ggplot2", "multcomp", "pastecs", "tidyr","dplyr", "ggiraph", "ggiraphExtra", "plyr", 
     "covreg", "Hmisc", "corrplot", "psych", "tidyverse", "hrbrthemes", "viridis", "gapminder",
     "ggExtra", "scatterplot3d", "reshape2", "rlang", "plyr", "data.table", "lme4", "magrittr", "fitdistrplus",
     "gridExtra", "statmod", "dotwhisker")
require(x)
lapply(x, require, character.only = TRUE)
savePlot <- function(myPlot) {
  pdf("myPlot.png")
  print(myPlot)
  dev.off()
}
#### locate the data and import it ####
# loading data
setwd('~/Desktop/21Projects/FGM_analysis')
fgmdata = read.csv('fgmdata.csv', header = TRUE)
fgmdata$cuedCat = ifelse(fgmdata$cuedAR < 0, -1, ifelse(fgmdata$cuedAR==-0, 0, 1))
fgmdata$uncuedCat = ifelse(fgmdata$uncuedAR < 0, -1, ifelse(fgmdata$uncuedAR==-0, 0, 1))
fgmdata$uncuedCat <- as.factor(fgmdata$uncuedCat)
fgmdata$respAcc <- ifelse( (fgmdata$responseAR > 0 & fgmdata$cuedCat == 1)  | (fgmdata$responseAR < 0 & fgmdata$cuedCat == -1) | (fgmdata$responseAR == 0 & fgmdata$cuedCat == 0), 1, 0)
table(fgmdata$respAcc)
number_of_sub <- unique(fgmdata$sub)
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
testToBeCleaned = below50P[1]
fgmdata.cleaning[fgmdata.cleaning$id == testToBeCleaned,]
plot(fgmdata$cuedAR[fgmdata$sub == testToBeCleaned], fgmdata$responseAR[fgmdata$sub == testToBeCleaned])
# those below40P all show bad performance in all metrics (e.g. regression beta, p-value, correlation, reaction time). 
incompletedPeople <- fgmdata.cleaning$id[fgmdata.cleaning$trialN<241]
incompletedPeople
#### CLEANED DATA ####
#fgmdata <- fgmdata[!( (fgmdata$sub %in% below50P)),]
fgmdata <- fgmdata[!( (fgmdata$sub %in% incompletedPeople)),]
#### plotting variables ####
yaxisLim <- 0.05
densityAlpha <- 0.2
densityColor <- "blue"
jitterAlpha <- 0.1
identicalMotion_label <- "Same"
differentMotion_label <- "Different"
identicalMotion_color <- "black"
differentMotion_color <- "darkgray"
lmAlpha <- 0.1
# plotting regression plot
# number of sub: 
length(unique(fgmdata$sub))
tmpdata <- aggregate(responseAR~ uncuedAR + sub + sameDirection1S0D, fgmdata, mean)
regression_plot <- ggplot(fgmdata, aes(x = uncuedAR, y = responseAR, color=as.factor(sameDirection1S0D))) + 
  geom_smooth(method=lm, aes(fill=as.factor(sameDirection1S0D)), fullrange=TRUE, alpha= lmAlpha)+
  coord_cartesian(ylim=c(-0.02, 0.04)) +
  #coord_cartesian(ylim=c(-0.06, 0.06)) +
  geom_hline(yintercept=0.0,linetype="longdash", color = "gray50") + 
  #geom_segment(aes(x=min(unique(uncuedAR)),xend=max(unique(uncuedAR)),y=0,yend=0), linetype="longdash",  color="gray50")+ 
  scale_color_manual(name="Motion Direction",
                     labels=c(differentMotion_label,identicalMotion_label)
                     ,values=c(differentMotion_color,
                               identicalMotion_color))+
  scale_fill_manual(values=c(differentMotion_color,
                             identicalMotion_color))+
  labs(x="(< flatter) Uncued AR (taller >)", y = "(< err on flatter) Mean Response Error ( err on taller >)", size = 10.5) +
  labs(title="", subtitle=" ")+
  scale_x_continuous(breaks = seq(-0.5, 0.5, by = 0.5))+
  theme_classic() +
  theme(
    #panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    #axis.text.x = element_text(size=12,color="black"),
    #axis.text.y = element_text(size=12,color="black"),
    #legend.title = element_text(size=14),
    #legend.text = element_text(size=12),
    legend.position = "none",#c(0.8, 0.14),
    #axis.title.y = element_text(size = rel(1.5), angle = 90, hjust = -0.5),
    #axis.title.x = element_text(size = rel(1.5), angle = 0,  vjust = -0.5)
  )
regression_plot
regression_plot + theme(legend.position = "none") #+ coord_cartesian(ylim=c(-0.03, 0.04))
ggsave(filename = "fgm_regression_plot_appendix.png",  width = 5, height = 5, units = "in", device='png', dpi=700) 


