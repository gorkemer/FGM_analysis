# April 12th, 2022.
# Plotting script for the foreground motion experiment. 
# Same/similar to bgm_plotting.R script. Only doing it for the fgm plots
# plots to be used for the manuscript
# good representative participant: 939045

# last update: 17 Agu 22

# clear the workspace
rm(list=ls()) 
# load libraries in bulk
x<-c("ggpubr", "ggplot2", "multcomp", "pastecs", "tidyr","dplyr", "ggiraph", "ggiraphExtra", "plyr", 
     "covreg", "Hmisc", "corrplot", "psych", "tidyverse", "hrbrthemes", "viridis", "gapminder",
     "ggExtra", "scatterplot3d", "reshape2", "rlang", "plyr", "data.table", "lme4", "magrittr", "fitdistrplus",
     "gridExtra", "statmod", "dotwhisker")
require(x)
lapply(x, require, character.only = TRUE)
source("~/Documents/GitHub/ger_R_functions/plot_functions.R")
savePlot <- function(myPlot) {
  pdf("myPlot.png")
  print(myPlot)
  dev.off()
}
#### locate the data and import it ####
# loading data
setwd('~/Desktop/21Projects/Single_FG_Motion')
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
tmpdata <- aggregate(responseError~ uncuedAR + sub + sameDirection1S0D, fgmdata, mean)
regression_plot <- ggplot(fgmdata, aes(x = uncuedAR, y = responseError, color=as.factor(sameDirection1S0D))) + 
  geom_smooth(method=lm, aes(fill=as.factor(sameDirection1S0D)), fullrange=TRUE, alpha= lmAlpha)+
  coord_cartesian(ylim=c(-0.02, 0.04)) +
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
# anova # ADDED TO THE MANUSCRIPT
plot01_reg <- lmer(responseError ~ uncuedAR * sameDirection1S0D + (1 | sub) + 
               (1 | sub:sameDirection1S0D) + (1 | sub:uncuedAR), data = fgmdata, REML=FALSE)
summary(plot01_reg)
anova(plot01_reg)
print(plot01_reg)
ranef(plot01_reg) # random effects
# end of ADDED TO THE MANUSCRIPT
#doing it with simple regression motion seperately
number_of_sub <- unique(fgmdata$sub)
tmpdata <- aggregate(responseError~ uncuedAR + sub + sameDirection1S0D, fgmdata, mean)
tmpdata_cued <- aggregate(responseError~ cuedAR + uncuedAR + sub + sameDirection1S0D, fgmdata, mean)
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
#### MELT DATA ####
#long to wide format
meltData <- melt(fgmdata.indv_beta[1:2])
head(meltData)
my_comparisons = list(c("X1","X2"))
ggpaired(meltData, x = "variable", y = "value", line.color = "gray", 
         line.size = 0.2, position = position_dodge(0.5))+
  stat_compare_means(paired = TRUE, label.y = 0.35, comparisons = my_comparisons)
gglinePlot <- ggline(meltData, x = "variable", y = "value", 
                     add = c("mean_ci", "jitter"), palette = "jco")+ 
  stat_compare_means(paired = TRUE, comparisons = my_comparisons, label.y = 0.3)+
  geom_violin(alpha = 1/60)
gglinePlot
ggpaired(meltData, x = "variable", y = "value", line.color = "gray",
         line.size = 0.2)+
  stat_compare_means(paired = TRUE, label.y = 0.35, comparisons = my_comparisons)
compare_means(value ~ variable, data = meltData, paired = TRUE,  method = "t.test")# alternative = "greater", method = "t.test"
# ADDED TO MANUSCRIPT #
t.test(meltData$value[meltData$variable == "X1"], meltData$value[meltData$variable == "X2"], paired = T)
# END OF ADDED TO THE MANUSCRIPT # 
t.test(meltData$value[meltData$variable == "X1"], meltData$value[meltData$variable == "X2"], paired = T, alternative = "less")
ggerrorplot(meltData, x = "variable", y = "value", 
            desc_stat = "mean_ci", color = "black",
            add = "jitter", add.params = list(color = "variable"))+
  stat_compare_means(comparisons = my_comparisons, paired = TRUE)+
  stat_compare_means(label.y = 0.5)+
  geom_violin(alpha = 1/60)+
  scale_color_manual(name="Motion Direction",
                     labels=c(differentMotion_label,identicalMotion_label)
                     ,values=c(differentMotion_color,
                               identicalMotion_color))+
  scale_fill_manual(values=c(differentMotion_color,
                             identicalMotion_color))
#another good graph
beta_plot <- ggline(meltData, x = "variable", y = "value",
       add = c("mean_ci", "jitter"), add.params = list(size = 2, alpha = 0.5, color ="variable"))+
  stat_compare_means(paired= TRUE, comparisons = my_comparisons)+
  scale_color_manual(name="Motion Direction",
                     labels=c(differentMotion_label,identicalMotion_label)
                     ,values=c("darkgray",
                               "black"))+
  geom_hline(yintercept=0.0,linetype="longdash", color = "gray30") + 
  coord_cartesian(ylim=c(-0.6, 0.6)) #stat_summary(fun.y=mean, shape=25, size=0.4, col = "darkred", fill="red")
beta_plot
ANOVA <- aov(responseError ~ sameDirection1S0D*uncuedAR + Error(as.factor(sub)/(sameDirection1S0D)), data=fgmdata)
summary(ANOVA)
# alternative to beta_plot
beta_plot2 <- ggpaired(meltData, x = "variable", y = "value", line.color = "gray", 
         line.size = 0.2)+
  stat_compare_means(paired = TRUE, comparisons = my_comparisons)+
  scale_color_manual(name="Motion Direction",
                     labels=c(differentMotion_label,identicalMotion_label)
                     ,values=c(differentMotion_color,
                               identicalMotion_color))+
  geom_hline(yintercept=0.0,linetype="longdash", color = "gray30")
#beta_plot2$layers <- beta_plot2$layers[-3]
beta_plot2
beta_plot3 <- beta_plot2
beta_plot3$layers <- beta_plot3$layers[-2]
beta_plot3
# uncuedCat plot
tmpdata <- aggregate(responseError~ uncuedCat + sub + sameDirection1S0D, fgmdata, mean)
tmpdata$sameDirection1S0D <- as.factor(tmpdata$sameDirection1S0D)
tmpdata$uncuedCat <- as.factor(tmpdata$uncuedCat)
my_comparisons = list(c("0","1"))
uncuedCat_plot <- ggline(tmpdata, x = "sameDirection1S0D", y = "responseError",
       add = c("mean_se", "jitter"),
       color = "uncuedCat", palette = "jco", add.params = list(size = 2, alpha = 0.2))+
  stat_compare_means(paired = TRUE, comparisons = my_comparisons)+
  #stat_compare_means(label.y = 0.25)+ # ADD THIS, works for both TALL and Circle, tiny effect though!
  scale_color_manual(name="Uncued Shape Category",
                     labels=c("Flat -","Tall |")
                     ,values=c("red",
                               "black"))+
  geom_hline(yintercept=0.0,linetype="longdash", color = "gray30")
uncuedCat_plot
uncuedCat_plot2 <- uncuedCat_plot + facet_grid(~uncuedCat)
# global org plot
tmpdata <- aggregate(responseError~ uncuedCat + sub + sameDirection1S0D + global_org, fgmdata, mean)
tmpdata$sameDirection1S0D <- as.factor(tmpdata$sameDirection1S0D)
tmpdata$uncuedCat <- as.factor(tmpdata$uncuedCat)
tmpdata$global_org <- as.factor(tmpdata$global_org)
my_comparisons = list(c("0","1"))
global_org_plot <- ggline(tmpdata, x = "sameDirection1S0D", y = "responseError",
                         add = c("mean_se", "jitter"),
                         color = "uncuedCat", palette = "jco")+
  stat_compare_means(paired = TRUE, comparisons = my_comparisons)+
  #stat_compare_means(label.y = 0.25)+ # ADD THIS, works for both TALL and Circle, tiny effect though!
  scale_color_manual(name="Motion Direction",
                     labels=c(differentMotion_label,identicalMotion_label)
                     ,values=c(differentMotion_color,
                               identicalMotion_color))+
  geom_hline(yintercept=0.0,linetype="longdash", color = "gray30")
global_org_plot
global_org_plot + facet_grid(~uncuedCat+global_org)
########## end ########## 

ylabel_beta <- 2
ylabel_uncued <- 2

regression_plot + theme(legend.position = "none") + coord_cartesian(ylim=c(-0.03, 0.04))
ggsave(filename = "fgm_regression_plot.png",  width = 5, height = 5, units = "in", device='png', dpi=700) 
beta_plot + coord_cartesian(ylim=c(-0.5, 0.5)) + stat_compare_means(label.y = ylabel_beta) + theme(legend.position = "none")
ggsave(filename = "fgm_beta_plot.png",  width = 5, height = 5, units = "in", device='png', dpi=700) 
beta_plot2 + coord_cartesian(ylim=c(-0.5, 0.5)) + stat_compare_means(label.y = ylabel_beta) + theme(legend.position = "none")
ggsave(filename = "fgm_beta_plot2.png",  width = 5, height = 5, units = "in", device='png', dpi=700) 
beta_plot3 + coord_cartesian(ylim=c(-0.5, 0.5)) + stat_compare_means(label.y = ylabel_beta) + theme(legend.position = "none")
ggsave(filename = "fgm_beta_plot3.png",  width = 5, height = 5, units = "in", device='png', dpi=700) 
uncuedCat_plot + coord_cartesian(ylim=c(-0.3, 0.3)) + stat_compare_means(label.y = ylabel_uncued) + theme(legend.position = "none")
ggsave(filename = "fgm_uncuedCat_plot.png",  width = 5, height = 5, units = "in", device='png', dpi=700) 
uncuedCat_plot + geom_violin(alpha=1/60) + coord_cartesian(ylim=c(-0.3, 0.3)) + stat_compare_means(label.y = ylabel_uncued) + theme(legend.position = "none")
ggsave(filename = "fgm_uncuedCat_plot_violin.png",  width = 5, height = 5, units = "in", device='png', dpi=700) 
uncuedCat_plot2 + coord_cartesian(ylim=c(-0.3, 0.3)) + stat_compare_means(label.y = ylabel_uncued) + theme(legend.position = "none")
ggsave(filename = "fgm_uncuedCat_plot2.png",  width = 5, height = 5, units = "in", device='png', dpi=700) 


# no global organization plot global_org_plot + facet_grid(~uncuedCat+global_org)
###################################################################



# below is the prior plotting (before 16 agu) that was used for CNS 22 SF.

#### identify/rename variables for plotting ####
# plot by cued vector vs uncued vector
fgmdata$cued_vector <- ifelse(fgmdata$cued_ellipse == 1, fgmdata$e1_motion_dir, fgmdata$e2_motion_dir)
fgmdata$uncued_vector <- ifelse(fgmdata$cued_ellipse == 1, fgmdata$e2_motion_dir, fgmdata$e1_motion_dir)
# reorder levels
fgmdata$cued_vector <- factor(fgmdata$cued_vector, levels = c(0, 180, 90, 270))
fgmdata$uncued_vector <- factor(fgmdata$uncued_vector, levels = c(0, 180, 90, 270))
# rename levels
fgmdata$cued_vector <- factor(fgmdata$cued_vector, labels = c( sprintf('\u2192'), sprintf('\u2190'), sprintf('\u2191'), sprintf('\u2193')))
fgmdata$uncued_vector <- factor(fgmdata$uncued_vector, labels = c(sprintf('\u2192'), sprintf('\u2190'), sprintf('\u2191'),sprintf('\u2193')))
fgmdata$global_org <- factor(fgmdata$global_org, labels = c("between", "within" ))
#### plotting variables ####
yaxisLim <- 0.05
densityAlpha <- 0.2
densityColor <- "blue"
jitterAlpha <- 0.1
identicalMotion_label <- "Same"
differentMotion_label <- "Different"
identicalMotion_color <- "red"
differentMotion_color <- "black"
lmAlpha <- 0.1
# remove scientific notation in the entire R session
options(scipen = 100)
#### fgm plot 01 ####
fgmplot01 <- ggplot(fgmdata,cex=3, aes(x = uncuedAR, y = responseAR, fill=as.factor(sameDirection1S0D) , colour=as.factor(sameDirection1S0D))) + 
  #geom_point(shape=1, size=0.5, alpha=jitterAlpha, show.legend = FALSE) +
  #geom_jitter(shape=1, size=0.5, alpha=jitterAlpha, show.legend = FALSE) +
  #geom_density_2d(color=densityColor, alpha=densityAlpha) + 
  #coord_cartesian(ylim=c(-0.1, 0.1)) +
  coord_cartesian(ylim=c(-0.02, 0.04)) +
  geom_smooth(method = "lm", span = 1, alpha= lmAlpha,
              aes(fill = as.factor(sameDirection1S0D))) +
  # geom_smooth(method="nls",
  #             formula= y ~ x * a * w *  sqrt(2)/exp(-(0.5)) * exp(-(w*x^2)), #y ~ a + b*x, # this is an nls argument
  #             start = list(a = 0.39, w = 0.63),
  #             se=FALSE, algorithm="port",
  #             color = "firebrick2") + #aes(color = as.factor(sameDirection1S0D))
  geom_segment(aes(x=min(unique(uncuedAR)),xend=max(unique(uncuedAR)),y=0,yend=0), linetype="longdash",  color="gray50")+ 
  scale_color_manual(name="Motion Direction",
                     labels=c(differentMotion_label,identicalMotion_label)
                     ,values=c(differentMotion_color,
                               identicalMotion_color))+
  scale_fill_manual(values=c(differentMotion_color,
                               identicalMotion_color))+
  labs(x="(< flatter) Uncued AR (taller >)", y = "(< flatter) Mean AR Response ( taller >)", size = 10.5) +
  labs(title="", subtitle=" ")+
  scale_x_continuous(breaks = seq(-0.5, 0.5, by = 0.5))+
  theme_classic() +
  theme(
    #panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    axis.text.x = element_text(size=12,color="black"),
    axis.text.y = element_text(size=12,color="black"),
    legend.title = element_text(size=14),
    legend.text = element_text(size=12),
    legend.position = "none",#c(0.8, 0.14),
    axis.title.y = element_text(size = rel(1.5), angle = 90, hjust = -0.5),
    axis.title.x = element_text(size = rel(1.5), angle = 0,  vjust = -0.5)
    )
fgmplot01
#ggsave(filename = "fgmplot01.png",  width = 5, height = 5, units = "in", device='png', dpi=700) 
#ggsave(filename = "fgmplot01.pdf", width = 5, height = 5, units = "in", device='pdf', dpi=700) 
#### fgm plot 02 ####
jitterAlpha <- 0.05
fgmplot02 <- ggplot(fgmdata, aes(x = uncuedAR, y = responseError, colour=as.factor(sameDirection1S0D))) + 
  #geom_point(shape=11, size=0.5, alpha=0.5, show.legend = FALSE) +
  geom_jitter(shape=1, size=0.5, alpha=jitterAlpha, show.legend = FALSE) +
  geom_density_2d(color="blue", alpha=densityAlpha) + 
  coord_cartesian(ylim=c(-0.2, 0.2)) +
  #theme(legend.key.size = unit(0.2, "cm")) + 
  geom_smooth(method = "lm", span = 1, alpha= lmAlpha,
              aes(color = as.factor(sameDirection1S0D))) +
  geom_segment(aes(x=min(unique(uncuedAR)),xend=max(unique(uncuedAR)),y=0,yend=0), linetype="longdash",  color="gray50")+ 
  scale_color_manual(name="Motion Direction",
                     labels=c(differentMotion_label,identicalMotion_label)
                     ,values=c(differentMotion_color,
                               identicalMotion_color))+
  labs(x="(<-flatter)   Uncued AR   (taller->)", y = "(<-flatter) Response Error (taller->)") +
  labs(title="", subtitle=" ")+
  scale_x_continuous(breaks = seq(-0.5, 0.5, by = 0.5))+
  theme_classic()+ 
  facet_wrap(~cued_vector*uncued_vector, nrow = 2)+ #global_org*
  theme_classic() +
  theme(panel.spacing.x = unit(1, "lines"))
fgmplot02
jitterAlpha <- 0.1
#ggsave(filename = "fgmplot02.png", width = 14, height = 8, units = "in", device='png', dpi=700) 
#### save plots ####
#ggsave(filename = "fgm_plot2.pdf", width = 14, height = 8, units = "in", device='pdf', dpi=700) 
#savePlot(interactionPlotVectors)
#### fgm plot 03 ####
agg <- aggregate(responseAR ~ cuedAR + uncuedAR + sameDirection1S0D, fgmdata, mean)
agg1 <- subset(agg, agg$cuedAR==-0.46331866)
agg2 <- subset(agg, agg$cuedAR==-0.41698684)
agg3 <- subset(agg, agg$cuedAR==-0.37065503)
agg4 <- subset(agg, agg$cuedAR==-0.32432321)
agg5 <- subset(agg, agg$cuedAR==-0.27799139)
agg6 <- subset(agg, agg$cuedAR==-0.23165957)
agg7 <- subset(agg, agg$cuedAR==-0.18532775)
agg8 <- subset(agg, agg$cuedAR==-0.13899593)
agg9 <- subset(agg, agg$cuedAR==-0.09266412)
agg10 <- subset(agg, agg$cuedAR==-0.04633230)
agg11 <- subset(agg, agg$cuedAR==-0.00000048)
agg12 <- subset(agg, agg$cuedAR==0.04633134)
agg13 <- subset(agg, agg$cuedAR==0.09266316)
agg14 <- subset(agg, agg$cuedAR==0.13899498)
agg15 <- subset(agg, agg$cuedAR==0.18532679)
agg16 <- subset(agg, agg$cuedAR==0.23165861)
agg17 <- subset(agg, agg$cuedAR==0.27799043)
agg18 <- subset(agg, agg$cuedAR==0.32432225)
agg19 <- subset(agg, agg$cuedAR==0.37065407)
agg20 <- subset(agg, agg$cuedAR==0.41698588)
agg21 <- subset(agg, agg$cuedAR==0.46331770)
#add barkod to each agg lists, e.g. agg1 = agg = 1
agg1$barkod <- 1
agg2$barkod <- 2
agg3$barkod <- 3
agg4$barkod <- 4
agg5$barkod <- 5
agg6$barkod <- 6
agg7$barkod <- 7
agg8$barkod <- 8
agg9$barkod <- 9
agg10$barkod <- 10
agg11$barkod <- 11
agg12$barkod <- 12
agg13$barkod <- 13
agg14$barkod <- 14
agg15$barkod <- 15
agg16$barkod <- 16
agg17$barkod <- 17
agg18$barkod <- 18
agg19$barkod <- 19
agg20$barkod <- 20
agg21$barkod <- 21
aggToget <- rbind(agg1,agg2, agg3, agg4, agg5, agg6, agg7, agg8, agg9, agg10, agg11, agg12, agg13, agg14, agg15, agg16, agg17, agg18, agg19, agg20, agg21)
aggToget$sameDirection1S0D <- factor(aggToget$sameDirection1S0D, labels = c("Ungrouped", "Grouped"))
plot03 <- ggplot(aggToget, aes(x = uncuedAR, y =responseAR )) + 
  geom_point(shape=1, size=0.2, alpha=0.5, show.legend = FALSE, aes(color = as.factor(cuedAR))) +
  coord_cartesian(ylim=c(-0.4, 0.4)) +
  #geom_jitter(shape=1, size=0.5, alpha=jitterAlpha + 0.03, show.legend = FALSE) +
  #geom_line(data = aggToget, aes(group = barkod, colour = as.factor(barkod))) +
  geom_density_2d(color=densityColor, alpha=densityAlpha) + 
  geom_smooth(method = "lm",
              aes(color = factor(cuedAR)), alpha = lmAlpha-0.3)+
  labs(x="(<-flatter)   Uncued AR   (taller->)", y = "(<-flatter) Response (taller->)")+ 
  facet_grid(~sameDirection1S0D) + 
  scale_colour_grey() + 
  theme_classic() + 
  theme(panel.spacing.x = unit(1.5, "lines"))
plot03
#ggsave(filename = "fgmplot03.pdf", width = 14, height = 8, units = "in", device='pdf', dpi=700) 
#### fgm plot 00 ####
dnorm_deriv1 <- function(x, mean = 0, sd = 1) {
  return(-((x-mean)/(sd^2))*dnorm(x, mean, sd))
} 
dnorm_deriv2 <- function(x, mean = 0, sd = 1) {
  return((((x-mean)^2) / (sd^4))*dnorm(x, mean, sd) 
         - (1/sd^2)*dnorm(x, mean, sd))
}
curve(dnorm, -4, 4, ylim = c(-0.4, 0.4), col = 'blue')
curve(dnorm_deriv1, -4, 4, add = T, col = 'red')
curve(dnorm_deriv2, -4, 4, add = T, col = ' green')
abline(v=0, h=0)
firstDerGauss <- function(x,a,w) {
  c = 2.33#sqrt(2)/exp(-(0.5))
  y = x * a * w * c *exp(-(w*x^2))
}
invGaussFormula = ' y ~ x * a * w * c * exp(-(w*x^2))' # "y = x * a * w * c *np.exp(-(w*x**2))"
y = x*a*w*c*exp(-(w*x)^2)
xdata = fgmdata$cuedAR
ydata = fgmdata$responseAR
fit_y = firstDerGauss(xdata, a, w)
plot(xdata, fit_y)
plot(xdata, ydata)
# Take the assumed values and fit into the model.
#Normalized Response AR Data
fgmdata$responseAR_norm <- (fgmdata$responseAR-min(fgmdata$responseAR))/(max(fgmdata$responseAR)-min(fgmdata$responseAR))
plot00 <- ggplot(fgmdata, aes(x = cuedAR, y =responseAR_norm)) + 
  geom_jitter(shape=1, size=0.2, alpha=0.5, show.legend = FALSE) +
  geom_density_2d(color=densityColor, alpha=densityAlpha) + 
  # geom_smooth( method = "glm", 
  #             method.args = list(family = "binomial"), 
  #             se = FALSE, linetype = 1)+
  geom_smooth(method="nls",
              formula= y ~ x * a * w * c* exp(-(w*x)^2), # this is an nls argument
              method.args = list(start=c(a=0.39,w=0.63,c=2.33)), # this too
              se=FALSE)+
  labs(x="(<-flatter)   Cued AR   (taller->)", y = "(<-flatter) Response (taller->)")
plot00 + facet_wrap(~sameDirection1S0D)
#write.csv(aggToget, "aggTogetBGM.csv")
x <- fgmdata$cuedAR
y <- fgmdata$responseAR
fit <- nls(y ~ (x * a * w * sqrt(2)/exp(-(0.5)) * exp(-(w*x^2)) ), 
           start = list(a = 0.39, w = 0.63),
           algorithm = "port")
fit
# Predict the fitted model to a denser grid of x values
dffit <- data.frame(cuedAR=seq(-1.5, 1.5, 0.01))
dffit$responseAR_fit <- predict(fit, newdata=dffit)
test <- predict(fit, newdata=dffit)
updated_df <- data.frame(fgmdata$cuedAR, test)
# Plot the data with the model superimposed
newData <- data.frame(fgmdata$cuedAR, responseAR_fit)
ggplot(updated_df, aes(x=colnames(updated_df)[1], y=colnames(updated_df)[2])) + geom_point() +
        geom_smooth(data=dffit, stat="identity", color="red", size=1.5)
plot(updated_df$fgmdata.cuedAR, fgmdata$responseAR)
## terminates in an error, because convergence cannot be confirmed:
try(nls(y ~ a + b*x, start = list(a = 0.12345, b = 0.54321)))
test_df <- data.frame(x = x, y = y)
constantP <- 50##(sqrt(2)/exp(-(0.5)))
testPlot <- ggplot(test_df, aes(x = x, y =y)) + 
  geom_jitter(shape=1, size=0.2, alpha=0.5, show.legend = FALSE) +
  #geom_density_2d(color=densityColor, alpha=densityAlpha) + 
  # geom_smooth( method = "glm", 
  #             method.args = list(family = "binomial"), 
  #             se = FALSE, linetype = 1)+
  geom_smooth(method="nls",
              formula= y ~ x * a * w * constantP * exp(-(w*x^2)), #y ~ a + b*x, # this is an nls argument
              start = list(a = 0.39, w = 0.63),
              se=FALSE, color = "maroon", algorithm="port")
testPlot
testPlot <- ggplot(test_df, aes(x = x, y =y)) + 
  geom_jitter(shape=1, size=0.2, alpha=0.5, show.legend = FALSE) +
  geom_smooth(method="lm")
testPlot
#### fgm plot 04 ####
# sanity check plot
#fgmdata$responseAR_norm <- (fgmdata$responseAR-min(fgmdata$responseAR))/(max(fgmdata$responseAR)-min(fgmdata$responseAR))
fgmdata.submean <- aggregate(responseAR ~ cuedAR + sub, fgmdata, mean)
fgmplot04 <- ggplot(fgmdata.submean, aes(x = cuedAR, y = responseAR)) +
  geom_jitter(alpha = 1/4, aes(color = as.factor(sub))) +
  geom_density_2d(color=densityColor, alpha=densityAlpha) +
  # geom_abline(linetype = 11,  color="gray50") +
  geom_segment(aes(x=min(unique(cuedAR)),xend=max(unique(cuedAR)),y=0,yend=0), linetype = 11,  color="gray30")+ 
  geom_segment(aes(x=min(unique(cuedAR)),xend=max(unique(cuedAR)),
                   y=min(unique(cuedAR)),yend=max(unique(cuedAR))), 
                   linetype = 11,  color="gray30")+ 
  geom_smooth(method = "lm", color = "firebrick2") +
  # geom_smooth(method="nls",
  #             formula= y ~ x * a * w *  sqrt(2)/exp(-(0.5)) * exp(-(w*x^2)), #y ~ a + b*x, # this is an nls argument
  #             start = list(a = 0.39, w = 0.63),
  #             se=FALSE, algorithm="port",
  #             color = "firebrick2") + #aes(color = as.factor(sameDirection1S0D))
  labs(x="(< flatter) Cued AR (taller >)", y = "(< flatter) Mean AR Response ( taller >)", size = 10.5) +
  labs(title="", subtitle=" ")+
  scale_x_continuous(breaks = seq(-0.5, 0.5, by = 0.5))+
  coord_cartesian(ylim=c(-0.5, 0.5)) +
  theme_classic() +
  theme(
    #panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    axis.text.x = element_text(size=12,color="black"),
    axis.text.y = element_text(size=12,color="black"),
    legend.title = element_text(size=14),
    legend.text = element_text(size=12),
    legend.position = "none",#c(0.8, 0.14),
    axis.title.y = element_text(size = rel(1.5), angle = 90, hjust = -0.5),
    axis.title.x = element_text(size = rel(1.5), angle = 0,  vjust = -0.5)
  )
fgmplot04 #+ facet_wrap(~sub) + #c(0.8, 0.14),
#ggsave(filename = "fgmplot04.png",  width = 5, height = 5, units = "in", device='png', dpi=700) 
#### fgmplot 05 - response error plot - ####
fgmdata.submean <- aggregate(responseError ~ cuedAR + sub, fgmdata, mean)
fgmplot05 <- ggplot(fgmdata.submean, aes(x = cuedAR, y = responseError)) +
  geom_jitter(alpha = 1/4, aes(color = as.factor(sub))) +
  geom_density_2d(color=densityColor, alpha=densityAlpha) +
  # geom_abline(linetype = 11,  color="gray50") +
  geom_segment(aes(x=min(unique(cuedAR)),xend=max(unique(cuedAR)),y=0,yend=0), linetype = 11,  color="gray60")+ 
  # geom_segment(aes(x=min(unique(cuedAR)),xend=max(unique(cuedAR)),
  #                  y=min(unique(cuedAR)),yend=max(unique(cuedAR))), 
  #              linetype = 11,  color="gray60")+ 
  # geom_smooth(method = "lm", color = "blue") +
  geom_smooth(method="nls",
              formula= y ~ x * a * w *  sqrt(2)/exp(-(0.5)) * exp(-(w*x^2)), #y ~ a + b*x, # this is an nls argument
              start = list(a = 0.39, w = 0.63),
              se=FALSE, algorithm="port",
              color = "firebrick2") + #aes(color = as.factor(sameDirection1S0D))
  labs(x="(< flatter) Cued AR (taller >)", y = "(< flatter) Response Error ( taller >)", size = 10.5) +
  labs(title="", subtitle=" ")+
  scale_x_continuous(breaks = seq(-0.5, 0.5, by = 0.5))+
  #scale_y_continuous(breaks = seq(-0.5, 0.5, by = 0.5))+
  coord_cartesian(ylim=c(-0.5, 0.5)) +
  theme_classic() +
  theme(
    #panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    axis.text.x = element_text(size=12,color="black"),
    axis.text.y = element_text(size=12,color="black"),
    legend.title = element_text(size=14),
    legend.text = element_text(size=12),
    legend.position = "none",#c(0.8, 0.14),
    axis.title.y = element_text(size = rel(1.5), angle = 90, hjust = -0.5),
    axis.title.x = element_text(size = rel(1.5), angle = 0,  vjust = -0.5)
  )
fgmplot05 #+ facet_wrap(~sub) + #c(0.8, 0.14),
#ggsave(filename = "fgmplot05.png",  width = 5, height = 5, units = "in", device='png', dpi=700) 
# response error x uncuedAR plot
fgmdata.submean <- aggregate(responseError ~ uncuedAR + sub + sameDirection1S0D, fgmdata, mean)
responseERPLot <- ggplot(fgmdata.submean, aes(x = uncuedAR, y = responseError)) +
  geom_jitter(alpha = 1/2, aes(color = as.factor(sub))) +
  geom_density_2d(color=densityColor, alpha=densityAlpha) +
  geom_smooth(method = "lm",aes(color = as.factor(sameDirection1S0D))) +
  # geom_smooth(method="nls",
  #             formula= y ~ x * a * w *  sqrt(2)/exp(-(0.5)) * exp(-(w*x^2)), #y ~ a + b*x, # this is an nls argument
  #             start = list(a = 0.39, w = 0.63),
  #             se=FALSE, algorithm="port",
  #             color = "maroon") + #aes(color = as.factor(sameDirection1S0D))
  theme_classic() #+ facet_wrap(~uncuedCatM1F0C1T, nrow=3)
responseERPLot +  theme(legend.position = "none") #+ facet_wrap(~sub) + #c(0.8, 0.14),
responseERPLot




