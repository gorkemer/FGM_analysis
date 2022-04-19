# April 12th, 2022.
# Plotting script for the foreground motion experiment. 
# Same/similar to bgm_plotting.R script. Only doing it for the fgm plots
# plots to be used for the manuscript
# good representative participant: 939045

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
  pdf("myPlot.pdf")
  print(myPlot)
  dev.off()
}

#### locate the data and import it ####
# loading data
setwd('~/Desktop/21Projects/Single_FG_Motion')
fgmdata = read.csv('fgmdata.csv', header = TRUE)

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

# for plotting on the aggregated data 
fgmdata.a <- aggregate(responseError ~ uncuedAR  + sameDirection1S0D + sub, fgmdata, mean)

#### fgm plot 01 ####

fgmplot01 <- ggplot(fgmdata,cex=3, aes(x = uncuedAR, y = responseAR, fill=as.factor(sameDirection1S0D) , colour=as.factor(sameDirection1S0D))) + 
  #geom_point(shape=1, size=0.5, alpha=jitterAlpha, show.legend = FALSE) +
  #geom_jitter(shape=1, size=0.5, alpha=jitterAlpha, show.legend = FALSE) +
  #geom_density_2d(color=densityColor, alpha=densityAlpha) + 
  #coord_cartesian(ylim=c(-0.1, 0.1)) +
  coord_cartesian(ylim=c(-0.02, 0.04)) +
  geom_smooth(method = "lm", span = 1, alpha= lmAlpha,
              aes(fill = as.factor(sameDirection1S0D))) +
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
ggsave(filename = "fgmplot01.png",  width = 5, height = 5, units = "in", device='png', dpi=700) 

ggsave(filename = "fgmplot01.pdf", width = 5, height = 5, units = "in", device='pdf', dpi=700) 

#### fgm plot 02 ####

fgmdata.a <- aggregate(responseError ~ uncuedAR  + sameDirection1S0D + sub + global_org+
                         cued_vector + uncued_vector, fgmdata, mean)
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
ggsave(filename = "fgmplot02.png", width = 14, height = 8, units = "in", device='png', dpi=700) 


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

ggsave(filename = "fgmplot03.pdf", width = 14, height = 8, units = "in", device='pdf', dpi=700) 


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
fgmplot04.png <- ggplot(fgmdata.submean, aes(x = cuedAR, y = responseAR)) +
  geom_jitter(alpha = 1/4, aes(color = as.factor(sub))) +
  geom_density_2d(color=densityColor, alpha=densityAlpha) +
  # geom_abline(linetype = 11,  color="gray50") +
  geom_segment(aes(x=min(unique(cuedAR)),xend=max(unique(cuedAR)),y=0,yend=0), linetype = 11,  color="gray60")+ 
  geom_segment(aes(x=min(unique(cuedAR)),xend=max(unique(cuedAR)),
                   y=min(unique(cuedAR)),yend=max(unique(cuedAR))), 
                   linetype = 11,  color="gray60")+ 
  # geom_smooth(method = "lm", color = "blue") +
  geom_smooth(method="nls",
              formula= y ~ x * a * w *  sqrt(2)/exp(-(0.5)) * exp(-(w*x^2)), #y ~ a + b*x, # this is an nls argument
              start = list(a = 0.39, w = 0.63),
              se=FALSE, algorithm="port",
              color = "firebrick2") + #aes(color = as.factor(sameDirection1S0D))
  labs(x="(< flatter) Cued AR (taller >)", y = "(< flatter) Mean AR Response ( taller >)", size = 10.5) +
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


fgmplot04.png #+ facet_wrap(~sub) + #c(0.8, 0.14),
ggsave(filename = "fgmplot04.png",  width = 5, height = 5, units = "in", device='png', dpi=700) 

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
ggsave(filename = "fgmplot05.png",  width = 5, height = 5, units = "in", device='png', dpi=700) 

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




