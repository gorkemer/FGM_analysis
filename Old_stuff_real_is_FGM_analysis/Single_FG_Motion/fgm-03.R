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

trunc <- function(x, ..., prec = 0) base::trunc(x * 10^prec, ...) / 10^prec;



# 1- confirm people did the task cued vs response
# 2- do the stim underestimation -> identical trials reg cued vs response
# 3- do the main regression analyses and check if global organization flips the mechanism

#remove scientific notation in the entire R session
options(scipen = 100)

setwd('~/Desktop/21Projects/Single_FG_Motion')

data = read.csv('motion_RA.csv', header = TRUE)
mNs <- data %>% 
  group_by(trial_participant_id) %>%  
  summarise(
            n = n()) 
head(mNs)
subset(mNs, mTrialComp$n<120)
#id 57746, 141906 are removed
#data = subset(data, !(data$trial_participant_id==141906 | data$trial_participant_id==57746))
#data = subset(data, !(data$trial_participant_id==958758 | data$trial_participant_id==52295))#remove says regression tables 958758,52295

data$motOrg = ifelse((data$e1MotDir==90 & data$e2MotDir == 270)| (data$e2MotDir == 270 | data$e1MotDir == 90) | (data$e2MotDir == 270 | data$e1MotDir == 270) | (data$e2MotDir == 90 | data$e1MotDir == 90), "tall", ifelse((data$e1MotDir==180 & data$e2MotDir == 0)|(data$e1MotDir==0 | data$e2MotDir==180)|(data$e1MotDir==0 | data$e2MotDir==0)|(data$e1MotDir==180 | data$e2MotDir==180), "flat", "?"))

data = data[c("trial_participant_id","coherence_level", "globalOrg", "respError", "arDiff", "sameARCat", "sameAR", "sameMotion", "cuedMotDir","uncuedAR", "cuedAR", "motOrg")]

data$UCShape <- ifelse(data$uncuedAR> -0.00000048, "tall", ifelse(data$uncuedAR==-0.00000048, "circle", "flat"))
data$UCShape_new <- factor(data$UCShape, levels= c("flat", "circle", "tall"))
data$cuedAR <- trunc(data$cuedAR, prec = 4)
data$motion <- ifelse(data$sameMotion==1, "same", "different")
#data$globalOrg <- ifelse(data$globalOrg==1, "tall(within)", "flat(between)")
targetALL <- aggregate(respError~ trial_participant_id + uncuedAR + sameMotion + globalOrg, data= data, mean, na.rm=TRUE)

plotBoxPlot(data, data$uncuedAR, data$respError, data$sameMotion) + facet_wrap(~globalOrg)

plotBoxPlot(data, data$UCShape, data$respError, as.factor(data$sameMotion)) + facet_wrap(~globalOrg)

# 9 x 14.5 inch dims
p1<- plotREF(data, data$uncuedAR, data$respError, as.factor(data$sameMotion))
p2<- plotREF(data, data$uncuedAR, data$respError, as.factor(data$sameMotion))

plotREF_gridCuedAR(data, data$uncuedAR, data$respError, as.factor(data$sameMotion))

plotREF_GO(data, data$uncuedAR, data$respError, as.factor(data$sameMotion))


#plot lm results
dwplot(list(main.m1, main.m2, main.m3), vline = geom_vline(
  xintercept = 0,
  colour = "grey40",
  linetype = 2,
),
vars_order = c("uncuedAR:motionsame:globalOrg", "motionsame:globalOrg", "uncuedAR:globalOrg", "globalOrg","uncuedAR:motionsame", "motionsame", "uncuedAR",  "cuedAR" ),
model_order = c("Model 3", "Model 2", "Model 1")
) + theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, face="bold")
  ) + xlab("Regression Coefficients\n") + coord_flip() + scale_color_manual(name="",values=c("gray10", "blue", "darkviolet")) #+ theme_bw() 


#xlab("Dose (mg)")
vip::grid.arrange(p1, p2, nrow = 1)

## Assign Variable Name
# library(sjlabelled)
# data$respError <- set_label(data$respError, label = "Response Error")
# data$cuedAR <- set_label(data$cuedAR, label = "Uncued AR")
# data$uncuedAR <- set_label(data$uncuedAR, label = "Uncued AR")
# data$motion <- set_label(data$motion, label = "Motion Direction")
# data$globalOrg <- set_label(data$motion, label = "Global Organization")
# data$motOrg <- set_label(data$motion, label = "Motion Direction Organization")

lesser.m1 <- lm(respError~cuedAR, data)
saturated.m1 <- lm(respError~cuedAR * uncuedAR, data)
main.m1 <- lm(respError ~ uncuedAR +cuedAR, data)
main.m2 <- lm(respError ~ uncuedAR * motion +cuedAR, data)
main.m3 <- lm(respError ~ uncuedAR * motion  * globalOrg +cuedAR, data)
main.m4 <- lm(respError ~ uncuedAR * motion  * globalOrg + motOrg +cuedAR, data)
main.m5 <- lm(respError ~ uncuedAR * motion  * globalOrg + motOrg +cuedAR, data)
main.GO_exp2 <- lm(respError ~ globalOrg * motOrg + cuedAR, data)
summary(main.GO_exp2)
anova(main.GO)

library(ggfortify)
autoplot(main.m4) # 23366 (958758 id), 23390 (958758 id) , 12313 (52295 id) outliers
#data <- data[-c(23366, 23390, 12313), ]

#plot lm results
dwplot(list(main.GO_exp1, main.GO_exp2), vline = geom_vline(
  xintercept = 0,
  colour = "grey40",
  linetype = 2,
),
vars_order = c("globalOrg:motOrgtall", "motOrgtall","globalOrg:boxOrgvertical",  "globalOrg:boxOrgno-box", "boxOrgvertical", "boxOrgno-box",  "globalOrg" ),
model_order = c("Model 2", "Model 1") 
) %>%
relabel_predictors(
  c(
    motOrgtall = "Global Motion Vertical",
    globalOrg:motOrgtall = "Global Tall x Global Motion Vertical",
    globalOrg:boxOrgvertical = "Global Tall x Vertical Box",
    globalOrg:boxOrgno-box = "Global Tall x No-Box",
    boxOrgvertical = "Vertical Box",
    boxOrgno-box = "No-Box",
    globalOrg = "Global Tall"
  )
) + theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, face="bold")
  ) + xlab("Regression Coefficients\n") + coord_flip() + scale_color_manual(name="", labels = c("Experiment 1", "Experiment 2"), values=c("gray10", "blue")) #+ theme_bw()  "darkviolet"

#merging box and motion experiments
#data_box <- data
data_box$expID <- "exp 1"
#data_box <- subset(data_box, !(data_box$boxOrg == "no-box")) #removing no-box trials

data$expID <- "exp 2"
data$boxOrg <- ifelse(data$motOrg=="tall", "vertical", "horizontal")

mergedData <- dplyr::bind_rows(data, data_box)

by_globalOrg <- data %>%
  group_by(globalOrg) %>%                                         # group data by trans
  do(broom::tidy(lm(respError ~ motion +uncuedAR + cuedAR + motion*uncuedAR, data = .))) %>% # run model on each grp
  rename(model = globalOrg) %>%                                     # make model variable
  relabel_predictors(c(
    respError = "Response Error",
    # relabel predictors
    cuedAR = "Cued AR",
    uncuedAR = "Uncued AR",
    motionsame = "Shared Mot. Dir.",
    `motionsame:uncuedAR` = "Uncued AR x Shared Mot. Dir."
  ))

dw1 <- dwplot(by_globalOrg,
       vline = geom_vline(
         xintercept = 0,
         colour = "grey40",
         linetype = 2,
       ),
       dodge_size = 0.3,
       dot_args = list(size = 2),
       whisker_args = list(size = 1),
       vars_order = c("Uncued AR x Shared Mot. Dir.", "Shared Mot. Dir.", "Uncued AR","Cued AR" ),
) + # plot line at zero _behind_ coefs
  theme_bw(base_size = 4) + xlab("Coefficient Estimate\n") + ylab("") +
  #ggtitle("Response Error by Global Organization") +
  theme_minimal() +
  theme(
    text = element_text(size=14),
    plot.title = element_text(face = "bold"),
    legend.position = c(0.76, 0.1),
    legend.justification = c(0, 0),
    legend.background = element_rect(colour = "grey80"),
    legend.title.align = .5,
    axis.text.x = element_text(colour ="black")
  ) +
  scale_colour_grey(
    start = .1,
    end = .5,
    name = "Global Organization",
    breaks = c(0, 1),
    labels = c("Between", "Within"),
    
  ) + coord_flip(xlim=c(-0.5, 0.2))
dw1 
ggsave(dw1,width=9, height=5, file="test.pdf", units = "in", dpi = 900)

#### plot merged data by experiments

mergedData$expID_2 <- factor(mergedData$expID)
mergedData2 <- subset(mergedData, !(mergedData$boxOrg == "no-box"))

by_exp <- mergedData %>%
  group_by(expID_2) %>%                                         # group data by trans
  do(broom::tidy(lm(respError ~ globalOrg * boxOrg + cuedAR, data = .))) %>% # run model on each grp
  rename(model = expID_2) %>% 
relabel_predictors(c(
  respError = "Response Error",
  # relabel predictors
  cuedAR = "Cued AR",
  globalOrg = "Tall Org.",
  `globalOrg:boxOrgvertical` = "Tall Org. x\n Sec. Global (Vertical)",
  `globalOrg:boxOrgno-box` = "Tall Org. x\n Sec. Global (No-Box)", #removed no-box condition
  `boxOrgvertical` = "Sec. Global\n (Vertical)",
  `boxOrgno-box` = "Sec. Global\n (no-box)"
))
by_exp

dw2 <- dwplot(by_exp,
              vline = geom_vline(
                xintercept = 0,
                colour = "grey40",
                linetype = 2,
              ),
              dodge_size = 0.3,
              dot_args = list(size = 2),
              whisker_args = list(size = 0.9) ,
              vars_order = c("Tall Org. x\n Sec. Global (No-Box)", "Sec. Global\n (no-box)","Tall Org. x\n Sec. Global (Vertical)", "Sec. Global\n (Vertical)", "Tall Org.","Cued AR" ),
) + # plot line at zero _behind_ coefs
  theme_bw(base_size = 2) + xlab("Coefficient Estimate\n") + ylab("") +
  #ggtitle("Response Error by Global Organization") +
  theme_minimal() +
  theme(
    text = element_text(size=12),
    plot.title = element_text(face = "bold"),
    legend.position = c(0.83, 0.1),
    legend.justification = c(0, 0),
    legend.background = element_rect(colour = "grey80"),
    legend.title.align = .5,
    axis.text.x = element_text(colour ="black")
  ) +
  scale_colour_grey(
    start = .1,
    end = .5,
    name = "Experiments",
    #breaks = c(0, 1),
    labels = c("Exp. 2 - Motion", "Exp. 1 - Box"),
    
  ) + coord_flip(xlim=c(-0.6, 0.1)) #xlim=c(-0.5, 0.2)
dw2 
ggsave(dw2,width=9, height=5, file="test2.pdf", units = "in", dpi = 900)

summary(lm(respError~boxOrg * globalOrg + cuedAR, data = subset(mergedData, mergedData$expID =="exp 1")))

#source("https://slcladal.github.io/rscripts/ExpR.r")
# check beta-error likelihood
expR(main.m4)# tabulate model results


# Cook's distance
plot(main.m4, 4)

# tabulate model results
sjPlot::tab_model(main.m1, main.m1)

# generate summary table
library(sjPlot) #file = "test.html"
regResults <- tab_model(main.m1, main.m2, main.m3,  p.style = "stars", collapse.ci = FALSE, emph.p = TRUE,dv.labels = c("Lesser Model","Model 1", "Model 2"), title = "Effects on Response Error", show.aic = T, show.se = TRUE, show.stat = TRUE, show.df= FALSE, show.loglik = TRUE, string.est = symbol("beta"))
regResults
library(webshot)
webshot("test.html", "regTable99.pdf")


#vars.order <- c("cuedAR", "uncuedAR", "box_", "globalOrg")
stargazer(main.m1, main.m2, main.m3,header=FALSE,
          title="My Nice Regression Table",
          ci = F, single.row = T,
          digits=2,
          font.size = "small",
          column.sep.width = "-45pt",
          #dep.var.caption  = "A better caption",
          # dep.var.labels.include = FALSE,
          # model.names = TRUE,
          # model.numbers = TRUE,
          #order=paste0("^", vars.order , "$")
          #column.separate = c(1, 2),
          #column.labels   = c("Good", "Better")
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          align = TRUE, 
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),
          notes.append=FALSE
)



#automated AIC based model fitting
step(main.m1, direction = "both")

AIC(main.m1)

## Compare two nested models
lmtest::lrtest(lesser.m1, main.m3)
lmtest::lrtest( main.m1)

main.m1$deviance-lesser.m1$deviance

data %>%
  ggplot(aes(UCShape, respError, color = sameMotion)) +
  facet_wrap(~motOrg) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  theme_set(theme_bw(base_size = 10)) +
  theme(legend.position = "top") +
  labs(x = "", y = "Observed Probabilty of eh") +
  #scale_color_manual(values = c("gray20", "gray70"))

