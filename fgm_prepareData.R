# April 5th, 2022.
# Analysis script for the foreground motion experiment. 
# I've revised the script of SingleFGMot_01_prepare.R script that sits locally on 
# my computer. This is a revised version for the manuscript I'm writing. 

# clear the workspace
rm(list=ls()) 

# load libraries in bulk
x<-c("ggpubr", "ggplot2", "multcomp", "pastecs", "tidyr","dplyr", "ggiraph", "ggiraphExtra", "plyr", 
     "covreg", "plot3D", "Hmisc", "corrplot", "psych", "tidyverse", "hrbrthemes", "viridis", "gapminder",
    "ggExtra", "scatterplot3d", "reshape2", "rlang", "plyr", "data.table")

require(x)
lapply(x, require, character.only = TRUE)

# locate the data and import it
setwd("/Users/gorkem.er/Desktop/21Projects/Single_FG_Motion")
d = read.csv("Motion_finalData_01.csv",header=TRUE, quote="\"") 

# remove id 57746, 141906, because those are my test-data
d = subset(d, !(d$trial_participant_id==57746 | d$trial_participant_id==141906))

# new variables

#### Issues ####
# calculate distance between shapes per subject
d$distBtwShapes <- ifelse(d$aperture_configuration==0, abs(d$aperture1_center_y - d$aperture2_center_y), abs(d$aperture1_center_x - d$aperture2_center_x) )#this is when organization is vertical, getting y values
distBtwShapesPerId <- aggregate(distBtwShapes~trial_participant_id, d, mean)
#find people who have less than 400 and higher than 500 distance between shapes
peopleWithCorrDist <- subset(distBtwShapesPerId,distBtwShapesPerId$distBtwShapes >= 350 & distBtwShapesPerId$distBtwShapes <= 522)
peopleWithLongerDist <- subset(distBtwShapesPerId,distBtwShapesPerId$distBtwShapes<=349 | distBtwShapesPerId$distBtwShapes>= 523)
# 61 subjects had correct distance, while 11 had either very high distance or very small distance.
#agg <- aggregate(list(d$trial_num), by = list(d$trial_participant_id), sum)

# take only the relevant pieces of data from the original/main data
fgmdata <- d[c("rt","aperture_configuration", "cueType","ellipse1_move_direction",
            "ellipse2_move_direction", "selected_ellipse_logAR", "cued_ellipse_logAR",
            "uncued_ellipse_logAR","coherence_level","trial_num","round_number","trial_participant_id", "distBtwShapes")]

# assign new column names
nms <- c("rt", "global_org", "cued_ellipse", "e1_motion_dir", "e2_motion_dir", "responseAR","cuedAR", "uncuedAR", "coherence", "trial_number", "round_number", "sub", "distBtwShapes")
setnames(fgmdata, nms)

# reorder columns for ease of eyeballing the data 
fgmdata <- fgmdata[,c("sub","distBtwShapes","trial_number", "cuedAR", "uncuedAR", "responseAR","rt","global_org", "cued_ellipse", "e1_motion_dir", "e2_motion_dir", "round_number", "coherence")]

# transform characters data into numeric type
fgmdata <- mutate_all(fgmdata, function(x) as.numeric(as.character(x)))


#### think about deleting ####
# remove na in r - remove rows - na.omit function / option
#fgmdata <- na.omit(fgmdata)



# What specific direction is cued shape moving?
# cued motion direction [cued_motDir]: 0, 90, 180, 270                
# populate cued_motion direction column -- use "cued_ellipse" and e1/e2 motion directions
fgmdata$cued_motion_dir <- ifelse(fgmdata$cued_ellipse == 1, fgmdata$e1_motion_dir, fgmdata$e2_motion_dir)

# re-code global_org so that "1" indicates vertical organization
fgmdata$global_org <- (fgmdata$global_org-1)*-1

# Are the two shapes moving in the same direction?
# motion direction same? [motDir_same]: 1 or 0                        
# (each shape can move in four direction: 0, 90, 180, 270 (in degrees) )

fgmdata$sameDirection1S0D <- ifelse(fgmdata$e1_motion_dir == fgmdata$e2_motion_dir, 1, 0)

# What's the difference between the response and the cued shape?
# responseAR-cuedAR, # positive values indicate = error on the "taller" side
fgmdata$responseError <- fgmdata$responseAR-fgmdata$cuedAR

# Are the two shapes in the same category of aspect ratio? i.e., both tall? Here 0.00000048 means 0
# Note here that circles are also included as same category
fgmdata$sameCatS1D0 <- ifelse(fgmdata$cuedAR < -0.00000048  & fgmdata$uncuedAR < -0.00000048 | fgmdata$cuedAR > -0.00000048 & fgmdata$uncuedAR > -0.00000048 | fgmdata$cuedAR == fgmdata$uncuedAR, 1, 0)

# Identify trials where cued and uncued shape has the same AR
fgmdata$identicalShapesI1D0 <- ifelse(fgmdata$cuedAR == fgmdata$uncuedAR, 1, 0)

# What is the shape category (tall vs flat) of the uncued shape?
fgmdata$uncuedCatM1F0C1T = ifelse(fgmdata$uncuedAR < -0.00000048, -1, ifelse(fgmdata$uncuedAR==-0.00000048, 0, 1))

# output the csv externally 
write.csv(fgmdata, "fgmdata.csv", row.names = FALSE)
