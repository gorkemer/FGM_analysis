summlibrary("scatterplot3d") # load
x<-c("ggpubr", "ggplot2", "multcomp", "ggpubr", "pastecs", "tidyr","dplyr", "ggiraph", "ggiraphExtra", "plyr", 
     "covreg", "plot3D", "Hmisc", "corrplot", "psych", "tidyverse", "hrbrthemes", "viridis", "gapminder",
     "ggplot2", "ggExtra", "ggMarginal", "scatterplot3d" )

require(x)
lapply(x, require, character.only = TRUE)

##### LOCATING THE DATA ##### 
setwd("~/Desktop/Motion_finalData")
motionData_final <- read.csv("Motion_finalData_01.csv",header=TRUE, quote="\"") 
original_df <- data.frame(motionData_final)


##### PRE PROCESS THE DATA 
# 1) mutating to code coherence level as high (0.80) and low (0.60)
original_df <- original_df %>%
  mutate(coherence_level = ifelse(coherence_level == 0.8,"high (0.80)",ifelse(coherence_level == 0.6,"low (0.60)","else")))

## creating new column based on motion direction same or not
original_df$motionSame <- ifelse(
  ( 
    (as.numeric(original_df$ellipse1_move_direction) == as.numeric(original_df$ellipse2_move_direction))
  ),
  "same",  # if condition is met, put 1
  "different"   # else put 0
)

## creating column for motion direction 1) vertical 2) horizontal 3) mix
original_df$overallMotionDirection <- ifelse(
  ( 
    (as.numeric(original_df$ellipse1_move_direction == "0")) & (as.numeric(original_df$ellipse2_move_direction == "180")) |    (as.numeric(original_df$ellipse1_move_direction == "180")) & (as.numeric(original_df$ellipse2_move_direction == "0"))  |    (as.numeric(original_df$ellipse1_move_direction == "180")) & (as.numeric(original_df$ellipse2_move_direction == "180"))  |    (as.numeric(original_df$ellipse1_move_direction == "0")) & (as.numeric(original_df$ellipse2_move_direction == "0"))
  ),
  "horizontal",  # if condition is met, put 1, horizontal
  ifelse(
    ( 
      (as.numeric(original_df$ellipse1_move_direction == "90")) & (as.numeric(original_df$ellipse2_move_direction == "270")) |    (as.numeric(original_df$ellipse1_move_direction == "270")) & (as.numeric(original_df$ellipse2_move_direction == "90")) |    (as.numeric(original_df$ellipse1_move_direction == "270")) & (as.numeric(original_df$ellipse2_move_direction == "270")) |    (as.numeric(original_df$ellipse1_move_direction == "90")) & (as.numeric(original_df$ellipse2_move_direction == "90"))
    ),
    "vertical",  # vertical, 0
    "else"   # else, 99999
  )
)

## creating column for motion direction AND sameDifferent: 4 conditions
original_df$sameAndDirection <- ifelse(
  ( 
    (as.numeric(original_df$ellipse1_move_direction == "90")) & (as.numeric(original_df$ellipse2_move_direction == "90")) |    (as.numeric(original_df$ellipse1_move_direction == "270")) & (as.numeric(original_df$ellipse2_move_direction == "270"))
  ),
  "same-vertical",  # same-vertical
  ifelse(
    ( 
      (as.numeric(original_df$ellipse1_move_direction == "180")) & (as.numeric(original_df$ellipse2_move_direction == "180")) |    (as.numeric(original_df$ellipse1_move_direction == "0")) & (as.numeric(original_df$ellipse2_move_direction == "0")) 
    ),
  "same-horizontal",  #same-horizontal
    ifelse(
      ( 
        (as.numeric(original_df$ellipse1_move_direction == "90")) & (as.numeric(original_df$ellipse2_move_direction == "270")) |    (as.numeric(original_df$ellipse1_move_direction == "270")) & (as.numeric(original_df$ellipse2_move_direction == "90")) 
      ),
      "diff-vertical",  #"diff-vertical"
      ifelse(
        ( 
          (as.numeric(original_df$ellipse1_move_direction == "0")) & (as.numeric(original_df$ellipse2_move_direction == "180")) |    (as.numeric(original_df$ellipse1_move_direction == "180")) & (as.numeric(original_df$ellipse2_move_direction == "0")) 
        ),
        "diff-horizontal",  #"diff-horizontal"
        "else"
      )
    )
  )
)

### creating uncued shape tall vs flat
original_df$uncuedOverallShape <- ifelse(
  ( 
    (as.numeric(original_df$uncued_ellipse_logAR > -0.00000048))
  ),
  "tall",  # if condition is met, put 1
  ifelse(
    ( 
      (as.numeric(original_df$uncued_ellipse_logAR < -0.00000048))
    ),
  "flat",  # vertical, 0
    "equal"   # else, 99999
  )
)
### creating uncued shape tall vs flat
original_df$uncuedRelativeShape <- ifelse(
  ( 
    (as.numeric(original_df$uncued_ellipse_logAR > original_df$cued_ellipse_logAR))
  ),
  "taller",  # if condition is met, put 1
  ifelse(
    ( 
      (as.numeric(original_df$uncued_ellipse_logAR <  original_df$cued_ellipse_logAR))
    ),
    "flatter",  # vertical, 0
    "equal"   # else, 99999
  )
)

original_df$reportedRelativeShape_fromCued <- ifelse(
  ( 
    (as.numeric(original_df$selected_ellipse_logAR > original_df$cued_ellipse_logAR))
  ),
  "taller",  # if condition is met, put 1
  ifelse(
    ( 
      (as.numeric(original_df$selected_ellipse_logAR <  original_df$cued_ellipse_logAR))
    ),
    "flatter",  # vertical, 0
    "equal"   # else, 99999
  )
)

### creating uncued shape tall vs flat
original_df$uncued_cued_difference <- original_df$uncued_ellipse_logAR - original_df$cued_ellipse_logAR


### creating uncued shape tall vs flat
original_df$setShape <- ifelse(
  ( 
    (as.numeric(original_df$cued_ellipse_logAR > 0.00000048)) & (as.numeric(original_df$uncued_ellipse_logAR > 0.00000048))
  ),
  "tall_set",  # if condition is met, put 1
  ifelse(
    ( 
      (as.numeric(original_df$cued_ellipse_logAR < 0.00000048)) & (as.numeric(original_df$uncued_ellipse_logAR < 0.00000048))
    ),
    "flat_set",  # vertical, 0
    ifelse(
      ( 
        (as.numeric(original_df$cued_ellipse_logAR == -0.00000048)) & (as.numeric(original_df$uncued_ellipse_logAR == -0.00000048))
      ),
      "circle_set",  # vertical, 0
      ifelse(
        ( 
          (as.numeric(original_df$cued_ellipse_logAR >= -0.00000048)) & (as.numeric(original_df$uncued_ellipse_logAR< 0.00000048))
        ),
        "cued_taller-uncued_flatter",  # vertical, 0
        ifelse(
          ( 
            (as.numeric(original_df$cued_ellipse_logAR <= -0.00000048)) & (as.numeric(original_df$uncued_ellipse_logAR > 0.00000048))
          ),
          "uncued_taller-cued_flatter",  # vertical, 0
          "mixed set"   # else, 99999
        )   # else, 99999
      )   # else, 99999
    )
  )
)

### creating selected - cued
original_df$cued_reported_difference <- (original_df$selected_ellipse_logAR - original_df$cued_ellipse_logAR)

### including a "reported_ellipse_logAR" ### 
original_df$reported_ellipse_logAR <-  original_df$selected_ellipse_logAR

relevantMotion_df <- select(original_df, 
                            cued_ellipse_logAR,
                            uncued_ellipse_logAR, 
                            selected_ellipse_logAR,
                            aperture_configuration,
                            uncuedRelativeShape,
                            uncuedOverallShape,
                            motionSame,
                            sameAndDirection,
                            setShape,
                            coherence_level,
                            overallMotionDirection,
                            uncued_cued_difference,
                            cued_reported_difference,
                            reported_ellipse_logAR,
                            reportedRelativeShape_fromCued)

#### factoring categorical variables #### 
relevantMotion_df$motionSame <- factor(relevantMotion_df$motionSame)
relevantMotion_df$coherence_level <- factor(relevantMotion_df$coherence_level)
relevantMotion_df$overallMotionDirection <- factor(relevantMotion_df$overallMotionDirection)
relevantMotion_df$sameAndDirection <- factor(relevantMotion_df$sameAndDirection)
relevantMotion_df$uncuedOverallShape <- factor(relevantMotion_df$uncuedOverallShape)
relevantMotion_df$uncuedRelativeShape <- factor(relevantMotion_df$uncuedRelativeShape)
relevantMotion_df$setShape <- factor(relevantMotion_df$setShape)

### swapping 0 and 1 ### 1 was horizontal, now making it 0, 0 vas vertical, now it is 1 (because 1 vertical is intuituve)
relevantMotion_df <- relevantMotion_df %>%
  mutate(aperture_configuration = ifelse(aperture_configuration == 0,1,0))

#### Subsetting Levels #### 
aggData_overallMotionDirection <- aggregate(x = as.numeric(original_df$selected_ellipse_logAR), by = list(original_df$overallMotionDirection), FUN = "mean")
aggData_sameAndDirection <- aggregate(x = as.numeric(original_df$selected_ellipse_logAR), by = list(original_df$sameAndDirection), FUN = "mean")

subset_by_only_uncued_taller <- aggregate(x = as.numeric(original_df$selected_ellipse_logAR), by = list(original_df$uncuedRelativeShape), FUN = "mean")
subset_by__uncued_taller_and_motionType <- aggregate(x = as.numeric(original_df$selected_ellipse_logAR), by = list(original_df$uncuedRelativeShape, original_df$motionSame), FUN = "mean")

taller_oldugunda_cued_average_shape <- aggregate(x = as.numeric(original_df$cued_ellipse_logAR), by = list(original_df$uncuedRelativeShape), FUN = "mean")
taller_oldugunda_uncued_average_shape <- aggregate(x = as.numeric(original_df$uncued_ellipse_logAR), by = list(original_df$uncuedRelativeShape), FUN = "mean")

######### |||||||||| STATISTICAL ANALYSIS ############# ||||||||||||||||||| ################## ||||||||||||||||
#### ||||||| ############ |||||||||||| ########## ||| ###### ||| #### |||||||| #########

### t_test for the overall Vertical-Horizontal-Same-different ####
verti_hori_same_different <- split(relevantMotion_df$selected_ellipse_logAR, relevantMotion_df$sameAndDirection)
t.test(as.numeric(verti_hori_same_different[["same-vertical"]]), as.numeric(verti_hori_same_different[["same-horizontal"]]))
t.test(as.numeric(verti_hori_same_different[["diff-vertical"]]), as.numeric(verti_hori_same_different[["diff-horizontal"]]))
t.test(as.numeric(verti_hori_same_different[["same-vertical"]]), as.numeric(verti_hori_same_different[["diff-vertical"]]))
t.test(as.numeric(verti_hori_same_different[["same-horizontal"]]), as.numeric(verti_hori_same_different[["diff-horizontal"]]))

verti_hori_same_different <- split(relevantMotion_df$cued_reported_difference, relevantMotion_df$sameAndDirection)
t.test(as.numeric(verti_hori_same_different[["same-vertical"]]), as.numeric(verti_hori_same_different[["same-horizontal"]]))

overall_horizontal_vertical <- split(relevantMotion_df$cued_reported_difference, relevantMotion_df$overallMotionDirection)
t.test(as.numeric(overall_horizontal_vertical[["vertical"]]), as.numeric(overall_horizontal_vertical[["horizontal"]]))

reportedAR_by_motionSame <- split(relevantMotion_df$cued_reported_difference, relevantMotion_df$motionSame)
t.test(as.numeric(reportedAR_by_motionSame[["different"]]), as.numeric(reportedAR_by_motionSame[["same"]]))

reportedAR_by_aperture_config <- split(relevantMotion_df$cued_reported_difference, factor(relevantMotion_df$aperture_configuration))
t.test(as.numeric(reportedAR_by_aperture_config[["0"]]), as.numeric(reportedAR_by_aperture_config[["1"]]))



### t_test for the overall VERTICAL vs HORIZONTAL motion direction ####
overall_horizontal_vertical <- split(relevantMotion_df$selected_ellipse_logAR, relevantMotion_df$overallMotionDirection)
t.test(as.numeric(overall_horizontal_vertical[["vertical"]]), as.numeric(overall_horizontal_vertical[["horizontal"]]))

### t_test for the UNCUED SHAPE TALLER vs FLATTER #####
reportedAR_by_uncuedShape <- split(relevantMotion_df$selected_ellipse_logAR, relevantMotion_df$uncuedRelativeShape)
t.test(as.numeric(reportedAR_by_uncuedShape[["taller"]]), as.numeric(reportedAR_by_uncuedShape[["equal"]]))
t.test(as.numeric(reportedAR_by_uncuedShape[["flatter"]]), as.numeric(reportedAR_by_uncuedShape[["equal"]]))
t.test(as.numeric(reportedAR_by_uncuedShape[["taller"]]), as.numeric(reportedAR_by_uncuedShape[["flatter"]]))

### t_test for the MOTION SAME vs DIFFERENT ####
reportedAR_by_motionSame <- split(relevantMotion_df$selected_ellipse_logAR, relevantMotion_df$motionSame)
t.test(as.numeric(reportedAR_by_motionSame[["different"]]), as.numeric(reportedAR_by_motionSame[["same"]]))

### t_test for the SETSHAPE cued taller - uncued flatter vs flat set vs vs ####
reportedAR_by_setShape <- split(relevantMotion_df$selected_ellipse_logAR, relevantMotion_df$setShape)
t.test(as.numeric(reportedAR_by_setShape[["flat_set"]]), as.numeric(reportedAR_by_setShape[["tall_set"]]))

### t_test for the Aperture_configuration for 1 Vertical 0 Horizontal ####
reportedAR_by_aperture_config <- split(relevantMotion_df$selected_ellipse_logAR, factor(relevantMotion_df$aperture_configuration))
t.test(as.numeric(reportedAR_by_aperture_config[["0"]]), as.numeric(reportedAR_by_aperture_config[["1"]]))

### t_test for sameAndDirection and uncued_cued_difference ###3
splitted_by_two_columns <- split(relevantMotion_df$selected_ellipse_logAR,list(relevantMotion_df$uncuedRelativeShape,relevantMotion_df$sameAndDirection))
t.test(as.numeric(splitted_by_two_columns[["flatter.same-vertical"]]), as.numeric(splitted_by_two_columns[["flatter.diff-horizontal"]]))

### t_test for the UNCUED SHAPE TALLER vs FLATTER #####
reportedAR_AndCued_by_uncuedShape <- split(relevantMotion_df$cued_reported_difference, relevantMotion_df$uncuedRelativeShape)
t.test(as.numeric(reportedAR_AndCued_by_uncuedShape[["taller"]]), as.numeric(reportedAR_AndCued_by_uncuedShape[["equal"]]))
t.test(as.numeric(reportedAR_AndCued_by_uncuedShape[["flatter"]]), as.numeric(reportedAR_AndCued_by_uncuedShape[["equal"]]))
t.test(as.numeric(reportedAR_AndCued_by_uncuedShape[["taller"]]), as.numeric(reportedAR_AndCued_by_uncuedShape[["flatter"]]))




####### ANOVA #####
oneWayAnova <- aov(selected_ellipse_logAR ~coherence_level, data = relevantMotion_df)
summary(oneWayAnova)

twoWayAnova <- aov(selected_ellipse_logAR ~ uncued_cued_difference * overallMotionDirection, data = relevantMotion_df)
summary(twoWayAnova)
twoWayAnova <- aov(cued_reported_difference ~ uncued_cued_difference * overallMotionDirection, data = relevantMotion_df)
summary(twoWayAnova)

threeWayAnova <- aov(selected_ellipse_logAR ~ uncuedRelativeShape * aperture_configuration * motionSame * setShape, data = relevantMotion_df  )
summary(threeWayAnova)

dat.aov <- aov(Group.1 ~ x, data=subset_by_only_uncued_taller)

#### REGRESSION ####
# converting integars to category or vice versa for reg analysis
transform(relevantMotion_df$motionSame,id=as.numeric(factor(relevantMotion_df$motionSame)))
transform(relevantMotion_df$uncuedRelativeShape,id=as.numeric(factor(relevantMotion_df$uncuedRelativeShape)))

model <- lm(cued_reported_difference ~ uncued_cued_difference *  sameAndDirection * aperture_configuration + coherence_level * overallMotionDirection, data = relevantMotion_df)
summary(model)

model <- lm(selected_ellipse_logAR ~ uncued_cued_difference *  sameAndDirection * aperture_configuration + coherence_level * overallMotionDirection, data = relevantMotion_df)
summary(model)



#### #PLOTTING ####### 

# 2-WAY PLOTTING #
relevantMotion_df$uncuedRelativeShape <- factor(relevantMotion_df$uncuedRelativeShape, levels = c("flatter", "equal", "taller"))

ggboxplot(relevantMotion_df, x = "motionSame", y = "cued_reported_difference", color = "uncuedRelativeShape") +
  stat_summary(fun = mean, geom = "point", shape = 0.0020, size = 1, color = "brown")

ggplot(data = relevantMotion_df, aes(x=motionSame, y=selected_ellipse_logAR)) + geom_boxplot(aes(fill=uncuedRelativeShape))
ggplot(data = relevantMotion_df, aes(x=motionSame, y=cued_reported_difference)) + geom_boxplot(aes(fill=uncuedRelativeShape))

#ordering categorical variables in x-axis
relevantMotion_df$uncuedRelativeShape <- factor(relevantMotion_df$uncuedRelativeShape, levels = c("flatter", "equal", "taller"))
ggboxplot(relevantMotion_df, x = "uncuedRelativeShape", y = "selected_ellipse_logAR") 

ggplot(relevantMotion_df, aes(x = uncuedRelativeShape, y = selected_ellipse_logAR, fill = aperture_configuration)) +
  geom_boxplot() +
  geom_jitter(shape = 0.1,
              color = "steelblue",
              position = position_jitter(0.0001)) +
  theme_classic()


# ggplot(data = relevantMotion_df, aes(x = sameAndDirection, y = selected_ellipse_logAR)) +
#   geom_point()

# ggplot(data = relevantPortion_boxData, mapping = aes(x = cued_ellipse_logAR, y = selected_ellipse_logAR)) +
#   geom_point(alpha = 0.1, aes(color = number_of_shapes_in_box))
# 
# ggplot(data = relevantPortion_boxData, mapping = aes(x = number_of_shapes_in_box, y = selected_ellipse_logAR)) +
#   geom_boxplot()


#### box plot
ggplot(relevantMotion_df, aes(factor(motionSame), selected_ellipse_logAR)) + geom_boxplot()

### violin plot
ggplot(relevantMotion_df, aes(factor(motionSame), selected_ellipse_logAR)) + geom_violin()

### dirty modelling
ggplot(relevantPortion_boxData, aes(factor(number_of_shapes_in_box), selected_ellipse_logAR)) +   geom_point() + geom_smooth()

ggboxplot(relevantMotion_df, x = "motionSame", y = "selected_ellipse_logAR")

ggplot(data = relevantPortion_boxData, mapping = aes(x = number_of_shapes_in_box, y = selected_ellipse_logAR)) +
  geom_boxplot(alpha = 0) +
  geom_jitter(alpha = 0.3, color = "tomato")

##scatterplot3d(x = motionData$uncued_ellipse_logAR, y = motionData$cued_ellipse,  z = motionData$selected_ellipse_logAR)



####### PCA ######

# relevantMotion_df <- relevantMotion_df %>%
#   mutate(motionSame =  ifelse(motionSame == "same",1,ifelse(motionSame == "different",0,"else")))
# 
# relevantMotion_df <- relevantMotion_df %>%
#   mutate(uncuedRelativeShape =  ifelse(uncuedRelativeShape == "flatter",1,ifelse(uncuedRelativeShape == "equal",0,-1)))


##relevantMotion_df$motionSame <- as.numeric(relevantMotion_df$motionSame)

##relevantMotion_df$aperture_configuration <- as.numeric(relevantMotion_df$aperture_configuration) 
          
##relevantMotion_df_2 <- select(relevantMotion_df, uncued_cued_difference,uncuedRelativeShape, selected_ellipse_logAR, motionSame,aperture_configuration )

res <- cor(relevantMotion_df_2)
round(res, 2)

plot(as.factor(original_df$setShape))

ggplot(relevantMotion_df, aes(uncuedRelativeShape, selected_ellipse_logAR, fill= aperture_configuration)) + 
  

  ###### PLOTTING  ####


boxplot(selected_ellipse_logAR ~uncuedRelativeShape, data = relevantMotion_df,ylab = "selected_ellipse_logAR", names = c("Flatter","Equal", "Taller"))
boxplot(selected_ellipse_logAR ~setShape, data = relevantMotion_df,ylab = "selected_ellipse_logAR", names = c("flat set","tall set", "uncued taller", "cued taller"), las=2, horizontal=TRUE)

boxplot(selected_ellipse_logAR ~aperture_configuration, data = relevantMotion_df,ylab = "selected_ellipse_logAR", notch=TRUE, col=(c("gold","darkgreen")),las=2, horizontal=TRUE)


ggplot(relevantMotion_df, aes(uncuedRelativeShape, selected_ellipse_logAR, fill= motionSame)) + 
  geom_violin()

ggplot(relevantMotion_df, aes(uncued_cued_difference, selected_ellipse_logAR, fill= motionSame)) + geom_point()

ggplot(relevantMotion_df, aes(uncuedRelativeShape, selected_ellipse_logAR, fill= aperture_configuration) + 
         geom_point(alpha=0.7) +
         scale_color_manual(values=c("grey", "red")) +
         theme(legend.position="none"))

ggplot(relevantMotion_df, aes(x = uncued_cued_difference, y = selected_ellipse_logAR, color = sameAndDirection)) +
  geom_boxplot()+
  geom_point(size = 0.02, position = position_jitter(width = 0.002)) +
  stat_summary(fun = mean, geom = "point", shape = 0.0020, size = 1, color = "blue")+
  theme_classic()

beanplot(selected_ellipse_logAR ~ setShape, data = relevantMotion_df, ll = 0.04, main = "beanplot", cutmin = 0, las=2)
#geom_boxplot()
ggplot(relevantMotion_df, aes(x=uncued_cued_difference, y=selected_ellipse_logAR, color = factor(motionSame))) + geom_area() # + scale_color_manual(values=c("blue", "yellow")) + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) 

ggplot(relevantMotion_df, aes(x=reported_cued_difference, y=reported_ellipse_logAR, color = factor(uncued_cued_difference))) + geom_jitter() + facet_wrap(~motionSame) # + scale_color_manual(values=c("blue", "yellow")) + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) 
ggplot(relevantMotion_df, aes(x = uncued_cued_difference, y=reported_cued_difference)) + geom_jitter(alpha=1/40) # + scale_color_manual(values=c("blue", "yellow")) + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) 


ggplot(relevantMotion_df, aes(x=uncued_cued_difference, y=selected_ellipse_logAR, color = factor(sameAndDirection))) + geom_area() + facet_wrap(~motionSame) # + scale_color_manual(values=c("blue", "yellow")) + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) 

#### ALMOST 4-D Data ####
ggplot(relevantMotion_df, aes(x=uncued_cued_difference, y=selected_ellipse_logAR, color = factor(motionSame))) + geom_area() + facet_wrap(~aperture_configuration) # + scale_color_manual(values=c("blue", "yellow")) + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) 
aaa <- ggplot(relevantMotion_df, aes(x=uncued_cued_difference, y=selected_ellipse_logAR, color = factor(overallMotionDirection)), legend=TRUE) + geom_jitter(alpha = 0.30) #+ facet_wrap(~overallMotionDirection) ##+ stat_summary(fun.y=median, geom="point")   # + scale_color_manual(values=c("blue", "yellow")) + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) 
aaa ## + stat_summary(fun = mean, geom = "point", shape = 0.0020, size = 1, color = "blue")

simple_gg <- ggplot(relevantMotion_df, aes(uncued_cued_difference, selected_ellipse_logAR)) 

simple_gg + geom_point((alpha=0.3)) + facet_wrap(vars(uncuedRelativeShape), scales = "free")

# classic plot :
p <- ggplot(relevantMotion_df, aes(x=uncued_cued_difference, y=selected_ellipse_logAR, color=factor(motionSame))) +
  geom_point() +
  theme(legend.position="none")

# with marginal histogram
p1 <- ggMarginal(p, type="histogram")

# marginal density
p2 <- ggMarginal(p, type="density")

# marginal boxplot
p3 <- ggMarginal(p, type="boxplot")

twoD_with_category_plot <- ggplot(relevantMotion_df, aes(uncued_cued_difference, selected_ellipse_logAR, color= factor(motionSame)) + 
                                    geom_point(alpha=0.59))
twoD_with_category_plot + scale_colour_manual(values=c("blue","orange"))

ggmap(m, extent = "normal") + geom_point(aes(x = uncued_cued_difference, y = selected_ellipse_logAR, color = factor(motionSame)), data = relevantMotion_df)

scale_colour_manual(values=c("red","orange","green","blue"))

twoD_with_category_plot+ scale_color_manual(breaks = c("8", "6", "4"),
                                            values=c("red", "blue", "green"))

boxplot(selected_ellipse_logAR ~ overallMotionDirection * motionSame, 
        data = relevantMotion_df,ylab = "selected_ellipse_logAR", notch=TRUE,
        col=(c("gold","darkgreen")),las=2, horizontal=TRUE)

beanplot(relevantMotion_df, aes(x=factor(setShape), y=selected_ellipse_logAR)) + geom_boxplot() + theme(axis.text.x  = element_text(angle=90, vjust=0.5))


op <- par(mfrow = c(1, 2) )
plot(relevantMotion_df$motionSame, relevantMotion_df$selected_ellipse_logAR, col = relevantMotion_df$uncuedRelativeShape + 1,  pch = relevantMotion_df$uncuedRelativeShape + 1)
plot(relevantMotion_df$aperture_configuration, relevantMotion_df$selected_ellipse_logAR, col = relevantMotion_df$uncuedRelativeShape, pch = relevantMotion_df$uncuedRelativeShape)



# Change dotsize and stack ratio
gg <- ggplot(relevantMotion_df, aes(x=uncuedRelativeShape, y=selected_ellipse_logAR)) + 
  geom_dotplot(binaxis='y', stackdir='center',
               stackratio=1.5, dotsize=0.2, alpha= 0.5)

gg + stat_summary(fun.y=median, geom="point", shape=18,
                  size=3, color="red") +   geom_boxplot()

hist(relevantMotion_df$reported_cued_difference)


ggplot(relevantMotion_df, aes(x = cued_reported_difference)) +
  geom_histogram(fill = "white", colour = "black", binwidth=1/80) +
  facet_grid(uncued_cued_difference ~ .)

ggplot(relevantMotion_df, aes(x = relevantMotion_df$cued_reported_difference, fill = uncuedRelativeShape)) +
  geom_histogram(position = "identity", alpha = 0.4)

ggplot(relevantMotion_df, aes(x = relevantMotion_df$selected_ellipse_logAR, fill = uncuedRelativeShape)) +
  geom_histogram(position = "identity", alpha = 0.4)

ggplot(relevantMotion_df, aes(x = cued_reported_difference, fill = uncuedRelativeShape)) +
  geom_histogram(position = "identity", alpha = 0.4) + facet_grid(aperture_configuration ~ .)
