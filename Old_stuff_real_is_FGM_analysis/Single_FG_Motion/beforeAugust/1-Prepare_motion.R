summlibrary("scatterplot3d") # load
x<-c("ggpubr", "ggplot2", "multcomp", "ggpubr", "pastecs", "tidyr","dplyr", "ggiraph", "ggiraphExtra", "plyr", 
     "covreg", "plot3D", "Hmisc", "corrplot", "psych", "tidyverse", "hrbrthemes", "viridis", "gapminder",
     "ggplot2", "ggExtra", "ggMarginal", "scatterplot3d" )

require(x)
lapply(x, require, character.only = TRUE)

##### LOCATING THE DATA ##### 
setwd("~/Desktop/Motion_finalData")
motionData_final <- read.csv("data/Motion_finalData_01.csv",header=TRUE, quote="\"") 
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

## creating column for motion direction AND sameDifferent: 4 conditions // 90 goes top, 270 bottom, 180 goes left, 0 goes right
original_df$motionDiagonal <- ifelse(
  ( 
    (as.numeric(original_df$ellipse1_move_direction == "90")) & (as.numeric(original_df$ellipse2_move_direction == "180")) |    (as.numeric(original_df$ellipse1_move_direction == "180")) & (as.numeric(original_df$ellipse2_move_direction == "90"))
  ),
  "same-vertical",  # one goes TOP  and the other goes LEFT
  ifelse(
    ( 
      (as.numeric(original_df$ellipse1_move_direction == "270")) & (as.numeric(original_df$ellipse2_move_direction == "0")) |    (as.numeric(original_df$ellipse1_move_direction == "0")) & (as.numeric(original_df$ellipse2_move_direction == "270")) 
    ),
    "same-horizontal",  # one goes BOTTOM other goes RIGHT
    ifelse(
      ( 
        (as.numeric(original_df$ellipse1_move_direction == "90")) & (as.numeric(original_df$ellipse2_move_direction == "0")) |    (as.numeric(original_df$ellipse1_move_direction == "0")) & (as.numeric(original_df$ellipse2_move_direction == "90")) 
      ),
      "diff-vertical",  # one goes TOP  other goes RIGHT
      ifelse(
        ( 
          (as.numeric(original_df$ellipse1_move_direction == "270")) & (as.numeric(original_df$ellipse2_move_direction == "180")) |    (as.numeric(original_df$ellipse1_move_direction == "180")) & (as.numeric(original_df$ellipse2_move_direction == "270")) 
        ),
        "diff-horizontal",  #one goes BOTTOM  other goes LEFT
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
original_df$uncued_cued_distance <- original_df$uncued_ellipse_logAR - original_df$cued_ellipse_logAR


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

original_df$FT_uncued_cued_difference <- ifelse(
  ( 
    (as.numeric(original_df$uncued_ellipse_logAR >0.00000048) & as.numeric(original_df$cued_ellipse_logAR > 0.00000048))
  ),
  "both_tall_set",#(original_df$uncued_ellipse_logAR - original_df$cued_ellipse_logAR),  # taller_set, uncued-cued difference
  ifelse(
    ( 
      (as.numeric(original_df$uncued_ellipse_logAR < 0.00000048) & as.numeric(original_df$cued_ellipse_logAR < 0.00000048))
    ),
    "both_flat_set", #original_df$uncued_ellipse_logAR - original_df$cued_ellipse_logAR,  # flatter_set, uncued-cued difference
    0   # equal_set should be zero
  )
)

##### 17 April
original_df$other_closeness_non_normalized <- (((original_df$uncued_ellipse_logAR - original_df$cued_ellipse_logAR) - (original_df$uncued_ellipse_logAR + original_df$cued_ellipse_logAR))-(-0.92664))/(0.92664-(-0.92664))
original_df$other_closeness <- original_df$uncued_ellipse_logAR - original_df$cued_ellipse_logAR

original_df$cuedDirection <- ifelse(
  ( 
    (as.numeric(original_df$cueType == 1))
  ),
  original_df$ellipse1_move_direction,
  ifelse(
    ( 
      (as.numeric(original_df$cueType == 2))
    ),
    original_df$ellipse2_move_direction,
    0   # equal_set should be zero
  )
)


check <- (((0.3 - 0.1) - (0.3 + 0.1))-(-0.46))/(0.46-(-0.46))
check_r <- (((-0.2 - 0.3) - (-0.2 + 0.3))-(-0.46))/(0.46-(-0.46))

check2 <- 0.3 - 0.1
check2_r <- -0.2 - 0.3

original_df$other_closeness <- original_df$uncued_ellipse_logAR - original_df$cued_ellipse_logAR


# reported_AR - cued_AR / uncued - cued  > for Tall and Flat seperately


### creating selected - cued
original_df$reported_cued_distance <- (original_df$selected_ellipse_logAR - original_df$cued_ellipse_logAR)

### including a "reported_ellipse_logAR" ### 
original_df$reported_AR <-  original_df$selected_ellipse_logAR

### swapping 0 and 1 ### 1 was horizontal, now making it 0, 0 vas vertical, now it is 1 (because 1 vertical is intuituve)
original_df <- original_df %>%
  mutate(aperture_configuration = ifelse(aperture_configuration == 0,1,0))

### creating Attraction Index
original_df$attractionIndex <- ( original_df$reported_cued_distance / (original_df$uncued_cued_distance + 0.00001))


just_flat_shapes_df <- original_df[original_df$FT_uncued_cued_difference == "both_flat_set",]
just_flat_shapes_df$attractionIndex <- ( just_flat_shapes_df$reported_cued_distance / (just_flat_shapes_df$uncued_cued_distance))

just_flat_shapes_df$attractionIndex[!is.finite(just_flat_shapes_df$attractionIndex)] <- NA
colMeans(m, na.rm=TRUE)


### creating selected - cued
original_df$reported_cued_distance <- (original_df$selected_ellipse_logAR - original_df$cued_ellipse_logAR)

original_df$reported_cued_distance <- (original_df$selected_ellipse_logAR - original_df$cued_ellipse_logAR)


original_df$attractionIndex_flatter <- (original_df$selected_ellipse_logAR - original_df$cued_ellipse_logAR)

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
                            uncued_cued_distance,
                            reported_cued_distance,
                            reported_AR,
                            reportedRelativeShape_fromCued,
                            motionDiagonal,
                            attractionIndex,
                            other_closeness_non_normalized,
                            cuedDirection)

#### factoring categorical variables #### 
relevantMotion_df$motionSame <- factor(relevantMotion_df$motionSame)
relevantMotion_df$coherence_level <- factor(relevantMotion_df$coherence_level)
relevantMotion_df$overallMotionDirection <- factor(relevantMotion_df$overallMotionDirection)
relevantMotion_df$sameAndDirection <- factor(relevantMotion_df$sameAndDirection)
relevantMotion_df$uncuedOverallShape <- factor(relevantMotion_df$uncuedOverallShape)
relevantMotion_df$uncuedRelativeShape <- factor(relevantMotion_df$uncuedRelativeShape)
relevantMotion_df$setShape <- factor(relevantMotion_df$setShape)



#### Subsetting Levels #### 
aggData_overallMotionDirection <- aggregate(x = as.numeric(original_df$selected_ellipse_logAR), by = list(original_df$overallMotionDirection), FUN = "mean")
aggData_sameAndDirection <- aggregate(x = as.numeric(original_df$selected_ellipse_logAR), by = list(original_df$sameAndDirection), FUN = "mean")

subset_by_only_uncued_taller <- aggregate(x = as.numeric(original_df$selected_ellipse_logAR), by = list(original_df$uncuedRelativeShape), FUN = "mean")
subset_by__uncued_taller_and_motionType <- aggregate(x = as.numeric(original_df$selected_ellipse_logAR), by = list(original_df$uncuedRelativeShape, original_df$motionSame), FUN = "mean")

taller_oldugunda_cued_average_shape <- aggregate(x = as.numeric(original_df$cued_ellipse_logAR), by = list(original_df$uncuedRelativeShape), FUN = "mean")
taller_oldugunda_uncued_average_shape <- aggregate(x = as.numeric(original_df$uncued_ellipse_logAR), by = list(original_df$uncuedRelativeShape), FUN = "mean")



IV_DV_anova <- aggregate(original_df$reported_cued_distance,by=list(original_df$aperture_configuration,
                                                                    original_df$uncued_cued_distance,
                                                                    original_df$trial_participant_id,
                                                                    original_df$overallMotionDirection,
                                                                    original_df$motionSame),mean)



IV_DV_anova <- aggregate(original_df$reported_cued_distance,by=list(original_df$aperture_configuration,
                                                                    original_df$uncuedRelativeShape,
                                                                    original_df$trial_participant_id,
                                                                    original_df$motionSame,
                                                                    original_df$attractionIndex,
                                                                    original_df$uncued_cued_distance),mean)

#xtabs(formula=reported_cued_distance~number_of_shapes_in_box + uncuedRelativeShape, data=sub1_df)

summary(aov(IV_DV_anova$x ~ IV_DV_anova$Group.4*IV_DV_anova$Group.2 + Error(IV_DV_anova$Group.3/(IV_DV_anova$Group.4*IV_DV_anova$Group.2)), data = IV_DV_anova))
summary(aov(IV_DV_anova$x ~ IV_DV_anova$Group.1*IV_DV_anova$Group.2), data = IV_DV_anova)




new_df <- original_df %>% filter(uncued_ellipse_logAR > uncued_ellipse_logAR)




