######### |||||||||| STATISTICAL ANALYSIS ############# ||||||||||||||||||| ################## ||||||||||||||||
#### ||||||| ############ |||||||||||| ########## ||| ###### ||| #### |||||||| #########


####### ANOVA #####
oneWayAnova <- aov(reported_cued_distance ~coherence_level, data = relevantMotion_df)
summary(oneWayAnova) #overallMotionDirection

twoWayAnova <- aov(reported_cued_distance ~ overallMotionDirection * uncued_cued_distance, data = relevantMotion_df)
summary(twoWayAnova)
twoWayAnova <- aov(reported_cued_distance ~ uncued_cued_distance * motionSame * aperture_configuration, data = relevantMotion_df)
summary(twoWayAnova)

threeWayAnova <- aov(selected_ellipse_logAR ~ uncuedRelativeShape * aperture_configuration * motionSame * setShape, data = relevantMotion_df  )
summary(threeWayAnova)

dat.aov <- aov(Group.1 ~ x, data=subset_by_only_uncued_taller)

### t_test for the overall Vertical-Horizontal-Same-different ####
verti_hori_same_different <- split(relevantMotion_df$reported_cued_distance, relevantMotion_df$sameAndDirection)
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


#### REGRESSION ####
# converting integars to category or vice versa for reg analysis
transform(relevantMotion_df$motionSame,id=as.numeric(factor(relevantMotion_df$motionSame)))
transform(relevantMotion_df$uncuedRelativeShape,id=as.numeric(factor(relevantMotion_df$uncuedRelativeShape)))

model <- lm(reported_cued_distance ~ uncued_cued_distance *  sameAndDirection * aperture_configuration + coherence_level * overallMotionDirection, data = relevantMotion_df)
summary(model)

model <- lm(reported_AR ~ cued_ellipse_logAR *  overallMotionDirection , data = relevantMotion_df)
summary(model)


relevantMotion_df <- relevantMotion_df %>%
  mutate(numeric_overall_motion_direction = ifelse(overallMotionDirection == "vertical",1,0))

relevantMotion_df <- relevantMotion_df %>%
  mutate(numeric_motion_same = ifelse(motionSame == "same",1,0))

model <- lm(reported_AR ~ uncued_cued_distance *  aperture_configuration, data = relevantMotion_df)
summary(model)

model <- lm(reported_cued_distance ~ aperture_configuration * uncued_cued_distance *  overallMotionDirection, data = relevantMotion_df)
summary(model)

model <- lm(selected_ellipse_logAR ~ uncued_cued_difference *  sameAndDirection * aperture_configuration + coherence_level * overallMotionDirection, data = relevantMotion_df)
summary(model)

#ANOVA variable
model <- lm(x  ~ Group.2  * Group.4 * Group.5, data = IV_DV_anova)
summary(model)

#17 April
model <- lm(reported_cued_distance ~ cuedDirection *  other_closeness_non_normalized , data = relevantMotion_df)
summary(model)


