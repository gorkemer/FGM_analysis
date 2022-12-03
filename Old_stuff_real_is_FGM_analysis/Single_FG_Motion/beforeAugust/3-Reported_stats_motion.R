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


