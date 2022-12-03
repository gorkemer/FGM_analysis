#### #PLOTTING ####### 


ggplot(data = IV_DV_anova, mapping = aes(x = Group.4, y = x, color = Group.2)) + geom_jitter(alpha=1) + ylim(-0.75,0.75)
geom_boxplot(alpha = 0) + facet_wrap(~Group.2)


# ordering categorical variables in x-axis #
relevantMotion_df$uncuedRelativeShape <- factor(relevantMotion_df$uncuedRelativeShape, levels = c("flatter", "equal", "taller"))


plot(relevantMotion_df$uncued_cued_distance,relevantMotion_df$attractionIndex )

ggplot_df <- ggplot(data = relevantMotion_df, mapping = aes(x = uncued_cued_distance, y = reported_cued_distance, )) #color = factor(Group.4)
  #geom_boxplot(alpha = 0) +
  
ggplot_df + geom_jitter(alpha = 0.3, color = "tomato") + facet_wrap(~overallMotionDirection) + stat_smooth(method = lm) +  geom_density_2d()

ggplot(just_flat_shapes_df, aes(x=uncued_cued_distance, y=attractionIndex)) + 
  geom_point(alpha=1/40,na.rm = TRUE)

ggplot(relevantMotion_df, aes(x=reported, y=attractionIndex)) + 
  geom_point(alpha=1/40,na.rm = TRUE)


##### GG BOX PLOTS ######

###### Outputs
#1)
ggplot(data = IV_DV_anova, mapping = aes(x = Group.4, y = x, )) + #color = factor(Group.4)
  geom_boxplot(alpha = 0) +
  geom_jitter(alpha = 0.3, color = "tomato") + facet_wrap(~Group.2)
#2)
ggplot(relevantMotion_df, aes(x = uncued_cued_distance, y=reported_cued_distance)) + geom_jitter(alpha=1/40) + facet_wrap(~overallMotionDirection)# + scale_color_manual(values=c("blue", "yellow")) + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) 
#3)
ggplot(relevantMotion_df, aes(x=uncued_cued_distance, y=reported_cued_distance, color = factor(sameAndDirection))) + geom_jitter(alpha=1/2) + facet_wrap(~motionSame) # + scale_color_manual(values=c("blue", "yellow")) + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) 

ggplot(IV_DV_anova, aes(x=Group.2, y=x, color = factor(Group.5))) + geom_jitter(alpha=1/5) + facet_wrap(~Group.4) # + scale_color_manual(values=c("blue", "yellow")) + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) 


# others
ggboxplot(relevantMotion_df, x = "motionSame", y = "reported_cued_distance", color = "uncuedRelativeShape") +
  stat_summary(fun = mean, geom = "point", shape = 0.0020, size = 1, color = "brown")

ggplot(data = relevantMotion_df, aes(x=motionSame, y=selected_ellipse_logAR)) + geom_boxplot(aes(fill=uncuedRelativeShape))
ggplot(data = relevantMotion_df, aes(x=motionSame, y=cued_reported_difference)) + geom_boxplot(aes(fill=uncuedRelativeShape))

### violin plot
ggplot(relevantMotion_df, aes(factor(motionSame), selected_ellipse_logAR)) + geom_violin()


boxplot(uncued_cued_distance ~uncuedRelativeShape, data = relevantMotion_df,ylab = "uncued_cued_distance", names = c("Flatter","Equal", "Taller"))

boxplot(selected_ellipse_logAR ~setShape, data = relevantMotion_df,ylab = "selected_ellipse_logAR", names = c("flat set","tall set", "uncued taller", "cued taller"), las=2, horizontal=TRUE)
boxplot(selected_ellipse_logAR ~aperture_configuration, data = relevantMotion_df,ylab = "selected_ellipse_logAR", notch=TRUE, col=(c("gold","darkgreen")),las=2, horizontal=TRUE)


ggplot(relevantMotion_df, aes(uncuedRelativeShape, selected_ellipse_logAR, fill= motionSame)) + 
  geom_violin()

ggplot(relevantMotion_df, aes(x=uncued_cued_distance, y=selected_ellipse_logAR, color = factor(motionSame))) + geom_area() # + scale_color_manual(values=c("blue", "yellow")) + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) 

#### ALMOST 4-D Data ####
aaa <- ggplot(relevantMotion_df, aes(x=uncued_cued_distance, y=reported_cued_distance, color = factor(overallMotionDirection)), legend=TRUE) + geom_jitter(alpha = 0.30) #+ facet_wrap(~overallMotionDirection) ##+ stat_summary(fun.y=median, geom="point")   # + scale_color_manual(values=c("blue", "yellow")) + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) 
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


a <- ggplot(relevantMotion_df, aes(x = reported_cued_distance, fill=cued_ellipse_logAR)) +
  geom_histogram(fill = "white", colour = "black", binwidth=1/80)  + geom_density(fill="blue")+
  facet_grid(motionSame ~ .)

ggplot(relevantMotion_df, aes(x = relevantMotion_df$reported_cued_distance, fill = uncuedRelativeShape)) +
  geom_histogram(position = "identity", alpha = 0.4)

ggplot(relevantMotion_df, aes(x = relevantMotion_df$selected_ellipse_logAR, fill = uncuedRelativeShape)) +
  geom_histogram(position = "identity", alpha = 0.4)

ggplot(relevantMotion_df, aes(x = cued_reported_difference, fill = uncuedRelativeShape)) +
  geom_histogram(position = "identity", alpha = 0.4) + facet_grid(aperture_configuration ~ .)


p <- relevantMotion_df %>%
  ggplot( aes(x=reported_AR,fill=motionSame)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 120) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

p + facet_wrap(~uncuedRelativeShape)

library('lattice')
histogram(~ reported_cued_distance | motionSame, data = relevantMotion_df)

#### scatter

scatterplot(reported_cued_distance ~ uncued_cued_distance, data = relevantMotion_df)

ggplot(relevantMotion_df, aes(x=uncued_cued_distance, y=reported_cued_distance)) + 
  geom_smooth(method=lm , color="red", se=FALSE) +
  geom_point(alpha=1/40)


