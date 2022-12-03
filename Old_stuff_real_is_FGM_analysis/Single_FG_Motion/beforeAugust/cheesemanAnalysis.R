install.packages("devtools")
library(devtools)
install_github("easyGgplot2", "kassambara")

original_df <- original_df %>%
  mutate(aperture_configuration = ifelse(aperture_configuration == 0,1,0))

## creating new column based on motion direction same or not
original_df$motionSame <- ifelse(
  ( 
    (as.numeric(original_df$ellipse1_move_direction) == as.numeric(original_df$ellipse2_move_direction))
  ),
  "same",  # if condition is met, put 1
  "different"   # else put 0
)


X1_minus_X2 <- original_df$cued_ellipse_logAR - original_df$uncued_ellipse_logAR
beta_minus_X1 <- original_df$selected_ellipse_logAR - original_df$cued_ellipse_logAR
motionDirection <- original_df$overallMotionDirection
globalOrganization <- original_df$aperture_configuration
B_abs <- abs(beta_minus_X1)
motionSame <- original_df$motionSame 

cheeseman_df <- data.frame(X1_minus_X2, beta_minus_X1,motionDirection, globalOrganization, B_abs, motionSame)
#Add linear regression line
ggplot(cheeseman_df, aes(x = X1_minus_X2, y = beta_minus_X1)) +
  geom_point(alpha=1/40) + geom_smooth(lwd = 2, se = FALSE) + facet_wrap(~motionSame)

