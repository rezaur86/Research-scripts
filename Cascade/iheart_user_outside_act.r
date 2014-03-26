source('~/scripts/cascade/tools.r')
source('~/scripts/cascade/plfit.r')
library('MASS')
library('distr')
library(plyr)

iheart_influence_model <- function(){
#	cross_app <- as.data.frame(read.csv('/home/rezaur/data/cross_app_user_act.txt', header=FALSE))
#	colnames(cross_app) <- c('iheart_indeg_before_act', 'iheart_indeg_after_act', 'iheart_odeg', 'iheart_act_life',
#			'hugged_indeg_before_act', 'hugged_indeg_after_act', 'hugged_odeg', 'hugged_act_life',
#			'ismile_indeg_before_act', 'ismile_indeg_after_act', 'ismile_odeg', 'ismile_act_life')
	# regression analysis
	cross_app$iheart_indeg_before_act <- (cross_app$iheart_indeg_before_act - min(cross_app$iheart_indeg_before_act)) / (
				max(cross_app$iheart_indeg_before_act) - min(cross_app$iheart_indeg_before_act))
	cross_app$hugged_indeg_before_act <- (cross_app$hugged_indeg_before_act - min(cross_app$hugged_indeg_before_act, na.rm = TRUE)) / (
				max(cross_app$hugged_indeg_before_act, na.rm = TRUE) - min(cross_app$hugged_indeg_before_act, na.rm = TRUE))
	cross_app$ismile_indeg_before_act <- (cross_app$ismile_indeg_before_act - min(cross_app$ismile_indeg_before_act, na.rm = TRUE)) / (
				max(cross_app$ismile_indeg_before_act, na.rm = TRUE) - min(cross_app$ismile_indeg_before_act, na.rm = TRUE))

	cross_app$iheart_indeg_after_act <- (cross_app$iheart_indeg_after_act - min(cross_app$iheart_indeg_after_act, na.rm = TRUE)) / (
				max(cross_app$iheart_indeg_after_act, na.rm = TRUE) - min(cross_app$iheart_indeg_after_act, na.rm = TRUE))
	cross_app$hugged_indeg_after_act <- (cross_app$hugged_indeg_after_act - min(cross_app$hugged_indeg_after_act, na.rm = TRUE)) / (
				max(cross_app$hugged_indeg_after_act, na.rm = TRUE) - min(cross_app$hugged_indeg_after_act, na.rm = TRUE))
	cross_app$ismile_indeg_after_act <- (cross_app$ismile_indeg_after_act - min(cross_app$ismile_indeg_after_act, na.rm = TRUE)) / (
				max(cross_app$ismile_indeg_after_act, na.rm = TRUE) - min(cross_app$ismile_indeg_after_act, na.rm = TRUE))

	cross_app$iheart_odeg <- (cross_app$iheart_odeg - min(cross_app$iheart_odeg, na.rm = TRUE)) / (
				max(cross_app$iheart_odeg, na.rm = TRUE) - min(cross_app$iheart_odeg, na.rm = TRUE))
	cross_app$hugged_odeg <- (cross_app$hugged_odeg - min(cross_app$hugged_odeg, na.rm = TRUE)) / (
				max(cross_app$hugged_odeg, na.rm = TRUE) - min(cross_app$hugged_odeg, na.rm = TRUE))
	cross_app$ismile_odeg <- (cross_app$ismile_odeg - min(cross_app$ismile_odeg, na.rm = TRUE)) / (
				max(cross_app$ismile_odeg, na.rm = TRUE) - min(cross_app$ismile_odeg, na.rm = TRUE))
	
	splitted_data <- split(cross_app, sample(1:2, nrow(cross_app), replace=TRUE, prob=c(1,2)))
	training_data <- splitted_data[[2]]
	test_data <- splitted_data[[1]]
#	model <- lm(iheart_indeg_before_act~hugged_indeg_before_act+hugged_indeg_after_act+hugged_odeg+
#					ismile_indeg_before_act+ismile_indeg_after_act+ismile_odeg, data = training_data)
	model <- glm(iheart_indeg_before_act~hugged_indeg_before_act+
					ismile_indeg_before_act, data = training_data, family = "binomial")
	sm <- summary(model)
#	plot(sm)
	print(sm)
	print(1 - sm$deviance/sm$null.deviance)
	return(training_data)
}