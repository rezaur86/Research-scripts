source('~/scripts/Cascade/tools.r')
library(ggplot2)
library(gtable)
library(reshape)
library(grid)
library(plyr)
library(Hmisc)
library(gridExtra) 
library(nnet)
library(pROC)
library('ROCR', quietly = TRUE, warn.conflicts = FALSE)
library('zoo', quietly = TRUE, warn.conflicts = FALSE) 
#For calculating area under a curve using trapezium figures (rollmean function is used for that)
library('RWeka', quietly = TRUE, warn.conflicts = FALSE)
library('leaps', quietly = TRUE, warn.conflicts = FALSE)
library('glmulti', quietly = TRUE, warn.conflicts = FALSE)
library('car', quietly = TRUE, warn.conflicts = FALSE)

load_features_and_plot <- function(file){
	adoption_feat <- as.data.frame(read.csv(file, header=FALSE))
	colnames(adoption_feat) <- c('id', 'adopted', 'inv_count', 'recep_burst',
			'inv_elapsed_hr', 'gift_veriety',
			'inviters_avg_invitation_count','inviters_avg_sent_ARs',
			'inviters_avg_active_children', 'avg_inviter_succ_ratio'
			)
	adoption_feat <- adoption_feat[adoption_feat$inv_count > 0, ] # & adoption_feat$avg_inviter_succ_ratio >= 0
	# Invitee's properties
	NR <- c('id', 'inv_count', 'recep_burst', 'inv_elapsed_hr', 'gift_veriety', 'inviters_avg_invitation_count')
	# Average inviters' properties
	NAS <- c('inviters_avg_sent_ARs', 'inviters_avg_active_children', 'avg_inviter_succ_ratio')

	chi_test <- 0 #feature_selction(adoption_feat, 'adopted', c(NR, NAS))
	
	adoption_feat$adopted <- factor(adoption_feat$adopted)
	categories <- levels(adoption_feat$adopted)
	adoption_feat$cat_label <- factor(adoption_feat$adopted, levels = categories, labels = c('No', 'Yes'))
	feature_summary <- list(
		users_inv_count = feature_boxplot(adoption_feat, 'inv_count', 
			'Adopted by invitee', 'Total number of received ARs', 'iheart_gift/users_inv_count.pdf'),
		users_inv_elapsed_hr = feature_boxplot(adoption_feat, 'inv_elapsed_hr', 
			'Adopted by invitee', 'Elapsed time in for receiving ARs (days)', 'iheart_gift/users_inv_elapsed_hr.pdf'),
		users_recep_burst = feature_boxplot(adoption_feat, 'recep_burst', 
			'Adopted by invitee', 'Reception burstiness', 'iheart_gift/users_recep_burst.pdf'),
		users_gift_veriety= feature_boxplot(adoption_feat, 'gift_veriety', 
			'Adopted by invitee', 'Gift variety in invitations', 'iheart_gift/users_gift_veriety.pdf'),

		avg_invitation_per_inviter = feature_boxplot(adoption_feat, 'inviters_avg_invitation_count', 
				'Adopted by invitee', 'Average invitations per inviter', 'iheart_gift/avg_inviters_invitation.pdf'),
		avg_inviters_sent_ARs = feature_boxplot(adoption_feat, 'inviters_avg_sent_ARs', 
				'Adopted by invitee', 'Inviters\' average number of sent ARs', 'iheart_gift/avg_inviters_sent_ARs.pdf'),
		avg_inviters_active_children = feature_boxplot(adoption_feat, 'inviters_avg_active_children', 
				'Adopted by invitee', 'Inviters\' average number of activated users', 'iheart_gift/avg_inviters_active_children.pdf'),
		avg_inviter_succ_ratio= feature_boxplot(adoption_feat, 'avg_inviter_succ_ratio', 
				'Adopted by invitee', 'Inviters\' average success ratio', 'iheart_gift/avg_inviter_succ_ratio.pdf')	
	)
	return(list(feat_summary = feature_summary, chi_test = chi_test))
}

feature_boxplot <- function(features, a_feature, label_x, label_y, figure_name){
	features.a_feature <- ddply(features, c('cat_label'), .drop=TRUE,
			.fun = function(one_partition){
				stats = boxplot.stats(one_partition[[a_feature]])$stats
				c(ymin=stats[1],
						lower=stats[2],
						middle=stats[3],
						upper=stats[4],
						ymax=stats[5],
						mean = mean(one_partition[[a_feature]]))
			})
	features.a_feature$cat_label <- factor(features.a_feature$cat_label)
	plot <- ggplot(features.a_feature, aes(x=cat_label, lower=lower, upper=upper, middle=middle, ymin=ymin, ymax=ymax)) + 
			geom_boxplot(stat="identity") +
			geom_point(data = features.a_feature, aes(x=cat_label, y=mean), shape = 8, size = 3)+
#			scale_y_continuous(breaks=seq(0,7200,720*2), labels=seq(0,300,30*2))+
			xlab(label_x) + ylab(label_y) 
	save_ggplot(plot, figure_name)
	return(features.a_feature)
}

grid_boxplots <- function(feature_summary_list, feature_set, label_x_set, label_y_set, figure_name){
	plist <- list()
	i <- 1
	for (a_feature in feature_set){
		plist[[a_feature]] <- ggplot(feature_summary_list[[a_feature]],
						aes(x=cat_label, lower=lower, upper=upper, middle=middle, ymin=ymin, ymax=ymax)) + 
				geom_boxplot(stat="identity") +
				geom_point(data = feature_summary_list[[a_feature]], aes(x=cat_label, y=mean), shape = 8, size = 3)+
				xlab(label_x_set[i]) + ylab(label_y_set[i]) +
				myPlotTheme(19)	
		i <- i + 1
	}
	pdf(figure_name, width = 15, height = 3.5)
	print(do.call("grid.arrange", c(plist, ncol=length(plist))))
	dev.off()
#	save_ggplot(do.call("grid.arrange", c(plist, ncol=length(plist))), figure_name, width=14)
}

feature_boxplot_comp <- function(features, feature_1, feature_2, label_x, label_y, figure_name,
		comp_label=c('Chosen inviter', 'Average of all the inviters')){
	comp_features_1 <- as.data.frame(features$cat_label)
	colnames(comp_features_1) <- c('cat_label')
	comp_features_1$a_feature <- features[[feature_1]]
	comp_features_1$comp_type <- 0
	comp_features_2 <- as.data.frame(features$cat_label)
	colnames(comp_features_2) <- c('cat_label')
	comp_features_2$a_feature <- features[[feature_2]]
	comp_features_2$comp_type <- 1
	comp_features <- rbind(comp_features_1, comp_features_2)
	features.a_feature <- ddply(comp_features, c('comp_type', 'cat_label'), .drop=TRUE,
			.fun = function(one_partition){
				stats = boxplot.stats(one_partition$a_feature)$stats
				c(ymin=stats[1],
						lower=stats[2],
						middle=stats[3],
						upper=stats[4],
						ymax=stats[5],
						mean = mean(one_partition$a_feature))
			})
	features.a_feature$comp_type <- factor(features.a_feature$comp_type)
	features.a_feature$cat_label <- factor(features.a_feature$cat_label)
	plot <- ggplot(features.a_feature, aes(x=cat_label, y=mean, fill = comp_type, 
							lower=lower, upper=upper, middle=middle, ymin=ymin, ymax=ymax)) + 
			geom_boxplot(stat="identity") +
			stat_summary(fun.y = "mean", geom = "point", shape= 8, size= 3, position=position_dodge(width=.9)) +
#			geom_point(data = features.a_feature, aes(x=cat_label, y=mean), shape = 8, size = 3)+
			scale_fill_manual(values=c("gray40", "gray65"), name = '', breaks=0:1, labels=comp_label)+
			xlab(label_x) + ylab(label_y) 
	save_ggplot(plot, figure_name,  24, opts(legend.position="bottom"))

#	roles.box$comp_type <- factor(roles.box$comp_type)
#	roles.box$rank.x <- factor(roles.box$rank.x)
#	print(roles.box)
#	plot <- ggplot(roles.box, aes(rank.x, mean, fill = comp_type, lower=lower, upper=upper, middle=middle, ymin=ymin, ymax=ymax)) + 
#			geom_boxplot(stat="identity", fatten = 4)+ #scale_fill_grey(start = .5, end = .9, name = '') +
#			stat_summary(fun.y = "mean", geom = "point", shape= 8, size= 3, position=position_dodge(width=.9)) +
#			scale_x_discrete(breaks=1:4, labels= c('1st', '2nd', '3rd', '4th'))+
#			scale_fill_manual(values=c("gray40", "gray65", "gray90"), name = '', breaks=0:2,
#					labels=c('Hugged vs.   \n   iSmile', 'Hugged vs.   \n   iHeart', 'iSmile vs.   \n  iHeart'))+
#			xlab('Class') + ylab('Role distance')
#	save_ggplot(plot, paste(c('raw_stat_apps/cross_app_', figure_name, '.pdf'), collapse = ''),
	
	return(features.a_feature)
}

feature_selction <- function(df, cat, features){
	chi_vals <- c()
	for (feature in features){
		chi_table <- table(df[[cat]], round(df[[feature]],2))
		chi <- chisq.test(chi_table)
		chi_vals <- c(chi_vals, chi$statistic)
	}
	normalized_chi_vals <- (chi_vals)/(max(chi_vals))
	chi_result <- as.data.frame(features)
	colnames(chi_result) <- c('Features')
	chi_result$chi_vals <- chi_vals
	chi_result$norm_chi_vals <- normalized_chi_vals
	return(chi_result)
}

inviter_details <- function(file){
	inviters <- as.data.frame(read.csv(file, header=FALSE))
	colnames(inviters) <- c('gender', 'locale', 'sent_ARs', 'active_children', 'children_count')
	inviters <- inviters[inviters$sent_ARs > 0, ]
	inviters$succ_ratio <- inviters$active_children / inviters$children_count
	inviters$gender <- factor(inviters$gender)
	categories <- levels(inviters$gender)
	inviters$cat_label <- factor(inviters$gender, levels = categories, labels = c('Female', 'Male'))
	
	feature_boxplot(inviters, 'active_children', 
			'Inviter gender', 'Number of active children', 'iheart_gift/inv_gender_active_children.pdf')
	feature_boxplot(inviters, 'succ_ratio', 
			'Inviter gender', 'Success ratio', 'iheart_gift/inv_gender_succ_ratio.pdf')
	
	data.m = melt(table(inviters$cat_label))
	names(data.m)[1] = "gender"
	names(data.m)[2] = "count"
	data.m$ratio=data.m$count/sum(data.m$count)
	mydf = ddply(data.m, .(gender), transform, position = cumsum(count) - 0.5*count) 
	print(mydf)
#	pie <- ggplot(mydf, aes(x = factor(1), y = count, fill = as.factor(gender))) +
#			geom_bar(stat = "identity", width = 1, colour = "gray55") +
#			facet_wrap(~gender) + 
#			coord_polar(theta = "y") +
#			geom_text(aes(label = sprintf("%1.2f%%", 100*ratio), y = position))
	pie <- ggplot(mydf, aes(x = factor(1), y = count, fill = gender)) + geom_bar(width = 1, colour = "gray55") + 
			coord_polar(theta = "y") +
			geom_text(aes(label = sprintf("%1.2f%%", 100*ratio), y = position)) + 
			
	save_ggplot(pie, 'iheart_gift/inv_gender.pdf')
	return (inviters)
}

feat_all <- load_features_and_plot('iheart_gift/imp_all_adoption_features_10M.csv')