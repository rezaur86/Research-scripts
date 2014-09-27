source('~/scripts/Cascade/tools.r')
library(plyr)
require(scales)
require(grid)

read_combined_data <- function(){
	comb_sent_AR_file <- '/home/rezaur/output_cascade/raw_stat_apps/comb_sent_ar.csv'
	comb_sent_AR <<- as.data.frame(read.csv(comb_sent_AR_file, header=FALSE))
	colnames(comb_sent_AR) <<-  c('seq', 'hugged', 'iheart', 'ismile')
	comb_sent_AR <<- comb_sent_AR[which(comb_sent_AR$hugged > 0 & comb_sent_AR$iheart > 0 & comb_sent_AR$ismile > 0),]
	print(comb_sent_AR_file)
	comb_act_child_file <- '/home/rezaur/output_cascade/raw_stat_apps/comb_act_child.csv'
	comb_act_child <<- as.data.frame(read.csv(comb_act_child_file, header=FALSE))
	colnames(comb_act_child) <<-  c('seq', 'hugged', 'iheart', 'ismile')
	comb_act_child <<- comb_act_child[which(comb_act_child$hugged >= 0 & comb_act_child$iheart >= 0 & comb_act_child$ismile >= 0),]
	print(comb_act_child_file)
	comb_succ_ratio_file <- '/home/rezaur/output_cascade/raw_stat_apps/comb_succ_ratio.csv'
	comb_succ <<- as.data.frame(read.csv(comb_succ_ratio_file, header=FALSE))
	colnames(comb_succ) <<-  c('seq', 'hugged', 'iheart', 'ismile')
	comb_succ <<- comb_succ[which(comb_succ$hugged >= 0 & comb_succ$iheart >= 0 & comb_succ$ismile >= 0),]
	print(comb_succ_ratio_file)
	comb_act_life_file = '/home/rezaur/output_cascade/raw_stat_apps/comb_act_life.csv'
	comb_act_life <<- as.data.frame(read.csv(comb_act_life_file, header=FALSE))
	colnames(comb_act_life) <<-  c('seq', 'hugged', 'iheart', 'ismile')
#	comb_act_life <<- comb_act_life[which(comb_act_life$hugged >= 0 & comb_act_life$hugged < 24855 &
#							comb_act_life$iheart >= 0 & comb_act_life$iheart < 24855 &
#							comb_act_life$ismile >= 0 & comb_act_life$ismile < 24855),]
	comb_act_life <<- comb_act_life[which(comb_act_life$seq %in% comb_sent_AR$seq),]
	print(comb_act_life_file)
}

apps_common_features <- function(df){
	hug <- as.data.frame(table(df$hugged))
	colnames(hug) <- c('feature', 'count')
	hug$feature <- as.numeric(levels(hug$feature))[hug$feature]
	hug$app_type <- 0
	ism <- as.data.frame(table(df$ismile))
	colnames(ism) <- c('feature', 'count')
	ism$feature <- as.numeric(levels(ism$feature))[ism$feature]
	ism$app_type <- 1
	ihe <- as.data.frame(table(df$iheart))
	colnames(ihe) <- c('feature', 'count')
	ihe$feature <- as.numeric(levels(ihe$feature))[ihe$feature]
	ihe$app_type <- 2
	comb <- rbind(hug, ism, ihe)
	comb.df <- ddply(comb, c('app_type'), function(one_partition){
				one_partition = one_partition[order(one_partition$feature),]
				one_partition$cum_count = cumsum(one_partition$count)
				one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
				one_partition$pdf_val = one_partition$count / max(one_partition$cum_count)
				one_partition
			})
	comb.df$app_type <- factor(comb.df$app_type)
	return (comb.df)
}

apps_invitations <- function(){
	comb.df <- apps_common_features(comb_sent_AR)
	plot <- ggplot(comb.df, aes(x = (feature), y = (cdf_val))) + 
			geom_line(aes(group = app_type, colour = app_type, linetype = app_type), size=1)+
			scale_linetype_manual(values=c(1,1,6), name='', breaks=0:2,
					labels=c('Hugged', 'iSmile', 'iHeart')) +
			scale_colour_manual(values=c("black", "gray55", "black"), name='', breaks=0:2,
					labels=c('Hugged', 'iSmile', 'iHeart')) +
			scale_x_log10(breaks=c(10, 100, 1000))+
			xlab('Application requests') + ylab('Empirical CDF')
	save_ggplot(plot, 'raw_stat_apps/sent_AR_cdf.pdf', 24, opts(legend.position=c(.8, .2)))
	return(comb.df)
}

apps_activated_users <- function(){
	comb.df <- apps_common_features(comb_act_child)
	plot <- ggplot(comb.df[comb.df$feature > 0, ], aes(x = (feature), y = (cdf_val))) + 
			geom_line(aes(group = app_type, colour = app_type, linetype = app_type), size=1)+
			scale_linetype_manual(values=c(1,1,6), name='', breaks=0:2,
					labels=c('Hugged', 'iSmile', 'iHeart')) +
			scale_colour_manual(values=c("black", "gray55", "black"), name='', breaks=0:2,
					labels=c('Hugged', 'iSmile', 'iHeart')) +
			scale_x_log10(breaks=c(10, 100))+
			xlab('Activated users') + ylab('Empirical CDF')
	save_ggplot(plot, 'raw_stat_apps/act_users_cdf.pdf', 24, opts(legend.position=c(.8, .2)))
	return(comb.df)
}

apps_success_ratio <- function(){
	comb.df <- apps_common_features(comb_succ)
	plot <- ggplot(comb.df, aes(x = (feature), y = (cdf_val))) + 
			geom_line(aes(group = app_type, colour = app_type, linetype = app_type), size=1)+
			scale_linetype_manual(values=c(1,1,6), name='', breaks=0:2,
					labels=c('Hugged', 'iSmile', 'iHeart')) +
			scale_colour_manual(values=c("black", "gray55", "black"), name='', breaks=0:2,
					labels=c('Hugged', 'iSmile', 'iHeart')) +
			xlab(expression(paste('Success ratio (',tau,')'))) + ylab('Empirical CDF')
	save_ggplot(plot, 'raw_stat_apps/success_ratio.pdf', 24, opts(legend.position=c(.8, .2)))
	return(comb.df)
}

apps_active_lifespan <- function(){
	comb.df <- apps_common_features(comb_act_life)
	plot <- ggplot(comb.df, aes(x = (feature), y = (cdf_val))) + 
			geom_line(aes(group = app_type, colour = app_type, linetype = app_type), size=1)+
			scale_linetype_manual(values=c(1,1,6), name='', breaks=0:2,
					labels=c('Hugged', 'iSmile', 'iHeart')) +
			scale_colour_manual(values=c("black", "gray55", "black"), name='', breaks=0:2,
					labels=c('Hugged', 'iSmile', 'iHeart')) +
			xlab("Lifetime (Days)") + ylab("Empirical CDF")
	save_ggplot(plot, 'raw_stat_apps/active_lifetime.pdf', 24, opts(legend.position=c(.8, .2)))
	return(comb.df)
}

class_distance_across_apps <- function(df, figure_name, feature_name){
	hug <- as.data.frame(df[,c('seq', 'hugged')])	
#	hug$rank <- as.numeric(-factor(hug$hugged))
	q <- quantile(hug$hugged, c(.5,.9,.99))
#	hug_top <- hug[hug$hugged >= q[3], ]
	hug$rank <- rank(hug$hugged, ties.method = "min") * 100 / length(hug$hugged)
	hug$top <- 1
	hug$top [hug$hugged < q[3]] <- 2
	hug$top [hug$hugged < q[2]] <- 3
#	hug <- as.data.frame(hug[,c('seq', 'rank')])
	
	ism <- as.data.frame(df[,c('seq', 'ismile')])
	q <- quantile(ism$ismile, c(.5,.9,.99))
	ism$rank <- rank(ism$ismile, ties.method = "min") * 100 / length(ism$ismile)
	ism$top <- 1
	ism$top [ism$ismile < q[3]] <- 2
	ism$top [ism$ismile < q[2]] <- 3
#	ism <- as.data.frame(ism[,c('seq', 'rank')])
	
	ihe <- as.data.frame(df[,c('seq', 'iheart')])
	q <- quantile(ihe$iheart, c(.5,.9,.99))
	ihe$rank <- rank(ihe$iheart, ties.method = "min") * 100 / length(ihe$iheart)
	ihe$top <- 1
	ihe$top [ihe$iheart < q[3]] <- 2
	ihe$top [ihe$iheart < q[2]] <- 3
#	ihe <- as.data.frame(ihe[,c('seq', 'rank')])

	hug_ism <- merge(hug, ism, by="seq", all.x = TRUE)
	hug_ihe <- merge(hug, ihe, by="seq", all.x = TRUE)
	ism_ihe <- merge(ism, ihe, by="seq", all.x = TRUE)
	
	top_stat_99 <- as.data.frame(
			c(length(which(hug_ism$top.x==1 & hug_ism$top.y==1))*100/length(which(hug_ism$top.x==1)),
			length(which(hug_ihe$top.x==1 & hug_ihe$top.y==1))*100/length(which(hug_ihe$top.x==1)),
			length(which(ism_ihe$top.x==1 & ism_ihe$top.y==1))*100/length(which(ism_ihe$top.x==1))),
	)
	colnames(top_stat_99) <- c('remained_top')
	top_stat_99$app_type <- c(0,3,6)
	top_stat_99$top_cat <- 0
	top_stat_90 <- as.data.frame(
			c(length(which(hug_ism$top.x<=2 & hug_ism$top.y<=2))*100/length(which(hug_ism$top.x<=2)),
					length(which(hug_ihe$top.x<=2 & hug_ihe$top.y<=2))*100/length(which(hug_ihe$top.x<=2)),
					length(which(ism_ihe$top.x<=2 & ism_ihe$top.y<=2))*100/length(which(ism_ihe$top.x<=2))),
	)
	colnames(top_stat_90) <- c('remained_top')
	top_stat_90$app_type <- c(0,3,6)
	top_stat_90$top_cat <- 1
	top_stat <- rbind(top_stat_99, top_stat_90)
	top_stat$top_cat <- factor(top_stat$top_cat)
	plot <-ggplot(data=top_stat, aes(x=app_type, y=remained_top, fill=top_cat)) + 
			geom_bar(stat="identity", position=position_dodge()) +
			scale_x_discrete(breaks=c(0,3,6), labels= c('Hugged vs.\niSmile', 'Hugged vs.\niHeart', 'iSmile vs.\niHeart'))+
			scale_fill_manual(values=c("gray40", "gray65"), name = '', breaks=0:1, labels=c('Top 1%', 'Top 10%'))+
			xlab('') + ylab('Percentage remained as top')
	save_ggplot(plot, paste(c('raw_stat_apps/top_stat_', figure_name, '.pdf'), collapse = ''), 24,
			opts(legend.position=c(.8, .7)))

	feat_1 <- as.data.frame(hug_ism[which(hug_ism$top.x<=1 & hug_ism$top.y>1),]$rank.y)
	colnames(feat_1) <- c('feature')
	feat_1$app_comp <- 0 #'Hugged vs iSmile'
	feat_1$app_num <- 0 #'Hugged'
	feat_11<- as.data.frame(hug_ism[which(hug_ism$top.x<=2 & hug_ism$top.y>2),]$rank.y)
	colnames(feat_11) <- c('feature')
	feat_11$app_comp <- 0 #'Hugged vs iSmile'
	feat_11$app_num <- 1 #'iSmile'
	feat_2 <- as.data.frame(hug_ihe[which(hug_ihe$top.x<=1 & hug_ihe$top.y>1),]$rank.y)
	colnames(feat_2) <- c('feature')
	feat_2$app_comp <- 1 #'Hugged vs iHeart'
	feat_2$app_num <- 0 #'Hugged'
	feat_21 <- as.data.frame(hug_ihe[which(hug_ihe$top.x<=2 & hug_ihe$top.y>2),]$rank.y)
	colnames(feat_21) <- c('feature')
	feat_21$app_comp <- 1 #'Hugged vs iHeart'
	feat_21$app_num <- 1 #'iHeart'
	feat_3 <- as.data.frame(ism_ihe[which(ism_ihe$top.x<=1 & ism_ihe$top.y>1),]$rank.y)
	colnames(feat_3) <- c('feature')
	feat_3$app_comp <- 2 #'iSmile vs iHeart'
	feat_3$app_num <- 0 #'iSmile'
	feat_31 <- as.data.frame(ism_ihe[which(ism_ihe$top.x<=2 & ism_ihe$top.y>2),]$rank.y)
	colnames(feat_31) <- c('feature')
	feat_31$app_comp <- 2 #'iSmile vs iHeart'
	feat_31$app_num <- 1 #'iHeart'
	feat <- rbind(feat_1, feat_11, feat_2, feat_21, feat_3, feat_31)
	feat.box <- ddply(feat, c('app_comp', 'app_num'), .drop=TRUE,
			.fun = function(one_partition){
				stats = boxplot.stats(one_partition$feature)$stats
				c(ymin=stats[1],
						lower=stats[2],
						middle=stats[3],
						upper=stats[4],
						ymax=stats[5],
						mean = mean(one_partition$feature))
			})
	print(feat.box)
	feat.box$app_comp <- factor(feat.box$app_comp)
	feat.box$app_num <- factor(feat.box$app_num)
	plot <- ggplot(feat.box, aes(x=app_comp, mean, fill = app_num, lower=lower, upper=upper, middle=middle, ymin=ymin, ymax=ymax)) + 
			geom_boxplot(stat="identity", fatten = 4) + #geom_point(data = feat.box, aes(x=app_type, y=mean), shape = 8, size = 3)+
			stat_summary(fun.y = "mean", geom = "point", shape= 8, size= 3, position=position_dodge(width=.9)) +
			scale_x_discrete(breaks=0:2, labels= c('Hugged vs.\niSmile', 'Hugged vs.\niHeart', 'iSmile vs.\niHeart'))+
			scale_fill_manual(values=c("gray40", "gray65"), name = '', breaks=0:1, labels=c('Top 1%', 'Top 10%'))+
			xlab('Comparison') + ylab(feature_name) 
	save_ggplot(plot, paste(c('raw_stat_apps/left_', figure_name, '.pdf'), collapse = ''), 24,
			opts(legend.position="bottom"))
	
	hug_ism <- merge(hug[,c('seq', 'rank')], ism[,c('seq', 'rank')], by="seq")
	hug_ism$distance <- (hug_ism$rank.y - hug_ism$rank.x)
	hug_ism$comp_type <- 0 #'Hugged vs.   \n   iSmile'
	
	hug_ihe <- merge(hug[,c('seq', 'rank')], ihe[,c('seq', 'rank')], by="seq")
	hug_ihe$distance <- (hug_ihe$rank.y - hug_ihe$rank.x)
	hug_ihe$comp_type <- 1 #'Hugged vs.   \n   iHeart'
	
	ism_ihe <- merge(ism[,c('seq', 'rank')], ihe[,c('seq', 'rank')], by="seq")
	ism_ihe$distance <- (ism_ihe$rank.y - ism_ihe$rank.x)
	ism_ihe$comp_type <- 2 #'iSmile vs.   \n  iHeart'
	
#	roles <- rbind(hug_ism, hug_ihe, ism_ihe)
#	roles.box <- ddply(roles, c('comp_type', 'rank.x'), .drop=TRUE,
#			.fun = function(one_partition){
#				stats = boxplot.stats(one_partition$distance)$stats
#				c(ymin=stats[1],
#						lower=stats[2],
#						middle=stats[3],
#						upper=stats[4],
#						ymax=stats[5],
#						mean = mean(one_partition$distance))
#			})
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
#	save_ggplot(plot, paste(c('raw_stat_apps/cross_app_', figure_name, '.pdf'), collapse = ''), 24,
#			opts(legend.position="bottom"))
#	
#	temp.box <- roles.box
	sim_hug_ism <- as.data.frame(hug_ism$distance)
	colnames(sim_hug_ism) <- 'distance'
	sim_hug_ism$comp_type <- 0 #'Hugged vs.\niSmile'
	sim_hug_ihe <- as.data.frame(hug_ihe$distance)
	colnames(sim_hug_ihe) <- 'distance'
	sim_hug_ihe$comp_type <- 1 #'Hugged vs.\niHeart'
	sim_ism_ihe <- as.data.frame(ism_ihe$distance)
	colnames(sim_ism_ihe) <- 'distance'
	sim_ism_ihe$comp_type <- 2 #'iSmile vs.\niHeart'
	roles <- rbind(sim_hug_ism, sim_hug_ihe, sim_ism_ihe)
	
	roles.box <- ddply(roles, c('comp_type'), .drop=TRUE,
			.fun = function(one_partition){
				stats = boxplot.stats(one_partition$distance)$stats
				c(ymin=stats[1],
						lower=stats[2],
						middle=stats[3],
						upper=stats[4],
						ymax=stats[5],
						mean = mean(one_partition$distance))
			})
	roles.box$comp_type <- factor(roles.box$comp_type)
	print(roles.box)
#	roles$comp_type <- factor(roles$comp_type)
#	plot <- ggplot(roles, aes(x=comp_type, y=distance)) + 
#			geom_boxplot(outlier.shape = NA, fatten = 4) + stat_summary(fun.y = "mean", geom = "point", shape= 8, size= 3)+
#			xlab('') + ylab('Role distance') 
	plot <- ggplot(roles.box, aes(x=comp_type, lower=lower, upper=upper, middle=middle, ymin=ymin, ymax=ymax)) + 
			geom_boxplot(stat="identity", fatten = 4) + geom_point(data = roles.box, aes(x=comp_type, y=mean), shape = 8, size = 3)+
			scale_x_discrete(breaks=0:2, labels= c('Hugged vs.\niSmile', 'Hugged vs.\niHeart', 'iSmile vs.\niHeart'))+
			xlab('') + ylab('Role distance') 
	save_ggplot(plot, paste(c('raw_stat_apps/similarity_', figure_name, '.pdf'), collapse = ''))
	return (list(sim=roles))
}

comb_app_user_features <- function (){
#	comb_sent_AR[!(comb_sent_AR > 0)] <<- NA
#	comb_succ[!(comb_succ >= 0)] <- NA
#	comb_act_child[!(comb_act_child >= 0)] <- NA
#	comb_act_life[!(comb_act_life$hugged > -1 & comb_act_life$hugged < 24855), ]$hugged <- NA
#	comb_act_life[!(comb_act_life$iheart > -1 & comb_act_life$iheart < 24855), ]$iheart <- NA
#	comb_act_life[!(comb_act_life$ismile > -1 & comb_act_life$ismile < 24855), ]$ismile <- NA

	print('Getting Spearman correlation')
	dflist <- list(comp1 = list(df1 = comb_sent_AR, df2 = comb_sent_AR),
			comp2 = list(df1 = comb_sent_AR, df2 = comb_act_child),
			comp3 = list(df1 = comb_sent_AR, df2 = comb_succ),
			comp4 = list(df1 = comb_sent_AR, df2 = comb_act_life),
			comp8 = list(df1 = comb_act_child, df2 = comb_act_child),
			comp6 = list(df1 = comb_act_child, df2 = comb_succ),
			comp9 = list(df1 = comb_act_child, df2 = comb_act_life),
			comp5 = list(df1 = comb_succ, df2 = comb_succ),
			comp7 = list(df1 = comb_succ, df2 = comb_act_life),
			comp10 = list(df1 = comb_act_life, df2 = comb_act_life))
	feature_comp <- c('AR vs. AR', 'AR vs. AU',
			'AR vs. SR', 'AR vs. LT',
			'AU vs. AU', 'AU vs. SR', 'AU vs. LT', 
			'SR vs. SR',  'SR vs. LT',
			'LT vs. LT')
	app_comp <- c('hug_ihe', 'hug_ism', 'ism_ihe')
	spearman_corr <- as.data.frame(feature_comp)
	pearson_corr <- as.data.frame(feature_comp)
	colnames(spearman_corr) <- c('Correlation')
	temp_1 <- c()
	temp_2 <- c()
	temp_3 <- c()
	temp_11 <- c()
	temp_22 <- c()
	temp_33 <- c()
	lapply(dflist, function(comp) {
				temp_1 <<- c(temp_1, spearman_correlation(comp$df1, comp$df2, 'hugged', 'iheart'))
				temp_2 <<- c(temp_2, spearman_correlation(comp$df1, comp$df2, 'hugged', 'ismile'))
				temp_3 <<- c(temp_3, spearman_correlation(comp$df1, comp$df2, 'ismile', 'iheart'))
				temp_11 <<- c(temp_11, cor(comp$df1$hugged, comp$df2$iheart))
				temp_22 <<- c(temp_22, cor(comp$df1$hugged, comp$df2$ismile))
				temp_33 <<- c(temp_33, cor(comp$df1$ismile, comp$df2$iheart))
				NULL
			})
	spearman_corr[[app_comp[1]]] <- temp_1
	spearman_corr[[app_comp[2]]] <- temp_2
	spearman_corr[[app_comp[3]]] <- temp_3
	pearson_corr[[app_comp[1]]] <- temp_11
	pearson_corr[[app_comp[2]]] <- temp_22
	pearson_corr[[app_comp[3]]] <- temp_33
	return(list(rank = spearman_corr, simple = pearson_corr))
}

spearman_correlation <- function (df1, df2, app1, app2, filter = 1){
	df1 <- df1[!is.na(df1[[app1]]),c('seq', app1)]
	df2 <- df2[!is.na(df2[[app2]]),c('seq', app2)]

	df1_df2 <- merge(df1, df2, by="seq")
	N <- ceiling(nrow(df1_df2)*filter)

	print(N)
	df1_df2$rank.x <- rank(-df1_df2[[app1]])
	df1_df2$rank.y <- rank(-df1_df2[[app2]])
	
	df1_df2_rank_cor <- 1 - 6 * sum ( (df1_df2$rank.x - df1_df2$rank.y)^2 ) / (N^3 - N)
	return(df1_df2_rank_cor)
}

#test <- spearman_correlation(comb_sent_AR, comb_sent_AR, 'hugged', 'iheart')

#ar <- apps_invitations()
#au <- apps_activated_users()
#sr <- apps_success_ratio()
#lt <- apps_active_lifespan()

class_inv <- class_distance_across_apps (comb_sent_AR, 'invitations', 'Application requests')
class_succ <- class_distance_across_apps (comb_succ, 'succ_ratio', expression(paste('Success ratio (',tau,')')))
class_act_users <- class_distance_across_apps (comb_act_child, 'act_users', 'Activated users')
class_act_life <- class_distance_across_apps (comb_act_life, 'act_life', 'Lifetime (Days)')

#corr <- comb_app_user_features()
