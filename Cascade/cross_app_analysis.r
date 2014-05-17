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
			xlab("Active lifetime (Days)") + ylab("Empirical CDF")
	save_ggplot(plot, 'raw_stat_apps/active_lifetime.pdf', 24, opts(legend.position=c(.8, .2)))
	return(comb.df)
}

#ar <- apps_invitations()
#au <- apps_activated_users()
#sr <- apps_success_ratio()
#lt <- apps_active_lifespan()

class_distance_across_apps <- function(df, figure_name){
	hug <- as.data.frame(df[,c('seq', 'hugged')])
	q <- quantile(hug$hugged, c(.5,.9,.99))
	hug$rank <- 1
	hug$rank [hug$hugged < q[3]] <- 2
	hug$rank [hug$hugged < q[2]] <- 3
	hug$rank [hug$hugged < q[1]] <- 4
	hug <- as.data.frame(hug[,c('seq', 'rank')])
	
	ism <- as.data.frame(df[,c('seq', 'ismile')])
	q <- quantile(ism$ismile, c(.5,.9,.99))
	ism$rank <- 1
	ism$rank [ism$ismile < q[3]] <- 2
	ism$rank [ism$ismile < q[2]] <- 3
	ism$rank [ism$ismile < q[1]] <- 4
	ism <- as.data.frame(ism[,c('seq', 'rank')])
	
	ihe <- as.data.frame(df[,c('seq', 'iheart')])
	q <- quantile(ihe$iheart, c(.5,.9,.99))
	ihe$rank <- 1
	ihe$rank [ihe$iheart < q[3]] <- 2
	ihe$rank [ihe$iheart < q[2]] <- 3
	ihe$rank [ihe$iheart < q[1]] <- 4
	ihe <- as.data.frame(ihe[,c('seq', 'rank')])
	
	hug_ihe <- merge(hug, ihe, by="seq")
	hug_ihe$distance <- (hug_ihe$rank.y - hug_ihe$rank.x)
	hug_ihe$comp_type <- 'Hugged vs.   \n   iHeart'
	
	hug_ism <- merge(hug, ism, by="seq")
	hug_ism$distance <- (hug_ism$rank.y - hug_ism$rank.x)
	hug_ism$comp_type <- 'Hugged vs.   \n   iSmile'
	
	ism_ihe <- merge(ism, ihe, by="seq")
	ism_ihe$distance <- (ism_ihe$rank.y - ism_ihe$rank.x)
	ism_ihe$comp_type <- 'iSmile vs.   \n  iHeart'
	
	roles <- rbind(hug_ihe, hug_ism, ism_ihe)
	roles.box <- ddply(roles, c('comp_type', 'rank.x'), .drop=TRUE,
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
	roles.box$rank.x <- factor(roles.box$rank.x)
	print(roles.box)
	plot <- ggplot(roles.box, aes(rank.x, mean, fill = comp_type, lower=lower, upper=upper, middle=middle, ymin=ymin, ymax=ymax)) + 
			geom_boxplot(stat="identity", fatten = 4)+ scale_fill_grey(start = .5, end = .9, name = '') +
			stat_summary(fun.y = "mean", geom = "point", shape= 8, size= 3, position=position_dodge(width=.9)) +
			scale_x_discrete(breaks=1:4, labels= c('1st', '2nd', '3rd', '4th'))+
			xlab('Class') + ylab('Role distance')
	save_ggplot(plot, paste(c('raw_stat_apps/cross_app_', figure_name, '.pdf'), collapse = ''), 24,
			opts(legend.position="bottom"))
	
	temp.box <- roles.box
	sim_hug_ihe <- as.data.frame(hug_ihe$distance)
	colnames(sim_hug_ihe) <- 'distance'
	sim_hug_ihe$comp_type <- 'Hugged vs.\niHeart'
	sim_hug_ism <- as.data.frame(hug_ism$distance)
	colnames(sim_hug_ism) <- 'distance'
	sim_hug_ism$comp_type <- 'Hugged vs.\niSmile'
	sim_ism_ihe <- as.data.frame(ism_ihe$distance)
	colnames(sim_ism_ihe) <- 'distance'
	sim_ism_ihe$comp_type <- 'iHeart vs.\niSmile'
	roles <- rbind(sim_hug_ihe, sim_hug_ism, sim_ism_ihe)
	
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
	plot <- ggplot(roles.box, aes(x=comp_type, lower=lower, upper=upper, middle=middle, ymin=ymin, ymax=ymax)) + 
			geom_boxplot(stat="identity", fatten = 4) + geom_point(data = roles.box, aes(x=comp_type, y=mean), shape = 8, size = 3)+
			xlab('') + ylab('Role distance') 
	save_ggplot(plot, paste(c('raw_stat_apps/similarity_', figure_name, '.pdf'), collapse = ''))
	return (list(class=temp.box, sim=roles))
}


class_inv <- class_distance_across_apps (comb_sent_AR, 'invitations')
class_succ <- class_distance_across_apps (comb_succ, 'succ_ratio')
class_act_users <- class_distance_across_apps (comb_act_child, 'act_users')
class_act_life <- class_distance_across_apps (comb_act_life, 'act_life')

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
	colnames(spearman_corr) <- c('Correlation')
	temp_1 <- c()
	temp_2 <- c()
	temp_3 <- c()
	lapply(dflist, function(comp) {
				temp_1 <<- c(temp_1, spearman_correlation(comp$df1, comp$df2, 'hugged', 'iheart'))
				temp_2 <<- c(temp_2, spearman_correlation(comp$df1, comp$df2, 'hugged', 'ismile'))
				temp_3 <<- c(temp_3, spearman_correlation(comp$df1, comp$df2, 'ismile', 'iheart'))
				NULL
			})
	spearman_corr[[app_comp[1]]] <- temp_1
	spearman_corr[[app_comp[2]]] <- temp_2
	spearman_corr[[app_comp[3]]] <- temp_3
	return(spearman_corr)
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

#spearman <- comb_app_user_features()

