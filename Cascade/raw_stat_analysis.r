source('~/scripts/Cascade/tools.r')
library(plyr)

lifespan_analysis <- function(lifespan_file_name){
	lifespan <- as.data.frame(read.csv(lifespan_file_name, header=FALSE))
	colnames(lifespan) <- c('time', 'count')
	plot <- ggplot(lifespan[lifespan$time>0,], aes(x = time, y = count/sum(count))) + geom_line() + xlab('Life time (bin = 600s)') + ylab('proportion of Count') + 
			scale_x_log10(breaks=c(6,12,24,48,72,96,144,288,4*144,7*144,14*144,28*144,2*28*144,4*28*144,8*28*144), labels=c('1hr','2hr','4hr','8hr','12hr','16hr','1day','2day','4day','1week','2weeks','4weeks','8weeks','16weeks','32weeks')) + 
			scale_y_log10() + # + xlim(1,200) + ylim(0,100000)
			myPlotTheme() + opts(axis.text.x=theme_text(angle=45,hjust=1,vjust=1))
	save_ggplot(plot, 'raw_stat/lifespan.pdf')
	lifespan <- lifespan[order(lifespan$time),]
	parent_lifespan <- lifespan[lifespan$time>0,]
	life_95 <- parent_lifespan[cumsum(parent_lifespan$count/sum(parent_lifespan$count))>=.95,]$time[1]
	print_report('95 percentile lifespan', life_95)
	plot <- ggplot(parent_lifespan, aes(x = time, y = cumsum(count/sum(count)))) + geom_line() +
			xlab('Active Lifetime') + ylab('Empirical CDF') + 
#			scale_x_log10(breaks=c(6,12,24,48,72,96,144,288,4*144,7*144,14*144,28*144,2*28*144,4*28*144,8*28*144), labels=c('1hr','2hr','4hr','8hr','12hr','16hr','1day','2day','4day','1week','2weeks','4weeks','8weeks','16weeks','32weeks')) +
			scale_x_log10(breaks=c(1,2,4,7,7*2,7*4,7*8,7*16,7*32), labels=c('1day','2day','4day','1week','2week','4week','8week','16week','32week')) +
			scale_y_continuous(breaks=c(0,.2,.4,.6,.8,1.0))+
#			scale_y_log10() + # + xlim(1,200) + ylim(0,100000)
			myPlotTheme() + opts(axis.text.x=theme_text(angle=45,hjust=1,vjust=1))
	pdf('raw_stat/lifespan_cum_pdf.pdf')
	print(plot)
	dev.off()
	return(parent_lifespan)
}

raw_outdeg_analysis <- function (raw_outdeg_file_name = 'raw_stat_v2/raw_outdeg_stat.csv',
		raw_indeg_file_name='raw_stat_v2/raw_indeg_stat.csv'){
	raw_outdeg <- as.data.frame(read.csv(raw_outdeg_file_name, header=FALSE))
	raw_outdeg[,3] <- 0
	raw_indeg <- as.data.frame(read.csv(raw_indeg_file_name, header=FALSE))
	raw_indeg[,3] <- 1
	raw_degree <- rbind(raw_outdeg, raw_indeg)
	colnames(raw_degree) <- c('degree', 'count', 'degree_type')
	raw_degree.df <- ddply(raw_degree, c('degree_type'), function(one_partition){
				one_partition = one_partition[order(one_partition$degree),]
				one_partition$cum_count = cumsum(one_partition$count)
				one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
				one_partition$pdf_val = one_partition$count / max(one_partition$cum_count)
				one_partition
			})
	raw_degree.df$degree_type <- factor(raw_degree.df$degree_type)
	print(summary(raw_degree.df))
#	raw_outdeg <- raw_outdeg[order(raw_outdeg$outdeg),]
#	raw_outdeg$cum_count <- cumsum(raw_outdeg$count)
#	raw_outdeg$cdf <- raw_outdeg$cum_count / max(raw_outdeg$cum_count)
#	raw_outdeg$pdf <- raw_outdeg$count / max(raw_outdeg$cum_count)
	plot <- ggplot(raw_degree.df[raw_degree.df$degree > 0, ], aes(x = degree, y = pdf_val)) +
			geom_point(aes(group = degree_type, colour = degree_type, shape = degree_type), size=1) +
			xlab('Number of sent/received ARs') + ylab('Empirical PDF') + scale_y_log10() +
			scale_x_log10(limits = c(1, 10^4))
	plot <- change_plot_attributes_fancy(plot, "", 0:1, c('Out degree', 'In degree'), "Number of sent/received ARs", "Empirical PDF")
	save_ggplot(plot, 'raw_stat_v2/degree.pdf')
}

raw_indeg_analysis <- function (raw_indeg_file_name){
	raw_indeg <<- as.data.frame(read.csv(raw_indeg_file_name, header=FALSE))
	colnames(raw_indeg) <<- c('indeg', 'count')
	raw_indeg <- raw_indeg[order(raw_indeg$indeg),]
	raw_indeg$cum_count <- cumsum(raw_indeg$count)
	raw_indeg$cdf <- raw_indeg$cum_count / max(raw_indeg$cum_count)
	raw_indeg$pdf <- raw_indeg$count / max(raw_indeg$cum_count)
	plot <- ggplot(raw_indeg[raw_indeg$indeg > 0, ], aes(x = indeg, y = pdf)) + geom_point(size=.8) +
			xlab('Number of received ARs') + ylab('Empirical PDF') + scale_y_log10() +
			scale_x_log10(limits = c(2, 10^4))
	save_ggplot(plot, 'raw_stat_v2/raw_indeg.pdf')
}

influence_threshold_analysis <- function (parent_count_file_name = 'raw_stat_v2/parent_count_before_act.csv',
		indeg_before_act_file= 'raw_stat_v2/indeg_before_act.csv'){
	influence <- as.data.frame(read.csv(parent_count_file_name, header=FALSE))
	influence[,4] <- 0
	colnames(influence) <- c('indeg', 'act_count', 'total_count', 'indeg_type')
	influence_ar <- as.data.frame(read.csv(indeg_before_act_file, header=FALSE))
	influence_ar[,4] <- 1
	colnames(influence_ar) <- c('indeg', 'act_count', 'total_count', 'indeg_type')
	influence <- rbind(influence, influence_ar)
	influence.df <- ddply(influence, c('indeg_type'), function(one_partition){
				one_partition = one_partition[order(one_partition$indeg),]
				one_partition$cum_count = cumsum(one_partition$total_count)
				one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
				one_partition$pdf_val = one_partition$total_count / max(one_partition$cum_count)
				one_partition$prob = one_partition$act_count/ one_partition$total_count
				one_partition
			})
	influence.df$indeg_type <- factor(influence.df$indeg_type)
#	influence <- influence[order(influence$indeg),]
#	plot <- ggplot(influence, aes(x = indeg, y = (cumsum(count/sum(count))))) + geom_point() + xlab("Distinct Parent Count in Received Gifts") + ylab('Cumulative Proportion of Users') + 
#			scale_x_log10(breaks=c(1,2,4,8,16,32,64,128,256,1000)) +
#			scale_y_continuous(breaks=c(.4,.5,.6,.7,.8,.9,1.0), labels=c('40%','50%','60%','70%','80%','90%','100%'), limits=c(.4,1.0))
	plot <- ggplot(influence.df, aes(x = (indeg), y = (prob))) + 
			geom_point(aes(group = indeg_type, colour = indeg_type, shape = indeg_type), size=1)+
			scale_x_log10(limits = c(1, 10^3)) #+ scale_y_log10()
#			geom_smooth(aes(group = indeg_type, colour = indeg_type, shape = indeg_type), se=FALSE)
	plot <- change_plot_attributes_fancy(plot, "Requests from", 0:1, c('Distinct Parent', 'Distinct AR'),
			"Number of Request", "Prob of activation")
	save_ggplot(plot, 'raw_stat_v2/adop_prob.pdf', 24,
			opts(axis.title.x = theme_text(vjust=-0.5), legend.position=c(.3, .7)))
}

success_ratio_analysis <- function (success_ratio_file='raw_stat_v2/act_proportion_count.csv'){
	influence <- as.data.frame(read.csv(success_ratio_file, header=FALSE))
	colnames(influence) <- c('proportion', 'count')
	influence <- influence[order(influence$proportion),]
	influence$cum_count <- cumsum(influence$count)
	influence$cdf_val <- influence$cum_count / max(influence$cum_count)
	plot <- ggplot(influence, aes(x = (proportion), y = (cdf_val))) + 
			geom_line() + xlab('Success Ratio (%)') + ylab('Empirical CDF')
	save_ggplot(plot, 'raw_stat_v2/success_ratio.pdf')
}

sender_success_ratio_analysis <- function (success_ratio_file='raw_stat_v2/parent_children_act.csv'){
	sender_success <- as.data.frame(read.csv(success_ratio_file, header=FALSE))
	colnames(sender_success) <- c('active_children', 'children')
	sender_success$ratio <- round(sender_success$active_children/sender_success$children, 3)
	sender_success.avg_ratio <- ddply(sender_success, c('children'), summarise, avg_success_ratio = mean (ratio))
	plot <- ggplot(sender_success.avg_ratio, aes(x = (avg_success_ratio*100), y = (children))) + 
			geom_point() + geom_smooth(method=lm,   # Add linear regression lines
					se=FALSE) + xlab('Average Success Ratio (%)') + ylab('Number of sent ARs')
	save_ggplot(plot, 'raw_stat_v2/success_avg_vs_ARs.pdf')
#	sender_success.avg_ar <- ddply(sender_success, c('ratio'), summarise, avg_children = mean (children))
#	plot <- ggplot(sender_success.avg_ar, aes(x = (ratio*100), y = (avg_children))) + 
#			geom_line() + xlab('Success Ratio (%)') + ylab('Average number of sent ARs')
#	save_ggplot(plot, 'raw_stat_v2/success_vs_avg_ARs.pdf')
#	return(list(avg_ratio = sender_success.avg_ratio, avg_ar = sender_success.avg_ar))
	return (sender_success.avg_ratio)
}

invitation_burstiness <- function(file='raw_stat_v2/invitation_burstiness_stat.csv'){
	invitations <- as.data.frame(read.csv(file, header=FALSE))
	colnames(invitations) <- c('burstiness', 'count', 'avg_elapsed_time', 'median_elapsed_time')
#	invitations$burstiness <- as.numeric(levels(invitations$burstiness))[invitations$burstiness]
	invitations <- invitations[order(invitations$burstiness), ]
	invitations$cum_sum <- cumsum(invitations$count)
	invitations$cdf <- invitations$cum_sum/max(invitations$cum_su)
	plot <- ggplot(invitations,
			aes(x = burstiness, y = count)) + geom_point(size=.8)+ geom_smooth(#method=lm,   # Add linear regression lines
			se=FALSE)+ xlab('Burstiness') + ylab('Count')
#			scale_x_log10() + scale_y_log10()
	save_ggplot(plot, 'raw_stat_v2/invitation_burstiness_count.pdf')
	plot <- ggplot(invitations,
			aes(x = burstiness, y = cdf)) + geom_line() + xlab('Burstiness') + ylab('Empirical CDF')
#			scale_x_log10() + scale_y_log10()
	save_ggplot(plot, 'raw_stat_v2/invitation_burstiness_cdf.pdf')
	plot <- ggplot(invitations,
					aes(x = burstiness, y = avg_elapsed_time)) + geom_point(size=.8)+ geom_smooth(#method=lm,
					se=FALSE)+ xlab('Burstiness') + ylab('Avg. elapsed time')
	save_ggplot(plot, 'raw_stat_v2/invitation_burstiness_avg_elapse_time.pdf')
	plot <- ggplot(invitations,
					aes(x = burstiness, y = median_elapsed_time)) + geom_point(size=.8)+ geom_smooth(#method=lm,
					se=FALSE)+ xlab('Burstiness') + ylab('Median elapsed time')
	save_ggplot(plot, 'raw_stat_v2/invitation_burstiness_median_elapsed_time.pdf')
	return(invitations)
}

invitation_elapsed_time_analysis <- function (invitation_elapsed_time_file='raw_stat_v2/invitation_elapsed_time_stat.csv'){
	elapsed_time <- as.data.frame(read.csv(invitation_elapsed_time_file, header=FALSE))
	colnames(elapsed_time) <- c('time', 'count')
	elapsed_time <- elapsed_time[order(elapsed_time$time),]
	elapsed_time$cum_count <- cumsum(elapsed_time$count)
	elapsed_time$cdf_val <- elapsed_time$cum_count / max(elapsed_time$cum_count)
	plot <- ggplot(elapsed_time, aes(x = (time), y = (cdf_val))) + 
			geom_line() + xlab('Elapsed time (Hour)') + ylab('Empirical CDF')
	save_ggplot(plot, 'raw_stat_v2/invitation_elased_time.pdf')
}

#parent_lifespan<-lifespan_analysis('raw_stat_1/lifespan_stat.csv')
#raw_outdeg_analysis('raw_stat_v2/raw_outdeg_stat.csv')
#influence_threshold_analysis('raw_stat_v2/parent_count_before_act.csv', 'raw_stat_v2/indeg_before_act.csv')
#active_proportion_analysis('raw_stat_v2/act_proportion_count.csv')
