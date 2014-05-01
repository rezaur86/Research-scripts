source('~/scripts/Cascade/tools.r')
library(plyr)

lifespan_analysis <- function(adoption_delay_file_name='raw_stat_wo_burst/delay_recv_AR_stat.csv',
		act_lifespan_file_name='raw_stat_wo_burst/act_lifespan_sent_AR_stat.csv'){
	adoption_delay <- as.data.frame(read.csv(adoption_delay_file_name, header=FALSE))
	adoption_delay[,5] <- 0
	colnames(adoption_delay) <- c('time', 'count', 'avg_AR', 'avg_AR_', 'life_type')
	adoption_delay[adoption_delay$time == 24855,]$time <- Inf
	adoption_delay <- adoption_delay[adoption_delay$time < 24855,]
	act_lifespan <- as.data.frame(read.csv(act_lifespan_file_name, header=FALSE))
	act_lifespan[,5] <- 1
	colnames(act_lifespan) <- c('time', 'count', 'avg_AR', 'avg_AR_', 'life_type')
	act_lifespan[act_lifespan$time == 24855,]$time <- Inf
	act_lifespan <- act_lifespan[act_lifespan$time < 24855,]
	adoption_times <- rbind(adoption_delay, act_lifespan)
	adoptions_temp <- adoption_times
	adoptions_temp$life_type <- factor(adoptions_temp$life_type)
	plot <- ggplot(adoptions_temp, aes(x = time, y = count)) +
			geom_line(aes(group = life_type, colour = life_type, shape = life_type), size=1) +
			scale_y_log10()
	plot <- change_plot_attributes_fancy(plot, "", 0:1, c('Adoption delay', 'Active lifespan'), "Days", "Count")
	save_ggplot(plot, 'raw_stat_v2/adoption_times.pdf')
	adoptions_temp.df <- ddply(adoptions_temp, c('life_type'), function(one_partition){
				one_partition = one_partition[order(one_partition$time),]
				one_partition$cum_count = cumsum(one_partition$count)
				one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
				one_partition$pdf_val = one_partition$count / max(one_partition$cum_count)
				one_partition
			})
	adoptions_temp.df$life_type <- factor(adoptions_temp.df$life_type)
	plot <- ggplot(adoptions_temp.df, aes(x = time, y = cdf_val)) +
			geom_line(aes(group = life_type, colour = life_type, shape = life_type), size=1) #+ scale_y_log10()	
#			scale_y_continuous(breaks=c(0,.2,.4,.6,.8,1.0))
	plot <- change_plot_attributes_fancy(plot, "", 0:1, c('Adoption delay', 'Active lifespan'), "Days", "Empirical CDF")
	save_ggplot(plot, 'raw_stat_v2/adoption_times_cdf.pdf', 24,
			opts(axis.text.x = element_text(angle = 0, hjust = 0), legend.position=c(.65, .65)))
	adoptions_temp.df$life_type <- as.numeric(adoptions_temp.df$life_type)
	plot <- ggplot(adoptions_temp.df[adoptions_temp.df$life_type == 1, ], aes(x = time, y = cdf_val)) + geom_line() +
			xlab('Adoption delay (Days)') + ylab('Empirical CDF')
	save_ggplot(plot, 'raw_stat_v2/adoption_delay.pdf')
	plot <- ggplot(adoptions_temp.df[adoptions_temp.df$life_type == 2, ], aes(x = time, y = cdf_val)) + geom_line() +
			xlab('Active lifetime (Days)') + ylab('Empirical CDF')
	save_ggplot(plot, 'raw_stat_v2/active_lifetime.pdf')
#	lifespan_indeg <- adoption_times[,c(1,3,5)]
#	colnames(lifespan_indeg) <- c('time', 'avg', 'life_type')
#	lifespan_indeg$degree_type <- 0
#	lifespan_odeg <- adoption_times[,c(1,4,5)]
#	colnames(lifespan_odeg) <- c('time', 'avg', 'life_type')
#	lifespan_odeg$degree_type <- 1
#	lifespan_indeg_odeg <- rbind(lifespan_indeg, lifespan_odeg)
#	print(summary(lifespan_indeg_odeg))
#	lifespan_indeg_odeg$degree_type <- do.call(paste, c(as.data.frame(cbind(
#									lifespan_indeg_odeg$degree_type, lifespan_indeg_odeg$life_type)), sep=""))
#	lifespan_indeg_odeg$degree_type <- factor(lifespan_indeg_odeg$degree_type)
#	plot <- ggplot(lifespan_indeg_odeg[lifespan_indeg_odeg$time < Inf, ], aes(x = time, y = avg)) +
#			geom_line(aes(group = degree_type, colour = degree_type, shape = degree_type), size=1) +
#			scale_y_log10()
#	plot <- change_plot_attributes_fancy(plot, "", c('00','10','01','11'),
#			c('Adoption delay vs. received ARs', 'Adoption delay vs. distinct inviter',
#			'Active lifespan vs. sent ARs', 'Active lifespan vs. distinct invitee'), "Days", "Avg. count")
#	save_ggplot(plot, 'raw_stat_v2/lifespan_AR.pdf', 24,
#			opts(axis.text.x = element_text(angle = 0, hjust = .4), legend.position=c(.4, .9)))
}

sent_AR_analysis <- function (raw_sent_AR_stat = 'raw_stat_wo_burst/raw_sent_AR_stat.csv',
		raw_sent_AR_children_stat = 'raw_stat_wo_burst/raw_sent_AR_children_stat.csv',
		act_lifespan_file_name='raw_stat_wo_burst/act_lifespan_sent_AR_stat.csv'){
	raw_sent_AR <- as.data.frame(read.csv(raw_sent_AR_stat, header=FALSE))
	raw_sent_AR[,3] <- 0
	raw_sent_AR_children <- as.data.frame(read.csv(raw_sent_AR_children_stat, header=FALSE))
	raw_sent_AR_children[,3] <- 1
	raw_degree <- rbind(raw_sent_AR, raw_sent_AR_children)
	colnames(raw_degree) <- c('degree', 'count', 'degree_type')
	raw_degree <- raw_degree[raw_degree$degree > 0, ]
	raw_degree.df <- ddply(raw_degree, c('degree_type'), function(one_partition){
				one_partition = one_partition[order(one_partition$degree),]
				one_partition$cum_count = cumsum(one_partition$count)
				one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
				one_partition$pdf_val = one_partition$count / max(one_partition$cum_count)
				one_partition
			})
	raw_degree.df$degree_type <- factor(raw_degree.df$degree_type)
	plot <- ggplot(raw_degree.df[raw_degree.df$degree > 0, ], aes(x = degree, y = pdf_val)) +
			geom_point(aes(group = degree_type, colour = degree_type, shape = degree_type), size=1) +
			scale_x_log10(limits = c(1, 10^4))
	plot <- change_plot_attributes(plot, "", 0:1, c('K = Number of sent invitations', 'K = Number of distinct invitees'),
			"K", "Empirical PDF")
	save_ggplot(plot, 'raw_stat_v2/sent_AR.pdf')
	plot <- ggplot(raw_degree.df[raw_degree.df$degree > 0, ], aes(x = degree, y = cdf_val)) +
			geom_point(aes(group = degree_type, colour = degree_type, shape = degree_type), size=1) +
			scale_x_log10(limits = c(1, 10^4))
	plot <- change_plot_attributes(plot, "", 0:1, c('K = Number of sent invitations', 'K = Number of distinct invitees'),
			"K", "Empirical CDF")
	save_ggplot(plot, 'raw_stat_v2/sent_AR_cdf.pdf', 24,
			opts(legend.position=c(.62, .3)))
	act_lifespan <- as.data.frame(read.csv(act_lifespan_file_name, header=FALSE))
	colnames(act_lifespan) <- c('time', 'count', 'avg_invitations', 'avg_invitees')
	act_lifespan <- act_lifespan[act_lifespan$time < 24855,]
	act_lifespan.invitations <- act_lifespan[,c(1,3)]
	colnames(act_lifespan.invitations) <- c('time', 'sent')
	act_lifespan.invitations$degree_type <- 0
	act_lifespan.invitees <- act_lifespan[,c(1,4)]
	colnames(act_lifespan.invitees) <- c('time', 'sent')
	act_lifespan.invitees$degree_type <- 1
	act_lifespan.df <- rbind(act_lifespan.invitations, act_lifespan.invitees)
	act_lifespan.df$degree_type <- factor(act_lifespan.df$degree_type)
	plot <- ggplot(act_lifespan.df, aes(x = time, y = sent)) +
			geom_point(aes(group = degree_type, colour = degree_type, shape = degree_type), size=1) +
			scale_y_log10()
	plot <- change_plot_attributes(plot, "", 0:1, c('K = Avg. number of sent invitations', 'K = Avg. number of distinct invitees'),
			"Active lifetime (Days)", "K")
	save_ggplot(plot, 'raw_stat_v2/act_sent_AR.pdf', 24,
			opts(legend.position=c(.43, .8)))
}

recved_AR_analysis <- function (raw_rec_AR_stat='raw_stat_wo_burst/raw_rec_AR_stat.csv',
		raw_rec_AR_parent_stat = 'raw_stat_wo_burst/raw_rec_AR_parent_stat.csv',
		adoption_delay_file_name='raw_stat_wo_burst/delay_recv_AR_stat.csv'){
	raw_rec_AR <- as.data.frame(read.csv(raw_rec_AR_stat, header=FALSE))
	raw_rec_AR[,3] <- 0
	raw_rec_AR_parent <- as.data.frame(read.csv(raw_rec_AR_parent_stat, header=FALSE))
	raw_rec_AR_parent[,3] <- 1
	raw_degree <- rbind(raw_rec_AR, raw_rec_AR_parent)
	colnames(raw_degree) <- c('degree', 'count', 'degree_type')
	raw_degree <- raw_degree[raw_degree$degree > 0, ]
	raw_degree.df <- ddply(raw_degree, c('degree_type'), function(one_partition){
				one_partition = one_partition[order(one_partition$degree),]
				one_partition$cum_count = cumsum(one_partition$count)
				one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
				one_partition$pdf_val = one_partition$count / max(one_partition$cum_count)
				one_partition
			})
	raw_degree.df$degree_type <- factor(raw_degree.df$degree_type)
	plot <- ggplot(raw_degree.df[raw_degree.df$degree > 0, ], aes(x = degree, y = pdf_val)) +
			geom_point(aes(group = degree_type, colour = degree_type, shape = degree_type), size=1) +
			scale_x_log10(limits = c(1, 10^4))
	plot <- change_plot_attributes(plot, "", 0:1, c('K = Number of received invitations', 'K = Number of distinct inviters'),
			"K", "Empirical PDF")
	save_ggplot(plot, 'raw_stat_v2/recved_AR.pdf')
	plot <- ggplot(raw_degree.df[raw_degree.df$degree > 0, ], aes(x = degree, y = cdf_val)) +
			geom_point(aes(group = degree_type, colour = degree_type, shape = degree_type), size=1) +
			scale_x_log10(limits = c(1, 10^4))
	plot <- change_plot_attributes(plot, "", 0:1, c('K = Number of received invitations', 'K = Number of distinct inviters'),
			"K", "Empirical CDF")
	save_ggplot(plot, 'raw_stat_v2/recved_AR_cdf.pdf', 24,
			opts(legend.position=c(.62, .3)))
	adoption_delay <- as.data.frame(read.csv(adoption_delay_file_name, header=FALSE))
	colnames(adoption_delay) <- c('time', 'count', 'avg_invitations', 'avg_inviters')
	adoption_delay <- adoption_delay[adoption_delay$time < 24855,]
	adoption_delay.invitations <- adoption_delay[,c(1,3)]
	colnames(adoption_delay.invitations) <- c('time', 'received')
	adoption_delay.invitations$degree_type <- 0
	adoption_delay.inviters <- adoption_delay[,c(1,4)]
	colnames(adoption_delay.inviters) <- c('time', 'received')
	adoption_delay.inviters$degree_type <- 1
	adoption_delay.df <- rbind(adoption_delay.invitations, adoption_delay.inviters)
	adoption_delay.df$degree_type <- factor(adoption_delay.df$degree_type)
	plot <- ggplot(adoption_delay.df, aes(x = time, y = received)) +
			geom_point(aes(group = degree_type, colour = degree_type, shape = degree_type), size=1) +
			scale_y_log10()
	plot <- change_plot_attributes(plot, "", 0:1, c('K = Avg. number of received invitations', 'K = Avg. number of distinct inviters'),
			"Adoption delay (Days)", "K")
	save_ggplot(plot, 'raw_stat_v2/delay_recved_AR.pdf', 24,
			opts(legend.position=c(.43, .8)))	
}

raw_outdeg_analysis <- function (raw_sent_AR_stat = 'raw_stat_wo_burst/raw_sent_AR_stat.csv',
		raw_sent_AR_children_stat = 'raw_stat_wo_burst/raw_sent_AR_children_stat.csv',
		raw_rec_AR_stat='raw_stat_wo_burst/raw_rec_AR_stat.csv',
		raw_rec_AR_parent_stat = 'raw_stat_wo_burst/raw_rec_AR_parent_stat.csv'){
	raw_sent_AR <- as.data.frame(read.csv(raw_sent_AR_stat, header=FALSE))
	raw_sent_AR[,3] <- 0
	raw_sent_AR_children <- as.data.frame(read.csv(raw_sent_AR_children_stat, header=FALSE))
	raw_sent_AR_children[,3] <- 1
	raw_rec_AR <- as.data.frame(read.csv(raw_rec_AR_stat, header=FALSE))
	raw_rec_AR[,3] <- 2
	raw_rec_AR_parent <- as.data.frame(read.csv(raw_rec_AR_parent_stat, header=FALSE))
	raw_rec_AR_parent[,3] <- 3
	raw_degree <- rbind(raw_sent_AR, raw_sent_AR_children, raw_rec_AR, raw_rec_AR_parent)
	colnames(raw_degree) <- c('degree', 'count', 'degree_type')
	raw_degree <- raw_degree[raw_degree$degree > 0, ]
	raw_degree.df <- ddply(raw_degree, c('degree_type'), function(one_partition){
				one_partition = one_partition[order(one_partition$degree),]
				one_partition$cum_count = cumsum(one_partition$count)
				one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
				one_partition$pdf_val = one_partition$count / max(one_partition$cum_count)
				one_partition
			})
	raw_degree.df$degree_type <- factor(raw_degree.df$degree_type)
	print(summary(raw_degree.df))
	plot <- ggplot(raw_degree.df[raw_degree.df$degree > 0, ], aes(x = degree, y = pdf_val)) +
			geom_point(aes(group = degree_type, colour = degree_type, shape = degree_type), size=1) +
			scale_y_log10() +
			scale_x_log10(limits = c(1, 10^4))
	plot <- change_plot_attributes_fancy(plot, "", 0:3, c('Sent ARs', 'Distinct invitee',
					'Received ARs', 'Disctinct inviter'), "Number of inviters/invitees", "Empirical PDF")
	save_ggplot(plot, 'raw_stat_v2/degree.pdf')
	plot <- ggplot(raw_degree.df[raw_degree.df$degree > 0, ], aes(x = degree, y = cdf_val)) +
			geom_line(aes(group = degree_type, colour = degree_type, shape = degree_type), size=1) +
#			scale_y_log10() +
			scale_y_continuous(breaks=c(0,.2,.4,.6,.8,1.0)) + scale_x_log10() #limits = c(1, 10^4)
	plot <- change_plot_attributes_fancy(plot, "", 0:3, c('K=Sent ARs by an inviter', 'K=Distinct invitee of an inviter',
					'K=Received ARs by an invitee', 'K=Disctinct inviter of an invitee'), "K", "Empirical CDF of K")
	save_ggplot(plot, 'raw_stat_v2/degree_cdf.pdf', 24,
			opts(legend.position=c(.66, .5)))
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
	plot <- change_plot_attributes(plot, "", 0:1, c('K = Number of distinct inviters', 'K = Number of invitations'),
			"K", "Activation probability")
	save_ggplot(plot, 'raw_stat_v2/adop_prob.pdf', 24,
			opts(axis.title.x = theme_text(vjust=-0.5), legend.position=c(.4, .7)))
}

success_ratio_analysis <- function (success_ratio_file='raw_stat_v2/act_proportion_count.csv'){
	influence <- as.data.frame(read.csv(success_ratio_file, header=FALSE))
	colnames(influence) <- c('proportion', 'count')
	influence$proportion <- influence$proportion / 100
	influence <- influence[order(influence$proportion),]
	influence$cum_count <- cumsum(influence$count)
	influence$cdf_val <- influence$cum_count / max(influence$cum_count)
	plot <- ggplot(influence, aes(x = (proportion), y = (cdf_val))) + 
			geom_line() + xlab('Success ratio') + ylab('Empirical CDF')
	save_ggplot(plot, 'raw_stat_v2/success_ratio.pdf')
}

success_ratio_vs_act_life <- function (success_ratio_file='raw_stat_v2/act_life_vs_avg_succ_ratio.csv'){
	success_life <- as.data.frame(read.csv(success_ratio_file, header=FALSE))
	colnames(success_life) <- c('day', 'proportion')
	plot <- ggplot(success_life, aes(x = (proportion), y = (day))) + 
			geom_point() + xlab('Avg. success Ratio (%)') + ylab('Active lifetime (Days)')
	save_ggplot(plot, 'raw_stat_v2/success_ratio_vs_act_life.pdf')
}

sender_success_ratio_analysis <- function (success_ratio_file='raw_stat_v2/parent_children_act.csv'){
	sender_success <- as.data.frame(read.csv(success_ratio_file, header=FALSE))
	colnames(sender_success) <- c('active_children', 'children')
	sender_success$ratio <- round(sender_success$active_children/sender_success$children, 3)
	sender_success.ratio <- as.data.frame(table(sender_success$ratio))
	colnames(sender_success.ratio) <- c('proportion', 'count')
	sender_success.ratio$proportion <- as.numeric(levels(sender_success.ratio$proportion))[sender_success.ratio$proportion]
	print(summary(sender_success.ratio))
	sender_success.ratio <- sender_success.ratio[order(sender_success.ratio$proportion),]
	sender_success.ratio$cum_count <- cumsum(sender_success.ratio$count)
	sender_success.ratio$cdf_val <- sender_success.ratio$cum_count / max(sender_success.ratio$cum_count)
	plot <- ggplot(sender_success.ratio, aes(x = (proportion), y = (cdf_val))) + 
			geom_line() + xlab(expression(paste('Success ratio (',tau,')'))) + ylab('Empirical CDF')
	save_ggplot(plot, 'raw_stat_v2/success_ratio.pdf')
	sender_success.avg_ratio <- ddply(sender_success, c('children'), summarise, avg_success_ratio = mean (ratio))
	plot <- ggplot(sender_success.avg_ratio, aes(x = (avg_success_ratio), y = (children))) + 
			geom_point() + #geom_smooth(method=lm, se=FALSE) +  # Add linear regression lines
			xlab(expression(paste('Average Success ratio (',tau,')'))) + ylab('Number of sent invitations')
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

gift_popularity_analysis <- function (file='raw_stat_v2/iheart_node_gift_type_stat.txt'){
	gift_stat <- as.data.frame(read.csv(file, header=FALSE))
	colnames(gift_stat) <- c('hid', 'count')
	gift_stat <- gift_stat[order(gift_stat$hid),]
	gift_stat$cum_count <- cumsum(gift_stat$count)
	gift_stat$cdf_val <- gift_stat$cum_count / max(gift_stat$cum_count)
	gift_stat$pdf_val <- gift_stat$count / max(gift_stat$cum_count)
	gift_stat$hid <- factor(gift_stat$hid)
	plot <- ggplot(gift_stat, aes(x = (hid), y = (count))) + geom_bar(stat = "identity") + scale_y_log10()+
			 xlab('Gift ids') + ylab('Count') + scale_x_discrete(breaks=factor(seq(0,450,50)), labels=seq(0,450,50))
	save_ggplot(plot, 'raw_stat_v2/gift_popularity.pdf')
}

#parent_lifespan<-lifespan_analysis('raw_stat_1/lifespan_stat.csv')
#raw_outdeg_analysis('raw_stat_v2/raw_outdeg_stat.csv')
#influence_threshold_analysis('raw_stat_v2/parent_count_before_act.csv', 'raw_stat_v2/indeg_before_act.csv')
#active_proportion_analysis('raw_stat_v2/act_proportion_count.csv')
