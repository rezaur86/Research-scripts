source('~/scripts/Cascade/tools.r')
library(plyr)
require(scales)

total_activities <- function (iheart_activities = 'iheart_cascade/activities_stat.txt',
		hugged_activities = 'raw_stat_hugged/activities_stat.txt',
		ismile_activities = 'raw_stat_ismile/activities_stat.txt'){
	hugged <- as.data.frame(read.csv(hugged_activities, header=FALSE))
	hugged[,6] <- 0
	colnames(hugged) <- c('year', 'week', 'day', 'hour', 'count', 'activity_type')
	iheart <- as.data.frame(read.csv(iheart_activities, header=FALSE))
	iheart[,6] <- 1
	colnames(iheart) <- c('year', 'week', 'day', 'hour', 'count', 'activity_type')
	ismile <- as.data.frame(read.csv(ismile_activities, header=FALSE))
	ismile[,6] <- 2
	colnames(ismile) <- c('year', 'week', 'day', 'hour', 'count', 'activity_type')
	activities <- rbind(hugged, iheart, ismile)
	activities.daily <- ddply(activities, c('activity_type', 'year', 'week', 'day'), summarise, daily_count = sum (count))
	print(tail(activities.daily))
	activities.df <- ddply(activities.daily, c('activity_type'), function(one_partition){
				one_partition$date = do.call(paste, c(as.data.frame(cbind(one_partition$year, one_partition$week, one_partition$day)), sep="-"))
				one_partition$week = do.call(paste, c(as.data.frame(cbind(one_partition$year, one_partition$week)), sep="-"))
				one_partition$rtime = as.POSIXct(strptime(one_partition$date, format = "%Y-%U-%w"))
				one_partition = one_partition[order(one_partition$rtime),]
				one_partition$cum_count = cumsum(one_partition$daily_count)
				one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
				one_partition$pdf_val = one_partition$daily_count / max(one_partition$cum_count)
				one_partition
			})
	print(head(activities.df))
	activities.df$activity_type <- factor(activities.df$activity_type)
	plot <- ggplot(activities.df, aes(x = (rtime), y = (daily_count))) +
			geom_line(aes(group = activity_type, linetype = activity_type, colour = activity_type), size=1) +
			scale_y_log10() + #limits = c(10^4, 10^8)
			scale_x_datetime(breaks = date_breaks("1 months"),
					labels = date_format("%y-%b"), limits=c(as.POSIXct('2008/02/01'), as.POSIXct('2010/10/31')))+
			scale_linetype_manual(values=c(1,1,6), name='', breaks=0:2,
					labels=c('Hugged', 'iHeart', 'iSmile')) +
			scale_colour_manual(values=c("black", "gray55", "black"), name='', breaks=0:2,
					labels=c('Hugged', 'iHeart', 'iSmile'))+
			xlab('Months') + ylab('Count')
	save_ggplot(plot, 'raw_stat_apps/all_activities.pdf', 10,
			opts(axis.text.x = element_text(angle = 90), legend.position=c(.2, .9)))
}

born_activities <- function (iheart_borns = 'raw_stat_v2/daily_born.csv',
		hugged_borns = 'raw_stat_hugged/daily_born.csv',
		ismile_borns = 'raw_stat_ismile/daily_born.csv'){
	hugged <- as.data.frame(read.csv(hugged_borns, header=FALSE))
	hugged[,6] <- 0
	colnames(hugged) <- c('year', 'week', 'day', 'hour', 'count', 'activity_type')
	iheart <- as.data.frame(read.csv(iheart_borns, header=FALSE))
	iheart[,6] <- 1
	colnames(iheart) <- c('year', 'week', 'day', 'hour', 'count', 'activity_type')
	ismile <- as.data.frame(read.csv(ismile_borns, header=FALSE))
	ismile[,6] <- 2
	colnames(ismile) <- c('year', 'week', 'day', 'hour', 'count', 'activity_type')
	activities <- rbind(hugged, iheart, ismile)
	activities.daily <- ddply(activities, c('activity_type', 'year', 'week', 'day'), summarise, daily_count = sum (count))
	print(tail(activities.daily))
	activities.df <- ddply(activities.daily, c('activity_type'), function(one_partition){
				one_partition$date = do.call(paste, c(as.data.frame(cbind(one_partition$year, one_partition$week, one_partition$day)), sep="-"))
				one_partition$week = do.call(paste, c(as.data.frame(cbind(one_partition$year, one_partition$week)), sep="-"))
				one_partition$rtime = as.POSIXct(strptime(one_partition$date, format = "%Y-%U-%w"))
				one_partition = one_partition[order(one_partition$rtime),]
				one_partition$cum_count = cumsum(one_partition$daily_count)
				one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
				one_partition$pdf_val = one_partition$daily_count / max(one_partition$cum_count)
				one_partition
			})
	print(head(activities.df))
	activities.df$activity_type <- factor(activities.df$activity_type)
	plot <- ggplot(activities.df, aes(x = (rtime), y = (daily_count))) +
			geom_line(aes(group = activity_type, linetype = activity_type, colour = activity_type), size=1) +
			scale_y_log10() +
			scale_x_datetime(breaks = date_breaks("1 months"),
					labels = date_format("%y-%b"), limits=c(as.POSIXct('2008/02/01'), as.POSIXct('2010/10/31')))+
			scale_linetype_manual(values=c(1,1,6), name='', breaks=0:2,
					labels=c('Hugged', 'iHeart', 'iSmile')) +
			scale_colour_manual(values=c("black", "gray55", "black"), name='', breaks=0:2,
					labels=c('Hugged', 'iHeart', 'iSmile'))+
			xlab('Months') + ylab('Count')
	save_ggplot(plot, 'raw_stat_apps/born.pdf', 24,
			opts(axis.text.x = element_text(angle = 90, hjust = 0), legend.position=c(.2, .9)), 10)
}

activations <- function (iheart_borns = 'raw_stat_v2/daily_activation.csv',
		hugged_borns = 'raw_stat_hugged/daily_activation.csv',
		ismile_borns = 'raw_stat_ismile/daily_activation.csv'){
	hugged <- as.data.frame(read.csv(hugged_borns, header=FALSE))
	hugged[,6] <- 0
	colnames(hugged) <- c('year', 'week', 'day', 'hour', 'count', 'activity_type')
	iheart <- as.data.frame(read.csv(iheart_borns, header=FALSE))
	iheart[,6] <- 1
	colnames(iheart) <- c('year', 'week', 'day', 'hour', 'count', 'activity_type')
	ismile <- as.data.frame(read.csv(ismile_borns, header=FALSE))
	ismile[,6] <- 2
	colnames(ismile) <- c('year', 'week', 'day', 'hour', 'count', 'activity_type')
	activities <- rbind(hugged, iheart, ismile)
	activities.daily <- ddply(activities, c('activity_type', 'year', 'week', 'day'), summarise, daily_count = sum (count))
	print(tail(activities.daily))
	activities.df <- ddply(activities.daily, c('activity_type'), function(one_partition){
				one_partition$date = do.call(paste, c(as.data.frame(cbind(one_partition$year, one_partition$week, one_partition$day)), sep="-"))
				one_partition$week = do.call(paste, c(as.data.frame(cbind(one_partition$year, one_partition$week)), sep="-"))
				one_partition$rtime = as.POSIXct(strptime(one_partition$date, format = "%Y-%U-%w"))
				one_partition = one_partition[order(one_partition$rtime),]
				one_partition$cum_count = cumsum(one_partition$daily_count)
				one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
				one_partition$pdf_val = one_partition$daily_count / max(one_partition$cum_count)
				one_partition
			})
	print(head(activities.df))
	activities.df$activity_type <- factor(activities.df$activity_type)
	plot <- ggplot(activities.df, aes(x = (rtime), y = (daily_count))) +
			geom_line(aes(group = activity_type, linetype = activity_type, colour = activity_type), size=1) +
			scale_y_log10() +
			scale_x_datetime(breaks = date_breaks("1 months"),
					labels = date_format("%y-%b"), limits=c(as.POSIXct('2008/02/01'), as.POSIXct('2010/10/31')))+
			scale_linetype_manual(values=c(1,1,6), name='', breaks=0:2,
					labels=c('Hugged', 'iHeart', 'iSmile')) +
			scale_colour_manual(values=c("black", "gray55", "black"), name='', breaks=0:2,
					labels=c('Hugged', 'iHeart', 'iSmile'))+
			xlab('Months') + ylab('Count')
	save_ggplot(plot, 'raw_stat_apps/activations.pdf', 24,
			opts(axis.text.x = element_text(angle = 90, hjust = 0), legend.position=c(.2, .9)), 10)
}

apps_sent_AR_analysis <- function (iheart_sent_AR_stat = 'raw_stat_wo_burst/raw_sent_AR_stat.csv',
		hugged_sent_AR_stat = 'raw_stat_hugged/raw_sent_AR_stat.csv',
		ismile_sent_AR_stat = 'raw_stat_ismile/raw_sent_AR_stat.csv',
		iheart_act_lifespan_file='raw_stat_wo_burst/act_lifespan_sent_AR_stat.csv',
		hugged_act_lifespan_file='raw_stat_hugged/act_lifespan_sent_AR_stat.csv',
		ismile_act_lifespan_file='raw_stat_ismile/act_lifespan_sent_AR_stat.csv'){
	hugged_raw_sent_AR <- as.data.frame(read.csv(hugged_sent_AR_stat, header=FALSE))
	hugged_raw_sent_AR[,3] <- 0
	iheart_raw_sent_AR <- as.data.frame(read.csv(iheart_sent_AR_stat, header=FALSE))
	iheart_raw_sent_AR[,3] <- 1
	ismile_raw_sent_AR <- as.data.frame(read.csv(ismile_sent_AR_stat, header=FALSE))
	ismile_raw_sent_AR[,3] <- 2
	raw_degree <- rbind(hugged_raw_sent_AR, iheart_raw_sent_AR, ismile_raw_sent_AR)
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
			geom_point(aes(group = degree_type, colour = degree_type, shape = degree_type), size=3) +
			scale_x_log10(limits = c(1, 10^4))
	plot <- change_plot_attributes(plot, "", 0:2, c('Hugged', 'iHeart', 'iSmile'),
			"Sent invitations", "Empirical PDF")
	save_ggplot(plot, 'raw_stat_apps/sent_AR.pdf')
	plot <- ggplot(raw_degree.df[raw_degree.df$degree > 0, ], aes(x = degree, y = cdf_val)) +
			geom_point(aes(group = degree_type, colour = degree_type, shape = degree_type), size=3) +
			scale_x_log10(limits = c(1, 10^4), breaks=c(10, 100, 1000))
	plot <- change_plot_attributes(plot, "", 0:2, c('Hugged', 'iHeart', 'iSmile'),
			"Sent invitations", "Empirical CDF")
	save_ggplot(plot, 'raw_stat_apps/sent_AR_cdf.pdf', 24,
			opts(legend.position=c(.62, .3)))
	hugged_act_lifespan <- as.data.frame(read.csv(hugged_act_lifespan_file, header=FALSE))
	colnames(hugged_act_lifespan) <- c('time', 'count', 'avg_invitations', 'avg_invitees')
	hugged_act_lifespan <- hugged_act_lifespan[hugged_act_lifespan$time < 24855,]
	hugged_act_lifespan$degree_type <- 0
	iheart_act_lifespan <- as.data.frame(read.csv(iheart_act_lifespan_file, header=FALSE))
	colnames(iheart_act_lifespan) <- c('time', 'count', 'avg_invitations', 'avg_invitees')
	iheart_act_lifespan <- iheart_act_lifespan[iheart_act_lifespan$time < 24855,]
	iheart_act_lifespan$degree_type <- 1
	ismile_act_lifespan <- as.data.frame(read.csv(ismile_act_lifespan_file, header=FALSE))
	colnames(ismile_act_lifespan) <- c('time', 'count', 'avg_invitations', 'avg_invitees')
	ismile_act_lifespan <- ismile_act_lifespan[ismile_act_lifespan$time < 24855,]
	ismile_act_lifespan$degree_type <- 2
	act_lifespan <- rbind(hugged_act_lifespan, iheart_act_lifespan, ismile_act_lifespan)
	act_lifespan$degree_type <- factor(act_lifespan$degree_type)
	plot <- ggplot(act_lifespan, aes(x = time, y = avg_invitations)) +
			geom_point(aes(group = degree_type, colour = degree_type, shape = degree_type), size=3) +
			scale_y_log10()
	plot <- change_plot_attributes(plot, "", 0:2, c('Hugged', 'iHeart', 'iSmile'),
			"Active lifespan (Days)", "Average number of sent invitations")
	save_ggplot(plot, 'raw_stat_apps/act_sent_AR.pdf', 24,
			opts(legend.position=c(.2, .8)))

	act_lifespan.df <- ddply(act_lifespan, c('degree_type'), function(one_partition){
				one_partition = one_partition[order(one_partition$time),]
				one_partition$cum_count = cumsum(one_partition$count)
				one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
				one_partition$pdf_val = one_partition$count / max(one_partition$cum_count)
				one_partition
			})
	plot <- ggplot(act_lifespan.df, aes(x = time, y = cdf_val)) + 
			geom_line(aes(group = degree_type, linetype = degree_type, colour = degree_type), size=1)+
			scale_linetype_manual(values=c(1,1,6), name='', breaks=0:2,
					labels=c('Hugged', 'iHeart', 'iSmile')) +
			scale_colour_manual(values=c("black", "gray55", "black"), name='', breaks=0:2,
					labels=c('Hugged', 'iHeart', 'iSmile'))+
			xlab("Active lifespan (Days)") + ylab("Empirical CDF")
	save_ggplot(plot, 'raw_stat_apps/active_lifetime.pdf', 24,
			opts(legend.position=c(.8, .2)))
	return(act_lifespan.df)
}


apps_recved_AR_analysis <- function (iheart_sent_AR_stat = 'raw_stat_wo_burst/raw_rec_AR_stat.csv',
		hugged_sent_AR_stat = 'raw_stat_hugged/raw_rec_AR_stat.csv',
		ismile_sent_AR_stat = 'raw_stat_ismile/raw_rec_AR_stat.csv',
		iheart_act_lifespan_file='raw_stat_wo_burst/delay_recv_AR_stat.csv',
		hugged_act_lifespan_file='raw_stat_hugged/delay_recv_AR_stat.csv',
		ismile_act_lifespan_file='raw_stat_ismile/delay_recv_AR_stat.csv'){
	hugged_raw_sent_AR <- as.data.frame(read.csv(hugged_sent_AR_stat, header=FALSE))
	hugged_raw_sent_AR[,3] <- 0
	iheart_raw_sent_AR <- as.data.frame(read.csv(iheart_sent_AR_stat, header=FALSE))
	iheart_raw_sent_AR[,3] <- 1
	ismile_raw_sent_AR <- as.data.frame(read.csv(ismile_sent_AR_stat, header=FALSE))
	ismile_raw_sent_AR[,3] <- 2
	raw_degree <- rbind(hugged_raw_sent_AR, iheart_raw_sent_AR, ismile_raw_sent_AR)
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
			geom_point(aes(group = degree_type, colour = degree_type, shape = degree_type), size=3) +
			scale_x_log10(limits = c(1, 10^4))
	plot <- change_plot_attributes(plot, "", 0:2, c('Hugged', 'iHeart', 'iSmile'),
			"Sent invitations", "Empirical PDF")
	save_ggplot(plot, 'raw_stat_apps/recved_AR.pdf')
	plot <- ggplot(raw_degree.df[raw_degree.df$degree > 0, ], aes(x = degree, y = cdf_val)) +
			geom_point(aes(group = degree_type, colour = degree_type, shape = degree_type), size=3) +
			scale_x_log10(limits = c(1, 10^4), breaks=c(10, 100, 1000))
	plot <- change_plot_attributes(plot, "", 0:2, c('Hugged', 'iHeart', 'iSmile'),
			"Sent invitations", "Empirical CDF")
	save_ggplot(plot, 'raw_stat_apps/recved_AR_cdf.pdf', 24,
			opts(legend.position=c(.62, .3)))
	
	hugged_act_lifespan <- as.data.frame(read.csv(hugged_act_lifespan_file, header=FALSE))
	colnames(hugged_act_lifespan) <- c('time', 'count', 'avg_invitations', 'avg_invitees')
	hugged_act_lifespan <- hugged_act_lifespan[hugged_act_lifespan$time < 24855,]
	hugged_act_lifespan$degree_type <- 0
	iheart_act_lifespan <- as.data.frame(read.csv(iheart_act_lifespan_file, header=FALSE))
	colnames(iheart_act_lifespan) <- c('time', 'count', 'avg_invitations', 'avg_invitees')
	iheart_act_lifespan <- iheart_act_lifespan[iheart_act_lifespan$time < 24855,]
	iheart_act_lifespan$degree_type <- 1
	ismile_act_lifespan <- as.data.frame(read.csv(ismile_act_lifespan_file, header=FALSE))
	colnames(ismile_act_lifespan) <- c('time', 'count', 'avg_invitations', 'avg_invitees')
	ismile_act_lifespan <- ismile_act_lifespan[ismile_act_lifespan$time < 24855,]
	ismile_act_lifespan$degree_type <- 2
	act_lifespan <- rbind(hugged_act_lifespan, iheart_act_lifespan, ismile_act_lifespan)
	act_lifespan$degree_type <- factor(act_lifespan$degree_type)
	plot <- ggplot(act_lifespan, aes(x = time, y = avg_invitations)) +
			geom_point(aes(group = degree_type, colour = degree_type, shape = degree_type), size=3) +
			scale_y_log10()
	plot <- change_plot_attributes(plot, "", 0:2, c('Hugged', 'iHeart', 'iSmile'),
			"Adoption delay (Days)", "Average number of received invitations")
	save_ggplot(plot, 'raw_stat_apps/delay_recved_AR.pdf', 24,
			opts(legend.position=c(.2, .8)))

	act_lifespan.df <- ddply(act_lifespan, c('degree_type'), function(one_partition){
				one_partition = one_partition[order(one_partition$time),]
				one_partition$cum_count = cumsum(one_partition$count)
				one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
				one_partition$pdf_val = one_partition$count / max(one_partition$cum_count)
				one_partition
			})
	plot <- ggplot(act_lifespan.df, aes(x = time, y = cdf_val)) + 
			geom_line(aes(group = degree_type, colour = degree_type, linetype = degree_type), size=1)+
			scale_linetype_manual(values=c(1,1,6), name='', breaks=0:2,
					labels=c('Hugged', 'iHeart', 'iSmile')) +
			scale_colour_manual(values=c("black", "gray55", "black"), name='', breaks=0:2,
					labels=c('Hugged', 'iHeart', 'iSmile'))+
			xlab("Adoption delay (Days)") + ylab("Empirical CDF")
	save_ggplot(plot, 'raw_stat_apps/adoption_delay.pdf', 24,
			opts(legend.position=c(.8, .2)))
}

apps_success_ratio_analysis <- function (iheart_success_ratio_file='raw_stat_v2/parent_children_act.csv',
		hugged_success_ratio_file='raw_stat_hugged/parent_children_act.csv',
		ismile_success_ratio_file='raw_stat_ismile/parent_children_act.csv'){
	hugged_sender_success <- as.data.frame(read.csv(hugged_success_ratio_file, header=FALSE))
	colnames(hugged_sender_success) <- c('active_children', 'children')
	hugged_sender_success$ratio <- round(hugged_sender_success$active_children/hugged_sender_success$children, 3)
	hugged_sender_success.ratio <- as.data.frame(table(hugged_sender_success$ratio))
	colnames(hugged_sender_success.ratio) <- c('proportion', 'count')
	hugged_sender_success.ratio$proportion <- as.numeric(levels(hugged_sender_success.ratio$proportion))[hugged_sender_success.ratio$proportion]
	hugged_sender_success.ratio$app_type <- 0
	iheart_sender_success <- as.data.frame(read.csv(iheart_success_ratio_file, header=FALSE))
	colnames(iheart_sender_success) <- c('active_children', 'children')
	iheart_sender_success$ratio <- round(iheart_sender_success$active_children/iheart_sender_success$children, 3)
	iheart_sender_success.ratio <- as.data.frame(table(iheart_sender_success$ratio))
	colnames(iheart_sender_success.ratio) <- c('proportion', 'count')
	iheart_sender_success.ratio$proportion <- as.numeric(levels(iheart_sender_success.ratio$proportion))[iheart_sender_success.ratio$proportion]
	iheart_sender_success.ratio$app_type <- 1
	ismile_sender_success <- as.data.frame(read.csv(ismile_success_ratio_file, header=FALSE))
	colnames(ismile_sender_success) <- c('active_children', 'children')
	ismile_sender_success$ratio <- round(ismile_sender_success$active_children/ismile_sender_success$children, 3)
	ismile_sender_success.ratio <- as.data.frame(table(ismile_sender_success$ratio))
	colnames(ismile_sender_success.ratio) <- c('proportion', 'count')
	ismile_sender_success.ratio$proportion <- as.numeric(levels(ismile_sender_success.ratio$proportion))[ismile_sender_success.ratio$proportion]
	ismile_sender_success.ratio$app_type <- 2
	sender_success <- rbind(hugged_sender_success.ratio, iheart_sender_success.ratio, ismile_sender_success.ratio)
	sender_success$app_type <- factor(sender_success$app_type)
	sender_success.df <- ddply(sender_success, c('app_type'), function(one_partition){
				one_partition = one_partition[order(one_partition$proportion),]
				one_partition$cum_count = cumsum(one_partition$count)
				one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
				one_partition$pdf_val = one_partition$count / max(one_partition$cum_count)
				one_partition
			})
	plot <- ggplot(sender_success.df, aes(x = (proportion), y = (cdf_val))) + 
			geom_line(aes(group = app_type, colour = app_type, linetype = app_type), size=1)+
			scale_linetype_manual(values=c(1,1,6), name='', breaks=0:2,
					labels=c('Hugged', 'iHeart', 'iSmile')) +
			scale_colour_manual(values=c("black", "gray55", "black"), name='', breaks=0:2,
					labels=c('Hugged', 'iHeart', 'iSmile')) +
			xlab(expression(paste('Success ratio (',tau,')'))) + ylab('Empirical CDF')
	save_ggplot(plot, 'raw_stat_apps/success_ratio.pdf', 24,
			opts(legend.position=c(.8, .2)))
}

apps_act_child_analysis <- function(comb_act_child_file = '/home/rezaur/output_cascade/raw_stat_apps/comb_act_child.csv'){
	comb_act_child <<- as.data.frame(read.csv(comb_act_child_file, header=FALSE))
	colnames(comb_act_child) <<-  c('seq', 'hugged', 'iheart', 'ismile')
	hug <- as.data.frame(table(comb_act_child[comb_act_child$hugged > -1,]$hugged))
	colnames(hug) <- c('ac', 'count')
	hug$ac <- as.numeric(levels(hug$ac))[hug$ac]
	hug$app_type <- 0
	ihe <- as.data.frame(table(comb_act_child[comb_act_child$iheart > -1,]$iheart))
	colnames(ihe) <- c('ac', 'count')
	ihe$ac <- as.numeric(levels(ihe$ac))[ihe$ac]
	ihe$app_type <- 1
	ism <- as.data.frame(table(comb_act_child[comb_act_child$ismile > -1,]$ismile))
	colnames(ism) <- c('ac', 'count')
	ism$ac <- as.numeric(levels(ism$ac))[ism$ac]
	ism$app_type <- 2
	comb <- rbind(hug, ihe, ism)
	comb.df <- ddply(comb, c('app_type'), function(one_partition){
				one_partition = one_partition[order(one_partition$ac),]
				one_partition$cum_count = cumsum(one_partition$count)
				one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
				one_partition$pdf_val = one_partition$count / max(one_partition$cum_count)
				one_partition
			})
	comb.df$app_type <- factor(comb.df$app_type)
	print(summary(comb.df))
	plot <- ggplot(comb.df[comb.df$ac > 0, ], aes(x = (ac), y = (cdf_val))) + 
			geom_line(aes(group = app_type, colour = app_type, linetype = app_type), size=1)+
			scale_linetype_manual(values=c(1,1,6), name='', breaks=0:2,
					labels=c('Hugged', 'iHeart', 'iSmile')) +
			scale_colour_manual(values=c("black", "gray55", "black"), name='', breaks=0:2,
					labels=c('Hugged', 'iHeart', 'iSmile')) +
			scale_x_log10(limits = c(1, 10^3), breaks=c(10, 100, 1000))+
			xlab('Active users') + ylab('Empirical CDF')
	save_ggplot(plot, 'raw_stat_apps/act_users_cdf.pdf', 24,
			opts(legend.position=c(.8, .2)))
}

pairwise_feature_analysis <- function(roles, x_label, y_label, figure_name){
	roles.box <- ddply(roles, c('rank.x'), .drop=TRUE,
			.fun = function(one_partition){
				stats = boxplot.stats(one_partition$distance)$stats
				c(ymin=stats[1],
						lower=stats[2],
						middle=stats[3],
						upper=stats[4],
						ymax=stats[5],
						mean = mean(one_partition$distance))
			})
	roles.box$rank.x <- factor(roles.box$rank.x)
	plot <- ggplot(roles.box, aes(x=rank.x, lower=lower, upper=upper, middle=middle, ymin=ymin, ymax=ymax)) + 
			geom_boxplot(stat="identity") + geom_point(data = roles.box, aes(x=rank.x, y=mean), shape = 8, size = 3)+
			xlab(x_label) + ylab(y_label)
	save_ggplot(plot, paste(c('raw_stat_apps/', figure_name, '.pdf'), collapse = ''))
}

class_distance_across_apps <- function(df, figure_name){
	hug <- as.data.frame(df[order(-df$hugged, na.last = NA),]$seq)
	colnames(hug) <- c('seq')
	hugged_users <- nrow(hug)
	ihe <- as.data.frame(df[order(-df$iheart, na.last = NA),]$seq)
	colnames(ihe) <- c('seq')
	iheart_users <- nrow(ihe)
	ism <- as.data.frame(df[order(-df$ismile, na.last = NA),]$seq)
	colnames(ism) <- c('seq')
	ismile_users <- nrow(ism)
	hug$rank <- c(rep(1, ceiling(hugged_users/4)), rep(2, ceiling(hugged_users/4)),
			rep(3, ceiling(hugged_users/4)), rep(4, (hugged_users - 3*ceiling(hugged_users/4))))
	
	ihe$rank <- c(rep(1, ceiling(iheart_users/4)), rep(2, ceiling(iheart_users/4)),
			rep(3, ceiling(iheart_users/4)), rep(4, (iheart_users - 3*ceiling(iheart_users/4))))
	
	ism$rank <- c(rep(1, ceiling(ismile_users/4)), rep(2, ceiling(ismile_users/4)),
			rep(3, ceiling(ismile_users/4)), rep(4, (ismile_users - 3*ceiling(ismile_users/4))))
	
	hug_ihe <- merge(hug, ihe, by="seq")
	hug_ihe$distance <- abs (hug_ihe$rank.x - hug_ihe$rank.y)
	hug_ihe$comp_type <- 'Hugged vs.\niHeart'
	
	hug_ism <- merge(hug, ism, by="seq")
	hug_ism$distance <- abs (hug_ism$rank.x - hug_ism$rank.y)
	hug_ism$comp_type <- 'Hugged vs.\niSmile'
	
	ihe_ism <- merge(ihe, ism, by="seq")
	ihe_ism$distance <- abs (ihe_ism$rank.x - ihe_ism$rank.y)
	ihe_ism$comp_type <- 'iHeart vs.\niSmile'
	
	roles <- rbind(hug_ihe, hug_ism, ihe_ism)
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
			xlab('Class') + ylab('Distance')
	save_ggplot(plot, paste(c('raw_stat_apps/cross_app_', figure_name, '.pdf'), collapse = ''), 24, opts(legend.position="bottom"))
	
	temp.box <- roles.box
	sim_hug_ihe <- as.data.frame(hug_ihe$distance)
	colnames(sim_hug_ihe) <- 'distance'
	sim_hug_ihe$comp_type <- 'Hugged vs.\niHeart'
	sim_hug_ism <- as.data.frame(hug_ism$distance)
	colnames(sim_hug_ism) <- 'distance'
	sim_hug_ism$comp_type <- 'Hugged vs.\niSmile'
	sim_ihe_ism <- as.data.frame(ihe_ism$distance)
	colnames(sim_ihe_ism) <- 'distance'
	sim_ihe_ism$comp_type <- 'iHeart vs.\niSmile'
	roles <- rbind(sim_hug_ihe, sim_hug_ism, sim_ihe_ism)
	
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
			xlab('') + ylab('Distance') 
	save_ggplot(plot, paste(c('raw_stat_apps/similarity_', figure_name, '.pdf'), collapse = ''))
	return (list(class=temp.box, sim=roles.box))
}

read_combined_data <- function(){
	comb_sent_AR_file <- '/home/rezaur/output_cascade/raw_stat_apps/comb_sent_ar.csv'
	comb_sent_AR <<- as.data.frame(read.csv(comb_sent_AR_file, header=FALSE))
	colnames(comb_sent_AR) <<-  c('seq', 'hugged', 'iheart', 'ismile')
	comb_sent_AR[!(comb_sent_AR > 0)] <<- NA
	print(comb_sent_AR_file)
	comb_succ_ratio_file <- '/home/rezaur/output_cascade/raw_stat_apps/comb_succ_ratio.csv'
	comb_succ <<- as.data.frame(read.csv(comb_succ_ratio_file, header=FALSE))
	colnames(comb_succ) <<-  c('seq', 'hugged', 'iheart', 'ismile')
	comb_succ[!(comb_succ >= 0)] <<- NA
	print(comb_succ_ratio_file)
	comb_act_child_file <- '/home/rezaur/output_cascade/raw_stat_apps/comb_act_child.csv'
	comb_act_child <<- as.data.frame(read.csv(comb_act_child_file, header=FALSE))
	colnames(comb_act_child) <<-  c('seq', 'hugged', 'iheart', 'ismile')
	comb_act_child[!(comb_act_child >= 0)] <<- NA
	print(comb_act_child_file)
	comb_act_life_file = '/home/rezaur/output_cascade/raw_stat_apps/comb_act_life.csv'
	comb_act_life <<- as.data.frame(read.csv(comb_act_life_file, header=FALSE))
	colnames(comb_act_life) <<-  c('seq', 'hugged', 'iheart', 'ismile')
	comb_act_life[!(comb_act_life$hugged > -1 & comb_act_life$hugged < 24855), ]$hugged <<- NA
	comb_act_life[!(comb_act_life$iheart > -1 & comb_act_life$iheart < 24855), ]$iheart <<- NA
	comb_act_life[!(comb_act_life$ismile > -1 & comb_act_life$ismile < 24855), ]$ismile <<- NA
	print(comb_act_life_file)
}

comb_sent_AR_analysis <- function(comb_sent_AR_file = '/home/rezaur/output_cascade/raw_stat_apps/comb_sent_ar.csv'){
	comb_sent_AR <<- as.data.frame(read.csv(comb_sent_AR_file, header=FALSE))
	colnames(comb_sent_AR) <<-  c('seq', 'hugged', 'iheart', 'ismile')
	temp <- comb_sent_AR[comb_sent_AR$hugged > 0,]
	hug <- as.data.frame(temp[order(-temp$hugged),]$seq)
	colnames(hug) <- c('seq')
	hugged_users <- nrow(hug)
	temp <- comb_sent_AR[comb_sent_AR$iheart > 0,]
	ihe <- as.data.frame(temp[order(-temp$iheart),]$seq)
	colnames(ihe) <- c('seq')
	iheart_users <- nrow(ihe)
	temp <- comb_sent_AR[comb_sent_AR$ismile > 0,]
	ism <- as.data.frame(temp[order(-temp$ismile),]$seq)
	colnames(ism) <- c('seq')
	ismile_users <- nrow(ism)
	hug$rank <- c(rep(1, ceiling(hugged_users/4)), rep(2, ceiling(hugged_users/4)),
			rep(3, ceiling(hugged_users/4)), rep(4, (hugged_users - 3*ceiling(hugged_users/4))))

	ihe$rank <- c(rep(1, ceiling(iheart_users/4)), rep(2, ceiling(iheart_users/4)),
			rep(3, ceiling(iheart_users/4)), rep(4, (iheart_users - 3*ceiling(iheart_users/4))))
	
	ism$rank <- c(rep(1, ceiling(ismile_users/4)), rep(2, ceiling(ismile_users/4)),
			rep(3, ceiling(ismile_users/4)), rep(4, (ismile_users - 3*ceiling(ismile_users/4))))
	
	hug_ihe <- merge(hug, ihe, by="seq")
	hug_ihe$distance <- abs (hug_ihe$rank.x - hug_ihe$rank.y)
#	pairwise_feature_analysis(hug_ihe, 'Hugged', 'iHeart', 'inv_hug_ihe')
	hug_ihe$comp_type <- 'Hugged vs.\niHeart'
	
	hug_ism <- merge(hug, ism, by="seq")
	hug_ism$distance <- abs (hug_ism$rank.x - hug_ism$rank.y)
#	pairwise_feature_analysis(hug_ism, 'Hugged', 'iSmile', 'inv_hug_ism')
	hug_ism$comp_type <- 'Hugged vs.\niSmile'
	
	ihe_ism <- merge(ihe, ism, by="seq")
	ihe_ism$distance <- abs (ihe_ism$rank.x - ihe_ism$rank.y)
#	pairwise_feature_analysis(ihe_ism, 'iHeart', 'iSmile', 'inv_ihe_ism')
	ihe_ism$comp_type <- 'iHeart vs.\niSmile'

	roles <- rbind(hug_ihe, hug_ism, ihe_ism)
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
	plot <- ggplot(roles.box, aes(rank.x, mean, fill = comp_type, lower=lower, upper=upper, middle=middle, ymin=ymin, ymax=ymax)) + 
			geom_boxplot(stat="identity")+ scale_fill_grey(start = .5, end = .9, name = '') +
			stat_summary(fun.y = "mean", geom = "point", shape= 8, size= 3, position=position_dodge(width=.9)) +
			xlab('Class') + ylab('Distance')
	save_ggplot(plot, 'raw_stat_apps/app_class_invitations.pdf', 24, opts(legend.position="bottom"))

	sim_hug_ihe <- as.data.frame(hug_ihe$distance)
	colnames(sim_hug_ihe) <- 'distance'
	sim_hug_ihe$comp_type <- 'Hugged vs.\niHeart'
	sim_hug_ism <- as.data.frame(hug_ism$distance)
	colnames(sim_hug_ism) <- 'distance'
	sim_hug_ism$comp_type <- 'Hugged vs.\niSmile'
	sim_ihe_ism <- as.data.frame(ihe_ism$distance)
	colnames(sim_ihe_ism) <- 'distance'
	sim_ihe_ism$comp_type <- 'iHeart vs.\niSmile'
	roles <- rbind(sim_hug_ihe, sim_hug_ism, sim_ihe_ism)
	
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
	plot <- ggplot(roles.box, aes(x=comp_type, lower=lower, upper=upper, middle=middle, ymin=ymin, ymax=ymax)) + 
			geom_boxplot(stat="identity") + geom_point(data = roles.box, aes(x=comp_type, y=mean), shape = 8, size = 3)+
			xlab('') + ylab('Distance') 
	save_ggplot(plot, 'raw_stat_apps/similarity_invitations.pdf')
#	return(roles)
}

comb_success_ratio_analysis <- function(comb_succ_ratio_file = '/home/rezaur/output_cascade/raw_stat_apps/comb_succ_ratio.csv'){
	comb_succ <<- as.data.frame(read.csv(comb_succ_ratio_file, header=FALSE))
	colnames(comb_succ) <<-  c('seq', 'hugged', 'iheart', 'ismile')
	temp <- comb_succ[comb_succ$hugged > -1,]
	hug <- as.data.frame(temp[order(-temp$hugged),]$seq)
	colnames(hug) <- c('seq')
	hugged_users <- nrow(hug)
	temp <- comb_succ[comb_succ$iheart > -1,]
	ihe <- as.data.frame(temp[order(-temp$iheart),]$seq)
	colnames(ihe) <- c('seq')
	iheart_users <- nrow(ihe)
	temp <- comb_succ[comb_succ$ismile > -1,]
	ism <- as.data.frame(temp[order(-temp$ismile),]$seq)
	colnames(ism) <- c('seq')
	ismile_users <- nrow(ism)
	hug$rank <- c(rep(1, ceiling(hugged_users/4)), rep(2, ceiling(hugged_users/4)),
			rep(3, ceiling(hugged_users/4)), rep(4, (hugged_users - 3*ceiling(hugged_users/4))))
	
	ihe$rank <- c(rep(1, ceiling(iheart_users/4)), rep(2, ceiling(iheart_users/4)),
			rep(3, ceiling(iheart_users/4)), rep(4, (iheart_users - 3*ceiling(iheart_users/4))))
	
	ism$rank <- c(rep(1, ceiling(ismile_users/4)), rep(2, ceiling(ismile_users/4)),
			rep(3, ceiling(ismile_users/4)), rep(4, (ismile_users - 3*ceiling(ismile_users/4))))
	
	hug_ihe <- merge(hug, ihe, by="seq")
	hug_ihe$distance <- abs (hug_ihe$rank.x - hug_ihe$rank.y)
#	pairwise_feature_analysis(hug_ihe, 'Hugged', 'iHeart', 'inv_hug_ihe')
	hug_ihe$comp_type <- 'Hugged vs.\niHeart'
	
	hug_ism <- merge(hug, ism, by="seq")
	hug_ism$distance <- abs (hug_ism$rank.x - hug_ism$rank.y)
#	pairwise_feature_analysis(hug_ism, 'Hugged', 'iSmile', 'inv_hug_ism')
	hug_ism$comp_type <- 'Hugged vs.\niSmile'
	
	ihe_ism <- merge(ihe, ism, by="seq")
	ihe_ism$distance <- abs (ihe_ism$rank.x - ihe_ism$rank.y)
#	pairwise_feature_analysis(ihe_ism, 'iHeart', 'iSmile', 'inv_ihe_ism')
	ihe_ism$comp_type <- 'iHeart vs.\niSmile'
	
	roles <- rbind(hug_ihe, hug_ism, ihe_ism)
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
	plot <- ggplot(roles.box, aes(rank.x, mean, fill = comp_type, lower=lower, upper=upper, middle=middle, ymin=ymin, ymax=ymax)) + 
			geom_boxplot(stat="identity")+ scale_fill_grey(start = .5, end = .9, name = '') +
			stat_summary(fun.y = "mean", geom = "point", shape= 8, size= 3, position=position_dodge(width=.9)) +
			xlab('Class') + ylab('Distance')
	save_ggplot(plot, 'raw_stat_apps/app_class_invitations.pdf', 24, opts(legend.position="bottom"))
	
	sim_hug_ihe <- as.data.frame(hug_ihe$distance)
	colnames(sim_hug_ihe) <- 'distance'
	sim_hug_ihe$comp_type <- 'Hugged vs.\niHeart'
	sim_hug_ism <- as.data.frame(hug_ism$distance)
	colnames(sim_hug_ism) <- 'distance'
	sim_hug_ism$comp_type <- 'Hugged vs.\niSmile'
	sim_ihe_ism <- as.data.frame(ihe_ism$distance)
	colnames(sim_ihe_ism) <- 'distance'
	sim_ihe_ism$comp_type <- 'iHeart vs.\niSmile'
	roles <- rbind(sim_hug_ihe, sim_hug_ism, sim_ihe_ism)
	
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
	plot <- ggplot(roles.box, aes(x=comp_type, lower=lower, upper=upper, middle=middle, ymin=ymin, ymax=ymax)) + 
			geom_boxplot(stat="identity") + geom_point(data = roles.box, aes(x=comp_type, y=mean), shape = 8, size = 3)+
			xlab('') + ylab('Distance') 
	save_ggplot(plot, 'raw_stat_apps/similarity_succ.pdf')
#	return(roles)
}

comb_act_child_analysis <- function(comb_act_child_file = '/home/rezaur/output_cascade/raw_stat_apps/comb_act_child.csv'){
	comb_act_child <<- as.data.frame(read.csv(comb_act_child_file, header=FALSE))
	colnames(comb_act_child) <<-  c('seq', 'hugged', 'iheart', 'ismile')
	temp <- comb_act_child[comb_act_child$hugged > -1,]
	hug <- as.data.frame(temp[order(-temp$hugged),]$seq)
	colnames(hug) <- c('seq')
	hugged_users <- nrow(hug)
	temp <- comb_act_child[comb_act_child$iheart > -1,]
	ihe <- as.data.frame(temp[order(-temp$iheart),]$seq)
	colnames(ihe) <- c('seq')
	iheart_users <- nrow(ihe)
	temp <- comb_act_child[comb_act_child$ismile > -1,]
	ism <- as.data.frame(temp[order(-temp$ismile),]$seq)
	colnames(ism) <- c('seq')
	ismile_users <- nrow(ism)
	hug$rank <- c(rep(1, ceiling(hugged_users/4)), rep(2, ceiling(hugged_users/4)),
			rep(3, ceiling(hugged_users/4)), rep(4, (hugged_users - 3*ceiling(hugged_users/4))))
	
	ihe$rank <- c(rep(1, ceiling(iheart_users/4)), rep(2, ceiling(iheart_users/4)),
			rep(3, ceiling(iheart_users/4)), rep(4, (iheart_users - 3*ceiling(iheart_users/4))))
	
	ism$rank <- c(rep(1, ceiling(ismile_users/4)), rep(2, ceiling(ismile_users/4)),
			rep(3, ceiling(ismile_users/4)), rep(4, (ismile_users - 3*ceiling(ismile_users/4))))
	
	hug_ihe <- merge(hug, ihe, by="seq")
	hug_ihe$distance <- abs (hug_ihe$rank.x - hug_ihe$rank.y)
	pairwise_feature_analysis(hug_ihe, 'Hugged', 'iHeart', 'act_child_hug_ihe')
	sim_hug_ihe <- as.data.frame(hug_ihe$distance)
	colnames(sim_hug_ihe) <- 'distance'
	sim_hug_ihe$comp_type <- 'Hugged vs.\niHeart'
	
	hug_ism <- merge(hug, ism, by="seq")
	hug_ism$distance <- abs (hug_ism$rank.x - hug_ism$rank.y)
	pairwise_feature_analysis(hug_ism, 'Hugged', 'iSmile', 'act_child_hug_ism')
	sim_hug_ism <- as.data.frame(hug_ism$distance)
	colnames(sim_hug_ism) <- 'distance'
	sim_hug_ism$comp_type <- 'Hugged vs.\niSmile'
	
	ihe_ism <- merge(ihe, ism, by="seq")
	ihe_ism$distance <- abs (ihe_ism$rank.x - ihe_ism$rank.y)
	pairwise_feature_analysis(ihe_ism, 'iHeart', 'iSmile', 'act_child_ihe_ism')
	sim_ihe_ism <- as.data.frame(ihe_ism$distance)
	colnames(sim_ihe_ism) <- 'distance'
	sim_ihe_ism$comp_type <- 'iHeart vs.\niSmile'
	
	roles <- rbind(sim_hug_ihe, sim_hug_ism, sim_ihe_ism)
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
	plot <- ggplot(roles.box, aes(x=comp_type, lower=lower, upper=upper, middle=middle, ymin=ymin, ymax=ymax)) + 
			geom_boxplot(stat="identity") + geom_point(data = roles.box, aes(x=comp_type, y=mean), shape = 8, size = 3)+
			xlab('') + ylab('Distance') 
	save_ggplot(plot, 'raw_stat_apps/similarity_act_child.pdf')
#	return(roles)
}

comb_act_life_analysis <- function(comb_act_life_file = '/home/rezaur/output_cascade/raw_stat_apps/comb_act_life.csv'){
	comb_act_life <<- as.data.frame(read.csv(comb_act_life_file, header=FALSE))
	colnames(comb_act_life) <<-  c('seq', 'hugged', 'iheart', 'ismile')
	temp <- comb_act_life[comb_act_life$hugged > -1 & comb_act_life$hugged < 24855,]
	hug <- as.data.frame(temp[order(-temp$hugged),]$seq)
	colnames(hug) <- c('seq')
	hugged_users <- nrow(hug)
	temp <- comb_act_life[comb_act_life$iheart > -1 & comb_act_life$iheart < 24855,]
	ihe <- as.data.frame(temp[order(-temp$iheart),]$seq)
	colnames(ihe) <- c('seq')
	iheart_users <- nrow(ihe)
	temp <- comb_act_life[comb_act_life$ismile > -1 & comb_act_life$ismile < 24855,]
	ism <- as.data.frame(temp[order(-temp$ismile),]$seq)
	colnames(ism) <- c('seq')
	ismile_users <- nrow(ism)
	hug$rank <- c(rep(1, ceiling(hugged_users/4)), rep(2, ceiling(hugged_users/4)),
			rep(3, ceiling(hugged_users/4)), rep(4, (hugged_users - 3*ceiling(hugged_users/4))))
	
	ihe$rank <- c(rep(1, ceiling(iheart_users/4)), rep(2, ceiling(iheart_users/4)),
			rep(3, ceiling(iheart_users/4)), rep(4, (iheart_users - 3*ceiling(iheart_users/4))))
	
	ism$rank <- c(rep(1, ceiling(ismile_users/4)), rep(2, ceiling(ismile_users/4)),
			rep(3, ceiling(ismile_users/4)), rep(4, (ismile_users - 3*ceiling(ismile_users/4))))
	
	hug_ihe <- merge(hug, ihe, by="seq")
	hug_ihe$distance <- abs (hug_ihe$rank.x - hug_ihe$rank.y)
	pairwise_feature_analysis(hug_ihe, 'Hugged', 'iHeart', 'act_life_hug_ihe')
	sim_hug_ihe <- as.data.frame(hug_ihe$distance)
	colnames(sim_hug_ihe) <- 'distance'
	sim_hug_ihe$comp_type <- 'Hugged vs.\niHeart'
	
	hug_ism <- merge(hug, ism, by="seq")
	hug_ism$distance <- abs (hug_ism$rank.x - hug_ism$rank.y)
	pairwise_feature_analysis(hug_ism, 'Hugged', 'iSmile', 'act_life_hug_ism')
	sim_hug_ism <- as.data.frame(hug_ism$distance)
	colnames(sim_hug_ism) <- 'distance'
	sim_hug_ism$comp_type <- 'Hugged vs.\niSmile'
	
	ihe_ism <- merge(ihe, ism, by="seq")
	ihe_ism$distance <- abs (ihe_ism$rank.x - ihe_ism$rank.y)
	pairwise_feature_analysis(ihe_ism, 'iHeart', 'iSmile', 'act_life_ihe_ism')
	sim_ihe_ism <- as.data.frame(ihe_ism$distance)
	colnames(sim_ihe_ism) <- 'distance'
	sim_ihe_ism$comp_type <- 'iHeart vs.\niSmile'
	
	roles <- rbind(sim_hug_ihe, sim_hug_ism, sim_ihe_ism)
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
	plot <- ggplot(roles.box, aes(x=comp_type, lower=lower, upper=upper, middle=middle, ymin=ymin, ymax=ymax)) + 
			geom_boxplot(stat="identity") + geom_point(data = roles.box, aes(x=comp_type, y=mean), shape = 8, size = 3)+
			xlab('') + ylab('Distance') 
	save_ggplot(plot, 'raw_stat_apps/similarity_act_life.pdf')
#	return(roles)
}

#class_inv <- class_distance_across_apps (comb_sent_AR, 'invitations')
#class_succ <- class_distance_across_apps (comb_succ, 'succ_ratio')
#class_act_users <- class_distance_across_apps (comb_act_child, 'act_users')
#class_act_life <- class_distance_across_apps (comb_act_life, 'act_life')

comb_app_user_features <- function (){
#	comb_sent_AR[!(comb_sent_AR > 0)] <<- NA
#	comb_succ[!(comb_succ >= 0)] <- NA
#	comb_act_child[!(comb_act_child >= 0)] <- NA
#	comb_act_life[!(comb_act_life$hugged > -1 & comb_act_life$hugged < 24855), ]$hugged <- NA
#	comb_act_life[!(comb_act_life$iheart > -1 & comb_act_life$iheart < 24855), ]$iheart <- NA
#	comb_act_life[!(comb_act_life$ismile > -1 & comb_act_life$ismile < 24855), ]$ismile <- NA

	print('Getting Spearman correlation')
	dflist <- list(comp1 = list(df1 = comb_sent_AR, df2 = comb_sent_AR),
			comp2 = list(df1 = comb_sent_AR, df2 = comb_succ),
			comp3 = list(df1 = comb_sent_AR, df2 = comb_act_child),
			comp4 = list(df1 = comb_sent_AR, df2 = comb_act_life),
			comp5 = list(df1 = comb_succ, df2 = comb_succ),
			comp6 = list(df1 = comb_succ, df2 = comb_act_child),
			comp7 = list(df1 = comb_succ, df2 = comb_act_life),
			comp8 = list(df1 = comb_act_child, df2 = comb_act_child),
			comp9 = list(df1 = comb_act_child, df2 = comb_act_life),
			comp10 = list(df1 = comb_act_life, df2 = comb_act_life))
	feature_comp <- c('Invitation vs. Invitation', 'Invitation vs. Activated users',
			'Invitation vs. Success ratio', 'Invitation vs. Active lifespan',
			'Activated users vs. Activated users', ' Active users vs. Success ratio', 'Active users vs. Active lifespan', 
			'Success ratio vs. Success ratio',  'Success ratio vs. Active lifespan',
			'Active lifespan vs. Active lifespan')
	app_comp <- c('hug_ihe', 'hug_ism', 'ihe_ism')
	spearman_corr <- as.data.frame(feature_comp)
	colnames(spearman_corr) <- c('Correlation')
#	hug_ism_corr <- as.data.frame(feature_comp)
#	colnames(hug_ism_corr) <- c('Feat.')
#	ihe_ism_corr <- as.data.frame(feature_comp)
#	colnames(ihe_ism_corr) <- c('Feat.')
	temp_1 <- c()
	temp_2 <- c()
	temp_3 <- c()
	lapply(dflist, function(comp) {
				temp_1 <<- c(temp_1, spearman_correlation(comp$df1, comp$df2, 'hugged', 'iheart'))
				temp_2 <<- c(temp_2, spearman_correlation(comp$df1, comp$df2, 'hugged', 'ismile'))
				temp_3 <<- c(temp_3, spearman_correlation(comp$df1, comp$df2, 'iheart', 'ismile'))
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

#spearman_1 <- comb_app_user_features()

