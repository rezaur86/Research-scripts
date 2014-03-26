source('~/scripts/Cascade/tools.r')
library(plyr)

temporal_analysis <- function (daily_born, daily_activation, daily_last_act, daily_last_seen){
	daily_born <- as.data.frame(read.csv(daily_born, header=FALSE))
	daily_born[,5] <- 0
	colnames(daily_born) <- c('year', 'week', 'day', 'count', 'activity_type')
	daily_activation <- as.data.frame(read.csv(daily_activation, header=FALSE))
	daily_activation[,5] <- 1
	colnames(daily_activation) <- c('year', 'week', 'day', 'count', 'activity_type')
	daily_last_act <- as.data.frame(read.csv(daily_last_act, header=FALSE))
	daily_last_act[,5] <- 2
	colnames(daily_last_act) <- c('year', 'week', 'day', 'count', 'activity_type')
	daily_last_seen <- as.data.frame(read.csv(daily_last_seen, header=FALSE))
	daily_last_seen[,5] <- 3
	colnames(daily_last_seen) <- c('year', 'week', 'day', 'count', 'activity_type')
	activities <- rbind(daily_born, daily_activation, daily_last_act, daily_last_seen)
	activities.df <- ddply(activities, c('activity_type'), function(one_partition){
				one_partition$date = do.call(paste, c(as.data.frame(cbind(one_partition$year, one_partition$week, one_partition$day)), sep="-"))
				one_partition$week = do.call(paste, c(as.data.frame(cbind(one_partition$year, one_partition$week)), sep="-"))
				one_partition$rtime = strptime(one_partition$date, format = "%Y-%U-%w")
				one_partition = one_partition[order(one_partition$rtime),]
				one_partition$cum_count = cumsum(one_partition$count)
				one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
				one_partition$pdf_val = one_partition$count / max(one_partition$cum_count)
				one_partition
			})
	print(head(activities.df))
	dates <- unique(activities.df$date)
	weeks <- unique(activities.df[activities.df$date%in%dates[seq(4, length(dates), 28)], ]$week)
	rtimes <- unique(activities.df[activities.df$date%in%dates[seq(4, length(dates), 28)], ]$rtime)
#	print(activities.df[activities.df$week %in% weeks[seq(length(weeks),15, -1)],])
	print(rtimes)
	print(weeks)
	activities.df$activity_type <- factor(activities.df$activity_type)
	plot <- ggplot(activities.df, aes(x = (rtime), y = (count))) + 
			geom_line(aes(group = activity_type, colour = activity_type, shape = activity_type), size=.5) #+ scale_y_log10()
#			scale_x_discrete(breaks=c(rtimes), labels=c(weeks))
	plot <- change_plot_attributes_fancy(plot, "Activity type", 0:3, c('Born', 'Activation', 'Last activity', 'Last seen' ),
			"Time", "User count")
	save_ggplot(plot, 'raw_stat_v2/weekly_activities.pdf', 10,
			opts(axis.text.x = element_text(angle = 90, hjust = 0), legend.position=c(.2, .8)))
	activities.df <- ddply(activities, c('activity_type', 'day'), summarise, daily_act = sum (count))
	activities.df <- ddply(activities.df, c('activity_type'), function(one_partition){
				one_partition = one_partition[order(one_partition$day),]
				one_partition$cum_count = cumsum(one_partition$daily_act)
				one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
				one_partition$pdf_val = one_partition$daily_act / max(one_partition$cum_count)
				one_partition
			})
	activities.df$activity_type <- factor(activities.df$activity_type)
	plot <- ggplot(activities.df, aes(x = (day), y = (pdf_val))) + 
			geom_line(aes(group = activity_type, colour = activity_type, shape = activity_type), size=.5)+
#			scale_y_log10()+
			scale_x_discrete(breaks=c(0,1,2,3,4,5,6), labels=c('Sun','Mon','Tue','Wed','Thu','Fri','Sat'))
	plot <- change_plot_attributes_fancy(plot, "Activity type", 0:3, c('Born', 'Activation', 'Last activity', 'Last seen' ),
			"Week day", "User count")
	save_ggplot(plot, 'raw_stat_v2/daily_activities.pdf', 10,
			opts(axis.text.x = element_text(angle = 0, hjust = 0), legend.position=c(.7, .7)))
}

act<-temporal_analysis('raw_stat_v2/daily_born.csv', 'raw_stat_v2/daily_activation.csv',
		'raw_stat_v2/daily_last_act.csv', 'raw_stat_v2/daily_last_seen.csv')
#raw_outdeg_analysis('raw_stat_v2/raw_outdeg_stat.csv')
#influence_threshold_analysis('raw_stat_v2/parent_count_before_act.csv', 'raw_stat_v2/indeg_before_act.csv')
#active_proportion_analysis('raw_stat_v2/act_proportion_count.csv')
