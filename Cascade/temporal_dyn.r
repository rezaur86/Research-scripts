source('~/scripts/Cascade/tools.r')
library(plyr)

temporal_analysis <- function (daily_born, daily_activation, daily_last_act, daily_last_seen){
	daily_born <- as.data.frame(read.csv(daily_born, header=FALSE))
	daily_born[,6] <- 0
	colnames(daily_born) <- c('year', 'week', 'day', 'hour', 'count', 'activity_type')
	daily_activation <- as.data.frame(read.csv(daily_activation, header=FALSE))
	daily_activation[,6] <- 1
	colnames(daily_activation) <- c('year', 'week', 'day', 'hour', 'count', 'activity_type')
	daily_last_act <- as.data.frame(read.csv(daily_last_act, header=FALSE))
	daily_last_act[,6] <- 2
	colnames(daily_last_act) <- c('year', 'week', 'day', 'hour', 'count', 'activity_type')
	daily_last_seen <- as.data.frame(read.csv(daily_last_seen, header=FALSE))
	daily_last_seen[,6] <- 3
	colnames(daily_last_seen) <- c('year', 'week', 'day', 'hour', 'count', 'activity_type')
	activities <- rbind(daily_born, daily_activation, daily_last_act, daily_last_seen)
	activities.daily <- ddply(activities, c('activity_type', 'year', 'week', 'day'), summarise, daily_count = sum (count))
	print(tail(activities.daily))
	activities.df <- ddply(activities.daily, c('activity_type'), function(one_partition){
				one_partition$date = do.call(paste, c(as.data.frame(cbind(one_partition$year, one_partition$week, one_partition$day)), sep="-"))
				one_partition$week = do.call(paste, c(as.data.frame(cbind(one_partition$year, one_partition$week)), sep="-"))
				one_partition$rtime = strptime(one_partition$date, format = "%Y-%U-%w")
				one_partition = one_partition[order(one_partition$rtime),]
				one_partition$cum_count = cumsum(one_partition$daily_count)
				one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
				one_partition$pdf_val = one_partition$daily_count / max(one_partition$cum_count)
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
	plot <- ggplot(activities.df, aes(x = (rtime), y = (daily_count))) +
			geom_line(aes(group = activity_type, colour = activity_type, shape = activity_type), size=.5) #+ scale_y_log10()
#			scale_x_discrete(breaks=c(rtimes), labels=c(weeks))
	plot <- change_plot_attributes_fancy(plot, "Activity type", 0:3, c('Born', 'Activation', 'Last activity', 'Last seen' ),
			"Time", "User count")
	save_ggplot(plot, 'raw_stat_v2/weekly_activities.pdf', 10,
			opts(axis.text.x = element_text(angle = 90, hjust = 0), legend.position=c(.2, .8)))
	activities$am_pm <- activities$hour < 12
	activities$am_pm[activities$am_pm == TRUE] <- 'AM'
	activities$am_pm[activities$am_pm == FALSE] <- 'PM'
	activities.hourly <- ddply(activities, c('activity_type', 'day', 'am_pm'), summarise, hourly_act = sum (count))
	activities.df <- ddply(activities.hourly, c('activity_type'), function(one_partition){
				one_partition = one_partition[order(one_partition$day, one_partition$am_pm),]
				one_partition$day_hour = do.call(paste, c(as.data.frame(cbind(one_partition$day, one_partition$am_pm)), sep=":"))
				one_partition$cum_count = cumsum(one_partition$hourly_act)
				one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
				one_partition$pdf_val = one_partition$hourly_act / max(one_partition$cum_count)
				one_partition
			})
	print(head(activities.df, 50))
	activities.df$activity_type <- factor(activities.df$activity_type)
	plot <- ggplot(activities.df, aes(x = (day_hour), y = (pdf_val))) + 
			geom_line(aes(group = activity_type, colour = activity_type, shape = activity_type), size=.5)+
#			scale_y_log10()+
			scale_x_discrete(breaks=c('0:AM','0:PM', '1:AM','1:PM', '2:AM','2:PM', '3:AM','3:PM', '4:AM','4:PM', '5:AM','5:PM', '6:AM','6:PM'),
					labels=c('Sun:AM','Sun:PM', 'Mon:AM','Mon:PM', 'Tue:AM','Tue:PM', 'Wed:AM','Wed:PM', 'Thu:AM','Thu:PM', 'Fri:AM','Fri:PM', 'Sat:AM', 'Sat:PM'))
	plot <- change_plot_attributes_fancy(plot, "Activity type", 0:3, c('Born', 'Activation', 'Last activity', 'Last seen' ),
			"Day:Hour", "User count")
	save_ggplot(plot, 'raw_stat_v2/daily_activities.pdf', 10,
			opts(axis.text.x = element_text(angle = 90, hjust = 0), legend.position=c(.9, .7)))
}

burstiness <- function(file){
	evolution <- as.data.frame(read.csv(file, header=FALSE))
	colnames(evolution) <- c('root', 'day', 'evolution')
	evolution.df <- ddply(evolution, c('root'), summarise, size_sd = sd(evolution), avg_size = mean(evolution),
			burstiness = round((size_sd - avg_size) / (size_sd + avg_size)), 3)
	evolution.df <- as.data.frame(table(evolution.df$burstiness))
	colnames(evolution.df) <- c('burstiness','count')
	plot <- ggplot(evolution.df, aes(x = burstiness, y = count)) + geom_line() + xlab('Burstiness') + ylab('Count') +
			scale_x_log10() + scale_y_log10()
	save_ggplot(plot, 'iheart_cascade/burstiness.pdf', 10,
			opts(axis.text.x = element_text(angle = 0, hjust = 0), legend.position=c(.7, .7)))
}

act<-temporal_analysis('raw_stat_v2/daily_born.csv', 'raw_stat_v2/daily_activation.csv',
		'raw_stat_v2/daily_last_act.csv', 'raw_stat_v2/daily_last_seen.csv')
burstiness ('iheart_cascade/')
#act<-temporal_analysis('iheart_test/daily_born.csv', 'iheart_test/daily_activation.csv',
#		'iheart_test/daily_last_act.csv', 'iheart_test/daily_last_seen.csv')
#raw_outdeg_analysis('raw_stat_v2/raw_outdeg_stat.csv')
#influence_threshold_analysis('raw_stat_v2/parent_count_before_act.csv', 'raw_stat_v2/indeg_before_act.csv')
#active_proportion_analysis('raw_stat_v2/act_proportion_count.csv')
