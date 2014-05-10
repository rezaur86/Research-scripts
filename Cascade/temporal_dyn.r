source('~/scripts/Cascade/tools.r')
library(plyr)
require(scales)

temporal_analysis <- function (daily_born = 'raw_stat_v2/daily_born.csv',
		daily_activation = 'raw_stat_v2/daily_activation.csv',
		daily_activities = 'iheart_cascade/activities_stat.txt'){
	daily_born <- as.data.frame(read.csv(daily_born, header=FALSE))
	daily_born[,6] <- 1
	colnames(daily_born) <- c('year', 'week', 'day', 'hour', 'count', 'activity_type')
	daily_activation <- as.data.frame(read.csv(daily_activation, header=FALSE))
	daily_activation[,6] <- 2
	colnames(daily_activation) <- c('year', 'week', 'day', 'hour', 'count', 'activity_type')
	daily_act <- as.data.frame(read.csv(daily_activities, header=FALSE))
	daily_act[,6] <- 0
	colnames(daily_act) <- c('year', 'week', 'day', 'hour', 'count', 'activity_type')
	activities <- rbind(daily_born, daily_activation, daily_act)
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
	dates <- unique(activities.df$date)
	weeks <- unique(activities.df[activities.df$date%in%dates[seq(4, length(dates), 28)], ]$week)
	rtimes <- unique(activities.df[activities.df$date%in%dates[seq(4, length(dates), 28)], ]$rtime)
#	print(activities.df[activities.df$week %in% weeks[seq(length(weeks),15, -1)],])
	print(rtimes)
	print(weeks)
#	activities.df$rtime <- as.Date(activities.df$rtime, format="%Y-%m-%d")
	activities.df$activity_type <- factor(activities.df$activity_type)
	plot <- ggplot(activities.df, aes(x = (rtime), y = (daily_count))) +
			geom_line(aes(group = activity_type, linetype = activity_type, colour = activity_type), size=1) +
			scale_y_log10(limits = c(10^4, 10^8)) +
			scale_x_datetime(breaks = date_breaks("1 months"),
					labels = date_format("%b"))+
			scale_linetype_manual(values=c(1,1,6), name='', breaks=0:2,
					labels=c( 'Total number of gifting activities','Number of first-time-invited users','Number of new activations/adoptions')) +
			scale_colour_manual(values=c("black", "gray55", "black"), name='', breaks=0:2,
					labels=c( 'Total number of gifting activities','Number of first-time-invited users','Number of new activations/adoptions'))+
			xlab('2009 - 2010') + ylab('Count')
#	plot <- change_plot_attributes_fancy(plot, "", 0:2, c( 'Number of activities', 'Number of first time invited users','Number of new activations/adoptions'),
#			"2009 - 2010", "Count")
	save_ggplot(plot, 'raw_stat_v2/overall_activities.pdf', 24,
			opts(axis.text.x = element_text(angle = 90, hjust = 0), legend.position=c(.45, .9)), 10)
#	activities$am_pm <- activities$hour < 12
#	activities$am_pm[activities$am_pm == TRUE] <- 'AM'
#	activities$am_pm[activities$am_pm == FALSE] <- 'PM'
	activities.hourly <- ddply(activities, c('activity_type', 'day', 'hour'), summarise, hourly_act = sum (count))
	print(head(activities.hourly))
	activities.df <- ddply(activities.hourly, c('activity_type'), function(one_partition){
				one_partition = one_partition[order(one_partition$day, one_partition$hour),]
				one_partition$day_hour = do.call(paste, c(as.data.frame(cbind(one_partition$day, one_partition$hour)), sep=":"))
				one_partition$rtime = as.POSIXct(strptime(one_partition$day_hour, format = "%w:%H"))
				one_partition$cum_count = cumsum(one_partition$hourly_act)
				one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
				one_partition$pdf_val = one_partition$hourly_act / max(one_partition$cum_count)
				one_partition
			})
	print(head(activities.df, 50))
	activities.df$activity_type <- factor(activities.df$activity_type)
	plot <- ggplot(activities.df, aes(x = (day_hour), y = (hourly_act))) + 
			geom_line(aes(group = activity_type, colour = activity_type, shape = activity_type), size=.5)+
#			scale_y_log10()+
			scale_x_discrete(breaks=c('0:0','0:11', '1:0','1:11', '2:0','2:11', '3:0','3:11', '4:0','4:11', '5:0','5:11', '6:0','6:11'),
					labels=c('Sun:AM','Sun:PM', 'Mon:AM','Mon:PM', 'Tue:AM','Tue:PM', 'Wed:AM','Wed:PM', 'Thu:AM','Thu:PM', 'Fri:AM','Fri:PM', 'Sat:AM', 'Sat:PM'))
	plot <- change_plot_attributes_fancy(plot, "Activity type", 0:2, c('Born', 'Adoption', 'Activity' ),
			"Day:Hour", "User count")
	save_ggplot(plot, 'raw_stat_v2/daily_activities.pdf', 24,
			opts(axis.text.x = element_text(angle = 90, hjust = 0), legend.position=c(.9, .7)))
}

burstiness_analysis <- function(file='iheart_cascade/top_size.csv_all_evolution.csv'){
	evolution <- as.data.frame(read.csv(file, header=FALSE))
	colnames(evolution) <- c('root', 'size', 'depth', 'width', 'first_day', 'last_day', 'burstiness')
	evolution$lifetime <- evolution$last_day-evolution$first_day+1
	evolution.df <- as.data.frame(table(evolution[evolution$first_day != evolution$last_day, ]$burstiness))
	colnames(evolution.df) <- c('burstiness','count')
	evolution.df$burstiness <- as.numeric(levels(evolution.df$burstiness))[evolution.df$burstiness]
	evolution.df <- evolution.df[order(evolution.df$burstiness), ]
	evolution.df$cum_sum <- cumsum(evolution.df$count)
	evolution.df$cdf <- evolution.df$cum_sum/max(evolution.df$cum_sum)
	plot <- ggplot(evolution.df, aes(x = burstiness, y = count)) + geom_point(size= 0.8) + xlab('Burstiness') + ylab('Count')+
			geom_smooth(se=FALSE)
#			scale_x_log10() + scale_y_log10()
	save_ggplot(plot, 'iheart_cascade/burstiness_count.pdf')
	plot <- ggplot(evolution.df,
			aes(x = burstiness, y = cdf)) + geom_line() + xlab('Burstiness') + ylab('Empirical CDF') +
			scale_y_continuous(breaks=c(0,.2,.4,.6,.8,1.0))
#			scale_x_log10() + scale_y_log10()
	save_ggplot(plot, 'iheart_cascade/burstiness_cdf.pdf')
	evolution.life <- as.data.frame(table(evolution$lifetime))
	colnames(evolution.life) <- c('life_time','count')
	evolution.life$life_time <- as.numeric(levels(evolution.life$life_time))[evolution.life$life_time]
	evolution.life <- evolution.life[order(evolution.life$life_time), ]
	evolution.life$cum_sum <- cumsum(evolution.life$count)
	evolution.life$cdf <- evolution.life$cum_sum/max(evolution.life$cum_sum)
	evolution.life$pdf <- evolution.life$count/max(evolution.life$cum_sum)
	plot <- ggplot(evolution.life, aes(x = life_time, y = pdf)) +
			geom_point() + xlab('Lifetime') + ylab('Empirical PDF') + scale_y_log10()+
			scale_x_log10(breaks=c(1, 7, 2*7, 4*7, 3*4*7, 12*4*7),
					labels=c('1d', '1w','2w', '4w', '3m', '1y'))	
	save_ggplot(plot, 'iheart_cascade/lifetime_pdf.pdf')
	evolution.df <- ddply(evolution, c('size'), summarise, avg_life = mean (lifetime))
	plot <- ggplot(evolution.df, aes(x = size, y = avg_life)) + geom_point() +
			xlab('Cascade size') + ylab('Average lifetime') +
			scale_x_log10() + #breaks=c(1, 10, 100, 1000, 10000), labels=c('1', '10','100', '1000', '10000')
			scale_y_log10(breaks=c(1, 7, 2*7, 4*7, 3*4*7, 12*4*7),
					labels=c('1d', '1w','2w', '4w', '3m', '1y'))
	save_ggplot(plot, 'iheart_cascade/size_vs_life.pdf')
	evolution.df <- evolution[evolution$first_day != evolution$last_day, c(2,7)]
	evolution.df <- ddply(evolution.df, c('burstiness'), summarise, avg_size = mean (size))
	plot <- ggplot(evolution.df, aes(x = burstiness, y = avg_size)) + geom_point() +
			xlab('Burstiness') + ylab('Average cascade size') + scale_y_log10(breaks=c(10, 100, 1000, 10000, 100000))
	save_ggplot(plot, 'iheart_cascade/burstiness_vs_size.pdf')
	evolution.df <- evolution[evolution$first_day != evolution$last_day, c(2,7)]
#	evolution.df <- ddply(evolution.df, c('size'), summarise, avg_burst = mean (burstiness))
	plot <- ggplot(evolution.df, aes(x = size, y = burstiness)) + geom_point(size = 0.5) +
			xlab('Size') + ylab('Burstiness') + scale_x_log10()
	save_ggplot(plot, 'iheart_cascade/size_vs_burstiness.pdf')
	evolution.related <- as.data.frame(evolution[,c(2,3,4,7)])
	evolution.related$lifetime <- evolution$last_day - evolution$first_day + 1
	return(evolution.df)
}

analyze_inter_adoption_time <- function(file = 'iheart_cascade/inter_adoption_time_stat.txt'){
	inter_adoption_time <- as.data.frame(read.csv(file, header=FALSE))
	colnames(inter_adoption_time) <- c('seconds_taken')
	max_inter_adoption_time <- max(inter_adoption_time$seconds_taken) #35030687
	time_bin <- unique(c(0:60, 2*60, 4*60, 6*60, 8*60, 10*60, 20*60, 1800, seq(3600, 86400, by=3600),
			seq(86400, 35030687, by=86400)))
	inter_adoption_binned <- transform(inter_adoption_time, bin = cut(inter_adoption_time$seconds_taken, breaks=time_bin))
	print(head(inter_adoption_binned))
#			cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", labs) ),
#			upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs) ))
	inter_adoption_binned <- as.data.frame(table(inter_adoption_binned$bin))
	colnames(inter_adoption_binned) <- c('bin','count')
	inter_adoption_binned$upper_bounds <- as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", levels(inter_adoption_binned$bin)))
	inter_adoption_binned <- inter_adoption_binned[order(inter_adoption_binned$upper_bounds), ]
	inter_adoption_binned$cum_count <- cumsum(inter_adoption_binned$count)
	inter_adoption_binned$cdf <- inter_adoption_binned$cum_count / max(inter_adoption_binned$cum_count) 
	plot <- ggplot(inter_adoption_binned, aes(x=upper_bounds, y=cdf))+ geom_line() +
			scale_x_log10(breaks=c(10, 60, 600, 3600, 10*3600, 86400, 10*86400, 100*86400),
			labels=c('10s', '1m', '10m', '1h', '10h', '1d', '10d', '100d'))+
			xlab('Inter-adoption time') + ylab('Empirical CDF')+
			scale_y_continuous(breaks=c(0,.2,.4,.6,.8,1.0))
	save_ggplot(plot,'iheart_cascade/inter_adoption_time.pdf', 20)
	return(inter_adoption_binned)
}

analyze_size_vs_inter_adoption_time <-  function(file = 'iheart_cascade/size_vs_inter_adoption_time.txt'){
	size_vs_adoption_time <- as.data.frame(read.csv(file, header=FALSE))
	size_vs_adoption_time.mean <- size_vs_adoption_time[,c(1,2)]
	size_vs_adoption_time.mean[,3] <- 1
	colnames(size_vs_adoption_time.mean) <- c('size', 'elapsed_time', 'centrality')
	size_vs_adoption_time.p_25 <- size_vs_adoption_time[,c(1,3)]
	size_vs_adoption_time.p_25[,3] <- 3
	colnames(size_vs_adoption_time.p_25) <- c('size', 'elapsed_time', 'centrality')
	size_vs_adoption_time.median <- size_vs_adoption_time[,c(1,4)]
	size_vs_adoption_time.median[,3] <- 2
	colnames(size_vs_adoption_time.median) <- c('size', 'elapsed_time', 'centrality')
	size_vs_adoption_time.p_75 <- size_vs_adoption_time[,c(1,5)]
	size_vs_adoption_time.p_75[,3] <- 0
	colnames(size_vs_adoption_time.p_75) <- c('size', 'elapsed_time', 'centrality')
	size_vs_adoption_time.df = rbind(size_vs_adoption_time.p_25, size_vs_adoption_time.mean, 
			size_vs_adoption_time.median, size_vs_adoption_time.p_75)
	size_vs_adoption_time.df$centrality <- factor(size_vs_adoption_time.df$centrality)
#	size_vs_adoption_time.df$size <- factor(size_vs_adoption_time.df$size)
	plot <- ggplot(size_vs_adoption_time.df, aes(x = size, y = elapsed_time)) +
			geom_point(aes(group = centrality, colour = centrality, shape = centrality), size = 2) +
#			geom_line(aes(group = centrality, colour = centrality, shape = centrality)) +
			scale_y_log10(breaks=(c(3600, 86400, 10*86400, 30*86400, 50*86400, 100*86400, 200*86400, 300*86400)),
					labels=c('1h', '1d', '10d', '30d', '50d', '100d', '200d', '300d')) +
			scale_x_log10()+
			scale_shape_manual(values=c(1,8,0,3), name='', breaks=c(0,1,2,3),
					labels=c('75th percentile', 'Average',  'Median', '25th percentile')) +
			scale_colour_manual(values=c("gray70", "black",  "gray40",  "gray20"), name='', breaks=c(0,1,2,3),
			labels=c('75th percentile', 'Average',  'Median', '25th percentile')) +
			xlab('Cascade size') + ylab('Inter-adoption time')
#	plot <- change_plot_attributes(plot, "", c(0,3,1,2), c('Average', '75 percentile', '25 percentile', 'Median'),
#			"Size", "Inter-adoption time")
	save_ggplot(plot, 'iheart_cascade/size_vs_inter_adoption_time.pdf', 24, opts(legend.position=c(.3, .12)))
	return(size_vs_adoption_time.df)
}

analyze_depth_vs_inter_adoption_time <-  function(file = 'iheart_gift/depth_vs_inter_adoption_time.txt'){
	depth_vs_adoption_time <- as.data.frame(read.csv(file, header=FALSE))
	colnames(depth_vs_adoption_time) <- c('depth', 'ia_time')
	depth_vs_adoption_time.df <- ddply(depth_vs_adoption_time, c('depth'), .drop=TRUE,
			.fun = function(one_partition){
				stats = boxplot.stats(one_partition$ia_time)$stats
				c(ymin=stats[1],
				lower=stats[2],
				middle=stats[3],
				upper=stats[4],
				ymax=stats[5],
				mean = mean(one_partition$ia_time))
			})
	print(head(depth_vs_adoption_time.df))
	depth_vs_adoption_time.df$depth <- factor(depth_vs_adoption_time.df$depth)
	plot <- ggplot(depth_vs_adoption_time.df, aes(x=depth, lower=lower, upper=upper, middle=middle, ymin=ymin, ymax=ymax)) + 
			geom_boxplot(stat="identity") +
			scale_y_discrete(breaks=(c(86400, 10*86400, 30*86400, 50*86400, 100*86400, 200*86400, 300*86400)),
					labels=c('1d', '10d', '30d', '50d', '100d', '200d', '300d')) +
			scale_x_discrete(breaks=seq(0,60,5), labels=seq(0,60,5)) +
			xlab('Generation of adoption') + ylab('Inter-adoptime time') 
	save_ggplot(plot, 'iheart_cascade/depth_vs_inter_adoption_time.pdf', width = 10)
	inter_generation_time <- depth_vs_adoption_time.df
	inter_generation_time$depth <- as.numeric(inter_generation_time$depth)
	inter_generation_time.mean <- inter_generation_time[,c('depth', 'mean')]
	inter_generation_time.mean[,3] <- 1
	colnames(inter_generation_time.mean) <- c('depth', 'elapsed_time', 'centrality')
	inter_generation_time.p_25 <- inter_generation_time[,c('depth', 'lower')]
	inter_generation_time.p_25[,3] <- 3
	colnames(inter_generation_time.p_25) <- c('depth', 'elapsed_time', 'centrality')
	inter_generation_time.median <- inter_generation_time[,c('depth', 'middle')]
	inter_generation_time.median[,3] <- 2
	colnames(inter_generation_time.median) <- c('depth', 'elapsed_time', 'centrality')
	inter_generation_time.p_75 <- inter_generation_time[,c('depth', 'upper')]
	inter_generation_time.p_75[,3] <- 0
	colnames(inter_generation_time.p_75) <- c('depth', 'elapsed_time', 'centrality')
	inter_generation_time.df = rbind(inter_generation_time.p_25, inter_generation_time.mean, 
			inter_generation_time.median, inter_generation_time.p_75)
	inter_generation_time.df$centrality <- factor(inter_generation_time.df$centrality)
#	inter_generation_time.df$depth <- factor(inter_generation_time.df$depth)
	plot <- ggplot(inter_generation_time.df, aes(x = depth, y = elapsed_time)) +
			geom_point(aes(group = centrality, colour = centrality, shape = centrality), size = 3) +
			geom_line(aes(group = centrality, colour = centrality, shape = centrality)) +
			scale_y_discrete(breaks=(c(86400, 10*86400, 30*86400, 60*86400, 90*86400, 180*86400, 365*86400)),
					labels=c('1d', '10d', '1m', '2m', '3m', '6m', '1y'))+
			scale_x_continuous(breaks=seq(0,55,5), labels=seq(0,55,5))#, limits=c(1,55))
	plot <- change_plot_attributes(plot, "", 0:3, c('75th percentile', 'Average', 'Median', '25th percentile'),
			"From n-1 to n generation", "Inter-adoption time")
	save_ggplot(plot, 'iheart_cascade/inter_adoption_time.pdf', 24, opts(legend.position=c(.7, .85)), 10)	
	return(depth_vs_adoption_time.df)
}

analyze_inter_generation_time <-  function(file = 'iheart_cascade/inter_generation_time_stat.txt'){
	inter_generation_time <- as.data.frame(read.csv(file, header=FALSE))
	inter_generation_time.mean <- inter_generation_time[,c(1,2)]
	inter_generation_time.mean[,3] <- 1
	colnames(inter_generation_time.mean) <- c('depth', 'elapsed_time', 'centrality')
	inter_generation_time.p_25 <- inter_generation_time[,c(1,3)]
	inter_generation_time.p_25[,3] <- 3
	colnames(inter_generation_time.p_25) <- c('depth', 'elapsed_time', 'centrality')
	inter_generation_time.median <- inter_generation_time[,c(1,4)]
	inter_generation_time.median[,3] <- 2
	colnames(inter_generation_time.median) <- c('depth', 'elapsed_time', 'centrality')
	inter_generation_time.p_75 <- inter_generation_time[,c(1,5)]
	inter_generation_time.p_75[,3] <- 0
	colnames(inter_generation_time.p_75) <- c('depth', 'elapsed_time', 'centrality')
	inter_generation_time.df = rbind(inter_generation_time.p_25, inter_generation_time.mean, 
			inter_generation_time.median, inter_generation_time.p_75)
	inter_generation_time.df$centrality <- factor(inter_generation_time.df$centrality)
#	inter_generation_time.df$depth <- factor(inter_generation_time.df$depth)
	plot <- ggplot(inter_generation_time.df, aes(x = depth, y = elapsed_time)) +
			geom_point(aes(group = centrality, colour = centrality, shape = centrality), size = 3) +
			geom_line(aes(group = centrality, colour = centrality, shape = centrality)) +
			scale_y_discrete(breaks=(c(86400, 10*86400, 30*86400, 60*86400, 90*86400, 180*86400, 365*86400)),
					labels=c('1d', '10d', '1m', '2m', '3m', '6m', '1y'))+
#			scale_y_log10(breaks=(c(3600, 86400, 10*86400, 30*86400, 50*86400, 100*86400, 200*86400, 300*86400)),
#					labels=c('1h', '1d', '10d', '30d', '50d', '100d', '200d', '300d')) +
			scale_x_continuous(breaks=seq(0,55,5), labels=seq(0,55,5))#, limits=c(0,55))
	plot <- change_plot_attributes(plot, "", 0:3, c('75th percentile', 'Average', 'Median', '25th percentile'),
					"From n-1 to n generation", "Inter-generation time")
	save_ggplot(plot, 'iheart_cascade/inter_generation_time.pdf', 24, opts(legend.position=c(.7, .85)), 10)	
}

#act<-temporal_analysis('raw_stat_v2/daily_born.csv', 'raw_stat_v2/daily_activation.csv',
#		'raw_stat_v2/daily_last_act.csv', 'raw_stat_v2/daily_last_seen.csv')
#burstiness ('iheart_cascade/')
#act<-temporal_analysis('iheart_test/daily_born.csv', 'iheart_test/daily_activation.csv',
#		'iheart_test/daily_last_act.csv', 'iheart_test/daily_last_seen.csv')
#raw_outdeg_analysis('raw_stat_v2/raw_outdeg_stat.csv')
#influence_threshold_analysis('raw_stat_v2/parent_count_before_act.csv', 'raw_stat_v2/indeg_before_act.csv')
#active_proportion_analysis('raw_stat_v2/act_proportion_count.csv')
