source('~/scripts/Cascade/tools.r')
library(plyr)
require(scales)

case_burstiness_analysis <- function(file='iheart_cascade/top_size.csv_all_evolution.csv'){
	evolution <- as.data.frame(read.csv(file, header=FALSE))
	colnames(evolution) <- c('root', 'size', 'depth', 'width', 'first_day', 'last_day', 'burstiness')
	evolution$lifetime <- evolution$last_day-evolution$first_day+1
	evolution.df <- evolution[evolution$first_day != evolution$last_day & evolution$size > 1000,]
#	evolution.df <- ddply(evolution.df, c('size'), summarise, avg_burst = mean (burstiness))
	plot <- ggplot(evolution.df, aes(x = size, y = burstiness)) + geom_point(size = 0.8) +
			xlab('Size') + ylab('Burstiness') + scale_x_log10() + 
			scale_y_continuous(breaks=seq(-1,1, by=0.2), labels=seq(-1,1, by=0.2))
	save_ggplot(plot, 'iheart_cascade/case_size_vs_burstiness.pdf')
	plot <- ggplot(evolution.df, aes(x = size, y = lifetime)) + geom_point(size = 0.8) +
			xlab('Size') + ylab('Lifetime (Days)') + scale_x_log10() 
#			scale_y_discrete(breaks=(c(200*86400, 300*86400, 400*86400)),
#					labels=c('200d', '300d', '400d'))
	save_ggplot(plot, 'iheart_cascade/case_size_vs_lifetime.pdf')
	return(evolution.df)
}

case_size_vs_inter_adoption_time <-  function(file = 'iheart_cascade/size_vs_inter_adoption_time.txt'){
	size_vs_adoption_time <- as.data.frame(read.csv(file, header=FALSE))
	size_vs_adoption_time.mean <- size_vs_adoption_time[,c(1,2)]
	size_vs_adoption_time.mean[,3] <- 0
	colnames(size_vs_adoption_time.mean) <- c('size', 'elapsed_time', 'centrality')
	size_vs_adoption_time.p_25 <- size_vs_adoption_time[,c(1,3)]
	size_vs_adoption_time.p_25[,3] <- 1
	colnames(size_vs_adoption_time.p_25) <- c('size', 'elapsed_time', 'centrality')
	size_vs_adoption_time.median <- size_vs_adoption_time[,c(1,4)]
	size_vs_adoption_time.median[,3] <- 2
	colnames(size_vs_adoption_time.median) <- c('size', 'elapsed_time', 'centrality')
	size_vs_adoption_time.p_75 <- size_vs_adoption_time[,c(1,5)]
	size_vs_adoption_time.p_75[,3] <- 3
	colnames(size_vs_adoption_time.p_75) <- c('size', 'elapsed_time', 'centrality')
	size_vs_adoption_time.df = rbind(size_vs_adoption_time.p_25, size_vs_adoption_time.mean, 
			size_vs_adoption_time.median, size_vs_adoption_time.p_75)
	size_vs_adoption_time.df <- size_vs_adoption_time.df[size_vs_adoption_time.df$size > 1000, ]
	size_vs_adoption_time.df$centrality <- factor(size_vs_adoption_time.df$centrality)
	plot <- ggplot(size_vs_adoption_time.df, aes(x = size, y = elapsed_time)) +
			geom_point(aes(group = centrality, shape = centrality, colour = centrality), size = 1) +
			scale_y_log10(breaks=(c(3*86400, 10*86400, 30*86400, 60*86400, 90*86400, 200*86400, 300*86400)),
					labels=c('3d', '10d', '30d', '60d', '90d', '200d', '300d')) +
			scale_x_log10()
	plot <- change_plot_attributes(plot, "", 0:3, c('Average', '25 percentile', 'Median', '75 percentile' ),
			"Size", "Inter-adoption time")
	save_ggplot(plot, 'iheart_cascade/case_size_vs_ia.pdf', 24, opts(legend.position=c(.8, .13)))
	return(size_vs_adoption_time.df)
}

case_size_vs_root <- function(file='iheart_gift/size_vs_root.csv'){
	size_vs_root <- unique(as.data.frame(read.csv(file, header=FALSE)))
	colnames(size_vs_root) <- c('root', 'size', 'depth' ,'width', 'major_gift',
			'root_act_lifespan', 'root_outdeg', 'root_contribution' , 'root_success_ratio')
	size_vs_root <- size_vs_root[size_vs_root$size > 1000, ]
	size_vs_root$contribution_ratio <- round(size_vs_root$root_contribution/ size_vs_root$size, 3)
	plot <- ggplot(size_vs_root, aes(x = size, y = contribution_ratio)) + geom_point(size = 0.8) +
			ylab('Contribution ratio of seeds') + xlab('Size') + scale_x_log10()
	save_ggplot(plot, 'iheart_cascade/case_seed_contr_vs_size.pdf')
	plot <- ggplot(size_vs_root, aes(x = size, y = root_success_ratio)) + geom_point(size = 0.8) +
			ylab('Success ratio of seeds') + xlab('Size') + scale_x_log10()
	save_ggplot(plot, 'iheart_cascade/case_seed_success_vs_size.pdf')
	return(size_vs_root)
}
