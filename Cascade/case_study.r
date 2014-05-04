source('~/scripts/Cascade/tools.r')
library(plyr)
require(scales)

case_evolution_analysis <- function(file='iheart_cascade/top_size.csv_all_evolution.csv'){
	evolution <- as.data.frame(read.csv(file, header=FALSE))
	colnames(evolution) <- c('root', 'size', 'depth', 'width', 'first_day', 'last_day', 'burstiness')
	evolution$lifetime <- evolution$last_day-evolution$first_day+1
#	evolution.df <- evolution[evolution$first_day != evolution$last_day & evolution$size > 1000,]
	size_bin <- unique(c(1, 70, 903, 10000000))
	size_cat <- c('Small', 'Medium', 'Large')
	evolution <- transform(evolution, bin = cut(evolution$size, breaks=size_bin))
	categories <- levels(evolution$bin)
	evolution$cat <- factor(evolution$bin, levels = categories, labels = size_cat) #c('(2,100]', '(100,1000]', '1000+'))
	evolution.depth <- ddply(evolution, c('cat'), .drop=TRUE,
			.fun = function(one_partition){
				stats = boxplot.stats(one_partition$depth)$stats
				c(ymin=stats[1],
						lower=stats[2],
						middle=stats[3],
						upper=stats[4],
						ymax=stats[5],
						mean = mean(one_partition$depth))
			})
	evolution.depth$cat <- factor(evolution.depth$cat)
	plot <- ggplot(evolution.depth, aes(x=cat, lower=lower, upper=upper, middle=middle, ymin=ymin, ymax=ymax)) + 
			geom_boxplot(stat="identity") + geom_point(data = evolution.depth, aes(x=cat, y=mean))+
			xlab('Cascade size range') + ylab('Max depth') 
	save_ggplot(plot, 'iheart_cascade/case_depth.pdf')
	
	evolution.width <- ddply(evolution, c('cat'), .drop=TRUE,
			.fun = function(one_partition){
				stats = boxplot.stats(one_partition$width)$stats
				c(ymin=stats[1],
						lower=stats[2],
						middle=stats[3],
						upper=stats[4],
						ymax=stats[5],
						mean = mean(one_partition$width))
			})
	evolution.width$cat <- factor(evolution.width$cat)
	plot <- ggplot(evolution.width, aes(x=cat, lower=lower, upper=upper, middle=middle, ymin=ymin, ymax=ymax)) + 
			geom_boxplot(stat="identity") + geom_point(data = evolution.width, aes(x=cat, y=mean))+
			xlab('Cascade size range') + ylab('Max width') 
	save_ggplot(plot, 'iheart_cascade/case_width.pdf')
	
	evolution.lifetime <- ddply(evolution, c('cat'), .drop=TRUE,
			.fun = function(one_partition){
				stats = boxplot.stats(one_partition$lifetime)$stats
				c(ymin=stats[1],
						lower=stats[2],
						middle=stats[3],
						upper=stats[4],
						ymax=stats[5],
						mean = mean(one_partition$lifetime))
			})
	evolution.lifetime$cat <- factor(evolution.lifetime$cat)
	plot <- ggplot(evolution.lifetime, aes(x=cat, lower=lower, upper=upper, middle=middle, ymin=ymin, ymax=ymax)) + 
			geom_boxplot(stat="identity") + geom_point(data = evolution.lifetime, aes(x=cat, y=mean))+
			xlab('Cascade size range') + ylab('Cascade lifetime (Days)') 
	save_ggplot(plot, 'iheart_cascade/case_lifetime.pdf')
	
	evolution.burstiness <- ddply(evolution, c('cat'), .drop=TRUE,
			.fun = function(one_partition){
				stats = boxplot.stats(one_partition$burstiness)$stats
				c(ymin=stats[1],
						lower=stats[2],
						middle=stats[3],
						upper=stats[4],
						ymax=stats[5],
						mean = mean(one_partition$burstiness))
			})
	evolution.burstiness$cat <- factor(evolution.burstiness$cat)
	plot <- ggplot(evolution.burstiness, aes(x=cat, lower=lower, upper=upper, middle=middle, ymin=ymin, ymax=ymax)) + 
			geom_boxplot(stat="identity") + geom_point(data = evolution.burstiness, aes(x=cat, y=mean))+
			xlab('Cascade size range') + ylab('Burstiness') 
	save_ggplot(plot, 'iheart_cascade/case_burstiness.pdf')
	
	return(evolution)
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
			'root_act_lifespan', 'root_outdeg', 'root_contribution' , 'root_success_ratio', 'rn_contr')
	size_vs_root <- size_vs_root[size_vs_root$size > 1, ]
	size_bin <- unique(c(1, 70, 903, 10000000))
	size_cat <- c('Small', 'Medium', 'Large')
	size_vs_root <- transform(size_vs_root, bin = cut(size_vs_root$size, breaks=size_bin))
	categories <- levels(size_vs_root$bin)
	size_vs_root$cat <- factor(size_vs_root$bin, levels = categories, labels = size_cat) # c('(2,100]', '(100,1000]', '1000+'))
	size_vs_root$root_contribution <- size_vs_root$root_contribution/ size_vs_root$size
	size_vs_root.root_contribution <- ddply(size_vs_root, c('cat'), .drop=TRUE,
			.fun = function(one_partition){
				stats = boxplot.stats(one_partition$root_contribution)$stats
				c(ymin=stats[1],
						lower=stats[2],
						middle=stats[3],
						upper=stats[4],
						ymax=stats[5],
						mean = mean(one_partition$root_contribution))
			})
	size_vs_root.root_contribution$cat <- factor(size_vs_root.root_contribution$cat)
	plot <- ggplot(size_vs_root.root_contribution, aes(x=cat, lower=lower, upper=upper, middle=middle, ymin=ymin, ymax=ymax)) + 
			geom_boxplot(stat="identity") + geom_point(data = size_vs_root.root_contribution, aes(x=cat, y=mean))+
			xlab('Cascade size range') + ylab('Contribution ratio of seeds') 
	save_ggplot(plot, 'iheart_cascade/case_root_contribution.pdf')
	
	size_vs_root$rn_contr <- size_vs_root$rn_contr/ size_vs_root$size
	size_vs_root.rn_contr <- ddply(size_vs_root, c('cat'), .drop=TRUE,
			.fun = function(one_partition){
				stats = boxplot.stats(one_partition$rn_contr)$stats
				c(ymin=stats[1],
						lower=stats[2],
						middle=stats[3],
						upper=stats[4],
						ymax=stats[5],
						mean = mean(one_partition$root_contribution))
			})
	size_vs_root.rn_contr$cat <- factor(size_vs_root.rn_contr$cat)
	plot <- ggplot(size_vs_root.rn_contr, aes(x=cat, lower=lower, upper=upper, middle=middle, ymin=ymin, ymax=ymax)) + 
			geom_boxplot(stat="identity") + geom_point(data = size_vs_root.rn_contr, aes(x=cat, y=mean))+
			xlab('Cascade size range') + ylab('Contribution ratio of first generation users') 
	save_ggplot(plot, 'iheart_cascade/case_rn_contr.pdf')
	
	size_vs_root.root_success_ratio <- ddply(size_vs_root, c('cat'), .drop=TRUE,
			.fun = function(one_partition){
				stats = boxplot.stats(one_partition$root_success_ratio)$stats
				c(ymin=stats[1],
						lower=stats[2],
						middle=stats[3],
						upper=stats[4],
						ymax=stats[5],
						mean = mean(one_partition$root_success_ratio))
			})
	size_vs_root.root_success_ratio$cat <- factor(size_vs_root.root_success_ratio$cat)
	plot <- ggplot(size_vs_root.root_success_ratio, aes(x=cat, lower=lower, upper=upper, middle=middle, ymin=ymin, ymax=ymax)) + 
			geom_boxplot(stat="identity") + geom_point(data = size_vs_root.root_success_ratio, aes(x=cat, y=mean))+
			xlab('Cascade size range') + ylab('Success ratio of the seeds') 
	save_ggplot(plot, 'iheart_cascade/case_root_success_ratio.pdf')
	
	return(size_vs_root)
}
