source('~/scripts/cascade/tools.r')
source('~/scripts/cascade/plfit.r')
library(ggplot2)
library(gtable)
library(grid)
library(plyr)
library(Hmisc)
library(gridExtra) 

influence_analysis <- function (dir){
	prev_dir = getwd()
	setwd(dir)
	influence_proportion <- unique(as.data.frame(read.csv('influence_proportion_stat.csv', header=FALSE)))
	colnames(influence_proportion) <- c('root', 'size','depth','alpha', 'count')
	print(head(influence_proportion))
	influence_proportion['category'] <- cut(influence_proportion$size, breaks = c(0,155,10^8))#c(0,10,10^2,155,10^3,10^4,10^5,10^6,10^7,10^8))
#	desired_root <- c(30615, 7511, 27512, 39957, 301174, 319062, 18216, 18175935, 26563978, 58721193)
#	desired_influence<- influence_proportion #[influence_proportion$root%in%desired_root, ]
	print('starting summarizing')
	size_vs_influence.df <- ddply(influence_proportion, c('alpha', 'category'), summarise,
			all_count = sum(count))
	size_vs_influence <- ddply(size_vs_influence.df, c('category'), function(one_partition){ #'root', 'size', 'depth'
				one_partition = one_partition[order(one_partition$alpha),]
				one_partition$cum_count = cumsum(one_partition$all_count)
				one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
				one_partition$pdf_val = one_partition$all_count / max(one_partition$cum_count)
				one_partition
			})
	size_vs_influence$category <- factor(size_vs_influence$category)
	plot <- ggplot(size_vs_influence, aes(x = alpha/1000, y = (pdf_val))) + geom_line(aes(group = category, colour = category)) +
			xlab('Influence proportion') + ylab('Empirical PDF')
	save_ggplot(plot, 'size_influence_cat_pdf.pdf')
	plot <- ggplot(size_vs_influence, aes(x = alpha/1000, y = (cdf_val))) + geom_line(aes(group = category, colour = category)) +
			xlab('Influence proportion') + ylab('Empirical CDF')
	save_ggplot(plot, 'size_influence_cat_cdf.pdf')
	#	size_vs_influence.small_cascades <- ddply(size_vs_influence[size_vs_influence$size < 15, ],
#			c('alpha'), summarise,
#			cdf_mean = mean(cdf_val), cdf_sd = sd(cdf_val),
#			pdf_mean = mean(pdf_val), pdf_sd = sd(pdf_val))
#	plot <- ggplot(size_vs_influence.small_cascades, aes(x=alpha, y=pdf_mean)) + 
	##			geom_errorbar(aes(ymin=pdf_mean-pdf_sd, ymax=pdf_mean+pdf_sd), width=.1) +
#			geom_line() +
#			geom_point()
#	save_ggplot(plot, 'small_size_influence.pdf')
#	size_vs_influence.large_cascades <- ddply(size_vs_influence[size_vs_influence$size >= 15, ],
#			c('alpha'), summarise,
#			cdf_mean = mean(cdf_val), cdf_sd = sd(cdf_val),
#			pdf_mean = mean(pdf_val), pdf_sd = sd(pdf_val))
#	plot <- ggplot(size_vs_influence.large_cascades, aes(x=alpha, y=pdf_mean)) + 
	##			geom_errorbar(aes(ymin=pdf_mean-pdf_sd, ymax=pdf_mean+pdf_sd), width=.1) +
#			geom_line() +
#			geom_point()
#	save_ggplot(plot, 'large_size_influence.pdf')
#	print(head(size_vs_influence.small_cascades))
#	print(head(size_vs_influence.large_cascades))
#	size_vs_influence$size <- factor(size_vs_influence$size)
#	plot <- ggplot(size_vs_influence, aes(x = alpha, y = (cdf_val))) + geom_point(aes(group = size, colour = size)) +
#			xlab('Influence proportion') + ylab('Empirical CDF')
#	save_ggplot(plot, 'size_influence.pdf')
#	size_vs_influence$depth <- factor(size_vs_influence$depth)
#	plot <- ggplot(size_vs_influence, aes(x = alpha, y = (cdf_val))) + geom_line(aes(group = depth, colour = depth, linetype = depth)) +
#			xlab('Influence proportion') + ylab('Empirical CDF')
#	save_ggplot(plot, 'depth_influence.pdf')
	setwd(prev_dir)
	return (influence_proportion)
}
