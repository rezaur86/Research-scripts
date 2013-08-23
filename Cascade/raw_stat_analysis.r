source('~/scripts/cascade/tools.r')
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
			xlab('Active Lifetime (bin = 600s)') + ylab('Cumulative Proportion of Count') + 
			scale_x_log10(breaks=c(6,12,24,48,72,96,144,288,4*144,7*144,14*144,28*144,2*28*144,4*28*144,8*28*144), labels=c('1hr','2hr','4hr','8hr','12hr','16hr','1day','2day','4day','1week','2weeks','4weeks','8weeks','16weeks','32weeks')) +
			scale_y_continuous(breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0))+
#			scale_y_log10() + # + xlim(1,200) + ylim(0,100000)
			myPlotTheme() + opts(axis.text.x=theme_text(angle=45,hjust=1,vjust=1))
	pdf('raw_stat/lifespan_cum_pdf.pdf')
	print(plot)
	dev.off()
	return(parent_lifespan)
}

raw_outdeg_analysis <- function (raw_outdeg_file_name){
	raw_outdeg <<- as.data.frame(read.csv(raw_outdeg_file_name, header=FALSE))
	colnames(raw_outdeg) <<- c('outdeg', 'count')
	plot <- ggplot(raw_outdeg, aes(x = outdeg, y = count)) + geom_line() + xlab('Raw out degree') + ylab('Count') + scale_x_log10() + scale_y_log10()
	save_ggplot(plot, 'raw_stat/raw_outdeg.pdf')
}

raw_indeg_analysis <- function (raw_indeg_file_name){
	raw_indeg <- as.data.frame(read.csv(raw_indeg_file_name, header=FALSE))
	colnames(raw_indeg) <- c('indeg', 'count')
	raw_indeg <- raw_indeg[order(raw_indeg$indeg),]
	plot <- ggplot(raw_indeg, aes(x = indeg, y = (cumsum(count/sum(count))))) + geom_point() + xlab("Distinct Parent Count in Received Gifts") + ylab('Cumulative Proportion of Users') + 
			scale_x_log10(breaks=c(1,2,4,8,16,32,64,128,256,1000)) +
			scale_y_continuous(breaks=c(.4,.5,.6,.7,.8,.9,1.0), labels=c('40%','50%','60%','70%','80%','90%','100%'), limits=c(.4,1.0))
	save_ggplot(plot, 'raw_stat/raw_indeg.pdf')
}

parent_lifespan<-lifespan_analysis('raw_stat_1/lifespan_stat.csv')
raw_outdeg_analysis('raw_stat_1/raw_outdeg_stat.csv')
raw_indeg_analysis('raw_stat_1/raw_indeg_stat.csv')
