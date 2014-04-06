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

raw_outdeg_analysis <- function (raw_outdeg_file_name){
	raw_outdeg <<- as.data.frame(read.csv(raw_outdeg_file_name, header=FALSE))
	colnames(raw_outdeg) <<- c('outdeg', 'count')
	raw_outdeg <- raw_outdeg[order(raw_outdeg$outdeg),]
	raw_outdeg$cum_count <- cumsum(raw_outdeg$count)
	raw_outdeg$cdf <- raw_outdeg$cum_count / max(raw_outdeg$cum_count)
	plot <- ggplot(raw_outdeg, aes(x = outdeg, y = cdf)) + geom_line() +
			xlab('Number of sent ARs') + ylab('Empirical CDF') + #+ scale_y_log10()
			scale_x_log10(limits = c(1, 1000))
	save_ggplot(plot, 'raw_stat_v2/raw_outdeg.pdf')
}

raw_indeg_analysis <- function (raw_indeg_file_name){
	raw_indeg <<- as.data.frame(read.csv(raw_indeg_file_name, header=FALSE))
	colnames(raw_indeg) <<- c('indeg', 'count')
	raw_indeg <- raw_indeg[order(raw_indeg$indeg),]
	raw_indeg$cum_count <- cumsum(raw_indeg$count)
	raw_indeg$cdf <- raw_indeg$cum_count / max(raw_indeg$cum_count)
	plot <- ggplot(raw_indeg, aes(x = indeg, y = cdf)) + geom_line() +
			xlab('Number of received ARs') + ylab('Empirical CDF') + #+ scale_y_log10()
			scale_x_log10(limits = c(1, 1000))
	save_ggplot(plot, 'raw_stat_v2/raw_indeg.pdf')
}

influence_threshold_analysis <- function (parent_count_file_name, indeg_before_act_file){
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
			scale_x_log10(limits = c(1, 1000)) #+ scale_y_log10() #+ theme(legend.position=c(.8, .7)) + xlim(0,log10(plot_x_lim*100))
	plot <- change_plot_attributes_fancy(plot, "Requests from", 0:1, c('Distinct Parent', 'Distinct AR'), "Number of Request", "Prob of activation")
	save_ggplot(plot, 'raw_stat_v2/adop_prob.pdf')
}

active_proportion_analysis <- function (active_proportion_file){
	influence <- as.data.frame(read.csv(active_proportion_file, header=FALSE))
	influence[,3] <- 0
	colnames(influence) <- c('proportion', 'count','dummy_type')
	influence.df <- ddply(influence, c('dummy_type'), function(one_partition){
				one_partition = one_partition[order(one_partition$proportion),]
				one_partition$cum_count = cumsum(one_partition$count)
				one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
				one_partition$pdf_val = one_partition$count / max(one_partition$cum_count)
				one_partition
			})
	influence.df$dummy_type <- factor(influence.df$dummy_type)
	plot <- ggplot(influence.df, aes(x = (proportion), y = (cdf_val))) + 
			geom_point(aes(group = dummy_type, colour = dummy_type, shape = dummy_type), size=1)
#			scale_x_log10(limits = c(1, 100)) #+ scale_y_log10() #+ theme(legend.position=c(.8, .7)) + xlim(0,log10(plot_x_lim*100))
	plot <- change_plot_attributes_fancy(plot, "", 0:0, c('Distinct Parent'), "Proportion", "CDF")
	save_ggplot(plot, 'raw_stat_v2/active_proportion.pdf')
}

invitation_burstiness <- function(file='raw_stat_v2/invitation_burstiness_stat.csv'){
	invitations <- as.data.frame(read.csv(file, header=FALSE))
	colnames(invitations) <- c('burstiness', 'count')
#	invitations$burstiness <- as.numeric(levels(invitations$burstiness))[invitations$burstiness]
	invitations <- invitations[order(invitations$burstiness), ]
	invitations$cum_sum <- cumsum(invitations$count)
	invitations$cdf <- invitations$cum_sum/max(invitations$cum_su)
	plot <- ggplot(invitations[((invitations$burstiness > -1) & (invitations$burstiness < 1)), ],
			aes(x = burstiness, y = count)) + geom_line() + xlab('Burstiness') + ylab('Count')
#			scale_x_log10() + scale_y_log10()
	save_ggplot(plot, 'raw_stat_v2/burstiness_count.pdf')
	plot <- ggplot(invitations[((invitations$burstiness > -1) & (invitations$burstiness < 1)), ],
			aes(x = burstiness, y = cdf)) + geom_line() + xlab('Burstiness') + ylab('Empirical CDF')
#			scale_x_log10() + scale_y_log10()
	save_ggplot(plot, 'raw_stat_v2/burstiness_cdf.pdf')
}

#parent_lifespan<-lifespan_analysis('raw_stat_1/lifespan_stat.csv')
#raw_outdeg_analysis('raw_stat_v2/raw_outdeg_stat.csv')
#influence_threshold_analysis('raw_stat_v2/parent_count_before_act.csv', 'raw_stat_v2/indeg_before_act.csv')
#active_proportion_analysis('raw_stat_v2/act_proportion_count.csv')
