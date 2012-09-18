change_plot_legend <- function(plot){
	plot <- plot + scale_colour_hue(name  ="Threshold", breaks=c("86400","172800","259200","345600","432000","518400","604800","691200","777600","864000","1209600","1814400"), 
					labels=c("1 day", "2 day", "3 day", "4 day", "5 day", "6 day", "7 day", "8 day", "9 day", "10 day", "2 weeks", "3 weeks")) +
	scale_shape_discrete(name  ="Threshold", breaks=c("86400","172800","259200","345600","432000","518400","604800","691200","777600","864000","1209600","1814400"), 
					labels=c("1 day", "2 day", "3 day", "4 day", "5 day", "6 day", "7 day", "8 day", "9 day", "10 day", "2 week", "3 week"))
	
	return(plot)
}

analyze_size <- function(directoryname, plot_x_lim){
	library(ggplot2)
	library(plyr)
	prev_dir <- getwd()
	setwd(directoryname)
	cacade_size <- as.data.frame(read.csv('size.csv', header=FALSE))
	colnames(cacade_size) <- c('size', 'count', 'threshold')
	cacade_size.df <- ddply(cacade_size, c('threshold'), function(one_partition){
				one_partition = one_partition[order(one_partition$size),]
				one_partition$cum_count = cumsum(one_partition$count)
				one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
				one_partition$pdf_val = one_partition$count / max(one_partition$cum_count)
				
				one_partition
			})
	cacade_size.df$threshold <- factor(cacade_size.df$threshold)
	plot_x_lim <- min(plot_x_lim,max(cacade_size.df$size))
	plot <- ggplot(cacade_size.df,aes(x = size, y = cdf_val)) + xlim(0,plot_x_lim) + geom_line(aes(group = threshold,colour = threshold))
	plot <- change_plot_legend(plot)
	ggsave(plot,file='size_cdf.eps')
	plot <- ggplot(cacade_size.df,aes(x = log10(size), y = cdf_val)) + xlim(0,log10(plot_x_lim)) + geom_line(aes(group = threshold,colour = threshold))
	plot <- change_plot_legend(plot)
	ggsave(plot,file='size_cdf_logx.eps')
	plot <- ggplot(cacade_size.df,aes(x = log10(size), y = log10(cdf_val))) + xlim(0,log10(plot_x_lim)) + geom_line(aes(group = threshold,colour = threshold))
	plot <- change_plot_legend(plot)
	ggsave(plot,file='size_cdf_log_log.eps')
	plot <- ggplot(cacade_size.df,aes(x = size, y = pdf_val)) + xlim(0,plot_x_lim) + geom_point(aes(group = threshold,colour = threshold))
	plot <- change_plot_legend(plot)
	ggsave(plot,file='size_pdf.eps')
	plot <- ggplot(cacade_size.df,aes(x = size, y = log(pdf_val))) + xlim(0,plot_x_lim) + geom_point(aes(group = threshold,colour = threshold))
	plot <- change_plot_legend(plot)
	ggsave(plot,file='size_pdf_logy.eps')
	plot <- ggplot(cacade_size.df,aes(x = log(size), y = log(pdf_val))) + xlim(0,log10(plot_x_lim)) + geom_point(aes(group = threshold,colour = threshold))
	plot <- change_plot_legend(plot)
	ggsave(plot,file='size_pdf_log_log.eps')
	setwd(prev_dir)
}

analyze_depth <- function(directoryname, plot_x_lim){
	library(ggplot2)
	library(plyr)
	prev_dir <- getwd()
	setwd(directoryname)
	cacade_depth <- as.data.frame(read.csv('depth.csv', header=FALSE))
	colnames(cacade_depth) <- c('depth', 'count', 'threshold')
	cacade_depth.df <- ddply(cacade_depth, c('threshold'), function(one_partition){
				one_partition = one_partition[order(one_partition$depth),]
				one_partition$cum_count = cumsum(one_partition$count)
				one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
				one_partition$pdf_val = one_partition$count / max(one_partition$cum_count)
				
				one_partition
			})
	cacade_depth.df$threshold <- factor(cacade_depth.df$threshold)
	plot_x_lim <- min(plot_x_lim,max(cacade_depth.df$depth))
	plot <- ggplot(cacade_depth.df,aes(x = depth, y = cdf_val)) + xlim(0,plot_x_lim) + geom_line(aes(group = threshold,colour = threshold))
	plot <- change_plot_legend(plot)
	ggsave(plot,file='depth_cdf.eps')
	plot <- ggplot(cacade_depth.df,aes(x = log10(depth), y = cdf_val)) + xlim(0,log10(plot_x_lim)) + geom_line(aes(group = threshold,colour = threshold))
	plot <- change_plot_legend(plot)
	ggsave(plot,file='depth_cdf_logx.eps')
	plot <- ggplot(cacade_depth.df,aes(x = log10(depth), y = log10(cdf_val))) + xlim(0,log10(plot_x_lim)) + geom_line(aes(group = threshold,colour = threshold))
	plot <- change_plot_legend(plot)
	ggsave(plot,file='depth_cdf_log_log.eps')
	plot <- ggplot(cacade_depth.df,aes(x = depth, y = pdf_val)) + xlim(0,plot_x_lim) + geom_point(aes(group = threshold,colour = threshold))
	plot <- change_plot_legend(plot)
	ggsave(plot,file='depth_pdf.eps')
	plot <- ggplot(cacade_depth.df,aes(x = depth, y = log(pdf_val))) + xlim(0,plot_x_lim) + geom_point(aes(group = threshold,colour = threshold))
	plot <- change_plot_legend(plot)
	ggsave(plot,file='depth_pdf_logy.eps')
	plot <- ggplot(cacade_depth.df,aes(x = log(depth), y = log(pdf_val))) + xlim(0,log10(plot_x_lim)) + geom_point(aes(group = threshold,colour = threshold))
	plot <- change_plot_legend(plot)
	ggsave(plot,file='depth_pdf_log_log.eps')
	setwd(prev_dir)
}

odeg_corr <- function(file_name){
	library(ggplot2)
	library(plyr)
	odeg <- as.data.frame(read.csv(file_name, header=FALSE))
	colnames(odeg) <- c('top_user_odeg', 'neighbour_odeg')
	odeg.df <- ddply(odeg, c('top_user_odeg'), summarise, avg_neighbour_odeg = mean(neighbour_odeg))
	plot <- ggplot(odeg.df, aes(x = top_user_odeg, y = avg_neighbour_odeg)) + geom_point() + geom_smooth(method=lm)
	ggsave(plot,file=paste(file_name,'corr.eps'))
	cor(odeg.df$top_user_odeg,odeg.df$avg_neighbour_odeg)
}
