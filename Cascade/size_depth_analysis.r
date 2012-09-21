change_plot_attributes <- function(plot, xlabel, ylabel){
	plot <- plot + scale_colour_hue(name  ="Window", breaks=c("86400","172800","259200","345600","432000","518400","604800","691200","777600","864000","1209600","1814400"), 
					labels=c("1 day", "2 day", "3 day", "4 day", "5 day", "6 day", "7 day", "8 day", "9 day", "10 day", "2 weeks", "3 weeks")) +
	scale_shape_discrete(name  ="Threshold", breaks=c("86400","172800","259200","345600","432000","518400","604800","691200","777600","864000","1209600","1814400"), 
					labels=c("1 day", "2 day", "3 day", "4 day", "5 day", "6 day", "7 day", "8 day", "9 day", "10 day", "2 week", "3 week"))
	
	plot <- plot + xlab(xlabel) + ylab(ylabel)
	return(plot)
}

analyze_size <- function(directoryname, plot_x_lim){
	library(ggplot2)
	library(plyr)
	prev_dir = getwd()
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
	plot <- change_plot_attributes(plot, "Cascade Size", "Cumulative Proportion")
	ggsave(plot,file='size_cdf.eps')
	plot <- ggplot(cacade_size.df,aes(x = log10(size), y = cdf_val)) + xlim(0,log10(plot_x_lim)) + geom_line(aes(group = threshold,colour = threshold))
	plot <- change_plot_attributes(plot, "log of Cascade Size", "Cumulative Proportion")
	ggsave(plot,file='size_cdf_logx.eps')
	plot <- ggplot(cacade_size.df,aes(x = log10(size), y = log10(cdf_val))) + xlim(0,log10(plot_x_lim)) + geom_line(aes(group = threshold,colour = threshold))
	plot <- change_plot_attributes(plot, "log of Cascade Size", "log of Cumulative Proportion")
	ggsave(plot,file='size_cdf_log_log.eps')
	plot <- ggplot(cacade_size.df,aes(x = size, y = pdf_val)) + xlim(0,plot_x_lim) + geom_point(aes(group = threshold,colour = threshold))
	plot <- change_plot_attributes(plot, "Cascade Size", "Proportion")
	ggsave(plot,file='size_pdf.eps')
	plot <- ggplot(cacade_size.df,aes(x = size, y = log10(pdf_val))) + xlim(0,plot_x_lim) + geom_point(aes(group = threshold,colour = threshold))
	plot <- change_plot_attributes(plot, "Cascade Size", "log of Proportion")
	ggsave(plot,file='size_pdf_plot_logy.eps')
	plot <- ggplot(cacade_size.df,aes(x = size, y = log10(pdf_val))) + xlim(0,plot_x_lim) + geom_line(aes(group = threshold,colour = threshold))
	plot <- change_plot_attributes(plot, "Cascade Size", "log of Proportion")
	ggsave(plot,file='size_pdf_line_logy.eps')
	setwd(prev_dir)
}

analyze_depth <- function(directoryname, plot_x_lim){
	library(ggplot2)
	library(plyr)
	prev_dir = getwd()
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
	plot <- change_plot_attributes(plot, "Cascade Depth", "Cumulative Proportion")
	ggsave(plot,file='depth_cdf.eps')
	plot <- ggplot(cacade_depth.df,aes(x = log10(depth), y = cdf_val)) + xlim(0,log10(plot_x_lim)) + geom_line(aes(group = threshold,colour = threshold))
	plot <- change_plot_attributes(plot, "log of Cascade Depth", "Cumulative Proportion")
	ggsave(plot,file='depth_cdf_logx.eps')
	plot <- ggplot(cacade_depth.df,aes(x = log10(depth), y = log10(cdf_val))) + xlim(0,log10(plot_x_lim)) + geom_line(aes(group = threshold,colour = threshold))
	plot <- change_plot_attributes(plot, "log of Cascade Depth", "log of Cumulative Proportion")
	ggsave(plot,file='depth_cdf_log_log.eps')
	plot <- ggplot(cacade_depth.df,aes(x = log10(depth), y = pdf_val)) + xlim(0,log10(plot_x_lim)) + geom_point(aes(group = threshold,colour = threshold))
	plot <- change_plot_attributes(plot, "log Cascade Depth", "Proportion")
	ggsave(plot,file='depth_pdf_logx.eps')
	plot <- ggplot(cacade_depth.df,aes(x = depth, y = log10(pdf_val))) + xlim(0,plot_x_lim) + geom_point(aes(group = threshold,colour = threshold))
	plot <- change_plot_attributes(plot, "Cascade Depth", "log of Proportion")
	ggsave(plot,file='depth_pdf_point_logy.eps')
	plot <- ggplot(cacade_depth.df,aes(x = depth, y = log10(pdf_val))) + xlim(0,plot_x_lim) + geom_line(aes(group = threshold,colour = threshold))
	plot <- change_plot_attributes(plot, "Cascade Depth", "log of Proportion")
	ggsave(plot,file='depth_pdf_line_log.eps')
	setwd(prev_dir)
}

odeg_corr <- function(file_name){
	library(ggplot2)
	library(plyr)
	odeg <- as.data.frame(read.csv(file_name, header=FALSE))
	colnames(odeg) <- c('top_user_odeg', 'neighbour_odeg')
	odeg.df <- ddply(odeg, c('top_user_odeg'), summarise, avg_neighbour_odeg = mean(neighbour_odeg), total_neighbour_odeg = sum(neighbour_odeg))
	plot <- ggplot(odeg.df, aes(x = top_user_odeg, y = avg_neighbour_odeg)) + xlim(0,100) + geom_point() + geom_smooth(method=lm)
	ggsave(plot,file=paste(file_name,'_avg_corr.eps'))
	print(cor(odeg.df$top_user_odeg,odeg.df$avg_neighbour_odeg))
	plot <- ggplot(odeg.df, aes(x = top_user_odeg, y = total_neighbour_odeg)) + xlim(0,100) + geom_point() + geom_smooth(method=lm)
	ggsave(plot,file=paste(file_name,'_total_corr.eps'))
	print(cor(odeg.df$top_user_odeg,odeg.df$total_neighbour_odeg))
	print (head(odeg.df))
}

depth_vs_expansion <- function(file_name){
	library(ggplot2)
	library(plyr)
	depth_expanstion <- as.data.frame(read.csv(file_name, header=FALSE))
	colnames(depth_expanstion) <- c('depth', 'expansion', 'user_id')
	depth_expanstion$user_id <- factor(depth_expanstion$user_id)
	plot <- ggplot(depth_expanstion, aes(x = depth, y = expansion)) + geom_line(aes(group = user_id,colour = user_id))
	ggsave(plot,file=paste(file_name,'_depth_expansion.eps'))
	
}


parent_type_size_depth <- function(directoryname){
	prev_dir = getwd()
	setwd(directoryname)
	sub_cascade <- c()
	cascade_size <- as.data.frame(read.csv('size.csv', header=FALSE))
	colnames(cascade_size) <- c('size', 'count', 'threshold')
	sub_cascade$size <- cascade_size[cascade_size$threshold == max(cascade_size$threshold),]
	cascade_depth <- as.data.frame(read.csv('depth.csv', header=FALSE))
	colnames(cascade_depth) <- c('depth', 'count', 'threshold')
	sub_cascade$depth <- cascade_depth[cascade_depth$threshold == max(cascade_depth$threshold),]
	setwd(prev_dir)
	return(sub_cascade)
}

parent_type_comp <- function(dir_vector){
	library(ggplot2)
	library(plyr)
	cascade_comp <- c()
	cascade_comp$size <- c()
	cascade_comp$depth <- c()
	for (dir in dir_vector){
		each_sub_cascade <- parent_type_size_depth(dir)
		each_sub_cascade$size[,4] <- dir
		cascade_comp$size <- rbind(cascade_comp$size,each_sub_cascade$size)
		each_sub_cascade$depth[,4] <- dir
		cascade_comp$depth <- rbind(cascade_comp$depth,each_sub_cascade$depth)
	}
	colnames(cascade_comp$size) <- c('size', 'count', 'threshold', 'parent_type')
	colnames(cascade_comp$depth) <- c('depth', 'count', 'threshold', 'parent_type')

#	cascade_comp.size <- ddply(cascade_comp$size, c('parent_type'), function(one_partition){
#				one_partition <- data.frame(fac = rep(cascade_comp$size$size, times = cascade_comp$size$count))
#				one_partition
#			})
#	print (head(cascade_comp.size))
	cascade_comp$size$parent_type <- factor(cascade_comp$size$parent_type)
	plot <- ggplot(cascade_comp$size, aes(y=log10(size), x = parent_type)) + geom_boxplot()
	ggsave(plot,file='parent_type_size_boxplot.eps')
	
	cascade_comp$depth$parent_type <- factor(cascade_comp$depth$parent_type)
	plot <- ggplot(cascade_comp$depth, aes(y=depth, x = parent_type)) + geom_boxplot()
	ggsave(plot,file='parent_type_depth_boxplot.eps')

	return(cascade_comp)
}