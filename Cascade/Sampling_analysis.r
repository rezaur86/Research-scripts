source('~/scripts/Cascade/tools.r')
library(ggplot2)
library(gtable)
library(grid)
library(plyr)
library(Hmisc)
library(gridExtra) 

load_size_depth <- function(directoryname){
	prev_dir = getwd()
	setwd(directoryname)
	sub_cascade <- c()
	cascade_size <- as.data.frame(read.csv('size.csv', header=FALSE))
	colnames(cascade_size) <- c('size', 'count', 'threshold')
	sub_cascade$size <- cascade_size#[cascade_size$threshold == max(cascade_size$threshold),]
	cascade_depth <- as.data.frame(read.csv('depth.csv', header=FALSE))
	colnames(cascade_depth) <- c('depth', 'count', 'threshold')
	sub_cascade$depth <- cascade_depth#[cascade_depth$threshold == max(cascade_depth$threshold),]
	setwd(prev_dir)
	return(sub_cascade)
}

#comp <- sampling_comp(c('fp_nt_u/','sample_RN/','sample_RDN/','sample_RE/','sample_RNE/','sample_RNN/'),c('Actual','RN','RDN','RE','RNE','RNN'))
sampling_comp <- function(dir_vector, parent_type_vector){
	plot_x_lim <- 100
	cascade_comp <- c()
	cascade_comp$size <- c()
	cascade_comp$depth <- c()
	parent_type_idx <- 0
	for (dir in dir_vector){
		parent_based_cascade <- load_size_depth(dir)
		parent_based_cascade$size[,4] <- parent_type_idx
		cascade_comp$size <- rbind(cascade_comp$size,parent_based_cascade$size)
		parent_based_cascade$depth[,4] <- parent_type_idx
		cascade_comp$depth <- rbind(cascade_comp$depth,parent_based_cascade$depth)
		parent_type_idx <- parent_type_idx + 1
	}
	colnames(cascade_comp$size) <- c('size', 'count', 'threshold', 'parent_type')
	colnames(cascade_comp$depth) <- c('depth', 'count', 'threshold', 'parent_type')
	##	Cascade size summary
	size_freq <- data.frame(size = rep(cascade_comp$size$size, times = cascade_comp$size$count), parent_type=rep(cascade_comp$size$parent_type, times = cascade_comp$size$count))
#	Cascade size box plot
	size_freq$parent_type <- factor(size_freq$parent_type)
	plot <- ggplot(size_freq, aes(y=(size), x=parent_type))+ geom_boxplot() + scale_x_discrete(breaks=0:(parent_type_idx-1), labels=parent_type_vector)+ scale_y_log10() # + geom_histogram(binwidth=0.2, position="dodge")
	plot <- change_plot_attributes_fancy(plot, "Sampling Method", 0:(parent_type_idx-1), parent_type_vector, "Parent Type", "Cascade Size")
	save_ggplot(plot,file='sample_comp/size_box.pdf')
#	Cascade size detail
	cascade_comp$size <- ddply(cascade_comp$size, c('threshold','parent_type'), function(one_partition){
				one_partition = one_partition[order(one_partition$size),]
				one_partition$cum_count = cumsum(one_partition$count)
				one_partition$cum_size = cumsum(one_partition$size)
				one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
				one_partition$pdf_val = one_partition$count / max(one_partition$cum_count)
				one_partition
			})
	cascade_comp$size$parent_type <- factor(cascade_comp$size$parent_type)
	plot <- ggplot(cascade_comp$size, aes(x = (size), y = (pdf_val))) + 
			geom_point(aes(group = parent_type, colour = parent_type, shape = parent_type), size=1)+
			scale_x_log10()+ scale_y_log10() #+ theme(legend.position=c(.8, .7)) + xlim(0,log10(plot_x_lim*100))
	plot <- change_plot_attributes_fancy(plot, "Sampling Method", 0:(parent_type_idx-1), parent_type_vector, "Cascade Size", "Empirical PDF")
	save_ggplot(plot,file='sample_comp/size_pdf_logx_logy.pdf')
#	Cascade depth summary
	depth_freq <- data.frame(depth = rep(cascade_comp$depth$depth, times = cascade_comp$depth$count), parent_type=rep(cascade_comp$depth$parent_type, times = cascade_comp$depth$count))
	cascade_comp$depth_summary <- ddply(depth_freq, c('parent_type'), summarise, min=summary(depth)[1], quart_1=summary(depth)[2], median=summary(depth)[3], mean=summary(depth)[4],
			quart_3=summary(depth)[5], max=summary(depth)[6], var=var(depth))
#	Cascade depth box plot
	depth_freq$parent_type <- factor(depth_freq$parent_type)
	plot <- ggplot(depth_freq, aes(y=depth, x=parent_type))+ scale_y_log10()+ geom_boxplot() #+ geom_histogram(binwidth=1, position="dodge") 
	plot <- change_plot_attributes_fancy(plot, "Sampling Method", 0:(parent_type_idx-1), parent_type_vector, "Parent type", "Cascade Depth")
	save_ggplot(plot,file='sample_comp/depth_box.pdf')
#	Cascade depth detail
	cascade_comp$depth <- ddply(cascade_comp$depth, c('threshold','parent_type'), function(one_partition){
				one_partition = one_partition[order(one_partition$depth),]
				one_partition$cum_count = cumsum(one_partition$count)
				one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
				one_partition$pdf_val = one_partition$count / max(one_partition$cum_count)
				one_partition
			})
	cascade_comp$depth$parent_type <- factor(cascade_comp$depth$parent_type)
	plot <- ggplot(cascade_comp$depth,aes(x = depth, y = (pdf_val))) + geom_point(aes(group = parent_type,colour = parent_type, shape = parent_type)) + 
			scale_y_log10() #+ theme(legend.position=c(.8, .7))
	plot <- change_plot_attributes_fancy(plot, "Sampling Method", 0:(parent_type_idx-1), parent_type_vector,  "Cascade Depth", "Empirical PDF")
	save_ggplot(plot,file='sample_comp/depth_pdf_logy.pdf')
	plot <- ggplot(cascade_comp$depth,aes(x = depth, y = (pdf_val))) + geom_point(aes(group = parent_type,colour = parent_type)) + scale_x_log10() + scale_y_log10()
	plot <- change_plot_attributes_fancy(plot, "Sampling Method", 0:(parent_type_idx-1), parent_type_vector,  "Cascade Depth", "Proportion")
	save_ggplot(plot,file='sample_comp/depth_pdf_log_log.pdf')
	size_null <- data.frame(size = rep(cascade_comp$size[cascade_comp$size$parent_type==0,]$size,
					times = cascade_comp$size[cascade_comp$size$parent_type==0,]$count))
	i <- 1
	while (i < parent_type_idx){
		size_sampled <- data.frame(size = rep(cascade_comp$size[cascade_comp$size$parent_type==i,]$size,
						times = cascade_comp$size[cascade_comp$size$parent_type==i,]$count))
		print_report('KS-Test for Cascade Size with', parent_type_vector[i+1])
		print (ks.test(size_sampled$size, size_null$size))
		i <- i + 1
	}
	depth_null <- data.frame(depth = rep(cascade_comp$depth[cascade_comp$depth$parent_type==0,]$depth,
					times = cascade_comp$depth[cascade_comp$depth$parent_type==0,]$count))
	i <- 1
	while (i < parent_type_idx){
		depth_sampled <- data.frame(depth = rep(cascade_comp$depth[cascade_comp$depth$parent_type==i,]$depth,
						times = cascade_comp$depth[cascade_comp$depth$parent_type==i,]$count))
		print_report('KS-Test for Cascade Depth with', parent_type_vector[i+1])
		print (ks.test(depth_sampled$depth, depth_null$depth))
		i <- i + 1
	}
	return(cascade_comp)
}

#node_info_null <<- as.data.frame(read.csv('../data/iheart_ext_preprocessed_basic.txt', header=FALSE))
#colnames(node_info_null) <- c('id', 'outdeg', 'indeg', 'deg')

sampling_node_basic_comp <- function(node_info_sample_file){
	node_info_sample <- as.data.frame(read.csv(node_info_sample_file, header=FALSE))
	colnames(node_info_sample) <- c('id', 'outdeg', 'indeg', 'deg', 'act_life')
	print('KS-Test for out degree distribution')
	print (ks.test(node_info_sample$outdeg, node_info_null$outdeg))
	print('KS-Test for in degree distribution')
	print (ks.test(node_info_sample$indeg, node_info_null$indeg))
}

#sampling_node_basic_comp('../data/iheart_prep_sample_RE_basic.txt')
#sampling_node_basic_comp('../data/iheart_prep_sample_RNE_basic.txt')
#sampling_node_basic_comp('../data/iheart_prep_sample_RNN_basic.txt')
