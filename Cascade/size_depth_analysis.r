source('~/scripts/Cascade/tools.r')
#source('~/scripts/Cascade/plfit.r')
library(ggplot2)
library(gtable)
library(grid)
library(plyr)
library(Hmisc)
library(gridExtra) 
library(nnet)
library(pROC)

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

parent_type_comp <- function(dir_vector, parent_type_vector){
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
#	print_report('First Parent: Power law alpha', plfit(size_freq[size_freq$parent_type==0 & size_freq$size<1000,]$size))
#	print_report('Last Parent: Power law alpha', plfit(size_freq[size_freq$parent_type==1 & size_freq$size<1000,]$size))
#	print_report('HOP Parent: Power law alpha', plfit(size_freq[size_freq$parent_type==2 & size_freq$size<1000,]$size))
#	print_report('Random Parent: Power law alpha', plfit(size_freq[size_freq$parent_type==3 & size_freq$size<1000,]$size))
#	print(kruskal.test(size~parent_type, data=size_freq))
#	cascade_comp$size_summary <- ddply(size_freq, c('parent_type'), summarise, min=summary(size)[1], quart_1=summary(size)[2], median=summary(size)[3], mean=summary(size)[4],
#			quart_3=summary(size)[5], max=summary(size)[6], var=var(size), total_coverage=sum(size))
#	Cascade size box plot
	size_freq$parent_type <- factor(size_freq$parent_type)
	plot <- ggplot(size_freq, aes(y=(size), x=parent_type))+ geom_boxplot() + scale_x_discrete(breaks=0:(parent_type_idx-1), labels=parent_type_vector)+ scale_y_log10() # + geom_histogram(binwidth=0.2, position="dodge")
	plot <- change_plot_attributes(plot, "Parent Type", 0:(parent_type_idx-1), parent_type_vector, "Parent Type", "Cascade Size")
	save_ggplot(plot,file='parent_comp/size_box.pdf')
#	Cascade size detail
	sample_plot <- function(colum_to_chose, x_above_limit, every=100){
		chosen <- rep(0,length(colum_to_chose))
		i <- 1
		while (i <= length(colum_to_chose)){
			if (colum_to_chose[i] <= x_above_limit){
				chosen[i] <- 1
				i <- i + 1
			}
			if (colum_to_chose[i] > x_above_limit & colum_to_chose[i] <= 80){
				chosen[i + sample(c(0:min(1, length(colum_to_chose)-i)), 1, replace=TRUE)] <- 1
				i <- i + 2
			}
			if (colum_to_chose[i] > 80 & colum_to_chose[i] <= 350){
				chosen[i + sample(c(0:min(19, length(colum_to_chose)-i)), 1, replace=TRUE)] <- 1
				i <- i + 10
			}
			if (colum_to_chose[i] > 350 & colum_to_chose[i] <= 10^4.5){
				chosen[i + sample(c(0:min(99, length(colum_to_chose)-i)), 1, replace=TRUE)] <- 1
				i <- i + 100
			}
			if (colum_to_chose[i] > 10^4.5){
				chosen[i] <- 1
				i <- i + 1
			}
		}
		return(chosen)
	}
	cascade_comp$size <- ddply(cascade_comp$size, c('threshold','parent_type'), function(one_partition){
				one_partition = one_partition[order(one_partition$size),]
				one_partition$cum_count = cumsum(one_partition$count)
				one_partition$cum_size = cumsum(one_partition$size)
				one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
				one_partition$pdf_val = one_partition$count / max(one_partition$cum_count)
				one_partition$chosen = sample_plot(one_partition$size, 10)
				one_partition
			})
	cascade_comp$size$parent_type <- factor(cascade_comp$size$parent_type)
	plot <- ggplot(cascade_comp$size[cascade_comp$size$chosen==1,],aes(x = (size), y = (pdf_val))) + 
			geom_point(aes(group = parent_type, colour = parent_type, shape = parent_type), size=1)+
			scale_x_log10()+ scale_y_log10() #+ theme(legend.position=c(.8, .7)) + xlim(0,log10(plot_x_lim*100))
	plot <- change_plot_attributes(plot, "Parent Type", 0:(parent_type_idx-1), parent_type_vector, "Cascade Size", "Empirical PDF")
	save_ggplot(plot,file='parent_comp/size_pdf_logx_logy.pdf')
#	Cascade depth summary
	depth_freq <- data.frame(depth = rep(cascade_comp$depth$depth, times = cascade_comp$depth$count), parent_type=rep(cascade_comp$depth$parent_type, times = cascade_comp$depth$count))
	cascade_comp$depth_summary <- ddply(depth_freq, c('parent_type'), summarise, min=summary(depth)[1], quart_1=summary(depth)[2], median=summary(depth)[3], mean=summary(depth)[4],
			quart_3=summary(depth)[5], max=summary(depth)[6], var=var(depth))
#	Cascade depth box plot
	depth_freq$parent_type <- factor(depth_freq$parent_type)
	plot <- ggplot(depth_freq, aes(y=depth, x=parent_type))+ scale_y_log10()+ geom_boxplot() #+ geom_histogram(binwidth=1, position="dodge") 
	plot <- change_plot_attributes(plot, "Parent Type", 0:(parent_type_idx-1), parent_type_vector, "Parent type", "Cascade Depth")
	save_ggplot(plot,file='parent_comp/depth_box.pdf')
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
	plot <- change_plot_attributes(plot, "Parent Type", 0:(parent_type_idx-1), parent_type_vector,  "Cascade Depth", "Empirical PDF")
	save_ggplot(plot,file='parent_comp/depth_pdf_logy.pdf')
	plot <- ggplot(cascade_comp$depth,aes(x = depth, y = (pdf_val))) + geom_point(aes(group = parent_type,colour = parent_type)) + scale_x_log10() + scale_y_log10()
	plot <- change_plot_attributes(plot, "Parent Type", 0:(parent_type_idx-1), parent_type_vector,  "Cascade Depth", "Proportion")
	save_ggplot(plot,file='parent_comp/depth_pdf_log_log.pdf')
	d <- cascade_comp$size
	latex(d, file="parent_comp/comp_size.tex", digits = 6, rowname = latexTranslate(rownames(d)))            # If you want all the data
	d <- cascade_comp$depth
	latex(d, file="parent_comp/comp_depth.tex", digits = 6, rowname = latexTranslate(rownames(d)))            # If you want all the data
#	latex(describe(d), file="comp_summary.lex")  # If you just want a summary
#	print('Exiting, Done')
	return(cascade_comp)
}

#parent_type_comp(c('fp_nt_u/','lp_nt_u/','hop_nt_u/','rp_nt_u/'),c('FP','LP','HODP', 'RP'))

parent_lifespan_comp <- function(dir_vector, lifespan_threshold_vector, hub_sec='Hub Seclusion Effect'){
	cascade_comp <- c()
	cascade_comp$size <- c()
	cascade_comp$depth <- c()
	lifespan_idx <- 1
	for (dir in dir_vector){
		parent_based_cascade <- load_size_depth(dir)
		parent_based_cascade$size[,4] <- lifespan_idx
		cascade_comp$size <- rbind(cascade_comp$size,parent_based_cascade$size)
		parent_based_cascade$depth[,4] <- lifespan_idx
		cascade_comp$depth <- rbind(cascade_comp$depth,parent_based_cascade$depth)
		lifespan_idx <- lifespan_idx + 1
	}
	colnames(cascade_comp$size) <- c('size', 'count', 'threshold', 'lifespan_threshold')
	colnames(cascade_comp$depth) <- c('depth', 'count', 'threshold', 'lifespan_threshold')
#	Cascade size summary
	size_freq <- data.frame(size = rep(cascade_comp$size$size, times = cascade_comp$size$count), lifespan_threshold=rep(cascade_comp$size$lifespan_threshold, times = cascade_comp$size$count))
	cascade_comp$size_summary <- ddply(size_freq, c('lifespan_threshold'), summarise, min=summary(size)[1], quart_1=summary(size)[2], median=summary(size)[3], mean=summary(size)[4],
			quart_3=summary(size)[5], max=summary(size)[6], var=var(size),total_coverage=sum(size))
#	Cascade size detail
	sample_plot <- function(colum_to_chose, x_above_limit, every=100){
		chosen <- rep(0,length(colum_to_chose))
		i <- 1
		while (i <= length(colum_to_chose)){
			if (colum_to_chose[i] <= x_above_limit){
				chosen[i] <- 1
				i <- i + 1
			}
			else if (colum_to_chose[i] > x_above_limit & colum_to_chose[i] <= 80){
				chosen[i + sample(c(0:min(1, length(colum_to_chose)-i)), 1, replace=TRUE)] <- 1
				i <- i + 2
			}
			else if (colum_to_chose[i] > 80 & colum_to_chose[i] <= 350){
				chosen[i + sample(c(0:min(19, length(colum_to_chose)-i)), 1, replace=TRUE)] <- 1
				i <- i + 10
			}
			else if (colum_to_chose[i] > 350 & colum_to_chose[i] <= 10^4.5){
				chosen[i + sample(c(0:min(99, length(colum_to_chose)-i)), 1, replace=TRUE)] <- 1
				i <- i + 100
			}
			else if (colum_to_chose[i] < 10^6.5){
				chosen[i] <- 1
				i <- i + 1
			}
			else {
				break
			}
		}
		return(chosen)
	}
	cascade_comp$size <- ddply(cascade_comp$size, c('threshold','lifespan_threshold'), function(one_partition){
				one_partition = one_partition[order(one_partition$size),]
				one_partition$cum_count = cumsum(one_partition$count)
				one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
				one_partition$pdf_val = one_partition$count / max(one_partition$cum_count)
				one_partition$chosen = sample_plot(one_partition$size, 10)
				one_partition
			})
	cascade_comp$size$lifespan_threshold <- factor(cascade_comp$size$lifespan_threshold)
	plot <- ggplot(cascade_comp$size[cascade_comp$size$chosen==1,],aes(x = (size), y = (pdf_val))) +
			geom_point(aes(group = lifespan_threshold,colour = lifespan_threshold, shape = lifespan_threshold), size=1) +
			scale_x_log10() + scale_y_log10() #+ theme(legend.position=c(.8, .7)) #+ xlim(2,3)+ ylim(-6,-3.5)
#			scale_x_reverse(limits = c(log10(max(cascade_comp$size$size)), 4)) + scale_y_log10()
	plot <- change_plot_attributes(plot, hub_sec, 1:(lifespan_idx-1),lifespan_threshold_vector, "Cascade Size", "Empirical PDF")
	save_ggplot(plot,file='lifespan_comp/size_pdf.pdf')
#	Log binned ploting of distribution
	cascade_comp$size$lifespan_threshold <- factor(cascade_comp$size$lifespan_threshold)
	max_size <- max(cascade_comp$size$size)
	size_bin <- unique(ceiling(2^(seq(0,ceiling(log(max_size)/log(2)),by=0.25)))) # Suitable bin finding
	print(size_bin)
	cascade_comp$size <- transform(cascade_comp$size, bin = cut(cascade_comp$size$size, breaks=size_bin, right=FALSE))
	cascade_comp$size <- ddply(cascade_comp$size, c('bin','lifespan_threshold'), summarise, avg_size=mean(size), pdf=mean(pdf_val))
	colnames(cascade_comp$size) <- c('bin', 'lifespan_threshold', 'size', 'pdf_val')
	cascade_comp$size$lifespan_threshold <- factor(cascade_comp$size$lifespan_threshold)
	plot <- ggplot(cascade_comp$size,aes(x = (size), y = (pdf_val))) + geom_point(aes(group = lifespan_threshold,colour = lifespan_threshold)) + scale_x_log10() + scale_y_log10() #+ xlim(2,3)+ ylim(-6,-3.5)#
	#		scale_x_reverse(limits = c(log10(max(cascade_comp$size$size)), 4)) + scale_y_log10()
	plot <- change_plot_attributes(plot, hub_sec, 1:(lifespan_idx-1),lifespan_threshold_vector, "Cascade Size", "Empirical PDF")
	returned_plot <- save_ggplot(plot,file='lifespan_comp/size_pdf_binned.pdf')
	
#	Cascade depth summary
	depth_freq <- data.frame(depth = rep(cascade_comp$depth$depth, times = cascade_comp$depth$count), lifespan_threshold=rep(cascade_comp$depth$lifespan_threshold, times = cascade_comp$depth$count))
	cascade_comp$depth_summary <- ddply(depth_freq, c('lifespan_threshold'), summarise, min=summary(depth)[1], quart_1=summary(depth)[2], median=summary(depth)[3], mean=summary(depth)[4],
			quart_3=summary(depth)[5], max=summary(depth)[6], var=var(depth))
#	Cascade detail data frame
	cascade_comp$depth <- ddply(cascade_comp$depth, c('threshold','lifespan_threshold'), function(one_partition){
				one_partition = one_partition[order(one_partition$depth),]
				one_partition$cum_count = cumsum(one_partition$count)
				one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
				one_partition$pdf_val = one_partition$count / max(one_partition$cum_count)
				one_partition
			})
	cascade_comp$depth$lifespan_threshold <- factor(cascade_comp$depth$lifespan_threshold)
	plot <- ggplot(cascade_comp$depth,aes(x = depth, y = pdf_val)) + geom_line(aes(group = lifespan_threshold,colour = lifespan_threshold)) + scale_x_log10() + scale_y_log10()
	plot <- change_plot_attributes(plot, "Hub Seclusion effect", 1:(lifespan_idx-1), lifespan_threshold_vector, "Cascade Depth", "Proportion of count")
	save_ggplot(plot,file='lifespan_comp/depth_pdf.pdf')
#	Loss comparison
	loss_comp <- as.data.frame(read.csv('lifespan_comp/loss_ratio.csv', header=FALSE))
	colnames(loss_comp) <- c('Threshold_name', 'Threshold', 'Total_coverage', 'Short_lived_Parents', 'Non_leaves', 'Leaves', 'Percent_removed_parent', 'Percent_loss','Percent_removed')
	plot <- ggplot(loss_comp, aes(x=Threshold, y=(1-Total_coverage/189989307))) + geom_line() + xlab('Removed Parents(%)') + ylab('Total Size Loss(%)') + 
			scale_x_log10(breaks=loss_comp$Threshold, labels=loss_comp$Percent_removed_parent) + 
			scale_y_continuous(breaks=(1-loss_comp$Total_coverage/189989307), labels=loss_comp$Percent_loss) +
			opts(axis.text.x=theme_text(angle=45,hjust=1,vjust=1))
	save_ggplot(plot,file='lifespan_comp/loss_vs_removed_parents.pdf')
	plot <- ggplot(loss_comp, aes(x=(Non_leaves+Leaves)/189989307, y=(1-Total_coverage/189989307))) + geom_line() + xlab('Directly Removed Nodes(%)') + ylab('Total Size Loss(%)') + 
			scale_x_continuous(breaks=(loss_comp$Non_leaves+loss_comp$Leaves)/189989307, labels=loss_comp$Percent_removed) + 
			scale_y_continuous(breaks=(1-loss_comp$Total_coverage/189989307), labels=loss_comp$Percent_loss) +
			opts(axis.text.x=theme_text(angle=45,hjust=1,vjust=1))
	save_ggplot(plot,file='lifespan_comp/loss_vs_removed.pdf')
	
	return (returned_plot)
}

analyze_coverage <- function(dir){
	cascade <<- load_size_depth(dir)
	cascade$size.df <<- ddply(cascade$size, c('threshold'), function(one_partition){
				one_partition = one_partition[order(-one_partition$size),]
				one_partition$cum_count = cumsum(one_partition$count)
				one_partition$cum_size = cumsum(one_partition$size*one_partition$count)/sum(one_partition$size*one_partition$count)
				one_partition
			})
	x_label <- c(10^(0:4),115000,10^6)/3734781
	plot <- ggplot(cascade$size.df,aes(x = (cum_count), y = (cum_size))) + geom_line()+ scale_x_log10(breaks=c(1,10,10^2,10^3,10^4,10^5,10^6),#,115000,10^6,3734781),
#					labels=c('1\n0%','1e+1\n2.7e-4%	','1e+2\n.0027%','1e+3\n.027%','1e+4\n.27%','115000\n3.08%','1e+6\n26.78%','3734781\n100%'))+
					labels=c('1\n','1e+1','1e+2','1e+3','1e+4','1e+5','1e+6'))+
			scale_y_continuous(breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0), labels=c('0','10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))+
			geom_segment(aes(x = 115000, y = 0, xend = 115000, yend = 0.8), arrow = arrow(length = unit(0.3, "cm")))+
			geom_text(aes(100000*4, .4, label="Top 3.08% of\ncascades"), size=8)+
			geom_segment(aes(x = 115000, y = 0.8, xend = 0, yend = 0.8), arrow = arrow(length = unit(0.3, "cm")))+
			geom_text(aes(15, .85, label="80% population"), size=8)
	
	plot <- change_plot_attributes(plot, "Parent type", 0:(4-1), 0:(4-1), "Cascades Ranked by Size (from Largest to Smallest)", "Coverage (Percentage of Total User Population)")
	prev_dir = getwd()
	setwd(dir)
	save_ggplot(plot,file='size_coverage.pdf')
	setwd(prev_dir)
}

size_distribution <- function(file_name='iheart_cascade/size.csv'){
	cascade_size <- as.data.frame(read.csv(file_name, header=FALSE))
	colnames(cascade_size) <- c('size', 'count')
	cascade_size <- cascade_size[cascade_size$size > 1, ]
	cascade_size$cum_count <- cumsum(cascade_size$count)
	cascade_size$pdf <- cascade_size$count / max(cascade_size$cum_count)
	plot <- ggplot(cascade_size, aes(x = size, y = pdf)) + geom_point(size=0.8) + xlab('Size') + ylab('Empirical PDF') +
			scale_x_log10() + scale_y_log10()
	save_ggplot(plot, 'iheart_cascade/size.pdf')
	return(cascade_size)
}
depth_distribution <- function(file_name='iheart_cascade/depth.csv'){
	cascade_depth <- as.data.frame(read.csv(file_name, header=FALSE))
	colnames(cascade_depth) <- c('depth', 'count')
	cascade_depth <- cascade_depth[cascade_depth$depth > 0, ]
	cascade_depth$cum_count <- cumsum(cascade_depth$count)
	cascade_depth$pdf <- cascade_depth$count / max(cascade_depth$cum_count)
	plot <- ggplot(cascade_depth, aes(x = depth, y = pdf)) + geom_point() + xlab('Max depth') + ylab('Empirical PDF') +
			scale_y_log10()
	save_ggplot(plot, 'iheart_cascade/depth.pdf')
	return(cascade_depth)
}
width_distribution <- function(file_name='iheart_cascade/top_size.csv_all__max_width.csv'){
	cascade_width <- as.data.frame(read.csv(file_name, header=FALSE))
	colnames(cascade_width) <- c('width', 'count')
	cascade_width$cum_count <- cumsum(cascade_width$count)
	cascade_width$pdf <- cascade_width$count / max(cascade_width$cum_count)
	plot <- ggplot(cascade_width, aes(x = width, y = pdf)) + geom_point(size=0.8) + xlab('Max width') + ylab('Empirical PDF') +
			scale_x_log10() + scale_y_log10()
	save_ggplot(plot, 'iheart_cascade/width.pdf')
	return(cascade_width)
}

size_vs_properties <- function(file='iheart_cascade/top_size.csv_all_evolution.csv'){
	evolution <- as.data.frame(read.csv(file, header=FALSE))
	colnames(evolution) <- c('root', 'size', 'depth', 'width', 'first_day', 'last_day', 'burstiness')
	size_vs_prop <- ddply(evolution, c('size'), summarise, avg_depth = mean(depth), avg_width = mean(width))
	plot <- ggplot(size_vs_prop, aes(x = size, y = avg_depth)) + geom_point() + xlab('Size') + ylab('Avg. depth') +
			scale_x_log10() #+ geom_smooth(method=lm, se=FALSE)
	save_ggplot(plot, 'iheart_cascade/size_vs_depth.pdf')
	plot <- ggplot(size_vs_prop, aes(x = size, y = avg_width)) + geom_point() + xlab('Size') + ylab('Avg. width') +
			scale_x_log10() + scale_y_log10() #+ geom_smooth(method=lm, se=FALSE)
	save_ggplot(plot, 'iheart_cascade/size_vs_width.pdf')
	return(size_vs_prop)
}

size_vs_root_contribution <- function(file='iheart_gift/size_vs_root.csv'){
	size_vs_root <- unique(as.data.frame(read.csv(file, header=FALSE)))
	colnames(size_vs_root) <- c('root', 'size', 'depth' ,'width', 'major_gift',
			'root_act_lifespan', 'root_outdeg', 'root_contribution' , 'root_success_ratio', 'rn_contr')
	size_vs_root <- size_vs_root[size_vs_root$size > 1, ]
	size_vs_root$contribution_ratio <- round(size_vs_root$root_contribution/ size_vs_root$size, 3)
	size_vs_root$rn_contr_ratio <- round(size_vs_root$rn_contr/ size_vs_root$size, 3)
	contr.df <- as.data.frame(table(size_vs_root$contribution_ratio))
	colnames(contr.df) <- c('ratio','count')
	contr.df$ratio <- as.numeric(levels(contr.df$ratio))[contr.df$ratio]
	contr.df$cum_count <- cumsum(contr.df$count)
	contr.df$cdf <- contr.df$cum_count/max(contr.df$cum_count)
	plot <- ggplot(contr.df, aes(x = ratio, y = cdf)) + geom_point() + xlab('Contribution ratio of seeds') + ylab('Empirical CDF') #+scale_x_log10()
	save_ggplot(plot, 'iheart_cascade/seed_contribution_ratio.pdf')
	size_vs_root.df <- ddply(size_vs_root, c('contribution_ratio'), summarise, avg_size = mean(size))
	plot <- ggplot(size_vs_root.df, aes(x = contribution_ratio, y = avg_size)) + geom_point() +
			xlab('Contribution ratio of seeds') + ylab('Avg. size') + scale_y_log10()
	save_ggplot(plot, 'iheart_cascade/seed_contribution_vs_size.pdf')
	size_vs_root.df <- ddply(size_vs_root, c('rn_contr_ratio'), summarise, avg_size = mean(size))
	plot <- ggplot(size_vs_root.df, aes(x = rn_contr_ratio, y = avg_size)) + geom_point() +
			xlab('Contribution ratio of seeds\' neighbors') + ylab('Avg. size') + scale_y_log10()
	save_ggplot(plot, 'iheart_cascade/seed_rn_contr_vs_size.pdf')
	size_vs_root.df <- ddply(size_vs_root, c('root_outdeg'), summarise, avg_size = mean(size))
	plot <- ggplot(size_vs_root.df, aes(x = root_outdeg, y = avg_size)) + geom_point() +
			xlab('Number of ARs sent by Seeds') + ylab('Avg. size') + scale_y_log10() + scale_x_log10()
	save_ggplot(plot, 'iheart_cascade/seed_ARs_vs_size.pdf')
	size_vs_root.df <- ddply(size_vs_root, c('root_success_ratio'), summarise, avg_size = mean(size))
	plot <- ggplot(size_vs_root.df, aes(x = root_success_ratio, y = avg_size)) + geom_point() +
			xlab('Success ratio of seeds') + ylab('Avg. size') + scale_y_log10()
	save_ggplot(plot, 'iheart_cascade/seed_success_ratio_vs_size.pdf')
	size_vs_root.df <- ddply(size_vs_root, c('major_gift'), summarise, avg_size = mean(size))
	plot <- ggplot(size_vs_root.df, aes(x = major_gift, y = avg_size)) + geom_point() +
			xlab('Major gift types') + ylab('Avg. size') + scale_y_log10()
	save_ggplot(plot, 'iheart_cascade/major_gift_vs_size.pdf')
	size_vs_root.df <- ddply(size_vs_root, c('major_gift'), summarise, avg_depth = mean(depth))
	plot <- ggplot(size_vs_root.df, aes(x = major_gift, y = avg_depth)) + geom_point() +
			xlab('Major gift types') + ylab('Avg. depth')
	save_ggplot(plot, 'iheart_cascade/major_gift_vs_depth.pdf')
	size_vs_root.df <- ddply(size_vs_root, c('major_gift'), summarise, avg_width = mean(width))
	plot <- ggplot(size_vs_root.df, aes(x = major_gift, y = avg_width)) + geom_point() +
			xlab('Major gift types') + ylab('Avg. width')
	save_ggplot(plot, 'iheart_cascade/major_gift_vs_width.pdf')
	return(size_vs_root)
}

size_vs_root_characteristics <- function (dir){
	prev_dir = getwd()
	setwd(dir)
	size_vs_root <- unique(as.data.frame(read.csv('size_vs_root.csv', header=FALSE)))
	colnames(size_vs_root) <- c('size','depth','lifespan','raw_root_outdeg')
	print('read 1')
	size_vs_root_odeg <- unique(as.data.frame(read.csv('size_vs_root_odeg.csv', header=FALSE)))
	colnames(size_vs_root_odeg) <- c('size', 'root_outdeg')
	print('read 2')
	plot1 <- ggplot(size_vs_root[size_vs_root$size>=156,], aes(x=depth, y=size))+ geom_point(size=.5)+ scale_y_log10() + #+ geom_smooth(method=lm)
			xlab("Reach of Influece of a Seed (Max Tree Depth)")+ylab("Size of the Adoption\nRooted at the Seed") + myPlotTheme(12)
	plot2 <- ggplot(size_vs_root[size_vs_root$size>=156,], aes(x=lifespan, y=size))+ geom_point(size=.5)+ scale_y_log10() + #+ geom_smooth(method=lm)
			scale_x_log10(breaks=c(86400,86400*7,2*86400*7,4*86400*7,8*86400*7,16*86400*7,32*86400*7,64*86400*7),labels=c('1day','1week','2week','4week','8week','16week','32week','64week'))+
			myPlotTheme(12) + opts(axis.text.x=theme_text(angle=45,hjust=1,vjust=1))+
			xlab("Active Lifetime of a Seed")+ylab("Size of the Adoption\nRooted at the Seed")
	plot3 <- ggplot(size_vs_root_odeg[size_vs_root_odeg$size>=156,], aes(x=root_outdeg, y=size))+ geom_point(size=.5)+ scale_y_log10() +
			scale_x_log10(breaks=c(10,100,1000), labels=c('10','100','1000')) + #+ geom_smooth(method=lm)
			xlab("Out-degree of a Seed")+ylab("Size of the Adoption\nRooted at the Seed") + myPlotTheme(12)
	print('ploted')
	pdf("size_vs_root.pdf")
	grid.arrange(plot1, plot2, plot3)
	dev.off()
	setwd(prev_dir)
	return (list(info=size_vs_root,odeg=size_vs_root_odeg))
}


cascade_lm_model <- function(file='iheart_gift/size_vs_root.csv',
		evolution_file='iheart_gift/top_size.csv_all_evolution.csv',
		growth_file = 'iheart_gift/top_size.csv_all_weekly_evolution.csv'){
	size_vs_root <- as.data.frame(read.csv(file, header=FALSE))
	colnames(size_vs_root) <- c('root', 'size', 'depth' ,'width', 'major_gift',
			'root_act_lifespan', 'root_outdeg', 'root_contribution' , 'root_success_ratio', 'rn_contr')
	size_vs_root <- size_vs_root[size_vs_root$size > 1, ]
	evolution <- as.data.frame(read.csv(evolution_file, header=FALSE))
	colnames(evolution) <- c('root', 'size', 'depth', 'width', 'first_day', 'last_day', 'burstiness')
	evolution <- evolution[evolution$size > 1, c(1,7)]
	growth <- as.data.frame(read.csv(growth_file, header=FALSE))
	colnames(growth) <- c('root', 'week_1', 'week_2', 'week_3', 'week_4', 'week_5',
			'week_6', 'week_7', 'week_8', 'week_9', 'week_10', 'week_11', 'week_12')
	growth <- growth[!is.na(growth$week_1),]
	temp <- merge(size_vs_root, evolution, by="root")
	cascade <- merge(temp, growth, by="root")
#	cascade.small <- cascade[cascade$size < 156, ]
#	nrow(cascade.small)
#	cascade.big <- cascade[cascade$size >= 156, ]
#	nrow(cascade.big)
#	training <- c()
#	test <- c()
#	splitted_data <- split(cascade.small, sample(1:2, nrow(cascade.small), replace=TRUE, prob=c(1,2)))
#	training$small <- splitted_data[[2]]
#	test$small <- splitted_data[[1]]
#	splitted_data <- split(cascade.big, sample(1:2, nrow(cascade.big), replace=TRUE, prob=c(1,2)))
#	training$big <- splitted_data[[2]]
#	test$big <- splitted_data[[1]]
	cascade <- cascade[cascade$size > 150, ]
	cascade$size_prop <- cascade$size / sum(cascade$size)
	cascade$week_1_prop <- cascade$week_1 / sum(cascade$size)
	cascade$week_2_prop <- cascade$week_2 / sum(cascade$size)
	splitted_data <- split(cascade, sample(1:2, nrow(cascade), replace=TRUE, prob=c(1,2)))
	training <- splitted_data[[2]]
	test <- splitted_data[[1]]
	model <- lm(size_prop~week_1_prop+week_2_prop, data = training)
	print(summary(model))
#	save_ggplot(plot(model), 'model.pdf')
	pred <- round(predict(model, list(week_1_prop = test$week_1_prop, week_2_prop = test$week_2_prop))*sum(cascade$size), 0)
	test_size <- count(test$size)
	test_size[,3] <- 0
	colnames(test_size) <- c('size', 'count', 'type')
	pred_size <- count(pred)
	pred_size[,3] <- 1
	colnames(pred_size) <- c('size', 'count', 'type')
	size_comp <- rbind(test_size, pred_size)
	size_comp.df <- ddply(size_comp, c('type'), function(one_partition){
				one_partition = one_partition[order(one_partition$size),]
				one_partition$cum_count = cumsum(one_partition$count)
				one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
				one_partition$pdf_val = one_partition$count / max(one_partition$cum_count)
				one_partition
			})
	size_comp.df$type <- factor(size_comp.df$type)
	plot <- ggplot(size_comp.df, aes(x = (size), y = (cdf_val))) + 
			geom_point(aes(group = type, colour = type, shape = type), size=.8)+
			scale_x_log10()
	plot <- change_plot_attributes(plot, "", 0:1, c('Actual','Simulated'), "Cascade Size", "Empirical CDF")
	save_ggplot(plot,file='iheart_gift/model_size.pdf')
	plot <- ggplot(size_comp.df, aes(x = (size), y = (pdf_val))) + 
			geom_point(aes(group = type, colour = type, shape = type), size=.8)+
			scale_x_log10()+ scale_y_log10() #+ theme(legend.position=c(.8, .7)) + xlim(0,log10(plot_x_lim*100))
	plot <- change_plot_attributes(plot, "", 0:1, c('Actual','Simulated'), "Cascade Size", "Empirical PDF")
	save_ggplot(plot,file='iheart_gift/model_size_pdf.pdf')
	return(list(training = training, test = test))
}

cascade_logit_model <- function(file='iheart_gift/size_vs_root.csv',
		evolution_file='iheart_gift/top_size.csv_all_evolution.csv',
		growth_file = 'iheart_gift/top_size.csv_all_weekly_evolution.csv'){
	size_vs_root <- as.data.frame(read.csv(file, header=FALSE))
	colnames(size_vs_root) <- c('root', 'size', 'depth' ,'width', 'major_gift',
			'root_act_lifespan', 'root_outdeg', 'root_contribution' , 'root_success_ratio', 'rn_contr')
	size_vs_root <- size_vs_root[size_vs_root$size > 1, c(1,2)]
#	evolution <- as.data.frame(read.csv(evolution_file, header=FALSE))
#	colnames(evolution) <- c('root', 'size', 'depth', 'width', 'first_day', 'last_day', 'burstiness')
#	evolution <- evolution[evolution$size > 1, c(1,7)]
	growth <- as.data.frame(read.csv(growth_file, header=FALSE))
	colnames(growth) <- c('root', 'root_contr_1', 'rn_contr_1', 'burst_1', 'depth_1','width_1', 'week_1',
			'root_contr_2', 'rn_contr_2', 'burst_2', 'depth_2','width_2', 'week_2',
			'root_contr_3', 'rn_contr_3', 'burst_3', 'depth_3','width_3','week_3',
			'root_contr_4', 'rn_contr_4', 'burst_4', 'depth_4', 'width_4','week_4')
	growth$root_contr_2[is.na(growth$root_contr_2)] <- growth$root_contr_1
	growth$root_contr_3[is.na(growth$root_contr_3)] <- growth$root_contr_2
	growth$root_contr_4[is.na(growth$root_contr_4)] <- growth$root_contr_3
	growth$rn_contr_2[is.na(growth$rn_contr_2)] <- growth$rn_contr_1
	growth$rn_contr_3[is.na(growth$rn_contr_3)] <- growth$rn_contr_2
	growth$rn_contr_4[is.na(growth$rn_contr_4)] <- growth$rn_contr_3
	growth$burst_2[is.na(growth$burst_2)] <- growth$burst_1
	growth$burst_3[is.na(growth$burst_3)] <- growth$burst_2
	growth$burst_4[is.na(growth$burst_4)] <- growth$burst_3
	growth$depth_2[is.na(growth$depth_2)] <- growth$depth_1
	growth$depth_3[is.na(growth$depth_3)] <- growth$depth_2
	growth$depth_4[is.na(growth$depth_4)] <- growth$depth_3
	growth$width_2[is.na(growth$width_2)] <- growth$width_1
	growth$width_3[is.na(growth$width_3)] <- growth$width_2
	growth$width_4[is.na(growth$width_4)] <- growth$width_3
	growth$week_2[is.na(growth$week_2)] <- 0
	growth$week_3[is.na(growth$week_3)] <- 0
	growth$week_4[is.na(growth$week_4)] <- 0
	
	growth$evol_1 <- growth$week_1
	growth$evol_2 <- growth$week_1 + growth$week_2
	growth$evol_3 <- growth$week_1 + growth$week_2 + growth$week_3
	growth$evol_4 <- growth$week_1 + growth$week_2 + growth$week_3 + growth$week_4
	
	growth$root_contr_1 <- growth$root_contr_1/growth$evol_1
	growth$root_contr_2 <- growth$root_contr_2/growth$evol_2
	growth$root_contr_3 <- growth$root_contr_3/growth$evol_3
	growth$root_contr_4 <- growth$root_contr_4/growth$evol_4
	
	growth$rn_contr_1 <- growth$rn_contr_1/growth$evol_1
	growth$rn_contr_2 <- growth$rn_contr_2/growth$evol_2
	growth$rn_contr_3 <- growth$rn_contr_3/growth$evol_3
	growth$rn_contr_4 <- growth$rn_contr_4/growth$evol_4
	
	growth$speed_1 <- growth$evol_1/1
	growth$speed_2 <- growth$evol_2/2
	growth$speed_3 <- growth$evol_3/3
	growth$speed_4 <- growth$evol_4/4
	
	cascade <- merge(size_vs_root, growth, by="root")	
#	cascade$speed_1 <- cascade$week_1
#	cascade$accelerate_1 <- (cascade$week_1 - 0)
#	cascade$speed_2 <- (cascade$week_1 + cascade$week_2)/2
#	cascade$accelerate_2 <- (cascade$week_2 - cascade$week_1)
#	cascade$speed_3 <- (cascade$week_1 + cascade$week_2 + cascade$week_3)/3
#	cascade$accelerate_3 <- (cascade$week_3 - cascade$week_2)
#	cascade$speed_4 <- (cascade$week_1 + cascade$week_2 + cascade$week_3 + cascade$week_4) / 4
#	cascade$accelerate_4 <- (cascade$speed_4 - cascade$speed_3)
#	cascade$avg_accelerate <- (cascade$accelerate_1 + cascade$accelerate_2 + cascade$accelerate_3 + cascade$accelerate_4) / 4
#	cascade$d_w_4 <- cascade$depth_4*cascade$width_4*cascade$avg_accelerate
	size_bin <- unique(c(1, 10, 100, 1000, 10000000))
	size_cat <- c('Tiny', 'Small', 'Medium', 'Large')
	cascade <- transform(cascade, bin = cut(cascade$size, breaks=size_bin))
	categories <- levels(cascade$bin)
	cascade$cat <- factor(cascade$bin, levels = categories, labels = c(1,2,3,4))
	cascade$cat <- relevel(cascade$cat, ref = 1)
	splitted_data <- split(cascade, sample(1:2, nrow(cascade), replace=TRUE, prob=c(1,2)))
	training <- splitted_data[[2]]
	test <- splitted_data[[1]]
	model <- multinom(cat ~ root_contr_4 + rn_contr_4 + burst_4 + speed_4 + depth_4 + width_4, data = training)
#	model_summary <- summary(model)
#	z <- model_summary$coefficients/model_summary$standard.errors
	p <- 0 # (1 - pnorm(abs(z), 0, 1)) * 2
	test$cat.pred <- predict(model, newdata = test)
	test$cat.pred_prob <- predict(model, newdata = test, "probs")
	print(multiclass.roc(test$cat, apply(test$cat.pred_prob, 1, function(row) which.max(row))))
	test$cat <- as.numeric(test$cat)
	true_pos <- c()
	false_pos <- c()
	prec <- c()
	recall <- c()
	for (i in seq(1, 4, by=1)){
		true_pos <- c(true_pos, length(which(test$cat.pred == test$cat & test$cat == i)))
		false_pos <- c(false_pos, length(which(test$cat.pred != test$cat & test$cat.pred == i)))
		prec <- c(prec, (true_pos[i] / (length(which(test$cat.pred == i)))))
		recall <- c(recall, (true_pos[i] / (length(which(test$cat == i)))))
	}
	print(prec)
	print(recall)
	print(2*prec*recall/ (prec+recall))
	print(true_pos/(true_pos+false_pos))
	test_size <- as.data.frame(table(test$cat))
	test_size[,3] <- 0
	print(summary(test_size))
	colnames(test_size) <- c('bin', 'count', 'type')
	print(summary(test_size))
	test_size$size <- as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", levels(test_size$bin)))
	print(summary(test_size))
	pred_size <- as.data.frame(table(test$cat.pred))
	pred_size[,3] <- 1
	colnames(pred_size) <- c('bin', 'count', 'type')
	pred_size$size <- as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", levels(pred_size$bin)))
	size_comp <- rbind(test_size, pred_size)
	size_comp.df <- ddply(size_comp, c('type'), function(one_partition){
				one_partition = one_partition[order(one_partition$size),]
				one_partition$cum_count = cumsum(one_partition$count)
				one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
				one_partition$pdf_val = one_partition$count / max(one_partition$cum_count)
				one_partition
			})
	size_comp.df$type <- factor(size_comp.df$type)
	plot <- ggplot(size_comp.df, aes(x = (size), y = (cdf_val))) + 
			geom_point(aes(group = type, colour = type, shape = type), size=.8)+
			scale_x_log10()
	plot <- change_plot_attributes(plot, "", 0:1, c('Actual','Simulated'), "Cascade Size", "Empirical CDF")
	save_ggplot(plot,file='iheart_gift/model_size.pdf')
	plot <- ggplot(size_comp.df, aes(x = (size), y = (pdf_val))) + 
			geom_point(aes(group = type, colour = type, shape = type), size=.8)+
			scale_x_log10()+ scale_y_log10() #+ theme(legend.position=c(.8, .7)) + xlim(0,log10(plot_x_lim*100))
	plot <- change_plot_attributes(plot, "", 0:1, c('Actual','Simulated'), "Cascade Size", "Empirical PDF")
	save_ggplot(plot,file='iheart_gift/model_size_pdf.pdf')
	return(list(training = training, test = test, prec=prec, recall=recall, p=p, true_pos = true_pos, false_pos = false_pos))
}


size_feature_correlation <- function(file='iheart_gift/size_vs_root.csv',
		evolution_file='iheart_gift/top_size.csv_all_evolution.csv',
		growth_file = 'iheart_gift/top_size.csv_all_weekly_evolution.csv'){
	size_vs_root <- as.data.frame(read.csv(file, header=FALSE))
	colnames(size_vs_root) <- c('root', 'size', 'depth' ,'width', 'major_gift',
			'root_act_lifespan', 'root_outdeg', 'root_contribution' , 'root_success_ratio', 'rn_contr')
	size_vs_root <- size_vs_root[size_vs_root$size > 1, c(1,2)]
	growth <- as.data.frame(read.csv(growth_file, header=FALSE))
	colnames(growth) <- c('root', 'root_contr_1', 'rn_contr_1', 'burst_1', 'depth_1','width_1', 'week_1',
			'root_contr_2', 'rn_contr_2', 'burst_2', 'depth_2','width_2', 'week_2',
			'root_contr_3', 'rn_contr_3', 'burst_3', 'depth_3','width_3','week_3',
			'root_contr_4', 'rn_contr_4', 'burst_4', 'depth_4', 'width_4','week_4')
	growth$root_contr_2[is.na(growth$root_contr_2)] <- growth$root_contr_1
	growth$root_contr_3[is.na(growth$root_contr_3)] <- growth$root_contr_2
	growth$root_contr_4[is.na(growth$root_contr_4)] <- growth$root_contr_3
	growth$rn_contr_2[is.na(growth$rn_contr_2)] <- growth$rn_contr_1
	growth$rn_contr_3[is.na(growth$rn_contr_3)] <- growth$rn_contr_2
	growth$rn_contr_4[is.na(growth$rn_contr_4)] <- growth$rn_contr_3
	growth$burst_2[is.na(growth$burst_2)] <- growth$burst_1
	growth$burst_3[is.na(growth$burst_3)] <- growth$burst_2
	growth$burst_4[is.na(growth$burst_4)] <- growth$burst_3
	growth$depth_2[is.na(growth$depth_2)] <- growth$depth_1
	growth$depth_3[is.na(growth$depth_3)] <- growth$depth_2
	growth$depth_4[is.na(growth$depth_4)] <- growth$depth_3
	growth$width_2[is.na(growth$width_2)] <- growth$width_1
	growth$width_3[is.na(growth$width_3)] <- growth$width_2
	growth$width_4[is.na(growth$width_4)] <- growth$width_3
	growth$week_2[is.na(growth$week_2)] <- 0
	growth$week_3[is.na(growth$week_3)] <- 0
	growth$week_4[is.na(growth$week_4)] <- 0
	growth$evol_1 <- growth$week_1
	growth$evol_2 <- growth$week_1 + growth$week_2
	growth$evol_3 <- growth$week_1 + growth$week_2 + growth$week_3
	growth$evol_4 <- growth$week_1 + growth$week_2 + growth$week_3 + growth$week_4
	cascade <- merge(size_vs_root, growth, by="root")	
	size_bin <- unique(c(1, 10, 100, 1000, 10000000))
	size_cat <- c('Tiny', 'Small', 'Medium', 'Large')
	cascade <- transform(cascade, bin = cut(cascade$size, breaks=size_bin))
	categories <- levels(cascade$bin)
	cascade$cat <- factor(cascade$bin, levels = categories, labels = c(1,2,3,4))
	cascade$cat <- relevel(cascade$cat, ref = 1)
	cor.df <- c()
	for (i in c(1,2,3,4)){
		cor <- as.data.frame(c(1,2,3,4))
		colnames(cor) <- c('Week')
		cor$root_contr <- c(cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$root_contr_1),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$root_contr_2),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$root_contr_3),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$root_contr_4))
		cor$rn_contr <- c(cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$rn_contr_1),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$rn_contr_2),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$rn_contr_3),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$rn_contr_4))
		cor$burst <- c(cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$burst_1),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$burst_2),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$burst_3),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$burst_4))
		cor$depth <- c(cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$depth_1),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$depth_2),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$depth_3),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$depth_4))
		cor$width <- c(cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$width_1),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$width_2),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$width_3),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$width_4))
		cor$evol <- c(cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$evol_1),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$evol_2),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$evol_3),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$evol_4))
		cor$cat <- size_cat[i]
		cor.df <- rbind(cor.df, cor)
	}
	return(cor.df)
}

#bla <- parent_lifespan_comp(c('fp_nt_u/','disc_heavy_users_with_sc/','disc_heavy_users_no_sc'),c('Original Cascade','W/ Second Chance','W/o Second Chance'),'Secluding Heavy Seeds')
#bla<-parent_lifespan_comp(c('fp_nt_u/','fp_less_356days_act_life/','fp_less_356days_nsc/','fp_less_290days_act_life/','fp_less_290days_nsc/'),c('Original Cascade','Top 1% w/ Second Chance','Top 1% w/o Second Chance','Top 5% w/ Second Chance','Top 5% w/o Second Chance'), 'Secluding Persistent Users')
#plot1<-parent_lifespan_comp(c('fp_nt_u/','fp_1hr_act_life/','fp_1day_act_life/','fp_2day_act_life/','fp_4day_act_life/',
#				'fp_1week_act_life/','fp_2week_act_life/', 'fp_4week_act_life/','fp_8week_act_life/','fp_16week_act_life/'),
#		c('Original Cascade','1 Hour','1 Day','2 Day','4 Day', '1 Week', '2 Week', '4 Week', '8 Week', '16 Week'),
#		'Secluding (w/ Second\nChance) Short-term\nUsers w/Active Lifetime\nBelow')
#
#plot2<-parent_lifespan_comp(c('fp_nt_u/','fp_1hr_act_life_nsc/','fp_1day_act_life_nsc/','fp_2day_act_life_nsc/','fp_4day_act_life_nsc/',
#				'fp_1week_act_life_nsc/','fp_2week_act_life_nsc/', 'fp_4week_act_life_nsc/','fp_8week_act_life_nsc/','fp_16week_act_life_nsc/'),
#		c('Original Cascade','1 Hour','1 Day','2 Day','4 Day', '1 Week', '2 Week', '4 Week', '8 Week', '16 Week'),
#		'Secluding (w/o Second\nChance) Short-term\nUsers w/ Active Lifetime\nBelow')
