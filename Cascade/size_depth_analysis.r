source('~/scripts/cascade/tools.r')
source('~/scripts/cascade/plfit.r')
library(ggplot2)
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
#	Cascade size summary
	size_freq <- data.frame(size = rep(cascade_comp$size$size, times = cascade_comp$size$count), parent_type=rep(cascade_comp$size$parent_type, times = cascade_comp$size$count))
	print(head(size_freq))
	print_report('First Parent: Power law alpha', plfit(size_freq[size_freq$parent_type==0 & size_freq$size<1000,]$size))
	print_report('Last Parent: Power law alpha', plfit(size_freq[size_freq$parent_type==1 & size_freq$size<1000,]$size))
	print_report('HOP Parent: Power law alpha', plfit(size_freq[size_freq$parent_type==2 & size_freq$size<1000,]$size))
	print_report('Random Parent: Power law alpha', plfit(size_freq[size_freq$parent_type==3 & size_freq$size<1000,]$size))
	print(kruskal.test(size~parent_type, data=size_freq))
	cascade_comp$size_summary <- ddply(size_freq, c('parent_type'), summarise, min=summary(size)[1], quart_1=summary(size)[2], median=summary(size)[3], mean=summary(size)[4],
			quart_3=summary(size)[5], max=summary(size)[6], var=var(size), total_coverage=sum(size))
#	Cascade size box plot
	size_freq$parent_type <- factor(size_freq$parent_type)
	plot <- ggplot(size_freq, aes(y=(size), x=parent_type))+ geom_boxplot() + scale_x_discrete(breaks=0:(parent_type_idx-1), labels=parent_type_vector)+ scale_y_log10() # + geom_histogram(binwidth=0.2, position="dodge")
	plot <- change_plot_attributes(plot, "Parent type", 0:(parent_type_idx-1), parent_type_vector, "Parent type", "Cascade Size")
	save_ggplot(plot,file='parent_comp/size_box.pdf')
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
	plot <- ggplot(cascade_comp$size,aes(x = (size), y = (pdf_val))) + geom_line(aes(group = parent_type,colour = parent_type))+ scale_x_log10()+ scale_y_log10() #+ xlim(0,log10(plot_x_lim*100))
	plot <- change_plot_attributes(plot, "Parent type", 0:(parent_type_idx-1), parent_type_vector, "Cascade Size", "Proportion")
	save_ggplot(plot,file='parent_comp/size_pdf_logx_logy.pdf')
#	Cascade depth summary
	depth_freq <- data.frame(depth = rep(cascade_comp$depth$depth, times = cascade_comp$depth$count), parent_type=rep(cascade_comp$depth$parent_type, times = cascade_comp$depth$count))
	cascade_comp$depth_summary <- ddply(depth_freq, c('parent_type'), summarise, min=summary(depth)[1], quart_1=summary(depth)[2], median=summary(depth)[3], mean=summary(depth)[4],
			quart_3=summary(depth)[5], max=summary(depth)[6], var=var(depth))
#	Cascade depth box plot
	depth_freq$parent_type <- factor(depth_freq$parent_type)
	plot <- ggplot(depth_freq, aes(y=depth, x=parent_type))+ scale_y_log10()+ geom_boxplot() #+ geom_histogram(binwidth=1, position="dodge") 
	plot <- change_plot_attributes(plot, "Parent type", 0:(parent_type_idx-1), parent_type_vector, "Parent type", "Cascade Depth")
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
	plot <- ggplot(cascade_comp$depth,aes(x = depth, y = (pdf_val))) + geom_line(aes(group = parent_type,colour = parent_type)) + scale_y_log10()
	plot <- change_plot_attributes(plot, "Parent type", 0:(parent_type_idx-1), parent_type_vector,  "Cascade Depth", "Proportion")
	save_ggplot(plot,file='parent_comp/depth_pdf_logy.pdf')
	plot <- ggplot(cascade_comp$depth,aes(x = depth, y = (pdf_val))) + geom_line(aes(group = parent_type,colour = parent_type)) + scale_x_log10() + scale_y_log10()
	plot <- change_plot_attributes(plot, "Parent type", 0:(parent_type_idx-1), parent_type_vector,  "Cascade Depth", "Proportion")
	save_ggplot(plot,file='parent_comp/depth_pdf_log_log.pdf')
	d <- cascade_comp$size
	latex(d, file="parent_comp/comp_size.tex", digits = 6, rowname = latexTranslate(rownames(d)))            # If you want all the data
	d <- cascade_comp$depth
	latex(d, file="parent_comp/comp_depth.tex", digits = 6, rowname = latexTranslate(rownames(d)))            # If you want all the data
#	latex(describe(d), file="comp_summary.lex")  # If you just want a summary
	print('Exiting, Done')
	return(cascade_comp)
}

parent_lifespan_comp <- function(dir_vector, lifespan_threshold_vector){
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
	cascade_comp$size <- ddply(cascade_comp$size, c('threshold','lifespan_threshold'), function(one_partition){
				one_partition = one_partition[order(one_partition$size),]
				one_partition$cum_count = cumsum(one_partition$count)
				one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
				one_partition$pdf_val = one_partition$count / max(one_partition$cum_count)
				one_partition
			})
	cascade_comp$size$lifespan_threshold <- factor(cascade_comp$size$lifespan_threshold)
	plot <- ggplot(cascade_comp$size,aes(x = log10(size), y = log10(pdf_val))) + geom_line(aes(group = lifespan_threshold,colour = lifespan_threshold))#+ xlim(2,3)+ ylim(-6,-3.5)# + scale_y_log10()
#			scale_x_reverse(limits = c(log10(max(cascade_comp$size$size)), 4)) + scale_y_log10()
	plot <- change_plot_attributes(plot, "Parent's lifespan effect", 1:(lifespan_idx-1),lifespan_threshold_vector, "log of Cascade Size", "Proportion of count")
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
	plot <- ggplot(cascade_comp$size,aes(x = log10(size), y = log10(pdf_val))) + geom_point(aes(group = lifespan_threshold,colour = lifespan_threshold))#+ xlim(2,3)+ ylim(-6,-3.5)# + scale_y_log10()
	#		scale_x_reverse(limits = c(log10(max(cascade_comp$size$size)), 4)) + scale_y_log10()
	plot <- change_plot_attributes(plot, "Parent's lifespan effect", 1:(lifespan_idx-1),lifespan_threshold_vector, "log of Cascade Size", "Proportion of count")
	save_ggplot(plot,file='lifespan_comp/size_pdf_binned.pdf')

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
	plot <- change_plot_attributes(plot, "Parent's lifespan\n threshold", 1:(lifespan_idx-1), lifespan_threshold_vector, "Cascade Depth", "Proportion of count")
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
	
	return (cascade_comp)
}

analyze_coverage <- function(dir){
	cascade <- load_size_depth(dir)
	cascade$size.df <- ddply(cascade$size, c('threshold'), function(one_partition){
				one_partition = one_partition[order(-one_partition$size),]
				one_partition$cum_count = cumsum(one_partition$count)
				one_partition$cum_size = cumsum(one_partition$size*one_partition$count)/sum(one_partition$size*one_partition$count)
				one_partition
			})
	x_label <- c(10^(0:4),115000,10^6)/3734781
	plot <- ggplot(cascade$size.df,aes(x = (cum_count), y = (cum_size))) + geom_line()+ scale_x_log10(breaks=c(1,10,10^2,10^3,10^4,115000,10^6,3734781),
			labels=c('1\n0%','1e+1\n2.7e-4%	','1e+2\n.0027%','1e+3\n.027%','1e+4\n.27%','115000\n3.08%','1e+6\n26.78%','3734781\n100%'))+
			scale_y_continuous(breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0), labels=c('0','10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))
	plot <- change_plot_attributes(plot, "Parent type", 0:(4-1), 0:(4-1), "Number of Top Cascades(Count and Percentage)", "Coverage(Percent of Total Population)")
	prev_dir = getwd()
	setwd(dir)
	save_ggplot(plot,file='size_coverage.pdf')
	setwd(prev_dir)
}

size_vs_root_characteristics <- function (dir){
	prev_dir = getwd()
	setwd(dir)
	size_lifespan <- as.data.frame(read.csv('size_vs_root_lifespan.csv', header=FALSE))
	colnames(size_lifespan) <- c('size', 'lifespan')
	root_info <- ddply(g_prep.df$root_depth_expansion, c('root_user_id'), summarise, size = sum(expansion), root_outdeg = expansion[depth==1], neighbor_outdeg = c(expansion[depth==2],0)[1])
	plot1 <- ggplot(size_lifespan, aes(x=lifespan, y=size))+ geom_point(size=.5)+ scale_y_log10() + #+ geom_smooth(method=lm)
			scale_x_log10(breaks=c(86400,86400*7,2*86400*7,4*86400*7,8*86400*7,16*86400*7,32*86400*7,64*86400*7),labels=c('1day','1week','2week','4week','8week','16week','32week','64week'))+
			opts(axis.text.x=theme_text(angle=45,hjust=1,vjust=1))+
			xlab("Active Lifetime of a Seed")+ylab("Size of the Adoption\nRooted at the Seed")
	plot2 <- ggplot(root_info, aes(x=root_outdeg, y=size))+ geom_point(size=.5)+ scale_y_log10() + #+ geom_smooth(method=lm)
			xlab("Outdegree of a Seed")+ylab("Size of the Adoption\nRooted at the Seed")
	plot3 <- ggplot(root_info, aes(x=neighbor_outdeg, y=size))+ geom_point(size=.5)+ scale_y_log10() + #+ geom_smooth(method=lm)
			xlab("Total Outdegree of the neighbors of a Seed")+ylab("Size of the Adoption\nRooted at the Seed")
	pdf("size_vs_root.pdf")
	grid.arrange(plot1, plot2, plot3)
	dev.off()
	setwd(prev_dir)
	return (size_lifespan)
}