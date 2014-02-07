source('~/scripts/cascade/tools.r')
source('~/scripts/cascade/plfit.r')
library('MASS')
library('distr')
library(plyr)

load_node_properties <- function(node_properties_file) {
	node_prop <<- as.data.frame(read.csv(node_properties_file, header=FALSE))
	colnames(node_prop) <<- c('id', 'odeg', 'indeg', 'deg')
}

random_degree_node <- function(sampling_ratio) {
	total_users <- nrow(node_prop)
	sampled_users <- sample(x=node_prop$id, size=ceiling(total_users*sampling_ratio), prob=(node_prop$deg/sum(node_prop$deg)))
	print(head(sampled_users))
#	write(sampled_users, file = '/home/rezaur/data/sampled_users/RDN.txt', ncolumns = 1)
	return(sampled_users)
}

random_degree_sender <- function(sampling_ratio) {
	total_users <- nrow(node_prop)
	sampled_users <- sample(x=node_prop[node_prop$odeg > 0]$id, size=ceil(total_users*sampling_ratio))
	write(sampled_users, file = '/home/rezaur/data/sampled_users/RNN.txt', ncolumns = 1)
}

analyze_outdeg_dist <- function(binned_dist){
	plot <- ggplot(data=binned_dist, aes(x = log10(outdeg), y = log10(count/sum(count)))) + geom_line(aes(group = 'Type', colour = 'Bin out degree')) + xlab('log of Out degree') + ylab('log of proportion of Count')
	unrolled_bin_odeg <- rep(binned_dist$outdeg, times = binned_dist$count)
	plfit_param <- plfit(unrolled_bin_odeg[unrolled_bin_odeg>0])
	plaw.df <<- data.frame(outdeg=binned_dist[binned_dist$outdeg>=plfit_param$xmin,]$outdeg, alpha=plfit_param$alpha, const=plfit_param$xmin)#/sum(binned_dist[binned_dist$outdeg>0,]$count))*sum(binned_dist$count))
	plot <- plot + geom_line(data=plaw.df, aes(log10(outdeg), log10(const*(outdeg^-alpha)), colour='power-law'),linetype="dashed")
	t2<<-data.frame(x=2, y=-0.5, l2=paste("gamma=",round(plfit_param$alpha,4)))
	plot <- plot + geom_text(data=t2, aes(x, y, label=l2),color = "grey50", size = 4)
	plot <- plot + geom_line(data=raw_outdeg, aes(log10(outdeg), log10(count/sum(count)), colour='Raw out degree'))
	unrolled_raw_odeg <- rep(raw_outdeg$outdeg, times=raw_outdeg$count)
	fitted_estimate <- fitdistr(unrolled_raw_odeg[unrolled_raw_odeg>0],'log-normal')
	print(fitted_estimate)
	fitted_lnorm_values <- c()
	fitted_lnorm_values$outdeg <- raw_outdeg[raw_outdeg$outdeg>0 & raw_outdeg$outdeg<10000,]$outdeg
	fitted_lnorm_values$count <- raw_outdeg[raw_outdeg$outdeg>0 & raw_outdeg$outdeg<10000,]$count
	fitted_lnorm_values$density <- (sum(fitted_lnorm_values$count)/sum(raw_outdeg$count))*(dlnorm(fitted_lnorm_values$outdeg, meanlog=fitted_estimate$estimate[1],sdlog=fitted_estimate$estimate[2]))
	fitted_lnorm_values <- as.data.frame(fitted_lnorm_values)
	plot <- plot + geom_line(data=fitted_lnorm_values, aes(log10(outdeg), log10(density), colour='log-normal'),linetype="dashed")
	t1<<-data.frame(x=2, y=0, l1=paste("lnN(.,.)=(",round(exp(fitted_estimate$estimate[1]),4),",",round(exp(fitted_estimate$estimate[2]),4),")"))
	plot <- plot + geom_text(data=t1, aes(x, y, label=l1),color = "grey50", size = 4)
	save_ggplot(plot, paste(c(output_dir,bin_size[i],'outdeg_dist.pdf'), collapse = ''))
}

analyze_branching <- function(output_dir){
	max_depth <- max(g_prep.df$outdeg_dist$depth)
	dist_bin <- list()
	bin_outdeg_dist <- c()
	root_outdeg <- g_prep.df$outdeg_dist[(g_prep.df$outdeg_dist$depth==0),]
#	dist_bin[[1]] <- ddply(root_outdeg, c('outdeg'), summarise, count=sum(count))
#	bin_size <- c(1,2,3,4,5,21,36,61) #seq(1,max_depth, by=bin_size)
#	bin_size <- c(1,11,21,31,41,51,61)
	for (i in 1:(length(bin_size)-1)){
		binned_data <- g_prep.df$outdeg_dist[(g_prep.df$outdeg_dist$depth>=bin_size[i] & g_prep.df$outdeg_dist$depth<bin_size[i+1]),]
		binned_dist <<- ddply(binned_data, c('outdeg'), summarise, count=sum(count))
#		analyze_outdeg_dist(binned_dist)
		bin_outdeg_dist <- rbind(bin_outdeg_dist,cbind(binned_dist,rep(bin_size[i],nrow(binned_dist))))
		for (j in bin_size[i]:min(bin_size[i+1],max_depth)){
			dist_bin[[j+1]] <- binned_dist
		}
	}
	colnames(bin_outdeg_dist) <- c('outdeg', 'count', 'depth')
	bin_outdeg_dist.df <- ddply (bin_outdeg_dist,  c('depth'), function (one_partition){
				one_partition$pdf = one_partition$count/sum(one_partition$count)
				one_partition
			})
	p.dist <<- bin_outdeg_dist.df[bin_outdeg_dist.df$depth==0,]$pdf
	q.dist <<- bin_outdeg_dist.df[bin_outdeg_dist.df$depth==1,]$pdf
	bin_outdeg_dist.df$depth <- factor(bin_outdeg_dist.df$depth)
	plot <- ggplot(bin_outdeg_dist.df, aes(x = (outdeg), y = (pdf))) + geom_point(aes(group = depth,colour = depth)) + scale_x_log10() + scale_y_log10()  + theme(legend.position=c(.8, .7)) #+ xlab('Out Degree') + ylab('Proportion of Total Users')
	plot <- change_plot_attributes(plot, "Class", 0:1, c('Seeds','Descendants\nof Seeds'), "Out Degree", "Proportion of Total Same Class Users")
	save_ggplot(plot, paste(c(output_dir,'outdeg_dist.pdf'), collapse = ''))
#	print_report('P(0 out degree)',nonroot_deg_dist$count[1]/sum(nonroot_deg_dist$count))
#	expected_root_odeg <- sum(root_deg_dist$outdeg*(root_deg_dist$count/sum(root_deg_dist$count)))
#	expected_nonroot_odeg <- sum(nonroot_deg_dist$outdeg*(nonroot_deg_dist$count/sum(nonroot_deg_dist$count)))
#	print_report('expected root degree', expected_root_odeg)
#	print_report('expected nonroot degree', expected_nonroot_odeg)
	options(expressions = 10000)
	simulated_cascades <- branching_process(dist_bin,trial,max_depth)
	return(simulated_cascades)
}

analyze_time_to_next_generation_degree_dist<-function(dist_file){
	odeg_dist <- as.data.frame(read.csv(dist_file, header=FALSE))
	colnames(odeg_dist) <- c('outdeg', 'count', 'week')
}

Comparing_simulation <- function(directoryname){
	prev_dir = getwd()
	setwd(directoryname)
	cascade_size <- as.data.frame(read.csv('size.csv', header=FALSE))
	cascade_size[,4] <- 0 #Actual
	colnames(cascade_size) <- c('size', 'count', 'threshold', 'simulation')
	cascade_depth <- as.data.frame(read.csv('depth.csv', header=FALSE))
	cascade_depth[,4] <- 0 #Actual
	colnames(cascade_depth) <- c('depth', 'count', 'threshold', 'simulation')
	setwd(prev_dir)
	simulated_cascade_size <- as.data.frame(ab$size)
	simulated_cascade_size[,2] <- 1 #Simulation
	colnames(simulated_cascade_size) <- c('size','simulation_type')
#	Analytic distribution loading and data frame managing
	analytic_dist <- as.data.frame(scan('branching/dist.txt'))
	analytic_dist <- as.data.frame(analytic_dist[-1,])
	analytic_dist[,2] <- 1:nrow(analytic_dist)
	analytic_dist[,3] <- 2
	colnames(analytic_dist) <- c('pdf','size','simulation_type')
#	simulated_cascade_size_binned <- as.data.frame(ab_e$size)
#	simulated_cascade_size_binned[,2] <- 2
#	colnames(simulated_cascade_size_binned) <- c('size','simulation_type')
#	simulated_cascade_size <- rbind(simulated_cascade_size, simulated_cascade_size_binned)
	size_freq <- data.frame(size = rep(cascade_size$size, times = cascade_size$count), simulation_type=rep(cascade_size$simulation, times = cascade_size$count))
	size_freq <- rbind(size_freq,simulated_cascade_size)
	size_freq$simulation_type <- factor(size_freq$simulation_type)
# 	Binned distribution plot
	max_size <- max(size_freq$size)
	size_bin <- cascade_size$size
#	size_bin <- unique(ceiling(1.1^(seq(0,ceiling(log(max_size)/log(1.1)),by=0.1))))
#	size_bin <- unique(ceiling(1.7^(seq(0,ceiling(log(max_size)/log(1.7)),by=0.25))))
	print(size_bin)
	size_dist <- transform(size_freq, bin = cut(size_freq$size, breaks=size_bin, right=FALSE))
	size_dist <- ddply(size_dist, c('bin','simulation_type'), summarise, avg_size=ceiling(mean(size)), pdf=length(bin)/3734781)
#	Analytic distribution binning
	analytic_dist$simulation_type <- factor(analytic_dist$simulation_type)
	analytic_dist <- transform(analytic_dist, bin = cut(analytic_dist$size, breaks=size_bin, right=FALSE))
	analytic_dist <- ddply(analytic_dist, c('bin','simulation_type'), summarise, avg_size=ceiling(mean(size)), pdf=sum(pdf))
#	size_dist <- rbind(size_dist, analytic_dist)
#	Marged all from actual and branching with analytic distribution
	colnames(size_dist) <- c('bin', 'simulation_type', 'size', 'pdf_val')
	size_dist$simulation_type <- factor(size_dist$simulation_type)
	plot <- ggplot(size_dist[size_dist$pdf_val>1e-7,],aes(x = (size), y = (pdf_val))) + geom_point(aes(group = simulation_type,colour = simulation_type)) + scale_x_log10() + scale_y_log10() + theme(legend.position=c(.8, .7))
	plot <- change_plot_attributes(plot, "", 0:2, c('Empirical','Simulation','Analysis'), "Cascade Size", "Proportion of Total Number of Cascades")
	save_ggplot(plot,file='branching_size_pdf.pdf')
#	Box plot
	plot <- ggplot(size_freq, aes(y=size, x=simulation_type))+ geom_boxplot() + scale_x_discrete(breaks=0:2, labels=c('Empirical Analysis','Simulation','Analysis'))+ scale_y_log10() # + geom_histogram(binwidth=0.2, position="dodge")
	plot <- change_plot_attributes(plot, "", 0:2, c('Empirical Analysis','Simulation','Analysis'), "", "Cascade Size")
	save_ggplot(plot,file='branching_boxplot.pdf')
	
	big_reproduced_order <- order(ab$size,decreasing=T)[1:10]
	big_reproduced_size <- ab$size[big_reproduced_order]
	big_reproduced_growth <- c()
	for (i in big_reproduced_order){
		growth_temp <- as.data.frame(c(1,ab$growth[i][[1]]))
		growth_temp[,2] <- 0:(nrow(growth_temp)-1)
		growth_temp[,3] <- i
		big_reproduced_growth <- rbind(big_reproduced_growth, growth_temp)
	}
	colnames(big_reproduced_growth) <- c('expansion','depth','cascade_id')
	big_reproduced_growth.df <- ddply(big_reproduced_growth, c('cascade_id'), function(one_partition){
				one_partition = one_partition[order(one_partition$depth),]
				one_partition$diff = (one_partition$expansion-c(0,one_partition$expansion[1:nrow(one_partition)-1]))
				one_partition
			})
	big_reproduced_growth.df$cascade_id <- factor(big_reproduced_growth.df$cascade_id)
	plot <- ggplot(big_reproduced_growth.df, aes(x = depth, y = (expansion))) + geom_line(aes(group = cascade_id,colour = cascade_id)) + xlab('Depth') + ylab('Shell size') 
	save_ggplot(plot, 'branching_depth_expansion.pdf')
	plot <- ggplot(big_reproduced_growth.df, aes(x = depth, y = (diff))) + geom_line(aes(group = cascade_id,colour = cascade_id)) + xlab('Depth') + ylab('Shell size growth') + scale_colour_hue(name  ="Cascade root")
	save_ggplot(plot, 'branching_depth_expansion_diff.pdf')
	
	return(size_freq)
}

#size_freq<-Comparing_simulation('fp_nt_u/')

#ab_10 <- analyze_branching('~/output_cascade/fp_nt_u/bin_10/',4,bin_size <- c(0,10,20,30,40,50,60))
#ab_v <- analyze_branching('~/output_cascade/fp_nt_u/bin_v/',4,bin_size <- c(0,1,2,3,4,5,21,36,61))
#ab_e <- analyze_branching('~/output_cascade/fp_nt_u/bin_e/',4,bin_size <- c(0,2,4,8,16,32,64))
#ab_1 <- analyze_branching('~/output_cascade/fp_nt_u/bin_1/',1,bin_size <- c(0:60))
#print_report('Summary size', summary(ab_10$size))
#print_report('Summary depth', summary(ab_10$depth))
#ab_5 <- analyze_branching('~/output_cascade/full_first_parent/top_size.csv_top_1000_branching_dist.csv',5)
#print_report('Summary size', summary(ab_5$size))
#print_report('Summary depth', summary(ab_5$depth))
#ab_1 <- analyze_branching('~/output_cascade/full_first_parent/top_size.csv_top_1000_branching_dist.csv',1)
#print_report('Summary size', summary(ab_1$size))
#print_report('Summary depth', summary(ab_1$depth))
#
#idx <- c(1:length(ab_10$size))
#sorted_idx <- idx[order(-ab_10$size)]
#ab_10.df <- data.frame(depth=0:length(ab_10$growth[[sorted_idx[1]]]), shell=c(1,ab_10$growth[[sorted_idx[1]]]), tree=rep(1, times=length(ab_10$growth[[sorted_idx[1]]])+1))
#for (i in 2:30){
#	data_f <- data.frame(depth=0:length(ab_10$growth[[sorted_idx[i]]]), shell=c(1,ab_10$growth[[sorted_idx[i]]]), tree=rep(i, times=length(ab_10$growth[[sorted_idx[i]]])+1))
#	ab_10.df <- rbind(ab_10.df,data_f)
#}
#ab_10.df$tree <- factor(ab_10.df$tree)
#plot <- ggplot(ab_10.df, aes(x = depth, y = (shell))) + geom_line(aes(group = tree,colour = tree)) + xlab('Depth') + ylab('Shell size') 
#save_ggplot(plot, paste('~/output_cascade/full_first_parent/branching_bin_10.pdf', collapse = ''))
#
#idx <- c(1:length(ab_5$size))
#sorted_idx <- idx[order(-ab_5$size)]
#ab_5.df <- data.frame(depth=0:length(ab_5$growth[[sorted_idx[1]]]), shell=c(1,ab_5$growth[[sorted_idx[1]]]), tree=rep(1, times=length(ab_5$growth[[sorted_idx[1]]])+1))
#for (i in 2:30){
#	data_f <- data.frame(depth=0:length(ab_5$growth[[sorted_idx[i]]]), shell=c(1,ab_5$growth[[sorted_idx[i]]]), tree=rep(i, times=length(ab_5$growth[[sorted_idx[i]]])+1))
#	ab_5.df <- rbind(ab_5.df,data_f)
#}
#ab_5.df$tree <- factor(ab_5.df$tree)
#plot <- ggplot(ab_5.df, aes(x = depth, y = (shell))) + geom_line(aes(group = tree,colour = tree)) + xlab('Depth') + ylab('Shell size') 
#save_ggplot(plot, paste('~/output_cascade/full_first_parent/branching_bin_5.pdf', collapse = ''))
#
#idx <- c(1:length(ab_1$size))
#sorted_idx <- idx[order(-ab_1$size)]
#ab_1.df <- data.frame(depth=0:length(ab_1$growth[[sorted_idx[1]]]), shell=c(1,ab_1$growth[[sorted_idx[1]]]), tree=rep(1, times=length(ab_1$growth[[sorted_idx[1]]])+1))
#for (i in 2:30){
#	data_f <- data.frame(depth=0:length(ab_1$growth[[sorted_idx[i]]]), shell=c(1,ab_1$growth[[sorted_idx[i]]]), tree=rep(i, times=length(ab_1$growth[[sorted_idx[i]]])+1))
#	ab_1.df <- rbind(ab_1.df,data_f)
#}
#ab_1.df$tree <- factor(ab_1.df$tree)
#plot <- ggplot(ab_1.df, aes(x = depth, y = (shell))) + geom_line(aes(group = tree,colour = tree)) + xlab('Depth') + ylab('Shell size') 
#save_ggplot(plot, paste('~/output_cascade/full_first_parent/branching_bin_1.pdf', collapse = ''))
