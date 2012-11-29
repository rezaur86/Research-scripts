source('~/scripts/cascade/tools.r')

library(plyr)

top_size_analysis <- function(file_name){
	users_correlated_info.df <- ddply(g_prep.df$users_correlated_info, c('top_user_odeg','size','depth'), summarise, avg_neighbour_odeg = mean(neighbour_odeg), total_neighbour_odeg = sum(neighbour_odeg))
	# top user's outged vs. avg neighbour outdeg
	plot <- ggplot(users_correlated_info.df, aes(x = top_user_odeg, y = avg_neighbour_odeg)) + xlim(0,100) + geom_point() + geom_smooth(method=lm)
	save_ggplot(plot,file=paste(c(file_name,'_avg_odeg_corr.pdf'), collapse = ''))
	print_report('Cor of top user outdeg vs. avg neighbour outdeg', cor(users_correlated_info.df$top_user_odeg,users_correlated_info.df$avg_neighbour_odeg))
	# top user's outdeg vs. total neihbour outdeg
	print(head(users_correlated_info.df))
	plot <- ggplot(users_correlated_info.df, aes(x = top_user_odeg, y = total_neighbour_odeg)) + xlim(0,100) + geom_point() + geom_smooth(method=lm) + xlab('Top users\' out degree') + ylab('Neighbor\'s total out degree')
	save_ggplot(plot,file=paste(c(file_name,'_total_odeg_corr.pdf'), collapse = ''))
	print_report('Cor of top user outdeg vs. total neighbour outdeg', cor(users_correlated_info.df$top_user_odeg,users_correlated_info.df$total_neighbour_odeg))
	# top user's size vs. outdeg
	plot <- ggplot(users_correlated_info.df, aes(x = top_user_odeg, y = log10(size))) + geom_point() + geom_smooth(method=lm) + xlim(0,300) + xlab('Top users\' out degree') + ylab('log of Cascade size')
	save_ggplot(plot,file=paste(c(file_name,'_size_corr.pdf'), collapse = ''))
	print_report('Cor of top user outdeg vs. size', cor(users_correlated_info.df$top_user_odeg,users_correlated_info.df$size))
	# top user's size vs. neighbour's total outdeg
	plot <- ggplot(users_correlated_info.df, aes(x = log10(size), y = total_neighbour_odeg)) + geom_point() + geom_smooth(method=lm) + xlab('log of Cacade size') + ylab('Total neighbor out degree')
	save_ggplot(plot,file=paste(c(file_name,'_size_total_outdeg_corr.pdf'), collapse = ''))
	print_report('Cor of size vs. total neighbour outdeg', cor(users_correlated_info.df$size,users_correlated_info.df$total_neighbour_odeg))
	# top user's size vs. depth
	plot <- ggplot(users_correlated_info.df, aes(x = log10(size), y = depth)) + geom_point() + geom_smooth(method=lm) + xlab('log of Cascade size') + ylab('Cascade depth')
	save_ggplot(plot,file=paste(c(file_name,'_size_depth_corr.pdf'), collapse = ''))
	print_report('Cor of size vs. depth', cor(users_correlated_info.df$size,users_correlated_info.df$depth))
	# regression analysis
	size_model <- glm(log(size)~top_user_odeg+total_neighbour_odeg, family="poisson",data= users_correlated_info.df)
#	print(summary(size_model))
	sm <- summary(size_model)
	pseudo_R_sq <- 1 - sm$deviance/sm$null.deviance
	#plot(sm)
	return(users_correlated_info.df)
}

draw_depth_expansion <- function(file_name,depth_expansion.df){
	cum_time_interval <- vector("list")
	cum_expansion <- vector("list")
	depth <- vector("list")
	max_x <- 0
	max_l_y <- 0
	max_r_y <- 0
	counter <- 0
	for(root_user in unique(depth_expansion.df$root_user_id)){
		counter <- counter + 1
		sub_data <- subset(depth_expansion.df, root_user_id==root_user)
		sub_data_size <- nrow(sub_data)
		cum_time_interval[2*counter-1] <- list(sub_data$cum_time_interval[2:sub_data_size])
		cum_time_interval[2*counter] <- list(sub_data$relative_max_time[2:sub_data_size])
		cum_expansion[counter] <- list(sub_data$cum_expansion[2:sub_data_size])
		depth[counter] <- list(sub_data$depth[2:sub_data_size])
		max_x <- max(max_x, max(cum_time_interval[[2*counter-1]]))
		max_x <- max(max_x, max(cum_time_interval[[2*counter]]))
		max_l_y <- max(max_l_y, max(cum_expansion[[counter]]))
		max_r_y <- max(max_r_y, max(depth[[counter]]))
	}
	draw_two_y_axes_graph(file_name=paste(c(file_name,'time.pdf'), collapse =''), 
			max_curve_count=counter, 
			x_val=cum_time_interval,
			y_l_val=cum_expansion,
			y_r_val=depth, 
			lim_x=c(0,max_x),
			lim_l_y=c(0,max_l_y),
			lim_r_y=c(0,max_r_y), 
			x_label= 'Arrival time',
			y_l_label='Cumulative shell size',
			y_r_label='Depth', 
			graph_name='Cumulative shell size and depth vs. arrival time', 
			x_mark=list(at=c(86400*7,2*86400*7,3*86400*7,4*86400*7,8*86400*7,12*86400*7,16*86400*7,24*86400*7,32*86400*7,52*86400*7), label=c('1 week','2 week','3 week','1 month','2 month','3 month','4 month','6 month', '8 month', '1 year'))
			)
	depth_expansion.df$root_user_id <- factor(depth_expansion.df$root_user_id)
	plot <- ggplot(depth_expansion.df, aes(x = depth, y = (expansion))) + geom_line(aes(group = root_user_id,colour = root_user_id)) + xlab('Depth') + ylab('Shell size') 
	save_ggplot(plot, paste(c(file_name,'_depth_expansion.pdf'), collapse = ''))
	plot <- ggplot(depth_expansion.df, aes(x = depth, y = log10(expansion))) + geom_line(aes(group = root_user_id,colour = root_user_id)) + xlab('Depth') + ylab('log of Shell size') 
	save_ggplot(plot, paste(c(file_name,'_depth_log_expansion.pdf'), collapse = ''))
	plot <- ggplot(depth_expansion.df, aes(x = depth, y = (diff))) + geom_line(aes(group = root_user_id,colour = root_user_id)) + xlab('Depth') + ylab('Shell size growth') + scale_colour_hue(name  ="Cascade root")
	save_ggplot(plot, paste(c(file_name,'_depth_expansion_diff.pdf'), collapse = ''))
}

build_model <- function (trainer_cascade){
	size_model <- glm(log(size)~total_1st_exp+total_2nd_exp+total_3rd_exp+total_4th_exp+ampl_4, family="poisson",data= trainer_cascade) #+total_3rd_exp+total_4th_exp
	sm <- summary(size_model)
	pseudo_R_sq <- 1 - sm$deviance/sm$null.deviance
	print_report('Model summary', summary(size_model))
	print_report('Model R_sq', pseudo_R_sq)
	print_report('Cor of size vs. 1st hop shell size', cor(trainer_cascade$size,trainer_cascade$total_1st_exp))
	print_report('Cor of size vs. 2nd hop shell size', cor(trainer_cascade$size,trainer_cascade$total_2nd_exp))
	print_report('Cor of size vs. 3rd hop shell size', cor(trainer_cascade$size,trainer_cascade$total_3rd_exp))
	print_report('Cor of size vs. 4th hop shell size', cor(trainer_cascade$size,trainer_cascade$total_4th_exp))
	print_report('Cor of size vs. 4 hop amplification', cor(trainer_cascade$size,trainer_cascade$ampl_4))
}

analyze_cascade_growth <- function (fraction, cascade_root){
	time_of_growth <- c()
	growth_rate_frac <- c()
	total_ampl_frac <- c()
	depth_of_growth <- c()
	amplifiers_count <- c()
	amplifiers <- c()
	for(a_root in cascade_root){
		depth_expansion.subdata <- subset(g_prep.df$root_depth_expansion, root_user_id==a_root)
		rooted_top_users.subdata <- subset(g_prep.df$rooted_top_users, root_user==a_root)
#		time_of_growth <- c(time_of_growth,min(depth_expansion.subdata[depth_expansion.subdata$cum_expansion/max(depth_expansion.subdata$cum_expansion)>=fraction,]$cum_time_interval))
#		growth_rate_frac <- c(growth_rate_frac,sum(depth_expansion.subdata[depth_expansion.subdata$cum_expansion/max(depth_expansion.subdata$cum_expansion)<=fraction,]$expansion)/max(depth_expansion.subdata[depth_expansion.subdata$cum_expansion/max(depth_expansion.subdata$cum_expansion)<=fraction,]$cum_time_interval))
#		total_ampl_frac <- c(total_ampl_frac, sum(depth_expansion.subdata[depth_expansion.subdata$cum_expansion/max(depth_expansion.subdata$cum_expansion)<=fraction,]$ampl))
#		depth_of_growth <- c(depth_of_growth,min(depth_expansion.subdata[depth_expansion.subdata$cum_expansion/max(depth_expansion.subdata$cum_expansion)>=fraction,]$depth)/max(depth_expansion.subdata$depth))
		amplifiers_count <- c(amplifiers_count, length(depth_expansion.subdata$ampl[depth_expansion.subdata$ampl > 0]))
		for (each_subroot_depth in unique(rooted_top_users.subdata$at_depth)){
			if(depth_expansion.subdata[depth_expansion.subdata$depth==each_subroot_depth,]$ampl > 0){
				amplifiers <- c(amplifiers, rooted_top_users.subdata[rooted_top_users.subdata$at_depth==each_subroot_depth,]$top_user)
			}
		}
	}
#	print_report('', growth_rate_frac)
#	print(total_ampl_frac)
#	print(length(time_of_growth))
#	print_report('Summary of', fraction)
#	print_report('Growth time', summary(time_of_growth[time_of_growth>0]/86400))
#	print_report('Growth depth', summary(depth_of_growth[depth_of_growth>0]))
	print_report('Amplifiers count per cascade', summary(amplifiers_count))
#	print_report('Cor of ', cor(total_ampl_frac,growth_rate_frac*86400))
#	data_ampl_95 <- data.frame(total_ampl_frac, growth_rate_frac)
#	data_ampl_95$total_ampl_frac <- total_ampl_frac 
#	data_ampl_95$growth_rate_frac <- growth_rate_frac
#	plot <- ggplot(data_ampl_95, aes(x = total_ampl_frac, y = growth_rate_frac*86400)) + geom_point() + geom_smooth(method=lm) + xlab('Amplification till 95% of size') + ylab('95% size/arrival days')
#	ggsave(plot,file=paste(rooted_top_users_file,'_ampl_95_size_corr.pdf'))
	return (amplifiers)
}

root_users_analysis <- function(rooted_top_users_file, depth_vs_expansion_file){
	sink(file = "output.txt", type = "output")
 	cascade_info.df <- ddply(g_prep.df$root_depth_expansion, c('root_user_id'), summarise, size = sum(expansion), max_depth=max(depth), total_ampl=sum(ampl), ampl_4=sum(ampl[depth<=4]), 
			total_1st_exp = expansion[depth==1], total_2nd_exp = c(expansion[depth==2],0)[1], total_3rd_exp = c(expansion[depth==3],0)[1],
			total_4th_exp = c(expansion[depth==4],0)[1],total_5th_exp = c(expansion[depth==5],0)[1])
	very_big_cascades <- cascade_info.df[cascade_info.df$max_depth >= 45,]$root_user_id
	draw_depth_expansion(depth_vs_expansion_file, g_prep.df$root_depth_expansion[g_prep.df$root_depth_expansion$root_user_id%in%very_big_cascades,])
	build_model(cascade_info.df)
	print_report('Cor of size vs total amplification', cor(cascade_info.df$size,cascade_info.df$total_ampl))
#	plot <- ggplot(cascade_info.df, aes(x = total_ampl, y = size)) + geom_point() + geom_smooth(method=lm) + xlab('Total amplification') + ylab('Cascade size')
#	save_ggplot(plot,file=paste(c(rooted_top_users_file,'_ampl_size_corr.pdf'), collapse = ''))
	nonroot_components <- g_prep.df$rooted_top_users[(g_prep.df$rooted_top_users$depth_matching<=5) & (g_prep.df$rooted_top_users$component_size_prop>0.80),]
	not_really_root <- unique(nonroot_components$root_user)
	print_report('Not really root count', length(not_really_root))
	amplifiers <- analyze_cascade_growth(fraction = .95, not_really_root)
	real_root <- union(setdiff(g_prep.df$roots,not_really_root), amplifiers)
	print_report('Real root count', length(real_root))
#	real_root <- sample(g_prep.df$rooted_top_users$root_user, 300)
#	print(real_root)
#	plot <- ggplot(g_prep.df$rooted_top_users, aes(x = depth_matching_prop, y = component_size_prop)) + geom_point()  + xlab('Depth difference') + ylab('Subrooted cascade size / rooted cascade size') #+ geom_smooth(method=lm)
#	ggsave(plot,file=paste(rooted_top_users_file,'_at_real_root_depth_size_prop_corr.pdf'))
#	print(cor(g_prep.df$rooted_top_users$at_depth,g_prep.df$rooted_top_users$component_size_prop))
	real_depth_expansion <- rbind(g_prep.df$root_depth_expansion[g_prep.df$root_depth_expansion$root_user_id%in%real_root,c(1:7,11:13)],
			g_prep.df$nonroot_depth_expansion[g_prep.df$nonroot_depth_expansion$top_user_id%in%real_root,])
#	draw_depth_expansion(depth_vs_expansion_file, g_prep.df$depth_expansion[g_prep.df$depth_expansion$root_user_id%in%real_root,])
	cascade_info.df <- ddply(real_depth_expansion, c('root_user_id'), summarise, size = sum(expansion), depth=max(depth), total_ampl=sum(ampl), ampl_4=sum(ampl[depth<=4]), 
			total_1st_exp = expansion[depth==1], total_2nd_exp = c(expansion[depth==2],0)[1], total_3rd_exp = c(expansion[depth==3],0)[1],
			total_4th_exp = c(expansion[depth==4],0)[1],total_5th_exp = c(expansion[depth==5],0)[1])
#	print(nrow(cascade_info.df))
	print_report('Cor of size vs total amplification (real roots)', cor(cascade_info.df$size,cascade_info.df$total_ampl))
	build_model(cascade_info.df)
	sink()
#	return(cascade_info.df)
}

#nrr <- root_users_analysis('~/output_cascade/full_first_parent/top_size.csv_top_10_rooted_top_users.csv','~/output_cascade/full_first_parent/top_size.csv_top_10_100_depth_vs_expansion.csv')
#colnames(nrr)<-c('','size','total amplification','4 hops amplification','hop 1 shell size','hop 2 shell size','hop 3 shell size','hop 4 shell size','')
#pairs(nrr[,2:6], panel = panel.smooth)

branching_process <- function(outdeg_dist, trial=0, max_generation){
	generation_population <- c()
	random_outdeg_chooser <- function(dist) {
		return(sample(x=dist$outdeg, size=1, prob=(dist$count/sum(dist$count))))
	}
	manage_generation_population <- function(generation, population){
		if (length(generation_population) >= generation)
			generation_population[generation] <<- generation_population[generation] + population
		else
			generation_population <<- c(generation_population, population)
	}
	branching <- function(dist, generation, max_generation){
		if(generation > 60){ #We don't care any population in later generations
		}
		else{
			if (generation < max_generation)
				outdeg <- random_outdeg_chooser(dist[[generation+1]])
			else
				outdeg <- random_outdeg_chooser(dist[[max_generation+1]])
			if(generation > 0)
				manage_generation_population(generation, 1)
			if (outdeg > 0){
				for (each_branch in 1:outdeg){	
					branching(dist, generation+1, max_generation)
				}
			}
		}
	}
	cascades <- list(size=c(),depth=c(),growth=vector("list"))
	for (i in 1:trial){
		generation_population <- c()
		branching(outdeg_dist, 0, max_generation)
		cascades$size <- c(cascades$size, sum(generation_population)+1)
		cascades$depth <- c(cascades$depth, length(generation_population))
		cascades$growth[[i]] <- generation_population
	}
	return(cascades)
}

analyze_branching <- function(dist_file, bin_size){
	outdeg_dist <- as.data.frame(read.csv(dist_file, header=FALSE))
	colnames(outdeg_dist) <- c('outdeg', 'count', 'depth')
	outdeg_dist <- outdeg_dist[order(outdeg_dist$depth,outdeg_dist$outdeg),]
	max_depth <- max(outdeg_dist$depth)
	dist_bin <- list()
	root_outdeg <- outdeg_dist[(outdeg_dist$depth==0),]
	dist_bin[[1]] <- ddply(root_outdeg, c('outdeg'), summarise, count=sum(count))
	for (i in seq(1,max_depth, by=bin_size)){
		binned_data <- outdeg_dist[(outdeg_dist$depth>=i & outdeg_dist$depth<i+bin_size),]
		binned_dist <- ddply(binned_data, c('outdeg'), summarise, count=sum(count))
		for (j in i:min((i+bin_size),max_depth)){
			dist_bin[[j+1]] <- binned_dist
		}
	}
#	print_report('P(0 out degree)',nonroot_deg_dist$count[1]/sum(nonroot_deg_dist$count))
#	expected_root_odeg <- sum(root_deg_dist$outdeg*(root_deg_dist$count/sum(root_deg_dist$count)))
#	expected_nonroot_odeg <- sum(nonroot_deg_dist$outdeg*(nonroot_deg_dist$count/sum(nonroot_deg_dist$count)))
#	print_report('expected root degree', expected_root_odeg)
#	print_report('expected nonroot degree', expected_nonroot_odeg)
	options(expressions = 10000)
	simulated_cascades <- branching_process(dist_bin,trial=10000,max_depth)
	return(simulated_cascades)
}
ab_10 <- analyze_branching('~/output_cascade/full_first_parent/top_size.csv_top_1000_branching_dist.csv',10)
print_report('Summary size', summary(ab_10$size))
print_report('Summary depth', summary(ab_10$depth))
ab_5 <- analyze_branching('~/output_cascade/full_first_parent/top_size.csv_top_1000_branching_dist.csv',5)
print_report('Summary size', summary(ab_5$size))
print_report('Summary depth', summary(ab_5$depth))
ab_1 <- analyze_branching('~/output_cascade/full_first_parent/top_size.csv_top_1000_branching_dist.csv',1)
print_report('Summary size', summary(ab_1$size))
print_report('Summary depth', summary(ab_1$depth))

idx <- c(1:length(ab_10$size))
sorted_idx <- idx[order(-ab_10$size)]
ab_10.df <- data.frame(depth=0:length(ab_10$growth[[sorted_idx[1]]]), shell=c(1,ab_10$growth[[sorted_idx[1]]]), tree=rep(1, times=length(ab_10$growth[[sorted_idx[1]]])+1))
for (i in 2:30){
	data_f <- data.frame(depth=0:length(ab_10$growth[[sorted_idx[i]]]), shell=c(1,ab_10$growth[[sorted_idx[i]]]), tree=rep(i, times=length(ab_10$growth[[sorted_idx[i]]])+1))
	ab_10.df <- rbind(ab_10.df,data_f)
}
ab_10.df$tree <- factor(ab_10.df$tree)
plot <- ggplot(ab_10.df, aes(x = depth, y = (shell))) + geom_line(aes(group = tree,colour = tree)) + xlab('Depth') + ylab('Shell size') 
save_ggplot(plot, paste('~/output_cascade/full_first_parent/branching_bin_10.pdf', collapse = ''))

idx <- c(1:length(ab_5$size))
sorted_idx <- idx[order(-ab_5$size)]
ab_5.df <- data.frame(depth=0:length(ab_5$growth[[sorted_idx[1]]]), shell=c(1,ab_5$growth[[sorted_idx[1]]]), tree=rep(1, times=length(ab_5$growth[[sorted_idx[1]]])+1))
for (i in 2:30){
	data_f <- data.frame(depth=0:length(ab_5$growth[[sorted_idx[i]]]), shell=c(1,ab_5$growth[[sorted_idx[i]]]), tree=rep(i, times=length(ab_5$growth[[sorted_idx[i]]])+1))
	ab_5.df <- rbind(ab_5.df,data_f)
}
ab_5.df$tree <- factor(ab_5.df$tree)
plot <- ggplot(ab_5.df, aes(x = depth, y = (shell))) + geom_line(aes(group = tree,colour = tree)) + xlab('Depth') + ylab('Shell size') 
save_ggplot(plot, paste('~/output_cascade/full_first_parent/branching_bin_5.pdf', collapse = ''))

idx <- c(1:length(ab_1$size))
sorted_idx <- idx[order(-ab_1$size)]
ab_1.df <- data.frame(depth=0:length(ab_1$growth[[sorted_idx[1]]]), shell=c(1,ab_1$growth[[sorted_idx[1]]]), tree=rep(1, times=length(ab_1$growth[[sorted_idx[1]]])+1))
for (i in 2:30){
	data_f <- data.frame(depth=0:length(ab_1$growth[[sorted_idx[i]]]), shell=c(1,ab_1$growth[[sorted_idx[i]]]), tree=rep(i, times=length(ab_1$growth[[sorted_idx[i]]])+1))
	ab_1.df <- rbind(ab_1.df,data_f)
}
ab_1.df$tree <- factor(ab_1.df$tree)
plot <- ggplot(ab_1.df, aes(x = depth, y = (shell))) + geom_line(aes(group = tree,colour = tree)) + xlab('Depth') + ylab('Shell size') 
save_ggplot(plot, paste('~/output_cascade/full_first_parent/branching_bin_1.pdf', collapse = ''))
