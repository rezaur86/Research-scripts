source('/home/rezaur/Documents/Code/Cascade/tools.r')

library(ggplot2)
library(plyr)

read_data <- function(rooted_top_users_file, depth_vs_expansion_file, users_correlated_info_file){
	depth_expansion <- as.data.frame(read.csv(depth_vs_expansion_file, header=FALSE))
	colnames(depth_expansion) <- c('depth', 'expansion', 'root_user_id', 'is_unique', 'time')
	users_correlated_info <- as.data.frame(read.csv(users_correlated_info_file, header=FALSE))
	colnames(users_correlated_info) <- c('top_user','top_user_odeg', 'neighbour_odeg', 'size', 'depth')	
	rooted_top_users <- as.data.frame(read.csv(rooted_top_users_file, header=FALSE))
	colnames(rooted_top_users) <- c('root_user', 'size', 'depth', 'component_top_user', 'at_depth', 'of_size', 'of_depth')
	return(list(depth_expansion,users_correlated_info,rooted_top_users))
}

load_data <- function(dir_name, top_n){
	prev_dir = getwd()
	setwd(dir_name)
	if (top_n == 10){
		data <- read_data('top_size.csv_top_10_rooted_top_users.csv','top_size.csv_top_10_100_depth_vs_expansion.csv', 'top_size.csv_top_10users_correlated_info.csv')
	}
	if (top_n == 100){
		data <- read_data('top_size.csv_top_100_rooted_top_users.csv','top_size.csv_top_100_100_depth_vs_expansion.csv', 'top_size.csv_top_100users_correlated_info.csv')		
	}
	if (top_n == 1000){
		data <- read_data('top_size.csv_top_1000_rooted_top_users.csv','top_size.csv_top_1000_100_depth_vs_expansion.csv', 'top_size.csv_top_1000users_correlated_info.csv')
	}
	if (top_n == 10000){
		data <- read_data('top_size.csv_top_10000_rooted_top_users.csv','top_size.csv_top_10000_100_depth_vs_expansion.csv', 'top_size.csv_top_10000users_correlated_info.csv')
	}
	setwd(prev_dir)
	return (data)
}

top_size_analysis <- function(file_name){
	users_correlated_info <- as.data.frame(read.csv(file_name, header=FALSE))
	colnames(users_correlated_info) <- c('top_user','top_user_odeg', 'neighbour_odeg', 'size', 'depth')	
	users_correlated_info.df <- ddply(users_correlated_info, c('top_user_odeg','size','depth'), summarise, avg_neighbour_odeg = mean(neighbour_odeg), total_neighbour_odeg = sum(neighbour_odeg))
	# top user's outged vs. avg neighbour outdeg
	plot <- ggplot(users_correlated_info.df, aes(x = top_user_odeg, y = avg_neighbour_odeg)) + xlim(0,100) + geom_point() + geom_smooth(method=lm)
	save_ggplot(plot,file=paste(c(file_name,'_avg_odeg_corr.pdf'), collapse = ''))
	print_report('Cor of top user outdeg vs. avg neighbour outdeg', cor(users_correlated_info.df$top_user_odeg,users_correlated_info.df$avg_neighbour_odeg))
	# top user's outdeg vs. total neihbour outdeg
	plot <- ggplot(users_correlated_info.df, aes(x = top_user_odeg, y = total_neighbour_odeg)) + xlim(0,100) + geom_point() + geom_smooth(method=lm) + xlab('Top users\' out degree') + ylab('Neighbor\'s total out degree')
	save_ggplot(plot,file=paste(c(file_name,'_total_odeg_corr.pdf', collapse = '')))
	print_report('Cor of top user outdeg vs. total neighbour outdeg', cor(users_correlated_info.df$top_user_odeg,users_correlated_info.df$total_neighbour_odeg))
	# top user's size vs. outdeg
	plot <- ggplot(users_correlated_info.df, aes(x = top_user_odeg, y = log10(size))) + geom_point() + geom_smooth(method=lm) + xlim(0,300) + xlab('Top users\' out degree') + ylab('log of Cascade size')
	save_ggplot(plot,file=paste(c(file_name,'_size_corr.pdf'), collapse = ''))
	print_report('Cor of top user outdeg vs. size', cor(users_correlated_info.df$top_user_odeg,users_correlated_info.df$size))
	# top user's size vs. neighbour's total outdeg
	plot <- ggplot(users_correlated_info.df, aes(x = log10(size), y = total_neighbour_odeg)) + geom_point() + geom_smooth(method=lm) + xlab('log of Cacade size') + ylab('Total neighbor out degree')
	save_ggplot(plot,file=paste(c(file_name,'_size_total_outdeg_corr.pdf', collapse = '')))
	print_report('Cor of size vs. total neighbour outdeg', cor(users_correlated_info.df$size,users_correlated_info.df$total_neighbour_odeg))
	# top user's size vs. depth
	plot <- ggplot(users_correlated_info.df, aes(x = log10(size), y = depth)) + geom_point() + geom_smooth(method=lm) + xlab('log of Cascade size') + ylab('Cascade depth')
	save_ggplot(plot,file=paste(c(file_name,'_size_depth_corr.pdf', collapse = '')))
	print_report('Cor of size vs. depth', cor(users_correlated_info.df$size,users_correlated_info.df$depth))
	# regression analysis
	size_model <- glm(log(size)~top_user_odeg+total_neighbour_odeg, family="poisson",data= users_correlated_info.df)
#	print(summary(size_model))
	sm <- summary(size_model)
	pseudo_R_sq <- 1 - sm$deviance/sm$null.deviance
	#plot(sm)
	return(size_model)
}

shell_growth_state <- function(diff2){
	state <- 0
	v_size <- length(diff2)
	state_out <- c(rep(0,v_size))
	ampl <- c(rep(0,v_size))
	ampl_state <- 0
	ampl_idx <- 1
	for (i in 2:v_size){
		if (diff2[i]>0 & state==0){
			state_out[i] <- 1
			state <- 1
		}
		else if (diff2[i]<=0 & state==0){
			state_out[i] <- -1
			state <- -1
		}
		else if (diff2[i]>0 & state==1){
			state_out[i] <- 2
			state <- 2
			ampl[ampl_idx] <- state - ampl_state
		}
		else if (diff2[i]<=0 & state==1){
			state_out[i] <- -1
			state <- -1
		}
		else if (diff2[i]>0 & state==2){
			state_out[i] <- 2
			ampl[ampl_idx] <- ampl[ampl_idx] + 1
		}
		else if (diff2[i]<=0 & state==2){
			state_out[i] <- 1
			state <- 1
		}
		else if (diff2[i]>0 & state==-1){
			state_out[i] <- 0
			state <- 0
		}
		else if (diff2[i]<=0 & state==-1){
			state_out[i] <- -2
			state <- -2
		}
		else if (diff2[i]>0 & state==-2){
			state_out[i] <- -1
			state <- -1
		}
		else if (diff2[i]<=0 & state==-2){
			state_out[i] <- -2
		}
		if ((state_out[i] <= state_out[i-1]) & (state_out[i] != 2)){
			ampl_state <- state
			ampl_idx <- i
		}
	}
	return(list(state=state_out, amplifier=ampl))
}

get_unique_cascade <- function (depth_expansion){
	unique_cascade <- depth_expansion[depth_expansion$is_unique==1,]
	return (list(depth_expansion=unique_cascade, count=length(unique(unique_cascade$root_user_id))))
}
get_non_unique_cascade <- function (depth_expansion){
	return (depth_expansion[depth_expansion$is_unique==0,])
}
process_data <- function (rooted_top_users, depth_expansion){
	depth_expansion.df <- ddply(depth_expansion, c('root_user_id'), function(one_partition){
				one_partition = one_partition[order(one_partition$depth),]
				one_partition$diff = (one_partition$expansion-c(0,one_partition$expansion[1:nrow(one_partition)-1]))
				one_partition$diff2 = (one_partition$diff-c(0,one_partition$diff[1:nrow(one_partition)-1]))
				sgs = shell_growth_state(one_partition$diff2)
				one_partition$state = sgs$state
				one_partition$ampl = sgs$amplifier
				one_partition$time_interval = (one_partition$time-c(one_partition$time[1],one_partition$time[2],one_partition$time[3:nrow(one_partition)-1]))
				one_partition$cum_time_interval = c(0,cumsum(one_partition$time_interval[2:nrow(one_partition)]))
				one_partition$cum_expansion = cumsum(one_partition$expansion)
				one_partition$factor = ((one_partition$expansion-c(NA,one_partition$expansion[1:nrow(one_partition)-1]))/c(1,one_partition$expansion[1:nrow(one_partition)-1]))
				alpha <- (max(one_partition$expansion))^(1/min(one_partition[one_partition$expansion==max(one_partition$expansion),]$depth))
				one_partition$expansion_norm = one_partition$expansion / (alpha^(one_partition$depth))
				one_partition
			})
	rooted_top_users.df <- ddply(rooted_top_users, c('root_user','size','depth'), function(one_partition){
				one_partition$component_size_prop = one_partition$of_size/one_partition$size
				one_partition$depth_matching_prop = one_partition$depth-one_partition$of_depth
				one_partition
			})
	return(list(rooted_top_users=rooted_top_users.df,depth_expansion=depth_expansion.df))
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
		cum_time_interval[counter] <- list(sub_data$cum_time_interval)
		cum_expansion[counter] <- list(sub_data$cum_expansion)
		depth[counter] <- list(sub_data$depth)
		max_x <- max(max_x, max(cum_time_interval[[counter]]))
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
			x_mark=list(at=c(86400*7,2*86400*7,3*86400*7,4*86400*7,8*86400*7,12*86400*7,16*86400*7), label=c('1 week','2 week','3 week','1 month','2 month','3 month','4 month')))
	depth_expansion.df$root_user_id <- factor(depth_expansion.df$root_user_id)
	plot <- ggplot(depth_expansion.df, aes(x = depth, y = (expansion))) + geom_line(aes(group = root_user_id,colour = root_user_id)) + xlab('Depth') + ylab('Shell size') 
	save_ggplot(plot, paste(c(file_name,'_depth_expansion.pdf'), collapse = ''))
	plot <- ggplot(depth_expansion.df, aes(x = depth, y = log10(expansion))) + geom_line(aes(group = root_user_id,colour = root_user_id)) + xlab('Depth') + ylab('log of Shell size') 
	save_ggplot(plot, paste(c(file_name,'_depth_log_expansion.pdf'), collapse = ''))
	plot <- ggplot(depth_expansion.df, aes(x = depth, y = (diff))) + geom_line(aes(group = root_user_id,colour = root_user_id)) + xlab('Depth') + ylab('Shell size growth') + scale_colour_hue(name  ="Cascade root")
	save_ggplot(plot, paste(c(file_name,'_depth_expansion_norm.pdf'), collapse = ''))
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
	print_report('Cor of size vs. 4 hop amplification hop shell size', cor(trainer_cascade$size,trainer_cascade$ampl_4))
}

analyze_cascade_growth <- function (fraction, depth_expansion.df, rooted_top_users.df){
	time_of_growth <- c()
	growth_rate_frac <- c()
	total_ampl_frac <- c()
	depth_of_growth <- c()
	amplifiers_count <- c()
	amplifiers <- c()
	for(a_root in unique(depth_expansion.df$root_user_id)){
		depth_expansion.subdata <- subset(depth_expansion.df, root_user_id==a_root)
		rooted_top_users.subdata <- subset(rooted_top_users.df, root_user==a_root)
		time_of_growth <- c(time_of_growth,min(depth_expansion.subdata[depth_expansion.subdata$cum_expansion/max(depth_expansion.subdata$cum_expansion)>=fraction,]$cum_time_interval))
		growth_rate_frac <- c(growth_rate_frac,sum(depth_expansion.subdata[depth_expansion.subdata$cum_expansion/max(depth_expansion.subdata$cum_expansion)<=fraction,]$expansion)/max(depth_expansion.subdata[depth_expansion.subdata$cum_expansion/max(depth_expansion.subdata$cum_expansion)<=fraction,]$cum_time_interval))
		total_ampl_frac <- c(total_ampl_frac, sum(depth_expansion.subdata[depth_expansion.subdata$cum_expansion/max(depth_expansion.subdata$cum_expansion)<=fraction,]$ampl))
		depth_of_growth <- c(depth_of_growth,min(depth_expansion.subdata[depth_expansion.subdata$cum_expansion/max(depth_expansion.subdata$cum_expansion)>=fraction,]$depth)/max(depth_expansion.subdata$depth))
		amplifiers_count <- c(amplifiers_count, length(depth_expansion.subdata$ampl[depth_expansion.subdata$ampl > 0]))
		for (each_subroot_depth in unique(rooted_top_users.subdata$at_depth)){
			if(depth_expansion.subdata[depth_expansion.subdata$depth==each_subroot_depth,]$ampl > 0){
				amplifiers <- c(amplifiers, rooted_top_users.subdata[rooted_top_users.subdata$at_depth==each_subroot_depth,]$component_top_user)
			}
		}
	}
#	print_report('', growth_rate_frac)
#	print(total_ampl_frac)
#	print(length(time_of_growth))
	print_report('Summary of', fraction)
	print_report('Growth time', summary(time_of_growth[time_of_growth>0]/86400))
	print_report('Growth depth', summary(depth_of_growth[depth_of_growth>0]))
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
	rooted_top_users <- as.data.frame(read.csv(rooted_top_users_file, header=FALSE))
	colnames(rooted_top_users) <- c('root_user', 'size', 'depth', 'component_top_user', 'at_depth', 'of_size', 'of_depth')
	depth_expansion <- as.data.frame(read.csv(depth_vs_expansion_file, header=FALSE))
	colnames(depth_expansion) <- c('depth', 'expansion', 'root_user_id', 'is_unique', 'time', 'max_time')
	framed_data <- process_data(rooted_top_users, depth_expansion)
	rooted_top_users.df <- framed_data$rooted_top_users
	depth_expansion.df <- framed_data$depth_expansion
	unique_cascade <- get_unique_cascade(depth_expansion.df)
	print_report('Unique cascade count', unique_cascade$count)
	print_report('Unique cascade covers', sum(unique_cascade$depth_expansion$expansion))
	users_correlated_info <- depth_expansion.df
	users_correlated_info.df <- ddply(users_correlated_info, c('root_user_id'), summarise, size = sum(expansion), total_ampl=sum(ampl), ampl_4=sum(ampl[depth<=4]), total_1st_exp = expansion[depth==1], total_2nd_exp = expansion[depth==2], total_3rd_exp = expansion[depth==3],total_4th_exp = expansion[depth==4],total_5th_exp = expansion[depth==5])
	draw_depth_expansion(depth_vs_expansion_file, unique_cascade$depth_expansion)
	build_model(users_correlated_info.df)
	print_report('Cor of size vs total amplification', cor(users_correlated_info.df$size,users_correlated_info.df$total_ampl))
	plot <- ggplot(users_correlated_info.df, aes(x = total_ampl, y = size)) + geom_point() + geom_smooth(method=lm) + xlab('Total amplification') + ylab('Cascade size')
	save_ggplot(plot,file=paste(c(rooted_top_users_file,'_ampl_size_corr.pdf'), collapse = ''))
	not_really_root <- rooted_top_users.df[(rooted_top_users.df$depth-rooted_top_users.df$of_dept>=1) & (rooted_top_users.df$component_size_prop>0.80),]
	amplifiers <- analyze_cascade_growth(fraction = .95, depth_expansion.df, rooted_top_users.df)
	real_root <- union(setdiff(unique(depth_expansion$root_user_id),unique(not_really_root$root_user)), amplifiers)
	print_report('Real root count', length(real_root))
#	real_root <- sample(rooted_top_users.df$root_user, 300)
#	print(real_root)
#	plot <- ggplot(rooted_top_users.df, aes(x = depth_matching_prop, y = component_size_prop)) + geom_point()  + xlab('Depth difference') + ylab('Subrooted cascade size / rooted cascade size') #+ geom_smooth(method=lm)
#	ggsave(plot,file=paste(rooted_top_users_file,'_at_real_root_depth_size_prop_corr.pdf'))
#	print(cor(rooted_top_users.df$at_depth,rooted_top_users.df$component_size_prop))
	rooted_top_users.df <- rooted_top_users.df[rooted_top_users.df$root_user%in%real_root,]
	users_correlated_info <- depth_expansion.df[depth_expansion.df$root_user_id%in%real_root,]
#	draw_depth_expansion(depth_vs_expansion_file, depth_expansion.df[depth_expansion.df$root_user_id%in%real_root,])
	users_correlated_info.df <- ddply(users_correlated_info, c('root_user_id'), summarise, size = sum(expansion), total_ampl=sum(ampl), ampl_4=sum(ampl[depth<=4]), total_1st_exp = expansion[depth==1], total_2nd_exp = expansion[depth==2], total_3rd_exp = expansion[depth==3],total_4th_exp = expansion[depth==4],total_5th_exp = expansion[depth==5])
	print(nrow(users_correlated_info.df))
	print(cor(users_correlated_info.df$size,users_correlated_info.df$total_ampl))
	build_model(users_correlated_info.df)
	return(users_correlated_info.df)
}

#nrr <- root_users_analysis('~/output_cascade/full_first_parent/top_size.csv_top_10_rooted_top_users.csv','~/output_cascade/full_first_parent/top_size.csv_top_10_100_depth_vs_expansion.csv')
#colnames(nrr)<-c('','size','total amplification','4 hops amplification','hop 1 shell size','hop 2 shell size','hop 3 shell size','hop 4 shell size','')
#pairs(nrr[,2:6], panel = panel.smooth)

branching_process <- function(dist_file){
	outdeg_dist <- as.data.frame(read.csv(dist_file, header=FALSE))
	colnames(outdeg_dist) <- c('outdeg', 'count', 'is_root')
	generation_population <- c()
	random_outdeg_chooser <- function(dist) {
		return(sample(x=dist$outdeg, size=1, prob=(dist$count/sum(dist$count))))
	}
	manage_generation_population <- function(generation, population){
		if (length(generation_population) >= generation){
			generation_population[generation] <<- generation_population[generation] + population
		}
		else{
			generation_population <<- c(generation_population, population)
		}
	}
	branching <- function(dist, generation){
		outdeg <- random_outdeg_chooser(dist)
		if (outdeg==0 | generation==100){
			manage_generation_population(generation, 1)
			return (1)
		}
		else{
			manage_generation_population(generation, 1)
			for (each_branch in 1:outdeg){	
				branching(dist, generation+1)
			}
			return(1)
		}
	}
	cascades <- vector("list")
	for (i in 1:sum(outdeg_dist[outdeg_dist$is_root==1,]$count)){
		cascades[i] <- list(size=c(1),depth=c(0),growth=c())
		root_odeg <- random_outdeg_chooser(outdeg_dist[outdeg_dist$is_root==1,])
		generation_population <- c()
		if (root_odeg != 0){
			for (j in 1:root_odeg){
				branching(outdeg_dist[outdeg_dist$is_root==0,], 1)
			}
			cascades[[i]]$size <- sum(generation_population)
			cascades[[i]]$depth <- length(generation_population)
			cascades[[i]]$growth <- generation_population
		}
		print(cascades[[i]])
	}
	return(cascades)
}