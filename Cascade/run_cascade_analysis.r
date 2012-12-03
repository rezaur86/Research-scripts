source('~/scripts/cascade/tools.r')

dir_name <- 'fp_test_1'

library(plyr)

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

process_data <- function(rooted_top_users_file, depth_vs_expansion_file, nonroot_depth_vs_expansion_file, roots_correlated_info_file, dist_file){
	rooted_top_users <- as.data.frame(read.csv(rooted_top_users_file, header=FALSE))
	colnames(rooted_top_users) <- c('root_user', 'size', 'depth', 'top_user', 'at_depth', 'of_size', 'of_depth')
	print_report("loading done",rooted_top_users_file)
	root_depth_expansion <- as.data.frame(read.csv(depth_vs_expansion_file, header=FALSE))
	root_depth_expansion[,4] <- 1
	colnames(root_depth_expansion) <- c('depth', 'expansion', 'root_user_id', 'is_unique', 'time','max_time')
	print_report("loading done",depth_vs_expansion_file)
	nonroot_depth_expansion <- as.data.frame(read.csv(nonroot_depth_vs_expansion_file, header=FALSE))
	nonroot_depth_expansion[,4] <- 0
	colnames(nonroot_depth_expansion) <- c('depth', 'expansion', 'top_user_id', 'is_unique', 'time','max_time')
	print_report("loading done",nonroot_depth_vs_expansion_file)
	roots_correlated_info <- as.data.frame(read.csv(roots_correlated_info_file, header=FALSE))
	colnames(roots_correlated_info) <- c('root_user','root_odeg', 'neighbour_odeg', 'size', 'depth')	
	odeg_dist <- as.data.frame(read.csv(dist_file, header=FALSE))
	print_report("loading done",roots_correlated_info_file)
	colnames(odeg_dist) <- c('outdeg', 'count', 'depth')
	odeg_dist <- odeg_dist[order(odeg_dist$depth,odeg_dist$outdeg),]
	print_report("loading done",dist_file)
	print_report("All file loading done",date())
	root_depth_expansion.df <- ddply(root_depth_expansion, c('root_user_id'), function(one_partition){
				one_partition = one_partition[order(one_partition$depth),]
				one_partition$diff = (one_partition$expansion-c(0,one_partition$expansion[1:nrow(one_partition)-1]))
				one_partition$diff2 = (one_partition$diff-c(0,one_partition$diff[1:nrow(one_partition)-1]))
				sgs <- shell_growth_state(one_partition$diff2)
				one_partition$state = sgs$state
				one_partition$ampl = sgs$amplifier
				one_partition$cum_time_interval = one_partition$time - one_partition$time[2]
				one_partition$relative_max_time = one_partition$max_time - one_partition$time[2]
				one_partition$cum_expansion = cumsum(one_partition$expansion)
#						one_partition$factor = ((one_partition$expansion-c(NA,one_partition$expansion[1:nrow(one_partition)-1]))/c(1,one_partition$expansion[1:nrow(one_partition)-1]))
#						alpha <- (max(one_partition$expansion))^(1/min(one_partition[one_partition$expansion==max(one_partition$expansion),]$depth))
#						one_partition$expansion_norm = one_partition$expansion / (alpha^(one_partition$depth))
				one_partition
			})
	print_report("Root expansion preprocessed",date())
	nonroot_depth_expansion.df <- ddply(nonroot_depth_expansion, c('top_user_id'), function(one_partition){
				one_partition = one_partition[order(one_partition$depth),]
				one_partition$diff = (one_partition$expansion-c(0,one_partition$expansion[1:nrow(one_partition)-1]))
				one_partition$diff2 = (one_partition$diff-c(0,one_partition$diff[1:nrow(one_partition)-1]))
				sgs <- shell_growth_state(one_partition$diff2)
				one_partition$state = sgs$state
				one_partition$ampl = sgs$amplifier
				one_partition$cum_time_interval = one_partition$time - one_partition$time[2]
				one_partition$relative_max_time = one_partition$max_time - one_partition$time[2]
				one_partition$cum_expansion = cumsum(one_partition$expansion)
				one_partition
			})
	print_report("Top users expansion preprocessed",date())
	rooted_top_users.df <- ddply(rooted_top_users, c('root_user','size','depth'), function(one_partition){
				one_partition$component_size_prop = one_partition$of_size/one_partition$size
				one_partition$depth_matching = one_partition$depth-one_partition$of_depth
				one_partition
			})
	print_report("Rooted top users preprocessed",date())
	roots_correlated_info.df <- ddply(roots_correlated_info, c('root_odeg','size','depth'), summarise, avg_neighbour_odeg = mean(neighbour_odeg), total_neighbour_odeg = sum(neighbour_odeg))
	root_users <- unique(root_depth_expansion$root_user_id)
	nonroot_users <- unique(nonroot_depth_expansion$top_user_id)
	return(list(root_depth_expansion=root_depth_expansion.df,
					nonroot_depth_expansion=nonroot_depth_expansion.df,
					roots = root_users,
					top_users = nonroot_users,
					roots_correlated_info=roots_correlated_info.df,
					rooted_top_users=rooted_top_users.df,
					outdeg_dist=odeg_dist))
}

prev_dir <- getwd()
setwd(dir_name)
g_prep.df <- process_data('rooted_top_users.csv','top_size.csv_all_100_depth_vs_expansion.csv', 'nonroot_top_users.csv_all_100_depth_vs_expansion.csv',
		'top_size.csv_all_roots_correlated_info.csv', 'top_size.csv_all_branching_dist.csv')		
setwd(prev_dir)
