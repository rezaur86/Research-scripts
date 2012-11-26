dir_name <- 'fp_nt_u'

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

process_data <- function(rooted_top_users_file, depth_vs_expansion_file, roots_correlated_info_file,dist_file){
	depth_expansion <- as.data.frame(read.csv(depth_vs_expansion_file, header=FALSE))
	colnames(depth_expansion) <- c('depth', 'expansion', 'root_user_id', 'is_unique', 'time','max_time')
	roots_correlated_info <- as.data.frame(read.csv(roots_correlated_info_file, header=FALSE))
	colnames(roots_correlated_info) <- c('root_user','root_odeg', 'neighbour_odeg', 'size', 'depth')	
	rooted_top_users <- as.data.frame(read.csv(rooted_top_users_file, header=FALSE))
	colnames(rooted_top_users) <- c('root_user', 'size', 'depth', 'top_user', 'at_depth', 'of_size', 'of_depth')
	odeg_dist <- as.data.frame(read.csv(dist_file, header=FALSE))
	colnames(odeg_dist) <- c('outdeg', 'count', 'depth')
	odeg_dist <- odeg_dist[order(odeg_dist$depth,odeg_dist$outdeg),]
	depth_expansion.df <- ddply(depth_expansion, c('root_user_id'), function(one_partition){
				one_partition = one_partition[order(one_partition$depth),]
				one_partition$diff = (one_partition$expansion-c(0,one_partition$expansion[1:nrow(one_partition)-1]))
				one_partition$diff2 = (one_partition$diff-c(0,one_partition$diff[1:nrow(one_partition)-1]))
				sgs = shell_growth_state(one_partition$diff2)
				one_partition$state = sgs$state
				one_partition$ampl = sgs$amplifier
				one_partition$cum_time_interval = one_partition$time - one_partition$time[2]
				one_partition$relative_max_time = one_partition$max_time - one_partition$time[2]
				one_partition$cum_expansion = cumsum(one_partition$expansion)
				one_partition$factor = ((one_partition$expansion-c(NA,one_partition$expansion[1:nrow(one_partition)-1]))/c(1,one_partition$expansion[1:nrow(one_partition)-1]))
				alpha <- (max(one_partition$expansion))^(1/min(one_partition[one_partition$expansion==max(one_partition$expansion),]$depth))
				one_partition$expansion_norm = one_partition$expansion / (alpha^(one_partition$depth))
				one_partition
			})
	rooted_top_users.df <- ddply(rooted_top_users, c('root_user','size','depth'), function(one_partition){
				one_partition$component_size_prop = one_partition$of_size/one_partition$size
				one_partition$depth_matching = one_partition$depth-one_partition$of_depth
				one_partition
			})
	roots_correlated_info.df <- ddply(roots_correlated_info, c('root_odeg','size','depth'), summarise, avg_neighbour_odeg = mean(neighbour_odeg), 
			total_neighbour_odeg = sum(neighbour_odeg))
	root_users <- unique(roots_correlated_info$root_user)
	return(list(depth_expansion=depth_expansion.df,
					roots = root_users,
					roots_correlated_info=roots_correlated_info.df,
					rooted_top_users=rooted_top_users.df,
					outdeg_dist=odeg_dist))
}

prev_dir <- getwd()
setwd(dir_name)
g_prep.df <- process_data('rooted_top_users.csv','top_size.csv_all_100_depth_vs_expansion.csv', 'top_size.csv_all_roots_correlated_info.csv', 'top_size.csv_all_branching_dist.csv')		
setwd(prev_dir)
