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

top_size_analysis <- function(file_name){
	library(ggplot2)
	library(plyr)
	users_correlated_info <- as.data.frame(read.csv(file_name, header=FALSE))
	colnames(users_correlated_info) <- c('top_user','top_user_odeg', 'neighbour_odeg', 'size', 'depth')	
	users_correlated_info.df <- ddply(users_correlated_info, c('top_user_odeg','size','depth'), summarise, avg_neighbour_odeg = mean(neighbour_odeg), total_neighbour_odeg = sum(neighbour_odeg))
	# top user's outged vs. avg neighbour outdeg
	plot <- ggplot(users_correlated_info.df, aes(x = top_user_odeg, y = avg_neighbour_odeg)) + xlim(0,100) + geom_point() + geom_smooth(method=lm)
	ggsave(plot,file=paste(file_name,'_avg_odeg_corr.eps'))
	print(cor(users_correlated_info.df$top_user_odeg,users_correlated_info.df$avg_neighbour_odeg))
	# top user's outdeg vs. total neihbour outdeg
	plot <- ggplot(users_correlated_info.df, aes(x = top_user_odeg, y = total_neighbour_odeg)) + xlim(0,100) + geom_point() + geom_smooth(method=lm) + xlab('Top users\' out degree') + ylab('Neighbor\'s total out degree')
	ggsave(plot,file=paste(file_name,'_total_odeg_corr.eps'))
	print(cor(users_correlated_info.df$top_user_odeg,users_correlated_info.df$total_neighbour_odeg))
	# top user's size vs. outdeg
	plot <- ggplot(users_correlated_info.df, aes(x = top_user_odeg, y = log10(size))) + geom_point() + geom_smooth(method=lm) + xlim(0,300) + xlab('Top users\' out degree') + ylab('log of Cascade size')
	ggsave(plot,file=paste(file_name,'_size_corr.eps'))
	print(cor(users_correlated_info.df$top_user_odeg,users_correlated_info.df$size))
	# top user's size vs. neighbour's total outdeg
	plot <- ggplot(users_correlated_info.df, aes(x = log10(size), y = total_neighbour_odeg)) + geom_point() + geom_smooth(method=lm) + xlab('log of Cacade size') + ylab('Total neighbor out degree')
	ggsave(plot,file=paste(file_name,'_size_total_outdeg_corr.eps'))
	print(cor(users_correlated_info.df$size,users_correlated_info.df$total_neighbour_odeg))
	# top user's size vs. depth
	plot <- ggplot(users_correlated_info.df, aes(x = log10(size), y = depth)) + geom_point() + geom_smooth(method=lm) + xlab('log of Cascade size') + ylab('Cascade depth')
	ggsave(plot,file=paste(file_name,'_size_depth_corr.eps'))
	print(cor(users_correlated_info.df$size,users_correlated_info.df$depth))
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

depth_vs_expansion <- function(file_name, depth_expansion){
	library(ggplot2)
	library(plyr)
	depth_expansion.df <- ddply(depth_expansion[depth_expansion$is_unique<=1,], c('root_user_id'), function(one_partition){
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
	print('total coverage')
	print(length(unique(depth_expansion[depth_expansion$is_unique<=1,]$root_user_id)))
	print(sum(depth_expansion[depth_expansion$is_unique==1,]$expansion))
#	print (head(depth_expansion.df,100))
#	print (nrow(depth_expansion.df[depth_expansion.df$depth==0,]))
	depth_expansion.df$root_user_id <- factor(depth_expansion.df$root_user_id)
	pdf(file=paste(file_name,'time.pdf'), #convert -density 300x300 top_size.csv_top_10_100_depth_vs_expansion.csv\ time.pdf time.jpg
			#width=10,
			#height=10
			)
	color <- rainbow(32)
	counter <- 1
	for(root_user in unique(depth_expansion.df$root_user_id)){
		sub_data <- subset(depth_expansion.df, root_user_id==root_user)
		if(counter == 1){
			plot(sub_data$cum_time_interval, y=sub_data$cum_expansion, ylim=c(0,1.5*max(sub_data$cum_expansion)),xlim=c(0,max(sub_data$cum_time_interval)),col=color[counter], type='n', main='Cumulative shell size and depth vs. arrival time', xlab='Arrival time', ylab='Cumulative shell size', xaxt='n', yaxt='n', lwd=.75)
			axis(2, pretty(c(0, 1.5*max(sub_data$cum_expansion))), col='blue')
			axis(1, pretty(c(0, max(sub_data$cum_time_interval))))
		}
		if(counter <= 32){
			lines(x=sub_data$cum_time_interval, y=sub_data$cum_expansion, col=color[counter], lwd=0.75, type="o", lty=2)
		}
		counter <- counter + 1
	}
	par(new=T)
	counter <- 1
	for(root_user in unique(depth_expansion.df$root_user_id)){
		sub_data <- subset(depth_expansion.df, root_user_id==root_user)
		if(counter == 1){
			plot(sub_data$cum_time_interval, y=sub_data$depth, ylim=c(0,1.5*max(sub_data$depth)),col=color[counter], type='n', lwd=0.75, xaxt='n', axes=F, ylab='', xlab='')
			axis(4, pretty(c(0, 1.5*max(depth_expansion.df$depth))), col='green', labels=T)
			mtext("Depth", side=4)
		}
		if(counter <= 32){
			lines(x=sub_data$cum_time_interval, y=sub_data$depth, col=color[counter], lwd=0.75, type = 's')
		}
		counter <- counter + 1
	}
	dev.off()
	plot <- ggplot(depth_expansion.df, aes(x = depth, y = (expansion))) + geom_line(aes(group = root_user_id,colour = root_user_id)) + xlab('Depth') + ylab('Shell size') 
	ggsave(plot,file=paste(file_name,'_depth_expansion.eps'))
	plot <- ggplot(depth_expansion.df, aes(x = depth, y = log10(expansion))) + geom_line(aes(group = root_user_id,colour = root_user_id)) + xlab('Depth') + ylab('log of Shell size') 
	ggsave(plot,file=paste(file_name,'_depth_log_expansion.eps'))
	plot <- ggplot(depth_expansion.df, aes(x = depth, y = (diff))) + geom_line(aes(group = root_user_id,colour = root_user_id)) + xlab('Depth') + ylab('Shell size growth') + scale_colour_hue(name  ="True influencer")
	ggsave(plot,file=paste(file_name,'_depth_expansion_norm.eps'))
	return(depth_expansion.df)
}

root_users_analysis <- function(file_name, file_root_info){
	library(ggplot2)
	library(plyr)
	rooted_top_users <- as.data.frame(read.csv(file_name, header=FALSE))
	colnames(rooted_top_users) <- c('root_user', 'size', 'depth', 'component_top_user', 'at_depth', 'of_size', 'of_depth')	
	rooted_top_users.df <- ddply(rooted_top_users, c('root_user','size','depth'), function(one_partition){
				one_partition$component_size_prop = one_partition$of_size/one_partition$size
				one_partition$depth_matching_prop = one_partition$depth-one_partition$of_depth
				one_partition
			})
	depth_expansion <- as.data.frame(read.csv(file_root_info, header=FALSE))
	colnames(depth_expansion) <- c('depth', 'expansion', 'root_user_id', 'is_unique', 'time')
	depth_expansion.df <- depth_vs_expansion(file_root_info, depth_expansion)
	plot <- ggplot(rooted_top_users.df, aes(x = at_depth, y = component_size_prop)) + geom_point() + xlab('Subrooted top user at depth') + ylab('Subrooted cascade size / rooted cascade size') #+ geom_smooth(method=lm)
	ggsave(plot,file=paste(file_name,'_at_depth_size_prop_corr.eps'))
#	print(cor(rooted_top_users.df$at_depth,rooted_top_users.df$component_size_prop))
	users_correlated_info <- depth_expansion.df
	users_correlated_info.df <- ddply(users_correlated_info, c('root_user_id'), summarise, size = sum(expansion), total_ampl=sum(ampl), ampl_4=sum(ampl[depth<=4]), total_1st_exp = expansion[depth==1], total_2nd_exp = expansion[depth==2], total_3rd_exp = expansion[depth==3],total_4th_exp = expansion[depth==4],total_5th_exp = expansion[depth==5])
	# regression analysis
	print(nrow(users_correlated_info.df))
	size_model <- glm(log(size)~total_1st_exp+total_2nd_exp+total_3rd_exp+total_4th_exp+ampl_4, family="poisson",data= users_correlated_info.df) #+total_3rd_exp+total_4th_exp
	sm <- summary(size_model)
	print(summary(size_model))
	pseudo_R_sq <- 1 - sm$deviance/sm$null.deviance
	print(pseudo_R_sq)
	print(cor(users_correlated_info.df$size,users_correlated_info.df$total_ampl))
	plot <- ggplot(users_correlated_info.df, aes(x = total_ampl, y = size)) + geom_point() + geom_smooth(method=lm) + xlab('Total amplification') + ylab('Cascade size')
	ggsave(plot,file=paste(file_name,'_ampl_size_corr.eps'))
	
	not_really_root <- rooted_top_users.df[(rooted_top_users.df$depth-rooted_top_users.df$of_dept>=1) & (rooted_top_users.df$component_size_prop>0.80),]
	amplifiers <- c()
	total_ampl_95 <- c()
	growth_rate_95 <- c()
	time_of_growth <- c()
	depth_of_growth <- c()
	amplifiers_count <- c()
	for(a_root in unique(depth_expansion.df$root_user_id)){
		depth_expansion.subdata <- subset(depth_expansion.df, root_user_id==a_root)
		rooted_top_users.subdata <- subset(rooted_top_users.df, root_user==a_root)
		time_of_growth <- c(time_of_growth,min(depth_expansion.subdata[depth_expansion.subdata$cum_expansion/max(depth_expansion.subdata$cum_expansion)>=0.95,]$cum_time_interval))
		growth_rate_95 <- c(growth_rate_95,sum(depth_expansion.subdata[depth_expansion.subdata$cum_expansion/max(depth_expansion.subdata$cum_expansion)<=0.95,]$expansion)/max(depth_expansion.subdata[depth_expansion.subdata$cum_expansion/max(depth_expansion.subdata$cum_expansion)<=0.95,]$cum_time_interval))
		total_ampl_95 <- c(total_ampl_95, sum(depth_expansion.subdata[depth_expansion.subdata$cum_expansion/max(depth_expansion.subdata$cum_expansion)<=0.95,]$ampl))
		depth_of_growth <- c(depth_of_growth,min(depth_expansion.subdata[depth_expansion.subdata$cum_expansion/max(depth_expansion.subdata$cum_expansion)>=0.95,]$depth)/max(depth_expansion.subdata$depth))
		amplifiers_count <- c(amplifiers_count, length(depth_expansion.subdata$ampl[depth_expansion.subdata$ampl > 0]))
		for (each_subroot_depth in unique(rooted_top_users.subdata$at_depth)){
			if(depth_expansion.subdata[depth_expansion.subdata$depth==each_subroot_depth,]$ampl > 0){
				amplifiers <- c(amplifiers, rooted_top_users.subdata[rooted_top_users.subdata$at_depth==each_subroot_depth,]$component_top_user)
			}
		}
	}
	print (growth_rate_95)
	print(total_ampl_95)
	print(length(time_of_growth))
	print(summary(time_of_growth[time_of_growth>0]/86400))
	print(summary(depth_of_growth[depth_of_growth>0]))
	print(summary(amplifiers_count))
	print(cor(total_ampl_95,growth_rate_95*86400))
	data_ampl_95 <- data.frame(total_ampl_95, growth_rate_95)
	data_ampl_95$total_ampl_95 <- total_ampl_95 
	data_ampl_95$growth_rate_95 <- growth_rate_95
	plot <- ggplot(data_ampl_95, aes(x = total_ampl_95, y = growth_rate_95*86400)) + geom_point() + geom_smooth(method=lm) + xlab('Amplification till 95% of size') + ylab('95% size/arrival days')
	ggsave(plot,file=paste(file_name,'_ampl_95_size_corr.eps'))
	real_root <- union(setdiff(unique(depth_expansion$root_user_id),unique(not_really_root$root_user)), amplifiers)
	print(length(real_root))
	#	real_root <- sample(rooted_top_users.df$root_user, 300)
#	print(real_root)
	rooted_top_users.df <- rooted_top_users.df[rooted_top_users.df$root_user%in%real_root,]
	plot <- ggplot(rooted_top_users.df, aes(x = depth_matching_prop, y = component_size_prop)) + geom_point()  + xlab('Depth difference') + ylab('Subrooted cascade size / rooted cascade size') #+ geom_smooth(method=lm)
	ggsave(plot,file=paste(file_name,'_at_real_root_depth_size_prop_corr.eps'))
#	print(cor(rooted_top_users.df$at_depth,rooted_top_users.df$component_size_prop))
	users_correlated_info <- depth_expansion.df[depth_expansion.df$root_user_id%in%real_root,]
	depth_vs_expansion(file_root_info, depth_expansion[depth_expansion$root_user_id%in%real_root,])
	users_correlated_info.df <- ddply(users_correlated_info, c('root_user_id'), summarise, size = sum(expansion), total_ampl=sum(ampl), ampl_4=sum(ampl[depth<=4]), total_1st_exp = expansion[depth==1], total_2nd_exp = expansion[depth==2], total_3rd_exp = expansion[depth==3],total_4th_exp = expansion[depth==4],total_5th_exp = expansion[depth==5])
	print(nrow(users_correlated_info.df))
	print(cor(users_correlated_info.df$size,users_correlated_info.df$total_ampl))
	# regression analysis
	size_model <- glm(log(size)~(total_1st_exp)+(total_2nd_exp)+total_3rd_exp+total_4th_exp+ampl_4, family="poisson", data=users_correlated_info.df) #+total_3rd_exp+total_4th_exp
	#size_model <- lm(log(size)~top_user_odeg+total_neighbour_odeg, data= users_correlated_info.df)
	sm <- summary(size_model)
	pseudo_R_sq <- 1 - sm$deviance/sm$null.deviance
	#plot(sm)
	print(pseudo_R_sq)
	print(cor(users_correlated_info.df$size,users_correlated_info.df$total_1st_exp))
	print(cor(users_correlated_info.df$size,users_correlated_info.df$total_2nd_exp))
	print(cor(users_correlated_info.df$size,users_correlated_info.df$total_3rd_exp))
	print(cor(users_correlated_info.df$size,users_correlated_info.df$total_4th_exp))
	print(cor(users_correlated_info.df$size,users_correlated_info.df$ampl_4))
	return(users_correlated_info.df)
}

#nrr <- root_users_analysis('~/Documents/Code/Cascade/First_parent/top_size.csv_top_1000_rooted_top_users.csv','~/Documents/Code/Cascade/First_parent/top_size.csv_top_1000_100_depth_vs_expansion.csv')
#colnames(nrr)<-c('','size','total amplification','4 hops amplification','hop 1 shell size','hop 2 shell size','hop 3 shell size','hop 4 shell size','')
#pairs(nrr[,2:6], panel = panel.smooth)

unique_cascade_summary <- function(dir_vector, filename='top_size.csv_top_1000_100_depth_vs_expansion.csv'){
	library(ggplot2)
	library(plyr)
	cascade_comp <- c()
	for (dir in dir_vector){
		prev_dir = getwd()
		setwd(dir)
		depth_expansion <- as.data.frame(read.csv(filename, header=FALSE))
		colnames(depth_expansion) <- c('depth', 'expansion', 'root_user_id','is_unique')
		each_sub_cascade <- ddply(depth_expansion[depth_expansion$is_unique==1,], c('root_user_id'), summarise, size = sum(expansion), depth=max(depth))
		each_sub_cascade[,4] <- dir
		cascade_comp <- rbind(cascade_comp,each_sub_cascade)
		setwd(prev_dir)
	}
	colnames(cascade_comp) <- c('root_user_id','size','depth','parent_type')
	cascade_comp$parent_type <- factor(cascade_comp$parent_type)
	plot <- ggplot(cascade_comp, aes(y=log10(size), x = parent_type)) + geom_boxplot() + scale_x_discrete(breaks=dir_vector, labels=c("First parent", "Highest Out deg parent", "Last parent","Random parent"))
	ggsave(plot,file='parent_type_size_boxplot.eps')
	
	cascade_comp$parent_type <- factor(cascade_comp$parent_type)
	plot <- ggplot(cascade_comp, aes(y=depth, x = parent_type)) + geom_boxplot() + scale_x_discrete(breaks=dir_vector, labels=c("First parent", "Highest Out deg parent", "Last parent","Random parent"))
	ggsave(plot,file='parent_type_depth_boxplot.eps')
#	return()
}

parent_type_comp <- function(dir_vector){
	library(ggplot2)
	library(plyr)
	cascade_comp <- c()
	cascade_comp$size <- c()
	cascade_comp$depth <- c()
	for (dir in dir_vector){
		prev_dir = getwd()
		setwd(directoryname)
#		each_sub_cascade <- parent_type_size_depth(dir)
		each_sub_cascade <- unique_cascade_summary('top_size.csv_top_1000_100_depth_vs_expansion.csv')
		each_sub_cascade$size[,4] <- dir
		cascade_comp$size <- rbind(cascade_comp$size,each_sub_cascade$size)
		each_sub_cascade$depth[,4] <- dir
		cascade_comp$depth <- rbind(cascade_comp$depth,each_sub_cascade$depth)
		setwd(prev_dir)
	}
	colnames(cascade_comp$size) <- c('size', 'count', 'threshold', 'parent_type')
	colnames(cascade_comp$depth) <- c('depth', 'count', 'threshold', 'parent_type')
	cascade_comp$size <- ddply(cascade_comp$size, c('parent_type'), function(one_partition){
				one_partition = one_partition[order(one_partition$size),]
				one_partition$cum_count = cumsum(one_partition$count)
				one_partition
			})
	cascade_comp$size.df <- ddply(cascade_comp$size, c('parent_type'), summarise, mean = sum(size*count)/sum(count), median = size[cum_count>=sum(count)/2][1], max=max(size), var=sum(count*(size-sum(size*count)/sum(count))^2)/sum(count))
#	cascade_comp.size <- ddply(cascade_comp$size, c('parent_type'), function(one_partition){
#				one_partition <- data.frame(fac = rep(cascade_comp$size$size, times = cascade_comp$size$count))
#				one_partition
#			})
#	print (head(cascade_comp.size))
	
	cascade_comp$depth <- ddply(cascade_comp$depth, c('parent_type'), function(one_partition){
				one_partition = one_partition[order(one_partition$depth),]
				one_partition$cum_count = cumsum(one_partition$count)
				one_partition
			})
	cascade_comp$depth.df <- ddply(cascade_comp$depth, c('parent_type'), summarise, mean = sum(depth*count)/sum(count), median = depth[cum_count>=sum(count)/2][1], max=max(depth), var=sum(count*(depth-sum(depth*count)/sum(count))^2)/sum(count))
	print(cascade_comp$depth.df)
	cascade_comp$size$parent_type <- factor(cascade_comp$size$parent_type)
	plot <- ggplot(cascade_comp$size, aes(y=log10(size), x = parent_type)) + geom_boxplot()
	ggsave(plot,file='parent_type_size_boxplot.eps')
	
	cascade_comp$depth$parent_type <- factor(cascade_comp$depth$parent_type)
	plot <- ggplot(cascade_comp$depth, aes(y=depth, x = parent_type)) + geom_boxplot()
	ggsave(plot,file='parent_type_depth_boxplot.eps')
	
#	return(cascade_comp)
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
	cascade_comp$size <- ddply(cascade_comp$size, c('parent_type'), function(one_partition){
				one_partition = one_partition[order(one_partition$size),]
				one_partition$cum_count = cumsum(one_partition$count)
				one_partition
			})
	cascade_comp$size.df <- ddply(cascade_comp$size, c('parent_type'), summarise, mean = sum(size*count)/sum(count), median = size[cum_count>=sum(count)/2][1], max=max(size), var=sum(count*(size-sum(size*count)/sum(count))^2)/sum(count))
#	cascade_comp.size <- ddply(cascade_comp$size, c('parent_type'), function(one_partition){
#				one_partition <- data.frame(fac = rep(cascade_comp$size$size, times = cascade_comp$size$count))
#				one_partition
#			})
#	print (head(cascade_comp.size))
	
	cascade_comp$depth <- ddply(cascade_comp$depth, c('parent_type'), function(one_partition){
				one_partition = one_partition[order(one_partition$depth),]
				one_partition$cum_count = cumsum(one_partition$count)
				one_partition
			})
	cascade_comp$depth.df <- ddply(cascade_comp$depth, c('parent_type'), summarise, mean = sum(depth*count)/sum(count), median = depth[cum_count>=sum(count)/2][1], max=max(depth), var=sum(count*(depth-sum(depth*count)/sum(count))^2)/sum(count))
	print(cascade_comp$depth.df)
	cascade_comp$size$parent_type <- factor(cascade_comp$size$parent_type)
	plot <- ggplot(cascade_comp$size, aes(y=log10(size), x = parent_type)) + geom_boxplot()
	ggsave(plot,file='parent_type_size_boxplot.eps')
	
	cascade_comp$depth$parent_type <- factor(cascade_comp$depth$parent_type)
	plot <- ggplot(cascade_comp$depth, aes(y=depth, x = parent_type)) + geom_boxplot()
	ggsave(plot,file='parent_type_depth_boxplot.eps')

#	return(cascade_comp)
}