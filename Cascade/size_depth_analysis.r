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
	colnames(depth_expansion) <- c('depth', 'expansion', 'root_user_id')
	plot <- ggplot(rooted_top_users.df, aes(x = at_depth, y = component_size_prop)) + geom_point() + xlab('Subrooted top user at depth') + ylab('Subrooted cascade size / rooted cascade size') #+ geom_smooth(method=lm)
	ggsave(plot,file=paste(file_name,'_at_depth_size_prop_corr.eps'))
	print(cor(rooted_top_users.df$at_depth,rooted_top_users.df$component_size_prop))
	users_correlated_info <- depth_expansion
	users_correlated_info.df <- ddply(users_correlated_info, c('root_user_id'), summarise, size = sum(expansion), total_1st_exp = expansion[depth==1], total_2nd_exp = expansion[depth==2], total_3rd_exp = expansion[depth==3],total_4th_exp = expansion[depth==4],total_5th_exp = expansion[depth==5])
	# regression analysis
	print(nrow(users_correlated_info.df))
	size_model <- glm(log(size)~total_1st_exp+total_2nd_exp+total_3rd_exp+total_4th_exp, family="poisson",data= users_correlated_info.df) #+total_3rd_exp+total_4th_exp
	sm <- summary(size_model)
	print(summary(size_model))
	pseudo_R_sq <- 1 - sm$deviance/sm$null.deviance
	print(pseudo_R_sq)
	print(cor(users_correlated_info.df$size,users_correlated_info.df$total_3rd_exp))
	print(cor(users_correlated_info.df$size,users_correlated_info.df$total_4th_exp))
	not_really_root <- rooted_top_users.df[((rooted_top_users.df$at_depth+rooted_top_users.df$of_dept)-rooted_top_users.df$depth<2) & (rooted_top_users.df$component_size_prop>0.8),]
	real_root <- setdiff(unique(depth_expansion$root_user_id),unique(not_really_root$root_user))
	rooted_top_users.df <- rooted_top_users.df[rooted_top_users.df$root_user%in%real_root,]
	plot <- ggplot(rooted_top_users.df, aes(x = depth_matching_prop, y = component_size_prop)) + geom_point()  + xlab('Depth difference') + ylab('Subrooted cascade size / rooted cascade size') #+ geom_smooth(method=lm)
	ggsave(plot,file=paste(file_name,'_at_real_root_depth_size_prop_corr.eps'))
	print(cor(rooted_top_users.df$at_depth,rooted_top_users.df$component_size_prop))
	users_correlated_info <- depth_expansion[depth_expansion$root_user_id%in%real_root,]
	users_correlated_info.df <- ddply(users_correlated_info, c('root_user_id'), summarise, size = sum(expansion), total_1st_exp = expansion[depth==1], total_2nd_exp = expansion[depth==2], total_3rd_exp = expansion[depth==3],total_4th_exp = expansion[depth==4],total_5th_exp = expansion[depth==5])
	print(nrow(users_correlated_info.df))
	# regression analysis
	size_model <- glm(log(size)~(total_1st_exp)+(total_2nd_exp)+total_3rd_exp+total_4th_exp, family="poisson", data=users_correlated_info.df) #+total_3rd_exp+total_4th_exp
	#size_model <- lm(log(size)~top_user_odeg+total_neighbour_odeg, data= users_correlated_info.df)
	sm <- summary(size_model)
	pseudo_R_sq <- 1 - sm$deviance/sm$null.deviance
	#plot(sm)
	print(pseudo_R_sq)
	print(cor(users_correlated_info.df$size,users_correlated_info.df$total_1st_exp))
	print(cor(users_correlated_info.df$size,users_correlated_info.df$total_2nd_exp))
	return(size_model)
}

depth_vs_expansion <- function(file_name){
	library(ggplot2)
	library(plyr)
	depth_expansion <- as.data.frame(read.csv(file_name, header=FALSE))
	colnames(depth_expansion) <- c('depth', 'expansion', 'root_user_id')
	depth_expansion.df <- ddply(depth_expansion, c('root_user_id'), function(one_partition){
				one_partition = one_partition[order(one_partition$depth),]
				one_partition$cum_expansion = cumsum(one_partition$expansion)
				one_partition$factor = one_partition$expansion/c(1,one_partition$expansion[1:nrow(one_partition)-1])
				factor_model_coeffs = coefficients(lm(factor~depth, data = one_partition))
				#alpha_coeff <- matrix(c(max(one_partition$cum_expansion)-1,-max(one_partition$cum_expansion),rep(0,max(one_partition$depth)-1),1))
				alpha <- (max(one_partition$expansion))^(1/min(one_partition[one_partition$expansion==max(one_partition$expansion),]$depth)) #polyroot(alpha_coeff)
				#alpha <- max(one_partition$cum_expansion)^(1/(max(one_partition$depth)+1)) #polyroot(alpha_coeff)
				print(alpha)
				one_partition$expansion_norm = one_partition$expansion / (alpha^(one_partition$depth))
				#print(alpha)
				#one_partition$expansion_norm = one_partition$expansion / ((factor_model_coeffs[1]+factor_model_coeffs[2]*one_partition$depth)^one_partition$depth)
				one_partition
			})
#	print (head(depth_expanstion.df,50))
	depth_expansion.df$root_user_id <- factor(depth_expansion.df$root_user_id)
	plot <- ggplot(depth_expansion.df, aes(x = depth, y = log10(expansion))) + geom_line(aes(group = root_user_id,colour = root_user_id)) + xlab('Depth') + ylab('log of Expansion') 
	ggsave(plot,file=paste(file_name,'_depth_expansion.eps'))
	plot <- ggplot(depth_expansion.df, aes(x = depth, y = log10(expansion_norm))) + geom_line(aes(group = root_user_id,colour = root_user_id)) + xlab('Depth') + ylab('log of Normalized Expansion') #+ geom_line(aes(x = depth, y = 1)) 
	ggsave(plot,file=paste(file_name,'_depth_expansion_norm.eps'))
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