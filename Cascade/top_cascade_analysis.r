source('~/scripts/cascade/tools.r')
source('~/scripts/cascade/plfit.r')
library('MASS')
library('distr')
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
	depth_expansion.df$size_rank <- factor(depth_expansion.df$size_rank)
	plot <- ggplot(depth_expansion.df, aes(x = depth, y = (expansion))) + geom_line(aes(group = size_rank, colour = size_rank, linetype = size_rank)) +
			xlab('Depth') + ylab('Shell Size') + myPlotTheme() + opts(legend.position=c(.7, .7)) +
			scale_linetype_manual(values=c(1,2,3,4,5,6,1,2,3,4), name='Cascades Size', breaks=1:10, labels=c('(1) 1820995', '(2) 1757747', '(3) 1053378', '(4) 874514', '(5) 780809',
							'(6) 681745', '(7) 622582', '(8) 554010', '(9) 497617', '(10) 480526')) +
			scale_colour_manual(values=c(rep("black",6), rep("gray55",4)), name='Cascades Size', breaks=1:10, labels=c('(1) 1820995', '(2) 1757747', '(3) 1053378', '(4) 874514', '(5) 780809',
							'(6) 681745', '(7) 622582', '(8) 554010', '(9) 497617', '(10) 480526'))
	save_ggplot(plot, paste(c(file_name,'_depth_expansion.pdf'), collapse = ''))
	plot <- ggplot(depth_expansion.df, aes(x = depth, y = log10(expansion))) + geom_line(aes(group = size_rank, colour = size_rank)) +
			xlab('Depth') + ylab('log of Shell size') 
	save_ggplot(plot, paste(c(file_name,'_depth_log_expansion.pdf'), collapse = ''))
	plot <- ggplot(depth_expansion.df, aes(x = depth, y = (diff))) + geom_line(aes(group = size_rank, colour = size_rank, linetype = size_rank)) +
			xlab('Depth') + ylab('Shell Size Growth Rate') + myPlotTheme() + opts(legend.position=c(.7, .7)) +
			scale_linetype_manual(values=c(1,2,3,4,5,6,1,2,3,4), name="Cascades Size", breaks=1:10, labels=c('(1) 1820995', '(2) 1757747', '(3) 1053378', '(4) 874514', '(5) 780809',
							'(6) 681745', '(7) 622582', '(8) 554010', '(9) 497617', '(10) 480526'))+
			scale_colour_manual(values=c(rep("black",6), rep("gray55",4)), name='Cascades Size', breaks=1:10, labels=c('(1) 1820995', '(2) 1757747', '(3) 1053378', '(4) 874514', '(5) 780809',
							'(6) 681745', '(7) 622582', '(8) 554010', '(9) 497617', '(10) 480526'))
	save_ggplot(plot, paste(c(file_name,'_depth_expansion_diff.pdf'), collapse = ''))
}

build_model <- function (trainer_cascade){
	size_model <- glm(size~total_1st_exp+total_2nd_exp+total_3rd_exp+total_4th_exp+ampl_4, family="poisson",data= trainer_cascade) #+total_3rd_exp+total_4th_exp
	sm <- summary(size_model)
	pseudo_R_sq <- 1 - sm$deviance/sm$null.deviance
	print_report('Model summary', summary(size_model))
	print_report('Model R_sq', pseudo_R_sq)
	print_report('Cor of size vs. 1st hop shell size', cor(trainer_cascade$size,trainer_cascade$total_1st_exp))
	print_report('Cor of size vs. 2nd hop shell size', cor(trainer_cascade$size,trainer_cascade$total_2nd_exp))
	print_report('Cor of size vs. 3rd hop shell size', cor(trainer_cascade$size,trainer_cascade$total_3rd_exp))
	print_report('Cor of size vs. 4th hop shell size', cor(trainer_cascade$size,trainer_cascade$total_4th_exp))
	print_report('Cor of size vs. 4 hop amplification', cor(trainer_cascade$size,trainer_cascade$ampl_4))
	return(size_model)
}

analyze_cascade_growth <- function (fraction, cascade_root){
	time_of_growth <- c()
	growth_rate_frac <- c()
	total_ampl_frac <- c()
	depth_of_growth <- c()
	amplifiers_count <- c()
	amplifiers <- c()
	roots <- c()
	for(a_root in cascade_root){
		root_depth_expansion.subdata <- subset(g_prep.df$root_depth_expansion, root_user_id==a_root)
		rooted_top_users.subdata <- subset(g_prep.df$rooted_top_users, root_user==a_root)
#		time_of_growth <- c(time_of_growth,min(depth_expansion.subdata[depth_expansion.subdata$cum_expansion/max(depth_expansion.subdata$cum_expansion)>=fraction,]$cum_time_interval))
#		growth_rate_frac <- c(growth_rate_frac,sum(depth_expansion.subdata[depth_expansion.subdata$cum_expansion/max(depth_expansion.subdata$cum_expansion)<=fraction,]$expansion)/max(depth_expansion.subdata[depth_expansion.subdata$cum_expansion/max(depth_expansion.subdata$cum_expansion)<=fraction,]$cum_time_interval))
#		total_ampl_frac <- c(total_ampl_frac, sum(depth_expansion.subdata[depth_expansion.subdata$cum_expansion/max(depth_expansion.subdata$cum_expansion)<=fraction,]$ampl))
#		depth_of_growth <- c(depth_of_growth,min(depth_expansion.subdata[depth_expansion.subdata$cum_expansion/max(depth_expansion.subdata$cum_expansion)>=fraction,]$depth)/max(depth_expansion.subdata$depth))
		amplifiers_count <- c(amplifiers_count, length(root_depth_expansion.subdata$ampl[root_depth_expansion.subdata$ampl > 0]))
		ampl_depth <- root_depth_expansion.subdata[root_depth_expansion.subdata$ampl > 0, ]$depth
		ampl <- rooted_top_users.subdata[rooted_top_users.subdata$at_depth%in%ampl_depth,]
		amplifiers <- c(amplifiers, ampl$top_user)
		roots <- c(roots, ampl$root_user)
#		for (each_subroot in rooted_top_users.subdata[rooted_top_users.subdata$at_depth%in%ampl_depth,]$top_user){
#			if(depth_expansion.subdata[depth_expansion.subdata$depth==each_subroot_depth,]$ampl > 0){
#				amplifiers <- c(amplifiers, rooted_top_users.subdata[rooted_top_users.subdata$at_depth==each_subroot_depth,]$top_user)
#			}
#		}
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
	return (list(ampl_users = amplifiers, replaced_roots=roots))
}

root_users_analysis <- function(output_dir){
	sink(file = paste(c(output_dir,'output.txt'), collapse = ''), type = "output")
# 	cascade_info.df <<- ddply(g_prep.df$root_depth_expansion, c('root_user_id'), summarise, size = sum(expansion), max_depth=max(depth), total_ampl=sum(ampl), ampl_4=sum(ampl[depth<=4]), 
#			total_1st_exp = expansion[depth==1], total_2nd_exp = c(expansion[depth==2],0)[1], total_3rd_exp = c(expansion[depth==3],0)[1],
#			total_4th_exp = c(expansion[depth==4],0)[1],total_5th_exp = c(expansion[depth==5],0)[1])
	cascade_info.sorted <<- cascade_info.df[order(-cascade_info.df$size),]
	very_big_cascades <<- cascade_info.sorted[cascade_info.sorted$size >= 480526,]
	very_big_cascades <<- within(very_big_cascades,size_rank<-rank(-very_big_cascades$size))
	very_big_root_depth_expansion <<- g_prep.df$root_depth_expansion[g_prep.df$root_depth_expansion$root_user_id%in%very_big_cascades$root_user_id,]
	very_big_root_depth_expansion.ranked <<- ddply(very_big_root_depth_expansion, c('root_user_id'), function(one_partition){
				one_partition$size = rep(very_big_cascades[very_big_cascades$root_user_id%in%one_partition$root_user_id,]$size, nrow(one_partition))
				one_partition$size_rank = rep(very_big_cascades[very_big_cascades$root_user_id%in%one_partition$root_user_id,]$size_rank, nrow(one_partition))
				one_partition$size_rank_label = rep(paste('(',very_big_cascades[very_big_cascades$root_user_id%in%one_partition$root_user_id,]$size_rank, ')', 
								very_big_cascades[very_big_cascades$root_user_id%in%one_partition$root_user_id,]$size), nrow(one_partition))
				one_partition
			})
	
	draw_depth_expansion(output_dir, very_big_root_depth_expansion.ranked)
	splitted_cascades <- split(cascade_info.df, sample(1:2, length(cascade_info.df), replace=TRUE, prob=c(1,2)))#cascade_info.df[sample.split(cascade_info.df, 2/3),]
	training_cascades <- splitted_cascades[[2]]
	test_cascades <- splitted_cascades[[1]]
	size_model <- build_model(training_cascades)
	print_report('RMSE for training', sqrt(sum((training_cascades$size-(size_model$fitted.values))^2)/nrow(training_cascades)))
	print_report('Total cascades', nrow(cascade_info.df))
	print_report('Training count', nrow(training_cascades))
	print_report('Testing count', nrow(test_cascades))
#	print_report('Test data', summary(test_cascades))
	pred.fit <- predict.glm(size_model, test_cascades, type = "response", se.fit = TRUE)
	print_report('RMSE for test', sqrt(sum((test_cascades$size-(pred.fit$fit))^2)/nrow(test_cascades)))
	model_1 <- build_model(cascade_info.df)
	sink()
	return(pred.fit)
	print_report('Cor of size vs total amplification', cor(cascade_info.df$size,cascade_info.df$total_ampl))
#	plot <- ggplot(cascade_info.df, aes(x = total_ampl, y = size)) + geom_point() + geom_smooth(method=lm) + xlab('Total amplification') + ylab('Cascade size')
#	save_ggplot(plot,file=paste(c(rooted_top_users_file,'_ampl_size_corr.pdf'), collapse = ''))
	nonroot_components <- g_prep.df$rooted_top_users[(g_prep.df$rooted_top_users$depth_matching<=5) & (g_prep.df$rooted_top_users$component_size_prop>0.80),]
	not_really_root <- unique(nonroot_components$root_user)
	print_report('Cascade root count', length(g_prep.df$roots))
	print_report('Not really root count', length(not_really_root))
	amplifiers <- analyze_cascade_growth(fraction = .95, not_really_root)
	print_report('Amplifier user count', length(amplifiers$ampl_user))
	print_report('Replaced root count', length(unique(amplifiers$replaced_roots)))
	no_ampl_roots <- setdiff(not_really_root,amplifiers$replaced_roots)
	top_user <-  union(nonroot_components[nonroot_components$root_user%in%no_ampl_roots, ]$top_user, amplifiers$ampl_user)
	real_root <- union(setdiff(g_prep.df$roots,not_really_root),top_user)
	print_report('Real root count', length(real_root))
#	real_root <- sample(g_prep.df$rooted_top_users$root_user, 300)
#	print(real_root)
#	plot <- ggplot(g_prep.df$rooted_top_users, aes(x = depth_matching_prop, y = component_size_prop)) + geom_point()  + xlab('Depth difference') + ylab('Subrooted cascade size / rooted cascade size') #+ geom_smooth(method=lm)
#	ggsave(plot,file=paste(rooted_top_users_file,'_at_real_root_depth_size_prop_corr.pdf'))
#	print(cor(g_prep.df$rooted_top_users$at_depth,g_prep.df$rooted_top_users$component_size_prop))
	real_infl_info <- g_prep.df$root_depth_expansion[g_prep.df$root_depth_expansion$root_user_id%in%real_root,]#c(1:7,11:13)]
	colnames(real_infl_info) <- colnames(g_prep.df$nonroot_depth_expansion)
	real_depth_expansion <- rbind(real_infl_info, g_prep.df$nonroot_depth_expansion[g_prep.df$nonroot_depth_expansion$top_user_id%in%real_root,])
#	draw_depth_expansion(depth_vs_expansion_file, g_prep.df$depth_expansion[g_prep.df$depth_expansion$root_user_id%in%real_root,])
	cascade_info.df <- ddply(real_depth_expansion, c('top_user_id'), summarise, size = sum(expansion), max_depth=max(depth), total_ampl=sum(ampl), ampl_4=sum(ampl[depth<=4]),
			total_1st_exp = expansion[depth==1], total_2nd_exp = c(expansion[depth==2],0)[1], total_3rd_exp = c(expansion[depth==3],0)[1],
			total_4th_exp = c(expansion[depth==4],0)[1],total_5th_exp = c(expansion[depth==5],0)[1])
	print_report('Cor of size vs total amplification (real roots)', cor(cascade_info.df$size,cascade_info.df$total_ampl))
	model_2 <- build_model(cascade_info.df)
	model_comp <- anova(model_2, model_1, test = "Chisq")
	print_report('Model comparison', model_comp)
	sink()
#	return(cascade_info.df)
}

analyze_amplifiers <- function(dir){
	ampl<-as.data.frame(g_prep.df$root_depth_expansion[g_prep.df$root_depth_expansion$ampl>0,c(10,3)])
	colnames(ampl) <- c('strength', 'root_user_id')
	ampl$strength <- factor(ampl$strength)
	plot <- ggplot(ampl, aes(x=strength))+geom_histogram(colour="black",fill="white")+scale_y_log10()+ xlab("Amplification Strength") + ylab("Number of Occurrence")
	save_ggplot(plot,file=paste(dir,'hist_ampl.pdf',collapse = '/'))
}

analyze_evolution <- function(dir){
	cascade_evolution <- as.data.frame(read.csv('fp_nt_u_temp/top_size.csv_top_10evolution.csv', header=FALSE))
	colnames(cascade_evolution) <- c('root_id', 'day_number', 'growth')
	cascade_evolution$root_id <- factor(cascade_evolution$root_id)
	plot <- ggplot(cascade_evolution, aes(x = (day_number-2059), y = growth)) + 
			geom_line(aes(group = root_id,colour = root_id)) + xlab("Week") + ylab("Growth") + scale_y_log10()
	save_ggplot(plot,file=paste(dir,'cascade_evolution.pdf',collapse = '/'))
}
analyze_time_to_next_generation <- function(dir){
	time_to_next_generation <- as.data.frame(read.csv('fp_nt_u_temp/top_size.csv_top_3734781time_to_next_generation.csv', header=FALSE))
	colnames(time_to_next_generation) <- c('parent_week', 'day_taken', 'count')
	time_to_next_generation$parent_week <- factor(time_to_next_generation$parent_week)
	time_to_next_generation.df <- ddply(time_to_next_generation, c('parent_week'), function(one_partition){
				one_partition$proportion = one_partition$count/(sum(one_partition$count))
				one_partition
			})
	
	plot <- ggplot(time_to_next_generation, aes(y=(week_taken), x=week))+ geom_point()# + scale_y_discrete(breaks=seq(0,60,2), labels=seq(0,60,2))
	save_ggplot(plot,file=paste(dir,'time_to_next_generation.pdf',collapse = '/'))
}
#nrr <- root_users_analysis('~/output_cascade/full_first_parent/top_size.csv_top_10_rooted_top_users.csv','~/output_cascade/full_first_parent/top_size.csv_top_10_100_depth_vs_expansion.csv')
#colnames(nrr)<-c('','size','total amplification','4 hops amplification','hop 1 shell size','hop 2 shell size','hop 3 shell size','hop 4 shell size','')
#pairs(nrr[,2:6], panel = panel.smooth)
