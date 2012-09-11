############## hist of cascade size###############
library(ggplot2)
output_format <- 'eps'

iheart_cascade_size_depth <- read.csv('iheart.csv', header=FALSE)
iheart_cascade_size_depth[,4] <- 'heart'
ihug_cascade_size_depth <- read.csv('ihug.csv', header=FALSE)
ihug_cascade_size_depth[,4] <- 'hug'
ismile_cascade_size_depth <- read.csv('ismile.csv', header=FALSE)
ismile_cascade_size_depth[,4] <- 'smile'

cascade_size_depth <- rbind (iheart_cascade_size_depth,ihug_cascade_size_depth,ismile_cascade_size_depth)
colnames(cascade_size_depth) <- c('seed','size','depth','app')

plot <- ggplot(iheart_cascade_size_depth, aes(x=log(iheart_cascade_size_depth[,2]))) + scale_x_discrete(name="size") +  geom_histogram(aes(y=..density..), binwidth=.5, colour="black", fill="white") +	geom_density(alpha=.2, fill="#FF6666") 
		
ggsave(plot,file='iheart_size.eps')

plot <- ggplot(iheart_cascade_size_depth, aes(x=log(iheart_cascade_size_depth[,3]))) +  geom_histogram(aes(y=..density..), binwidth=.5, colour="black", fill="white") +	geom_density(alpha=.2, fill="#FF6666")
ggsave(plot,file='iheart_depth.eps')

plot <- ggplot(iheart_cascade_size_depth, aes(x = iheart_cascade_size_depth[,2], y = cascade_size_depth[,3])) + geom_point(shape=1) + geom_smooth(method=lm)
ggsave(plot,file='iheart_size_depth.eps')

plot <- ggplot(ihug_cascade_size_depth, aes(x=log(ihug_cascade_size_depth[,2])))  +  geom_histogram(aes(y=..density..), binwidth=.5, colour="black", fill="white") +	geom_density(alpha=.2, fill="#FF6666")
ggsave(plot,file='ihug_size.eps')

plot <- ggplot(ihug_cascade_size_depth, aes(x=log(ihug_cascade_size_depth[,3])))  +  geom_histogram(aes(y=..density..), binwidth=.5, colour="black", fill="white") +	geom_density(alpha=.2, fill="#FF6666")
ggsave(plot,file='ihug_depth.eps')

plot <- ggplot(ihug_cascade_size_depth, aes(x = ihug_cascade_size_depth[,2], y = ihug_cascade_size_depth[,3])) + geom_point(shape=1) + geom_smooth(method=lm)
ggsave(plot,file='ihug_size_depth.eps')

plot <- ggplot(ismile_cascade_size_depth, aes(x=log(ismile_cascade_size_depth[,2]))) +  geom_histogram(aes(y=..density..), binwidth=.5, colour="black", fill="white") +	geom_density(alpha=.2, fill="#FF6666")
ggsave(plot,file='ismile_size.eps')

plot <- ggplot(ismile_cascade_size_depth, aes(x=log(ismile_cascade_size_depth[,3])))  +  geom_histogram(aes(y=..density..), binwidth=.5, colour="black", fill="white") +	geom_density(alpha=.2, fill="#FF6666")
ggsave(plot,file='ismile_depth.eps')

plot <- ggplot(ismile_cascade_size_depth, aes(x = ismile_cascade_size_depth[,2], y = ismile_cascade_size_depth[,3])) + geom_point(shape=1) + geom_smooth(method=lm)
ggsave(plot,file='ismile_size_depth.eps')

plot <- ggplot(cascade_size_depth, aes(x=log(cascade_size_depth$size), colour=cascade_size_depth$app)) + geom_density(alpha=.2, fill="#FF6666")
ggsave(plot,file='cross_app_size_hist.eps')

plot <- ggplot(cascade_size_depth, aes(x=log(cascade_size_depth$depth), colour=cascade_size_depth$app), xlim = c(0,2)) + geom_density(alpha=.2, fill="#FF6666")
ggsave(plot,file='cross_app_depth_hist.eps')

cascade_size_depth$app <- factor(cascade_size_depth$app)
plot <- ggplot(cascade_size_depth, aes(y=log(cascade_size_depth$size), x = cascade_size_depth$app)) + geom_boxplot()
ggsave(plot,file='cross_app_size_boxplot.eps')

cascade_size_depth$app <- factor(cascade_size_depth$app)
plot <- ggplot(cascade_size_depth, aes(y=log(cascade_size_depth$depth), x = cascade_size_depth$app)) + geom_boxplot()
ggsave(plot,file='cross_app_depth_boxplot.eps')


#df = ddply(size_1st_parent, c('threshold'), function(one_partition){
#			f = ecdf(one_partition$count)
#			one_partition$cdf_val = f(one_partition$count)
#			
#			one_partition
#		})
#size_1st_parent.df <- ddply(size_1st_parent.sort, .(threshold), transform, f=ecdf(sort(count))(count))

library(ggplot2)
library(plyr)
size_1st_parent <- as.data.frame(read.csv('size_1st_parent.csv', header=FALSE))
colnames(size_1st_parent) <- c('size', 'count', 'threshold')
size_1st_parent.df <- ddply(size_1st_parent, c('threshold'), function(one_partition){
			one_partition = one_partition[order(one_partition$size),]
			one_partition$cum_count = cumsum(one_partition$count)
			one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
			one_partition$pdf_val = one_partition$count / max(one_partition$cum_count)
			
			one_partition
		})
size_1st_parent.df$threshold <- factor(size_1st_parent.df$threshold)
plot <- ggplot(size_1st_parent.df,aes(x = log10(size), y = cdf_val)) + geom_line(aes(group = threshold,colour = threshold))
ggsave(plot,file='size_1st_parent_log.eps')
plot <- ggplot(size_1st_parent.df,aes(x = size, y = log(pdf_val))) +xlim(0,3000) + geom_point(aes(group = threshold,colour = threshold))
ggsave(plot,file='size_1st_parent_log_pdf.eps')
plot <- ggplot(size_1st_parent.df,aes(x = size, y = cdf_val)) + xlim(0,100) + geom_line(aes(group = threshold,colour = threshold))
ggsave(plot,file='size_1st_parent_100.eps')
plot <- ggplot(size_1st_parent.df,aes(x = size, y = cdf_val)) + xlim(0,10) + geom_line(aes(group = threshold,colour = threshold))
ggsave(plot,file='size_1st_parent_10.eps')
plot <- ggplot(size_1st_parent.df, aes(y=(size_1st_parent.df$size), x = size_1st_parent.df$threshold)) + geom_boxplot()
ggsave(plot,file='size_1st_parent_boxplot.eps')


size_all_parent <- as.data.frame(read.csv('size_all_parent.csv', header=FALSE))
colnames(size_all_parent) <- c('size', 'count', 'threshold')
size_all_parent.df <- ddply(size_all_parent, c('threshold'), function(one_partition){
			one_partition = one_partition[order(one_partition$size),]
			one_partition$cum_count = cumsum(one_partition$count)
			one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
			
			one_partition
		})
size_all_parent.df$threshold <- factor(size_all_parent.df$threshold)
plot <- ggplot(size_all_parent.df,aes(x = size, y = cdf_val)) + xlim(0,100) + geom_line(aes(group = threshold,colour = threshold))
ggsave(plot,file='size_all_parent_100.eps')
plot <- ggplot(size_all_parent.df,aes(x = size, y = cdf_val)) + xlim(0,10) + geom_line(aes(group = threshold,colour = threshold))
ggsave(plot,file='size_all_parent_10.eps')
plot <- ggplot(size_all_parent.df, aes(y=(size_all_parent.df$size), x = size_all_parent.df$threshold)) + geom_boxplot()
ggsave(plot,file='size_all_parent_boxplot.eps')

size_odeg_parent <- as.data.frame(read.csv('size_odeg.csv', header=FALSE))
colnames(size_odeg_parent) <- c('size', 'count', 'threshold')
size_odeg_parent.df <- ddply(size_odeg_parent, c('threshold'), function(one_partition){
			one_partition = one_partition[order(one_partition$size),]
			one_partition$cum_count = cumsum(one_partition$count)
			one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
			one_partition$pdf_val = one_partition$count / max(one_partition$cum_count)
			
			one_partition
		})
size_odeg_parent.df$threshold <- factor(size_odeg_parent.df$threshold)
plot <- ggplot(size_odeg_parent.df,aes(x = size, y = cdf_val)) + xlim(0,100) + geom_line(aes(group = threshold,colour = threshold))
ggsave(plot,file='size_odeg_parent_100.eps')
plot <- ggplot(size_odeg_parent.df,aes(x = size, y = pdf_val)) + xlim(0,100) + geom_line(aes(group = threshold,colour = threshold))
ggsave(plot,file='size_odeg_parent_100_pdf.eps')
plot <- ggplot(size_odeg_parent.df,aes(x = log10(size), y = log10(cdf_val))) + xlim(0,10) + geom_line(aes(group = threshold,colour = threshold))
ggsave(plot,file='size_odeg_parent_10.eps')
plot <- ggplot(size_odeg_parent.df, aes(y=(size_odeg_parent.df$size), x = size_odeg_parent.df$threshold)) + geom_boxplot()
ggsave(plot,file='size_odeg_parent_boxplot.eps')

depth_1st_parent <- as.data.frame(read.csv('depth_1st_parent.csv', header=FALSE))
colnames(depth_1st_parent) <- c('depth', 'count', 'threshold')
depth_1st_parent.df <- ddply(depth_1st_parent, c('threshold'), function(one_partition){
			one_partition = one_partition[order(one_partition$depth),]
			one_partition$cum_count = cumsum(one_partition$count)
			one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
			one_partition$pdf_val = one_partition$count / max(one_partition$cum_count)
			
			one_partition
		})
depth_1st_parent.df$threshold <- factor(depth_1st_parent.df$threshold)
plot <- ggplot(depth_1st_parent.df,aes(x = log(depth), y = cdf_val)) + geom_line(aes(group = threshold,colour = threshold))
ggsave(plot,file='depth_1st_parent_log.eps')
plot <- ggplot(depth_1st_parent.df,aes(x = depth, y = log10(pdf_val))) + geom_line(aes(group = threshold,colour = threshold))
ggsave(plot,file='depth_1st_parent_log_pdf.eps')
plot <- ggplot(depth_1st_parent.df,aes(x = depth, y = cdf_val)) + xlim(0,10) + geom_line(aes(group = threshold,colour = threshold))
ggsave(plot,file='depth_1st_parent_10.eps')

depth_all_parent <- as.data.frame(read.csv('depth_odeg_parent.csv', header=FALSE))
colnames(depth_all_parent) <- c('depth', 'count', 'threshold')
depth_all_parent.df <- ddply(depth_all_parent, c('threshold'), function(one_partition){
			one_partition = one_partition[order(one_partition$depth),]
			one_partition$cum_count = cumsum(one_partition$count)
			one_partition$cdf_val = one_partition$cum_count / max(one_partition$cum_count)
			
			one_partition
		})
depth_all_parent.df$threshold <- factor(depth_all_parent.df$threshold)
plot <- ggplot(depth_all_parent.df,aes(x = depth, y = cdf_val)) + xlim(0,10) + geom_line(aes(group = threshold,colour = threshold))
ggsave(plot,file='depth_odeg_parent.eps')

