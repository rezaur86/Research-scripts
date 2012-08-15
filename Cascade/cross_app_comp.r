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
