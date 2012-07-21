library('igraph')

setwd('/home/rezaur/iheart')

d <- read.table('filename')

gd <- graph.data.frame(d)
odeg <- sort(degree(gd, mode = 'out'), decreasing=T)
indeg <- sort(degree(gd, mode = 'in'), decreasing=F)

senders_deg <- subset(odeg, odeg > 0) # 0 outdegress are receivers

top_senders <- function (x_percent){
	n_top_senders <- ceiling(x_percent*length(senders_deg)/100)
	return (head (senders_deg, n=n_top_senders))
}

top5 <- top_senders(5)

############## hist of cascade size###############
cascade_sizes <- read.csv('iheart_modified_orig-2010-26.csv', header=FALSE)
library(ggplot2)
colnames(cascade_sizes) <- c('seed','size')
plot <- ggplot(cascade_sizes, aes(x=cascade_sizes$size)) + geom_histogram(binwidth=30000)
ggsave(plot,file="iheart_modified_orig-2010-26.pdf")

################### Reachability ############################
reachability <- function(g, m) {
	reach <- c()
	t_top5 <- top5
	while (length(names(t_top5)) > 0) {
		this_node_reach <- subcomponent(g, names(t_top5)[1], mode = m)

		reach <- union(reach, names(t_top5)[1])
		for (j in 1:(length(this_node_reach))) {
			reach <- union(reach, V(g)[this_node_reach[j]])
		}

		t_top5 <- setdiff(names(t_top5),reach)
		g <- delete.vertices(g, reach)
	}
	return(reach)
}

top5_reach <- reachability(gd, "out")
