source('~/scripts/cascade/tools.r')
library(plyr)

raw_outdeg_analysis <- function (raw_outdeg_file_name){
	raw_outdeg <- as.data.frame(read.csv(raw_outdeg_file_name, header=FALSE))
	colnames(raw_outdeg) <- c('hugged', 'ismile', 'iheart')
	print(head(raw_outdeg))
	max_hug <- max(raw_outdeg$hugged,na.rm=TRUE)
	min_hug <- min(raw_outdeg$hugged,na.rm=TRUE)
	raw_outdeg$hugged <- (raw_outdeg$hugged - min_hug) / (max_hug - min_hug)
	max_ismile <- max(raw_outdeg$ismile,na.rm=TRUE)
	min_ismile <- min(raw_outdeg$ismile,na.rm=TRUE)
	raw_outdeg$ismile <- (raw_outdeg$ismile - min_ismile) / (max_ismile - min_ismile)
	max_iheart <- max(raw_outdeg$iheart,na.rm=TRUE)
	min_iheart <- min(raw_outdeg$iheart,na.rm=TRUE)
	raw_outdeg$iheart <- (raw_outdeg$iheart - min_iheart) / (max_iheart - min_iheart)
	print(head(raw_outdeg))
	print_report('Out Degree', cor(raw_outdeg, use = "pairwise.complete.obs"))
}

raw_indeg_analysis <- function (raw_indeg_file_name){
	raw_indeg <- as.data.frame(read.csv(raw_indeg_file_name, header=FALSE))
	colnames(raw_indeg) <- c('hugged', 'ismile', 'iheart')
	raw_indeg$hugged <- raw_indeg$hugged / (sum(raw_indeg$hugged,na.rm=TRUE))
	raw_indeg$ismile <- raw_indeg$ismile / (sum(raw_indeg$ismile,na.rm=TRUE))
	raw_indeg$iheart <- raw_indeg$iheart / (sum(raw_indeg$iheart,na.rm=TRUE))
	print_report('In Degree', cor(raw_indeg, use = "pairwise.complete.obs"))
}

raw_lifespan_analysis <- function (raw_lifespan_file_name){
	raw_lifespan <- as.data.frame(read.csv(raw_lifespan_file_name, header=FALSE))
	colnames(raw_lifespan) <- c('hugged', 'ismile', 'iheart')
	print_report('Lifespan', cor(raw_lifespan, use = "pairwise.complete.obs"))
}

raw_act_lifespan_analysis <- function (raw_act_lifespan_file_name){
	raw_act_lifespan <<- as.data.frame(read.csv(raw_act_lifespan_file_name, header=FALSE))
	colnames(raw_act_lifespan) <<- c('hugged', 'ismile', 'iheart')
	print_report('Active Lifespan', cor(raw_act_lifespan, use = "pairwise.complete.obs"))
}

raw_outdeg_analysis('common_outdegree.txt')
raw_indeg_analysis('common_indegree.txt')
#raw_lifespan_analysis('common_lifespan.txt')
#raw_act_lifespan_analysis('common_active_lifespan.txt')
