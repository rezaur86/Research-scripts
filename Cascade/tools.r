library(ggplot2)

print_report <- function(title, variable){
	cat("::",title,"::\n")
	print (variable)
}

myPlotTheme <- function(base_size = 24){
	return(theme_bw(base_size = base_size, ))	
}

save_ggplot <- function (plot, file, theme_size = 24, theme_opts = opts(legend.position=c(.7, .7)), width=7){
	file_name <- file
#	ggsave(plot,file=file_name)
	pdf(file_name, width = width)
	plot <- plot + myPlotTheme(theme_size) + theme_opts
	print(plot)
	dev.off()
	return(plot)
#	file_name_split <- strsplit(file_name,'\\.')[[1]]
#	file_name_split <- file_name_split[file_name_split!="pdf"]
#	command <- paste("convert -density 300x300", file_name, paste(c(file_name_split,'png'), collapse = '.'))
#	system(command)
}

change_plot_attributes <- function(plot, group_title, group_ids, group_labels, xlabel, ylabel){
	plot <- plot + #scale_colour_grey(name=group_title, breaks=group_ids, labels=group_labels) +
			scale_colour_manual(values=c("black","gray55","gray55", rep("black", 2), rep("gray55", 3), rep("black", 3)),
					name=group_title, breaks=group_ids, labels=group_labels)+
			scale_shape_manual(values=c(0,1,2,3,4,5,6,7,8,9,10,12),
					name =group_title,  breaks=group_ids, labels=group_labels)
	plot <- plot + xlab(xlabel) + ylab(ylabel)
	return(plot)
}

change_plot_attributes_fancy <- function(plot, group_title, group_ids, group_labels, xlabel, ylabel){
	plot <- plot + #scale_colour_grey(name=group_title, breaks=group_ids, labels=group_labels) +
			scale_colour_hue(name=group_title, breaks=group_ids, labels=group_labels)+
			scale_shape_discrete(name =group_title,  breaks=group_ids, labels=group_labels)
	plot <- plot + xlab(xlabel) + ylab(ylabel)
	return(plot)
}

draw_two_y_axes_graph <- function (file_name, max_curve_count, x_val, y_l_val, y_r_val, lim_x, lim_l_y, lim_r_y, x_label, y_l_label, y_r_label, graph_name, x_mark=NA, y_l_mark=NA, y_r_mark=NA){
	pdf(file=file_name,width=10,height=10)
	color <- rainbow(max_curve_count)
	par(mar=c(5, 5, 5, 5))
	for (i in 1:max_curve_count){
		if(i == 1){
			plot(x=x_val[[1]], y=y_l_val[[1]],col=color[1], type='n', main=graph_name, xlab=x_label, ylab=y_l_label, xlim=lim_x, xaxt='n', yaxt='n', lwd=.75)
			if (typeof(x_mark) != 'list')
				axis(1, pretty(lim_x))
			else 
				axis(1, at=x_mark$at, labels=x_mark$label, col.axis="blue", las=2, cex.axis=0.7, tck=-.01)
			if (typeof(y_l_mark) != 'list')
				axis(2, pretty(lim_l_y), col='blue')
			else 
				axis(2, at=y_l_mark$at, labels=y_l_mark$label, col.axis="blue", las=2, cex.axis=0.7, tck=-.01)
		}
		if(i <= max_curve_count){
			lines(x=x_val[[2*i-1]], y=y_l_val[[i]], col=color[i], lwd=0.75, type="o", lty=2)
		}	
	}
	par(new=T)
	for (i in 1:max_curve_count){
		if(i == 1){
			plot(x=x_val[[1]], y=y_r_val[[1]],col=color[1], xlim=lim_x, type='n', xaxt='n', yaxt='n', lwd=.75, axes=F, ylab='', xlab='')
			if (typeof(y_r_mark) != 'list')
				axis(4, pretty(lim_r_y), col='green', labels=T)
			else
				axis(4, at=y_r_mark$at, labels=y_r_mark$label, col.axis="blue", las=2, cex.axis=0.7, tck=-.01)
			mtext(y_r_label, side=4, line=3)
		}
		if(i <= max_curve_count){
			lines(x=x_val[[2*i-1]], y=y_r_val[[i]], col=color[i], lwd=0.75, type="s")
			lines(x=x_val[[2*i]], y=y_r_val[[i]], col=color[i], lwd=0.75, type="o", lty=1, pch=22)
		}	
	}
	dev.off()	
	file_name_split <- strsplit(file_name,'\\.')[[1]]
	file_name_split <- file_name_split[file_name_split!="pdf"]
	command <- paste("convert -density 300x300", file_name, paste(c(file_name_split,'png'), collapse = '.'))
	system(command)
} 

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
	require(grid)
	
	# Make a list from the ... arguments and plotlist
	plots <- c(list(...), plotlist)
	
	numPlots = length(plots)
	
	# If layout is NULL, then use 'cols' to determine layout
	if (is.null(layout)) {
		# Make the panel
		# ncol: Number of columns of plots
		# nrow: Number of rows needed, calculated from # of cols
		layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
				ncol = cols, nrow = ceiling(numPlots/cols))
	}
	
	if (numPlots==1) {
		print(plots[[1]])
		
	} else {
		# Set up the page
		grid.newpage()
		pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
		
		# Make each plot, in the correct location
		for (i in 1:numPlots) {
			# Get the i,j matrix positions of the regions that contain this subplot
			matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
			
			print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
							layout.pos.col = matchidx$col))
		}
	}
}