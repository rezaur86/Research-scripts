library(ggplot2)

print_report <- function(title, variable){
	cat("::",title,"::\n")
	print (variable)
}

save_ggplot <- function (plot, file){
	file_name <- file
#	ggsave(plot,file=file_name)
	pdf(file_name)
	print(plot)
	dev.off()
	file_name_split <- strsplit(file_name,'\\.')[[1]]
	file_name_split <- file_name_split[file_name_split!="pdf"]
	command <- paste("convert -density 300x300", file_name, paste(c(file_name_split,'png'), collapse = '.'))
	system(command)
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
