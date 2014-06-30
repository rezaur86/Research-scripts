source('~/scripts/Cascade/tools.r')
#source('~/scripts/Cascade/plfit.r')
library(ggplot2)
library(gtable)
library(grid)
library(plyr)
library(Hmisc)
library(gridExtra) 
library(nnet)
library(pROC)
library(ROCR)

getROC_AUC = function(probs, true_Y){
	probsSort = sort(probs, decreasing = TRUE, index.return = TRUE)
	val = unlist(probsSort$x)
	idx = unlist(probsSort$ix)  
	roc_y = true_Y[idx];
	stack_x = cumsum(roc_y == 0)/sum(roc_y == 0)
	stack_y = cumsum(roc_y == 1)/sum(roc_y == 1)
	auc = sum((stack_x[2:length(roc_y)]-stack_x[1:length(roc_y)-1])*stack_y[2:length(roc_y)])
	return(list(stack_x=stack_x, stack_y=stack_y, auc=auc))
}

plotROC <- function(truth, predicted){
	pred <- prediction(abs(predicted), truth)    
	perf <- performance(pred,"tpr","fpr")
	print(summary(perf))
	pdf_file <- paste('roc.pdf')
	pdf(pdf_file)
	print(plot(perf))
	dev.off()
}

multinomial_ROC_AUC <- function(class, probs, fig_n){
	class_count <- length(unique(class))
	data_length <- length(class)
	ROCs <- c()
	AUCs <- rep('', class_count)
	aucs <- c()
	classes <- c('Small','Medium','Large')
	for (each_class in seq(1, class_count, by=1)){
		temp_true_Y <- rep(1, data_length)
		temp_probs <- rep(1, data_length)
		for (i in seq(1, data_length, by=1)){
			if (class[i] == each_class){
				temp_probs[i] <- probs[i, each_class]
			}
			else{
				temp_true_Y[i] <- 0
				temp_probs[i] <- probs[i, each_class]
			}
		}
		aList <- getROC_AUC(temp_probs, temp_true_Y)
		ROCs <- rbind(ROCs, as.data.frame(list(
		stack_x = unlist(aList$stack_x),
		stack_y = unlist(aList$stack_y),
		cat = each_class)))
		AUCs[each_class] <- paste(classes[each_class], ": AUC =", round(unlist(aList$auc),3))
		aucs <- c(aucs, round(unlist(aList$auc),3))
	}
#	ROCs$cat <- factor(ROCs$cat)
#	plot <- ggplot(ROCs, aes(x = stack_x, y = stack_y)) +
#			geom_line(aes(group = cat, colour = cat, linetype = cat))+
#			scale_linetype_manual(values=c(1,1,5,2), name='', breaks=1:class_count, labels=AUCs) +
#			scale_colour_manual(values=c("black", "gray55", "black", "gray55"), name='', breaks=1:class_count, labels=AUCs)+
#			xlab('False positive rate') + ylab('True positive rate')
#	save_ggplot(plot, paste(c('ROC_', fig_n, '.pdf'), collapse = ''), 24, opts(legend.position=c(.65, .65)))
	return(aucs)
}

adoption_logit_model <- function(file){
	adoption_feat <- as.data.frame(read.csv(file, header=FALSE))
	colnames(adoption_feat) <- c('id', 'adopted', 'gender' ,'locale', 'inv_count', 'inviter_count', 'recep_burst',
			'inv_elapsed_hr', 'delay_1', 'delay_n' , 'gift_veriety',
			'avg_inviter_succ_ratio',
			'popular_inv_gender', 'popular_inv_locale',
			'popular_inv_invitation_count', 'popular_inv_fav_gift', 
			'popular_inv_sent_ARs', 'popular_inv_children_count', 'popular_inv_active_children')
	adoption_feat <- adoption_feat[adoption_feat$inv_count > 0 & adoption_feat$avg_inviter_succ_ratio >= 0, ]
	adoption_feat$int_sex <- 0
	adoption_feat$int_sex[which(adoption_feat$gender == 1 & adoption_feat$popular_inv_gender == 0)] <- 1
	adoption_feat$int_sex[which(adoption_feat$gender == 0 & adoption_feat$popular_inv_gender == 1)] <- -1
	adoption_feat$succ_ratio <- adoption_feat$popular_inv_active_children / adoption_feat$popular_inv_children_count
	print(summary(adoption_feat))
	splitted_data <- split(adoption_feat, sample(1:2, nrow(adoption_feat), replace=TRUE, prob=c(1,2)))
	training <- splitted_data[[2]]
	test <- splitted_data[[1]]
	model <- glm(adopted ~ id + gender + locale + inv_count + inviter_count + recep_burst +
					inv_elapsed_hr + gift_veriety +
					avg_inviter_succ_ratio +
					int_sex +
					popular_inv_gender + popular_inv_locale +
					popular_inv_invitation_count + popular_inv_fav_gift +
					popular_inv_sent_ARs + succ_ratio,
					data = training)
#	model_summary <- summary(model)
#	z <- model_summary$coefficients/model_summary$standard.errors
	p <- 0 # (1 - pnorm(abs(z), 0, 1)) * 2
	test$adop.pred <- predict(model, newdata = test, "response")
#	test$adop.pred_prob <- predict(model, newdata = test, "response")
	print('OK')
	aucs <- getROC_AUC(test$adop.pred, test$adopted)
	print(aucs$auc)
	return(model)
	
	roc <- multiclass.roc(test$cat, apply(test$adop.pred_prob, 1, function(row) which.max(row)))
	print(roc)
	test$cat <- as.numeric(test$cat)
	true_pos <- c()
	false_pos <- c()
	false_pos_rate <- c()
	prec <- c()
	recall <- c()
	for (i in seq(1, 3, by=1)){
		true_pos <- c(true_pos, length(which(test$adop.pred == test$cat & test$cat == i)))
		false_pos <- c(false_pos, length(which(test$adop.pred != test$cat & test$adop.pred == i)))
		prec <- c(prec, (true_pos[i] / (length(which(test$adop.pred == i)))))
		recall <- c(recall, (true_pos[i] / (length(which(test$cat == i))))) #true positive rate (TPR)
		TN <- (nrow(test) + true_pos[i]) - (length(which(test$adop.pred == i)) + length(which(test$cat == i)))
		false_pos_rate <- c(false_pos_rate, false_pos[i]/ (false_pos[i] + TN))
	}
	print(prec)
	print(recall)
	print(false_pos_rate)
	print(2*prec*recall/ (prec+recall))
	print(true_pos/(true_pos+false_pos))
	aucs <- multinomial_ROC_AUC(test$cat, test$adop.pred_prob, week_n)
	return(list(training = training, test = test, prec=prec, recall=recall, p=p, true_pos = true_pos, false_pos = false_pos, FP_rate = false_pos_rate,
					model = model, aucs=aucs))
}

build_all_models <- function(file='iheart_gift/size_vs_root.csv',
		evolution_file='iheart_gift/top_size.csv_all_evolution.csv',
		growth_file = 'iheart_gift/top_size.csv_all_weekly_evolution.csv',
		class_bin = c(1, 70, 903, 10000000),
		week_n = c(TRUE, TRUE, TRUE, TRUE)){
	size_vs_root <- as.data.frame(read.csv(file, header=FALSE))
	colnames(size_vs_root) <- c('root', 'size', 'depth' ,'width', 'major_gift',
			'root_act_lifespan', 'root_outdeg', 'root_contribution' , 'root_success_ratio', 'rn_contr')
	size_vs_root <- size_vs_root[size_vs_root$size > 1, c(1,2)]
#	evolution <- as.data.frame(read.csv(evolution_file, header=FALSE))
#	colnames(evolution) <- c('root', 'size', 'depth', 'width', 'first_day', 'last_day', 'burstiness')
#	evolution <- evolution[evolution$size > 1, c(1,7)]
	growth <- as.data.frame(read.csv(growth_file, header=FALSE))
	colnames(growth) <- c('root', 'root_contr_1', 'rn_contr_1', 'burst_1', 'depth_1','width_1', 'week_1',
			'root_contr_2', 'rn_contr_2', 'burst_2', 'depth_2','width_2', 'week_2',
			'root_contr_3', 'rn_contr_3', 'burst_3', 'depth_3','width_3','week_3',
			'root_contr_4', 'rn_contr_4', 'burst_4', 'depth_4', 'width_4','week_4')
	growth$root_contr_2[is.na(growth$root_contr_2)] <- growth$root_contr_1
	growth$root_contr_3[is.na(growth$root_contr_3)] <- growth$root_contr_2
	growth$root_contr_4[is.na(growth$root_contr_4)] <- growth$root_contr_3
	growth$rn_contr_2[is.na(growth$rn_contr_2)] <- growth$rn_contr_1
	growth$rn_contr_3[is.na(growth$rn_contr_3)] <- growth$rn_contr_2
	growth$rn_contr_4[is.na(growth$rn_contr_4)] <- growth$rn_contr_3
	growth$burst_2[is.na(growth$burst_2)] <- growth$burst_1
	growth$burst_3[is.na(growth$burst_3)] <- growth$burst_2
	growth$burst_4[is.na(growth$burst_4)] <- growth$burst_3
	growth$depth_2[is.na(growth$depth_2)] <- growth$depth_1
	growth$depth_3[is.na(growth$depth_3)] <- growth$depth_2
	growth$depth_4[is.na(growth$depth_4)] <- growth$depth_3
	growth$width_2[is.na(growth$width_2)] <- growth$width_1
	growth$width_3[is.na(growth$width_3)] <- growth$width_2
	growth$width_4[is.na(growth$width_4)] <- growth$width_3
	growth$week_2[is.na(growth$week_2)] <- 0
	growth$week_3[is.na(growth$week_3)] <- 0
	growth$week_4[is.na(growth$week_4)] <- 0
	
	growth$evol_1 <- growth$week_1
	growth$evol_2 <- growth$week_1 + growth$week_2
	growth$evol_3 <- growth$week_1 + growth$week_2 + growth$week_3
	growth$evol_4 <- growth$week_1 + growth$week_2 + growth$week_3 + growth$week_4
	
	growth$root_contr_1 <- growth$root_contr_1/growth$evol_1
	growth$root_contr_2 <- growth$root_contr_2/growth$evol_2
	growth$root_contr_3 <- growth$root_contr_3/growth$evol_3
	growth$root_contr_4 <- growth$root_contr_4/growth$evol_4
	
	growth$rn_contr_1 <- growth$rn_contr_1/growth$evol_1
	growth$rn_contr_2 <- growth$rn_contr_2/growth$evol_2
	growth$rn_contr_3 <- growth$rn_contr_3/growth$evol_3
	growth$rn_contr_4 <- growth$rn_contr_4/growth$evol_4
	
	growth$speed_1 <- growth$evol_1/1
	growth$speed_2 <- growth$evol_2/2
	growth$speed_3 <- growth$evol_3/3
	growth$speed_4 <- growth$evol_4/4
	
	cascade <- merge(size_vs_root, growth, by="root")	
	size_bin <- unique(class_bin)
	size_cat <- c('Small', 'Medium', 'Large')
	cascade <- transform(cascade, bin = cut(cascade$size, breaks=size_bin))
	categories <- levels(cascade$bin)
	cascade$cat <- factor(cascade$bin, levels = categories, labels = c(1,2,3))
	cascade$cat <- relevel(cascade$cat, ref = 1)
	print('Size of dataset:')
	print(nrow(cascade))
	if (week_n[1])
		model_1 <- cascade_logit_model(cascade, 1)
	if (week_n[2])
		model_2 <- cascade_logit_model(cascade, 2)
	if (week_n[3])
		model_3 <- cascade_logit_model(cascade, 3)
	if (week_n[4])
		model_4 <- cascade_logit_model(cascade, 4)
#	print(model_1$recall)
#	print(model_1$FP_rate)
#	print(model_1$prec)
#	print(model_1$aucs)
	models <- as.data.frame(c('Small', '','', '', 'Medium', '','','', 'Large', '','',''))
	colnames(models) <- c('Class')
	models$measures <- c('Precision','TPR','FPR','AUC', 'Precision','TPR','FPR','AUC', 'Precision','TPR','FPR','AUC')
	if (week_n[1])
		models$week_1 <- c(model_1$prec[1], model_1$recall[1], model_1$FP_rate[1], model_1$aucs[1],
				model_1$prec[2], model_1$recall[2], model_1$FP_rate[2], model_1$aucs[2],
				model_1$prec[3], model_1$recall[3], model_1$FP_rate[3], model_1$aucs[3])
	if (week_n[2])
		models$week_2 <- c(model_2$prec[1], model_2$recall[1], model_2$FP_rate[1], model_2$aucs[1],
				model_2$prec[2], model_2$recall[2], model_2$FP_rate[2], model_2$aucs[2],
				model_2$prec[3], model_2$recall[3], model_2$FP_rate[3], model_2$aucs[3])
	if (week_n[3])
		models$week_3 <- c(model_3$prec[1], model_3$recall[1], model_3$FP_rate[1], model_3$aucs[1],
				model_3$prec[2], model_3$recall[2], model_3$FP_rate[2], model_3$aucs[2],
				model_3$prec[3], model_3$recall[3], model_3$FP_rate[3], model_3$aucs[3])
	if (week_n[4])
		models$week_4 <- c(model_4$prec[1], model_4$recall[1], model_4$FP_rate[1], model_4$aucs[1],
				model_4$prec[2], model_4$recall[2], model_4$FP_rate[2], model_4$aucs[2],
				model_4$prec[3], model_4$recall[3], model_4$FP_rate[3], model_4$aucs[3])
	print(xtable(models,digits=c(3)), include.rownames=FALSE)
	return (models)
}

#hugged_model <- build_all_models('hugged_cascade/size_vs_root.csv', 'hugged_cascade/top_size.csv_all_evolution.csv', 
#	'hugged_cascade/top_size.csv_all_weekly_evolution.csv',  c(1, 99, 1169, 10000000), c(TRUE, TRUE, TRUE, TRUE))

size_feature_correlation <- function(file='iheart_gift/size_vs_root.csv',
		evolution_file='iheart_gift/top_size.csv_all_evolution.csv',
		growth_file = 'iheart_gift/top_size.csv_all_weekly_evolution.csv'){
	size_vs_root <- as.data.frame(read.csv(file, header=FALSE))
	colnames(size_vs_root) <- c('root', 'size', 'depth' ,'width', 'major_gift',
			'root_act_lifespan', 'root_outdeg', 'root_contribution' , 'root_success_ratio', 'rn_contr')
	size_vs_root <- size_vs_root[size_vs_root$size > 1, c(1,2)]
	growth <- as.data.frame(read.csv(growth_file, header=FALSE))
	colnames(growth) <- c('root', 'root_contr_1', 'rn_contr_1', 'burst_1', 'depth_1','width_1', 'week_1',
			'root_contr_2', 'rn_contr_2', 'burst_2', 'depth_2','width_2', 'week_2',
			'root_contr_3', 'rn_contr_3', 'burst_3', 'depth_3','width_3','week_3',
			'root_contr_4', 'rn_contr_4', 'burst_4', 'depth_4', 'width_4','week_4')
	growth$root_contr_2[is.na(growth$root_contr_2)] <- growth$root_contr_1
	growth$root_contr_3[is.na(growth$root_contr_3)] <- growth$root_contr_2
	growth$root_contr_4[is.na(growth$root_contr_4)] <- growth$root_contr_3
	growth$rn_contr_2[is.na(growth$rn_contr_2)] <- growth$rn_contr_1
	growth$rn_contr_3[is.na(growth$rn_contr_3)] <- growth$rn_contr_2
	growth$rn_contr_4[is.na(growth$rn_contr_4)] <- growth$rn_contr_3
	growth$burst_2[is.na(growth$burst_2)] <- growth$burst_1
	growth$burst_3[is.na(growth$burst_3)] <- growth$burst_2
	growth$burst_4[is.na(growth$burst_4)] <- growth$burst_3
	growth$depth_2[is.na(growth$depth_2)] <- growth$depth_1
	growth$depth_3[is.na(growth$depth_3)] <- growth$depth_2
	growth$depth_4[is.na(growth$depth_4)] <- growth$depth_3
	growth$width_2[is.na(growth$width_2)] <- growth$width_1
	growth$width_3[is.na(growth$width_3)] <- growth$width_2
	growth$width_4[is.na(growth$width_4)] <- growth$width_3
	growth$week_2[is.na(growth$week_2)] <- 0
	growth$week_3[is.na(growth$week_3)] <- 0
	growth$week_4[is.na(growth$week_4)] <- 0
	growth$evol_1 <- growth$week_1
	growth$evol_2 <- growth$week_1 + growth$week_2
	growth$evol_3 <- growth$week_1 + growth$week_2 + growth$week_3
	growth$evol_4 <- growth$week_1 + growth$week_2 + growth$week_3 + growth$week_4
	cascade <- merge(size_vs_root, growth, by="root")	
	size_bin <- unique(c(1, 10, 100, 1000, 10000000))
	size_cat <- c('Tiny', 'Small', 'Medium', 'Large')
	cascade <- transform(cascade, bin = cut(cascade$size, breaks=size_bin))
	categories <- levels(cascade$bin)
	cascade$cat <- factor(cascade$bin, levels = categories, labels = c(1,2,3,4))
	cascade$cat <- relevel(cascade$cat, ref = 1)
	cor.df <- c()
	for (i in c(1,2,3,4)){
		cor <- as.data.frame(c(1,2,3,4))
		colnames(cor) <- c('Week')
		cor$root_contr <- c(cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$root_contr_1),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$root_contr_2),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$root_contr_3),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$root_contr_4))
		cor$rn_contr <- c(cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$rn_contr_1),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$rn_contr_2),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$rn_contr_3),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$rn_contr_4))
		cor$burst <- c(cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$burst_1),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$burst_2),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$burst_3),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$burst_4))
		cor$depth <- c(cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$depth_1),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$depth_2),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$depth_3),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$depth_4))
		cor$width <- c(cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$width_1),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$width_2),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$width_3),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$width_4))
		cor$evol <- c(cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$evol_1),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$evol_2),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$evol_3),
				cor(cascade[cascade$cat == i, ]$size, cascade[cascade$cat == i, ]$evol_4))
		cor$cat <- size_cat[i]
		cor.df <- rbind(cor.df, cor)
	}
	return(cor.df)
}

feature_selction <- function(cat, features){
	chi_vals <- c()
	for (feature in features){
		chi_table <- table(cat, round(feature,2))
		chi <- chisq.test(chi_table)
		chi_vals <- c(chi_vals, chi$statistic)
	}
	normalized_chi_vals <- (chi_vals)/(max(chi_vals))
	print(normalized_chi_vals)
	return(list(chi_vals, normalized_chi_vals))
}

