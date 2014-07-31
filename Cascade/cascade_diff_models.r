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
library(xtable)
library('ROCR', quietly = TRUE, warn.conflicts = FALSE)
library('zoo', quietly = TRUE, warn.conflicts = FALSE) 
#For calculating area under a curve using trapezium figures (rollmean function is used for that)
library('RWeka', quietly = TRUE, warn.conflicts = FALSE)
library('leaps', quietly = TRUE, warn.conflicts = FALSE)
library('glmulti', quietly = TRUE, warn.conflicts = FALSE)
library('car', quietly = TRUE, warn.conflicts = FALSE)

NaiveBayes <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")

getPerformanceAt <- function(pred, prec, rec, fpr, f, cutoff_idx){
	return(c(f@x.values[[1]][cutoff_idx], prec@y.values[[1]][cutoff_idx], rec@y.values[[1]][cutoff_idx], 
					fpr@y.values[[1]][cutoff_idx], f@y.values[[1]][cutoff_idx]))
}

myRLogisticRegression <- function(fmla, data, control = NULL){ #control is to make our function signature compatible with WEKA
	tr_m=glm(fmla,data=data,family="binomial")
	return(tr_m)
}

buildUniversalModel <- function(classifier_func, tr_data, te_data, fmla, is_weka, weka_params, eval_metrics){
	tr_m = classifier_func(fmla, data=tr_data, control = weka_params)#, options=c('model' = TRUE, 'instances' = TRUE))
#	return(tr_m)
	############################### Training set performance, along with maximum f-cutoff on training set
	if (is_weka){
		tr_pred <- predict(tr_m, newdata = tr_data, type='probability')
		if (!is.null(ncol(tr_pred)) && ncol(tr_pred) > 1) tr_pred = tr_pred[, 2]
	}else{ #R Logistic Regression
		tr_pred = tr_m$fitted.values
	}
	
	tr_pred = prediction(tr_pred,tr_data$cat)
	tr_f = performance(tr_pred,measure="f")
	tr_max_f_idx = which.max(tr_f@y.values[[1]])
	#tr_max_f_measure = tr_f@y.values[[1]][tr_max_f_idx]
	tr_max_cutoff = tr_f@x.values[[1]][tr_max_f_idx]
	#auc= performance(tr_pred,measure="auc")@y.values[[1]][1]	
	###########	####################
	
#	te_perf = getPerformanceROCR(eval_metrics, tr_m, is_weka,tr_max_cutoff, te_data)
#	wcre_evol = calculateWCREEvolution(eval_metrics, tr_prj, tr_data, tr_te_pred, te_prj, te_data, te_perf$p_buggy)
#	hypothesis_test = calculateHypothesisTest(eval_metrics, tr_m)
#	
#	te_perf$performance = c(te_perf$performance, wcre_evol, hypothesis_test)
	
	te_pred<-predict(tr_m,te_data,type=ifelse(is_weka, 'probability', 'response'))
	if (!is.null(ncol(te_pred)) && ncol(te_pred) > 1) te_pred = te_pred[, 2]
	pred = prediction(te_pred,te_data$cat)
	
	auc = performance(pred,measure="auc")@y.values[[1]][1]
#	acc = performance(pred,measure="acc")
	prec = performance(pred,measure="prec")
	rec = performance(pred,measure="rec")
	fpr = performance(pred,measure="fpr")
	f = performance(pred,measure="f")
	
	tr_cutoff <- tr_max_cutoff
	
	tr_max_f_idx = which.min(abs(f@x.values[[1]] - tr_cutoff))
	tr_cutoff_perf = getPerformanceAt(pred, prec, rec, fpr, f, tr_max_f_idx)
	
	te_max_f_idx = which.max(f@y.values[[1]]) #We are only returning metrics values at MAXIMUM F-score
	te_cutoff_perf = getPerformanceAt(pred, prec, rec, fpr, f, te_max_f_idx)
	
	te_50_idx = which.min(abs(f@x.values[[1]] - 0.5))
	te_50_perf = getPerformanceAt(pred, prec, rec, fpr, f, te_50_idx)
	
	ret = list(auc=auc, te_50_perf=te_50_perf, tr_cutoff_perf=tr_cutoff_perf, te_cutoff_perf=te_cutoff_perf)
	
	return(ret)	
}

buildClassifier <- function(classifier, tr_data, te_data, fmla, eval_metrics){
	weka_params = Weka_control()
	if (classifier == 1){
		classifier_func = Logistic
		print('Classifier Weka Logistic')
	}else if (classifier == 2){ #J48
		classifier_func = J48
		weka_params = Weka_control(U = TRUE, A = TRUE)
		print('Classifier J48')
#		Use unpruned tree and use laplace smoothing for probabilities. 
#       Ref: https://list.scms.waikato.ac.nz/pipermail/wekalist/2007-September/037750.html
#		The so-called probability estimation tree is just a C4.5 without pruning
#		> and Laplace smoothing. The two options have been implemented in WEKA.
#		> However,
#		> you may want to change the code to trun off "collapse" by yourself.
	}else if (classifier == 3){ #SVM
		classifier_func = SMO
		weka_params = Weka_control(M = TRUE) #Fits Logistic Regression model for better probability estimate
		print('Classifier Weka SVM')
	}else if (classifier == 4){ #NaiveBayes
		classifier_func = NaiveBayes
		print('Classifier Weka NaiveBayes')
	}else if (classifier == 0){ #R Logistic Regression
		classifier_func = myRLogisticRegression
		print('Classifier R Logistic Regression')
	}else{
		stop("Not a valid classifier!")
	}
	return(buildUniversalModel(classifier_func, tr_data, te_data, fmla, classifier != 0, weka_params, eval_metrics))
}

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
	ROCs$cat <- factor(ROCs$cat)
	plot <- ggplot(ROCs, aes(x = stack_x, y = stack_y)) +
			geom_line(aes(group = cat, colour = cat, linetype = cat))+
			scale_linetype_manual(values=c(1,1,5,2), name='', breaks=1:class_count, labels=AUCs) +
			scale_colour_manual(values=c("black", "gray55", "black", "gray55"), name='', breaks=1:class_count, labels=AUCs)+
			xlab('False positive rate') + ylab('True positive rate')
	save_ggplot(plot, paste(c('ROC_', fig_n, '.pdf'), collapse = ''), 24, opts(legend.position=c(.65, .65)))
	return(aucs)
}

cascade_logit_model <- function(cascade, week_n, classifier){
	splitted_data <- split(cascade, sample(1:2, nrow(cascade), replace=TRUE, prob=c(1,2)))
	training <- splitted_data[[2]]
	test <- splitted_data[[1]]
	xnames.struct <- c()
	xnames.evol <- c()
	xnames.root <- c()
	for (w_n in 1:week_n){
		xnames.struct <- c(xnames.struct, paste(c('depth_', w_n), collapse= ""), paste(c('width_', w_n), collapse= ""))
		xnames.evol <- c(xnames.evol, paste(c("speed_", w_n), collapse= ""), paste(c("burst_", w_n), collapse= ""))
#				paste(c("gr_week_", w_n), collapse= ""))
		xnames.root <- c(xnames.root, paste(c("root_contr_", w_n), collapse= ""), paste(c("rn_contr_", w_n), collapse= ""),
#				paste(c("sent_ARs_", w_n), collapse= ""), paste(c("act_", w_n), collapse= ""),
				paste(c("succ_", w_n), collapse= ""))
	}
	xnames <- c(xnames.struct, xnames.evol, xnames.root) #, 'root'	
#	xnames <- c(paste(c("root_contr_", week_n), collapse= ""), 
#					paste(c("rn_contr_", week_n), collapse= ""), 
#					paste(c("burst_", week_n), collapse= ""),
#					paste(c("speed_", week_n), collapse= ""), 
#					paste(c("depth_", week_n), collapse= ""), 
#					paste(c("width_", week_n), collapse= ""))
	model.formula <- as.formula(paste("cat~", paste(xnames.struct, collapse= "+")))
	print(model.formula)
	model.struct <- buildClassifier(classifier, training, test, model.formula, NULL)
	model.formula <- as.formula(paste("cat~", paste(xnames.evol, collapse= "+")))
	print(model.formula)
	model.evol <- buildClassifier(classifier, training, test, model.formula, NULL)
	model.formula <- as.formula(paste("cat~", paste(xnames.root, collapse= "+")))
	print(model.formula)
	model.root <- buildClassifier(classifier, training, test, model.formula, NULL)
	model.formula <- as.formula(paste("cat~", paste(xnames, collapse= "+")))
	print(model.formula)
	model.all <- buildClassifier(classifier, training, test, model.formula, NULL)
	return(list(struct = model.struct, evol = model.evol, root = model.root, all = model.all))
#	model <- multinom(model.formula, data = training)
#	model_summary <- summary(model)
#	z <- model_summary$coefficients/model_summary$standard.errors
	p <- 0 # (1 - pnorm(abs(z), 0, 1)) * 2
	test$cat.pred <- predict(model, newdata = test)
	test$cat.pred_prob <- predict(model, newdata = test, "probs")
	roc <- multiclass.roc(test$cat, apply(test$cat.pred_prob, 1, function(row) which.max(row)))
	print(roc)
	test$cat <- as.numeric(test$cat)
	true_pos <- c()
	false_pos <- c()
	false_pos_rate <- c()
	prec <- c()
	recall <- c()
	for (i in seq(1, 3, by=1)){
		true_pos <- c(true_pos, length(which(test$cat.pred == test$cat & test$cat == i)))
		false_pos <- c(false_pos, length(which(test$cat.pred != test$cat & test$cat.pred == i)))
		prec <- c(prec, (true_pos[i] / (length(which(test$cat.pred == i)))))
		recall <- c(recall, (true_pos[i] / (length(which(test$cat == i))))) #true positive rate (TPR)
		TN <- (nrow(test) + true_pos[i]) - (length(which(test$cat.pred == i)) + length(which(test$cat == i)))
		false_pos_rate <- c(false_pos_rate, false_pos[i]/ (false_pos[i] + TN))
	}
	print(prec)
	print(recall)
	print(false_pos_rate)
	print(2*prec*recall/ (prec+recall))
	print(true_pos/(true_pos+false_pos))
	aucs <- multinomial_ROC_AUC(test$cat, test$cat.pred_prob, week_n)
	return(list(training = training, test = test, prec=prec, recall=recall, p=p, true_pos = true_pos,
					false_pos = false_pos, FP_rate = false_pos_rate, model = model, aucs=aucs))
}

build_all_models <- function(file='iheart_gift/size_vs_root.csv',
		evolution_file='iheart_gift/root_weekly_evolution.csv',
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
	root_evolution <- as.data.frame(read.csv(evolution_file, header=FALSE))
	colnames(root_evolution) <- c('root', 'sent_ARs_1', 'act_1', 'child_1',
			'sent_ARs_2', 'act_2', 'child_2',
			'sent_ARs_3', 'act_3', 'child_3',
			'sent_ARs_4', 'act_4', 'child_4')
	root_evolution$succ_1 <- root_evolution$act_1 / root_evolution$child_1
	root_evolution$succ_2 <- root_evolution$act_2 / root_evolution$child_2
	root_evolution$succ_3 <- root_evolution$act_3 / root_evolution$child_3
	root_evolution$succ_4 <- root_evolution$act_4 / root_evolution$child_4
	growth <- as.data.frame(read.csv(growth_file, header=FALSE))
	colnames(growth) <- c('root', 'root_contr_1', 'rn_contr_1', 'burst_1', 'depth_1','width_1', 'week_1',
			'root_contr_2', 'rn_contr_2', 'burst_2', 'depth_2','width_2', 'week_2',
			'root_contr_3', 'rn_contr_3', 'burst_3', 'depth_3','width_3','week_3',
			'root_contr_4', 'rn_contr_4', 'burst_4', 'depth_4', 'width_4','week_4')
	growth$root_contr_2[is.na(growth$root_contr_2)] <- growth$root_contr_1[is.na(growth$root_contr_2)]
	growth$root_contr_3[is.na(growth$root_contr_3)] <- growth$root_contr_2[is.na(growth$root_contr_3)]
	growth$root_contr_4[is.na(growth$root_contr_4)] <- growth$root_contr_3[is.na(growth$root_contr_4)]
	growth$rn_contr_2[is.na(growth$rn_contr_2)] <- growth$rn_contr_1[is.na(growth$rn_contr_2)]
	growth$rn_contr_3[is.na(growth$rn_contr_3)] <- growth$rn_contr_2[is.na(growth$rn_contr_3)]
	growth$rn_contr_4[is.na(growth$rn_contr_4)] <- growth$rn_contr_3[is.na(growth$rn_contr_4)]
	growth$burst_2[is.na(growth$burst_2)] <- growth$burst_1[is.na(growth$burst_2)]
	growth$burst_3[is.na(growth$burst_3)] <- growth$burst_2[is.na(growth$burst_3)]
	growth$burst_4[is.na(growth$burst_4)] <- growth$burst_3[is.na(growth$burst_4)]
	growth$depth_2[is.na(growth$depth_2)] <- growth$depth_1[is.na(growth$depth_2)]
	growth$depth_3[is.na(growth$depth_3)] <- growth$depth_2[is.na(growth$depth_3)]
	growth$depth_4[is.na(growth$depth_4)] <- growth$depth_3[is.na(growth$depth_4)]
	growth$width_2[is.na(growth$width_2)] <- growth$width_1[is.na(growth$width_2)]
	growth$width_3[is.na(growth$width_3)] <- growth$width_2[is.na(growth$width_3)]
	growth$width_4[is.na(growth$width_4)] <- growth$width_3[is.na(growth$width_4)]
	growth$gr_week_1 <- 1
	growth$gr_week_2 <- growth$gr_week_1
	growth$gr_week_2[!is.na(growth$week_2)] <- growth$gr_week_1 + 1
	growth$week_2[is.na(growth$week_2)] <- 0
	growth$gr_week_3 <- growth$gr_week_2
	growth$gr_week_3[!is.na(growth$week_3)] <- growth$gr_week_2 + 1
	growth$week_3[is.na(growth$week_3)] <- 0
	growth$gr_week_4 <- growth$gr_week_3
	growth$gr_week_4[!is.na(growth$week_4)] <- growth$gr_week_3 + 1
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
	
	cascade_t <- merge(size_vs_root, growth, by="root")	
	cascade <- merge(cascade_t, root_evolution, by="root")	
#	size_bin <- unique(class_bin)
#	size_cat <- c('Small', 'Medium', 'Large')
#	cascade <- transform(cascade, bin = cut(cascade$size, breaks=size_bin))
#	categories <- levels(cascade$bin)
#	cascade$cat <- factor(cascade$bin, levels = categories, labels = c(1,2,3))
#	cascade$cat <- relevel(cascade$cat, ref = 1)
	cascade$cat <- 0
	cascade$cat[which(cascade$size >= class_bin[2])] <- 1
#	chi <- list()
#	for (w_n in 1:4){
#		xnames.struct <- c(paste(c('depth_', w_n), collapse= ""), paste(c('width_', w_n), collapse= ""))
#		xnames.evol <- c(paste(c("speed_", w_n), collapse= ""), paste(c("burst_", w_n), collapse= ""))
#		xnames.root <- c(paste(c("root_contr_", w_n), collapse= ""), paste(c("rn_contr_", w_n), collapse= ""),
#				paste(c("succ_", w_n), collapse= ""))
#		xnames <- c(xnames.struct, xnames.evol, xnames.root)
#		print(xnames)
#		chi[[w_n]] <- feature_selction(cascade, 'cat', xnames)
#	}
#	return(chi)
	cascade$cat <- factor(cascade$cat)
	print('Size of dataset:')
	print(nrow(cascade))
	if (week_n[1])
		model_1 <- cascade_logit_model(cascade, 1, 0)
	if (week_n[2])
		model_2 <- cascade_logit_model(cascade, 2, 0)
	if (week_n[3])
		model_3 <- cascade_logit_model(cascade, 3, 0)
	if (week_n[4])
		model_4 <- cascade_logit_model(cascade, 4, 0)
#		return(model_4)
	models <- as.data.frame(c('Structural', '','', '', 'Evolutionary', '','','', 'Root', '','','', 'All', '','',''))
	colnames(models) <- c('Features')
	models$measures <- c('Precision','TPR','FPR','AUC', 'Precision','TPR','FPR','AUC',
			'Precision','TPR','FPR','AUC', 'Precision','TPR','FPR','AUC')
	if (week_n[1])
		models$week_1 <- c(model_1$struct$te_50_perf[2], model_1$struct$te_50_perf[3], model_1$struct$te_50_perf[4], model_1$struct$auc,
				model_1$evol$te_50_perf[2], model_1$evol$te_50_perf[3], model_1$evol$te_50_perf[4], model_1$evol$auc,
				model_1$root$te_50_perf[2], model_1$root$te_50_perf[3], model_1$root$te_50_perf[4], model_1$root$auc,
				model_1$all$te_50_perf[2], model_1$all$te_50_perf[3], model_1$all$te_50_perf[4], model_1$all$auc)
	if (week_n[2])
		models$week_2 <- c(model_2$struct$te_50_perf[2], model_2$struct$te_50_perf[3], model_2$struct$te_50_perf[4], model_2$struct$auc,
				model_2$evol$te_50_perf[2], model_2$evol$te_50_perf[3], model_2$evol$te_50_perf[4], model_2$evol$auc,
				model_2$root$te_50_perf[2], model_2$root$te_50_perf[3], model_2$root$te_50_perf[4], model_2$root$auc,
				model_2$all$te_50_perf[2], model_2$all$te_50_perf[3], model_2$all$te_50_perf[4], model_2$all$auc)
	if (week_n[3])
		models$week_3 <- c(model_3$struct$te_50_perf[2], model_3$struct$te_50_perf[3], model_3$struct$te_50_perf[4], model_3$struct$auc,
				model_3$evol$te_50_perf[2], model_3$evol$te_50_perf[3], model_3$evol$te_50_perf[4], model_3$evol$auc,
				model_3$root$te_50_perf[2], model_3$root$te_50_perf[3], model_3$root$te_50_perf[4], model_3$root$auc,
				model_3$all$te_50_perf[2], model_3$all$te_50_perf[3], model_3$all$te_50_perf[4], model_3$all$auc)
	if (week_n[4])
		models$week_4 <- c(model_4$struct$te_50_perf[2], model_4$struct$te_50_perf[3], model_4$struct$te_50_perf[4], model_4$struct$auc,
				model_4$evol$te_50_perf[2], model_4$evol$te_50_perf[3], model_4$evol$te_50_perf[4], model_4$evol$auc,
				model_4$root$te_50_perf[2], model_4$root$te_50_perf[3], model_4$root$te_50_perf[4], model_4$root$auc,
				model_4$all$te_50_perf[2], model_4$all$te_50_perf[3], model_4$all$te_50_perf[4], model_4$all$auc)
	print(xtable(models,digits=c(3)), include.rownames=FALSE)
	return (models)
}

feature_selction <- function(df, cat, features){
	print(features)
	chi_vals <- c()
	for (feature in features){
		chi_table <- table(df[[cat]], round(df[[feature]],2))
		chi <- chisq.test(chi_table)
		chi_vals <- c(chi_vals, chi$statistic)
	}
	normalized_chi_vals <- (chi_vals)/(max(chi_vals))
	chi_result <- as.data.frame(features)
	colnames(chi_result) <- c('Features')
	chi_result$chi_vals <- chi_vals
	chi_result$norm_chi_vals <- normalized_chi_vals
	return(chi_result)
}

hugged_model <- build_all_models('hugged_cascade/size_vs_root.csv', 'hugged_cascade/root_weekly_evolution.csv', 
		'hugged_cascade/top_size.csv_all_weekly_evolution.csv',  c(1, 99, 10000000), c(TRUE, TRUE, TRUE, TRUE)) #5% 386
iheart_model <- build_all_models(class_bin = c(1, 70, 10000000), week_n=c(TRUE, TRUE, TRUE, TRUE)) #5% 281

#hugged_model <- build_all_models('hugged_cascade/size_vs_root.csv', 'hugged_cascade/top_size.csv_all_evolution.csv', 
#		'hugged_cascade/top_size.csv_all_weekly_evolution.csv',  c(1, 99, 1169, 10000000), c(TRUE, TRUE, TRUE, TRUE))

