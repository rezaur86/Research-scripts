source('~/scripts/Cascade/tools.r')
#source('~/scripts/Cascade/plfit.r')
library(ggplot2)
library(gtable)
library(reshape)
library(grid)
library(plyr)
library(Hmisc)
library(gridExtra) 
library(nnet)
library(pROC)
library('ROCR', quietly = TRUE, warn.conflicts = FALSE)
library('zoo', quietly = TRUE, warn.conflicts = FALSE) #For calculating area under a curve using trapezium figures (rollmean function is used for that)
library('RWeka', quietly = TRUE, warn.conflicts = FALSE)
library('leaps', quietly = TRUE, warn.conflicts = FALSE)
library('glmulti', quietly = TRUE, warn.conflicts = FALSE)
library('car', quietly = TRUE, warn.conflicts = FALSE)

NaiveBayes <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")

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

getPerformanceAt <- function(pred, prec, rec, f, cutoff_idx){
	return(c(f@x.values[[1]][cutoff_idx], prec@y.values[[1]][cutoff_idx], rec@y.values[[1]][cutoff_idx], f@y.values[[1]][cutoff_idx]))
}

myRLogisticRegression <- function(fmla, data, control = NULL){ #control is to make our function signature compatible with WEKA
	tr_m=glm(fmla,data=data,family="binomial")
	return(tr_m)
}

buildUniversalModel <- function(classifier_func, tr_data, te_data, fmla, is_weka, weka_params, eval_metrics){
	tr_m = classifier_func(fmla, data=tr_data, control = weka_params)#, options=c('model' = TRUE, 'instances' = TRUE))
	
	############################### Training set performance, along with maximum f-cutoff on training set
	if (is_weka){
		tr_pred <- predict(tr_m, newdata = tr_data, type='probability')
		if (!is.null(ncol(tr_pred)) && ncol(tr_pred) > 1) tr_pred = tr_pred[, 2]
	}else{ #R Logistic Regression
		tr_pred = tr_m$fitted.values
	}
	
	tr_pred = prediction(tr_pred,tr_data$adopted)
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
	pred = prediction(te_pred,te_data$adopted)
	
	auc = performance(pred,measure="auc")@y.values[[1]][1]
#	acc = performance(pred,measure="acc")
	prec = performance(pred,measure="prec")
	rec = performance(pred,measure="rec")
	f = performance(pred,measure="f")

	tr_cutoff <- tr_max_cutoff
	
	tr_max_f_idx = which.min(abs(f@x.values[[1]] - tr_cutoff))
	tr_cutoff_perf = getPerformanceAt(pred, prec, rec, f, tr_max_f_idx)
	
	te_max_f_idx = which.max(f@y.values[[1]]) #We are only returning metrics values at MAXIMUM F-score
	te_cutoff_perf = getPerformanceAt(pred, prec, rec, f, te_max_f_idx)
	
	te_50_idx = which.min(abs(f@x.values[[1]] - 0.5))
	te_50_perf = getPerformanceAt(pred, prec, rec, f, te_50_idx)
	
	ret = list(auc=auc, te_50_perf=te_50_perf, tr_cutoff_perf=tr_cutoff_perf, te_cutoff_perf=te_cutoff_perf)
	
	return(ret)	
}

buildClassifier <- function(classifier, tr_data, te_data, fmla, eval_metrics){
	weka_params = Weka_control()
	if (classifier == 1){
		classifier_func = Logistic
	}else if (classifier == 2){ #J48
		classifier_func = J48
		weka_params = Weka_control(U = TRUE, A = TRUE)
#		Use unpruned tree and use laplace smoothing for probabilities. 
#       Ref: https://list.scms.waikato.ac.nz/pipermail/wekalist/2007-September/037750.html
#		The so-called probability estimation tree is just a C4.5 without pruning
#		> and Laplace smoothing. The two options have been implemented in WEKA.
#		> However,
#		> you may want to change the code to trun off "collapse" by yourself.
	}else if (classifier == 3){ #SVM
		classifier_func = SMO
		weka_params = Weka_control(M = TRUE) #Fits Logistic Regression model for better probability estimate
	}else if (classifier == 4){ #NaiveBayes
		classifier_func = NaiveBayes
	}else if (classifier == 0){ #R Logistic Regression
		classifier_func = myRLogisticRegression
	}else{
		stop("Not a valid classifier!")
	}
	return(buildUniversalModel(classifier_func, tr_data, te_data, fmla, classifier != 0, weka_params, eval_metrics))
}

load_features <- function(file){
	adoption_feat <- as.data.frame(read.csv(file, header=FALSE))
	colnames(adoption_feat) <- c('id', 'adopted', 'gender' ,'locale', 'inv_count', 'inviter_count', 'recep_burst',
			'inv_elapsed_hr', 'gift_veriety',
			'male_inviters', 'female_inviters', 'inviters_locale_popularity',
			'inviters_avg_invitation_count','inviters_avg_sent_ARs',
			'inviters_avg_active_children', 'inviters_avg_children_count', 'avg_inviter_succ_ratio',
			'excep_avg_invitation_count','excep_avg_sent_ARs',
			'excep_avg_active_children', 'excep_avg_children_count', 'avg_excep_succ_ratio')
	adoption_feat <- adoption_feat[adoption_feat$inv_count > 0, ] # & adoption_feat$avg_inviter_succ_ratio >= 0
	adoption_feat$int_gender <- 0
	adoption_feat$int_gender[which(adoption_feat$gender == 1 & adoption_feat$female_inviters > adoption_feat$male_inviters)] <- 1
	adoption_feat$int_gender[which(adoption_feat$gender == 0 & adoption_feat$male_inviters > adoption_feat$female_inviters)] <- -1
	adoption_feat$int_locale <- 1
	adoption_feat$int_locale[which(adoption_feat$locale == adoption_feat$inviters_locale_popularity)] <- 0
	adoption_feat$adopted <- factor(adoption_feat$adopted)
	categories <- levels(adoption_feat$adopted)
	adoption_feat$cat_label <- factor(adoption_feat$adopted, levels = categories, labels = c('No', 'Yes'))

	feature_boxplot(adoption_feat, 'inv_count', 
			'Adopted by invitee', 'Number of received invitations', 'iheart_gift/users_inv_count.pdf')
	feature_boxplot(adoption_feat, 'inviter_count', 
			'Adopted by invitee', 'Number of inviters', 'iheart_gift/users_inviter_count.pdf')
	feature_boxplot(adoption_feat, 'recep_burst', 
			'Adopted by invitee', 'Reception burstiness', 'iheart_gift/users_recep_burst.pdf')
	feature_boxplot(adoption_feat, 'inv_elapsed_hr', 
			'Adopted by invitee', 'Invitation elapsed hour', 'iheart_gift/users_inv_elapsed_hr.pdf')
	feature_boxplot(adoption_feat, 'gift_veriety', 
			'Adopted by invitee', 'Gift variation count in invitations', 'iheart_gift/users_gift_veriety.pdf')
	
#	feature_boxplot(adoption_feat, 'chosen_inv_sent_ARs', 
#			'Adopted by invitee', 'Inviters\' sent AR count', 'iheart_gift/inv_sent_ARs.pdf')
#	feature_boxplot(adoption_feat, 'chosen_inv_children_count', 
#			'Adopted by invitee', 'Inviters\' children count', 'iheart_gift/inv_children_count.pdf')
#	feature_boxplot(adoption_feat, 'chosen_inv_active_children', 
#			'Adopted by invitee', 'Inviters\' active children count', 'iheart_gift/inv_active_children.pdf')
#	feature_boxplot(adoption_feat, 'succ_ratio', 
#			'Adopted by invitee', 'Inviters\' success ratio', 'iheart_gift/inv_succ_ratio.pdf')
#	
#	feature_boxplot(adoption_feat, 'chosen_inv_invitation_count', 
#			'Adopted by invitee', 'Number of invitations received from the inviter', 'iheart_gift/link_inv_count.pdf')
#	feature_boxplot(adoption_feat, 'neighboring', 
#			'Adopted by invitee', 'Jaccard coefficient of neighbor similarity', 'iheart_gift/link_jaccard_neighbor.pdf')
	
	feature_boxplot(adoption_feat, 'inviters_avg_invitation_count', 
			'Adopted by invitee', 'Inviters\' average invitation count', 'iheart_gift/avg_inviters_invitation.pdf')
	feature_boxplot(adoption_feat, 'inviters_avg_sent_ARs', 
			'Adopted by invitee', 'Inviters\' average sent AR count', 'iheart_gift/avg_inviters_sent_ARs.pdf')
	feature_boxplot(adoption_feat, 'inviters_avg_active_children', 
			'Adopted by invitee', 'Inviters\' average active children count', 'iheart_gift/avg_inviters_active_children.pdf')
	feature_boxplot(adoption_feat, 'inviters_avg_children_count', 
			'Adopted by invitee', 'Inviters\' average children count', 'iheart_gift/avg_inviters_children_count.pdf')
	feature_boxplot(adoption_feat, 'avg_inviter_succ_ratio', 
			'Adopted by invitee', 'Inviters\' average success ratio', 'iheart_gift/avg_inviter_succ_ratio.pdf')
	
	feature_boxplot(adoption_feat, 'excep_avg_invitation_count', 
			'Adopted by invitee', 'Inviters\' avg. exceptional invitation count', 'iheart_gift/avg_excep_invitation.pdf')
	feature_boxplot(adoption_feat, 'excep_avg_sent_ARs', 
			'Adopted by invitee', 'Inviters\' avg. exceptional sent AR count', 'iheart_gift/avg_excep_sent_ARs.pdf')
	feature_boxplot(adoption_feat, 'excep_avg_active_children', 
			'Adopted by invitee', 'Inviters\' avg. exceptional active children count', 'iheart_gift/avg_excep_active_children.pdf')
	feature_boxplot(adoption_feat, 'excep_avg_children_count', 
			'Adopted by invitee', 'Inviters\' avg. exceptional children count', 'iheart_gift/avg_excep_children_count.pdf')
	feature_boxplot(adoption_feat, 'avg_excep_succ_ratio', 
			'Adopted by invitee', 'Inviters\' avg. exceptional success ratio', 'iheart_gift/avg_excep_succ_ratio.pdf')
	
	splitted_data <- split(adoption_feat, sample(1:3, nrow(adoption_feat), replace=TRUE, prob=c(1,2,7)))
	training <- splitted_data[[2]]
	test <- splitted_data[[1]]
	training$adopted <- factor(training$adopted)
	test$adopted <- factor(test$adopted)
	return(list(training = training, test = test))
}

feature_boxplot <- function(features, a_feature, label_x, label_y, figure_name){
	features.a_feature <- ddply(features, c('cat_label'), .drop=TRUE,
			.fun = function(one_partition){
				stats = boxplot.stats(one_partition[[a_feature]])$stats
				c(ymin=stats[1],
						lower=stats[2],
						middle=stats[3],
						upper=stats[4],
						ymax=stats[5],
						mean = mean(one_partition[[a_feature]]))
			})
	features.a_feature$cat_label <- factor(features.a_feature$cat_label)
	plot <- ggplot(features.a_feature, aes(x=cat_label, lower=lower, upper=upper, middle=middle, ymin=ymin, ymax=ymax)) + 
			geom_boxplot(stat="identity") +
			geom_point(data = features.a_feature, aes(x=cat_label, y=mean), shape = 8, size = 3)+
			xlab(label_x) + ylab(label_y) 
	save_ggplot(plot, figure_name)
}


adoption_logit_model <- function(feat){
	# Invitee's properties
	fmla <- adopted ~ gender + locale + inv_count + inviter_count + recep_burst +
			inv_elapsed_hr + gift_veriety
	m1 <- (buildClassifier(0, feat$training, feat$test, fmla, NULL))
#	# Inviter's properties
#	fmla <- adopted ~ chosen_inv_gender + chosen_inv_locale + succ_ratio + 
#			chosen_inv_sent_ARs + chosen_inv_children_count + chosen_inv_active_children
#	m2 <- (buildClassifier(0, feat$training, feat$test, fmla, NULL))
#	# Invitee and inviter's properties
#	fmla <- adopted ~ gender + locale + inv_count + inviter_count + recep_burst +
#			inv_elapsed_hr + gift_veriety +
#			chosen_inv_gender + chosen_inv_locale + succ_ratio + 
#			chosen_inv_sent_ARs + chosen_inv_children_count + chosen_inv_active_children
#	m3 <- (buildClassifier(0, feat$training, feat$test, fmla, NULL))
	# Average inviters' properties
	fmla <- adopted ~ inviters_avg_invitation_count + inviters_avg_sent_ARs + inviters_avg_active_children +
			inviters_avg_children_count + avg_inviter_succ_ratio
	m2 <- (buildClassifier(0, feat$training, feat$test, fmla, NULL))
	# Invitee and average inviters' properties
	fmla <- adopted ~ gender + locale + inv_count + inviter_count + recep_burst +
			inv_elapsed_hr + gift_veriety +
			inviters_avg_invitation_count + inviters_avg_sent_ARs + inviters_avg_active_children +
			inviters_avg_children_count + avg_inviter_succ_ratio
	m3 <- (buildClassifier(0, feat$training, feat$test, fmla, NULL))
	fmla <- adopted ~ excep_avg_invitation_count + excep_avg_sent_ARs + excep_avg_active_children +
			excep_avg_children_count + avg_excep_succ_ratio
	m4 <- (buildClassifier(0, feat$training, feat$test, fmla, NULL))
	# Invitee and average inviters' properties
	fmla <- adopted ~ gender + locale + inv_count + inviter_count + recep_burst +
			inv_elapsed_hr + gift_veriety +
			excep_avg_invitation_count + excep_avg_sent_ARs + excep_avg_active_children +
			excep_avg_children_count + avg_excep_succ_ratio
	m5 <- (buildClassifier(0, feat$training, feat$test, fmla, NULL))
	# Link based properties
#	fmla <- adopted ~ int_gender + int_locale + male_inviters + female_inviters + inviters_locale_popularity
#	m6 <- (buildClassifier(0, feat$training, feat$test, fmla, NULL))
#	# Invitee's and link based properties
#	fmla <- adopted ~ gender + locale + inv_count + inviter_count + recep_burst +
#			inv_elapsed_hr + gift_veriety +
#			int_gender + int_locale + neighboring + chosen_inv_invitation_count + chosen_inv_fav_gift
#	m7 <- (buildClassifier(0, feat$training, feat$test, fmla, NULL))
#	# Inviter's and link based properties
#	fmla <- adopted ~ chosen_inv_gender + chosen_inv_locale + succ_ratio + 
#			chosen_inv_sent_ARs + chosen_inv_children_count + chosen_inv_active_children +
#			int_gender + int_locale + neighboring + chosen_inv_invitation_count + chosen_inv_fav_gift
#	m8 <- (buildClassifier(0, feat$training, feat$test, fmla, NULL))
	fmla <- adopted ~ gender + locale + inv_count + inviter_count + recep_burst +
			inv_elapsed_hr + gift_veriety +
			int_gender + int_locale + male_inviters + female_inviters + inviters_locale_popularity +
			inviters_avg_invitation_count + inviters_avg_sent_ARs + inviters_avg_active_children +
			inviters_avg_children_count + avg_inviter_succ_ratio
	m8 <- (buildClassifier(0, feat$training, feat$test, fmla, NULL))
	fmla <- adopted ~ gender + locale + inv_count + inviter_count + recep_burst +
			inv_elapsed_hr + gift_veriety +
			int_gender + int_locale + male_inviters + female_inviters + inviters_locale_popularity +
			inviters_avg_invitation_count + inviters_avg_sent_ARs + inviters_avg_active_children +
			inviters_avg_children_count + avg_inviter_succ_ratio +
			excep_avg_invitation_count + excep_avg_sent_ARs + excep_avg_active_children +
			excep_avg_children_count + avg_excep_succ_ratio
	m9 <- (buildClassifier(0, feat$training, feat$test, fmla, NULL))
	return(list(m1=m1, m2=m2, m3=m3, m4=m4, m5=m5, m8=m8, m9=m9))
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
	return(list(training = training, test = test, prec=prec, recall=recall, p=p,
					true_pos = true_pos, false_pos = false_pos, FP_rate = false_pos_rate,
					model = model, aucs=aucs))
}

#feat <- load_features('iheart_gift/succ_adoption_features.csv')
#logistic_model <- adoption_logit_model(feat)


#$auc
#[1] 0.8951207
#
#$te_50_perf
#2570758
#0.5000000 0.7748823 0.6495590 0.7067076
#
#$tr_cutoff_perf
#59093433
#0.3515862 0.6947291 0.7699954 0.7304285
#
#$te_cutoff_perf
#41090095
#0.3479512 0.6924155 0.7728916 0.7304436

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

inviter_details <- function(file){
	inviters <- as.data.frame(read.csv(file, header=FALSE))
	colnames(inviters) <- c('gender', 'sent_ARs', 'active_children', 'children_count')
	inviters <- inviters[inviters$sent_ARs > 0, ]
	inviters$succ_ratio <- inviters$active_children / inviters$children_count
	inviters$gender <- factor(inviters$gender)
	categories <- levels(inviters$gender)
	inviters$cat_label <- factor(inviters$gender, levels = categories, labels = c('Female', 'Male'))
	
	feature_boxplot(inviters, 'active_children', 
			'Inviter gender', 'Number of active children', 'iheart_gift/inv_gender_active_children.pdf')
	feature_boxplot(inviters, 'succ_ratio', 
			'Inviter gender', 'Success ratio', 'iheart_gift/inv_gender_succ_ratio.pdf')
	
	data.m = melt(table(inviters$cat_label))
	names(data.m)[1] = "gender"
	names(data.m)[2] = "count"
	data.m$ratio=data.m$count/sum(data.m$count)
	mydf = ddply(data.m, .(gender), transform, position = cumsum(count) - 0.5*count) 
	print(mydf)
#	pie <- ggplot(mydf, aes(x = factor(1), y = count, fill = as.factor(gender))) +
#			geom_bar(stat = "identity", width = 1, colour = "gray55") +
#			facet_wrap(~gender) + 
#			coord_polar(theta = "y") +
#			geom_text(aes(label = sprintf("%1.2f%%", 100*ratio), y = position))
	pie <- ggplot(mydf, aes(x = factor(1), y = count, fill = gender)) + geom_bar(width = 1, colour = "gray55") + coord_polar(theta = "y") +
			geom_text(aes(label = sprintf("%1.2f%%", 100*ratio), y = position)) + 
			
	save_ggplot(pie, 'iheart_gift/inv_gender.pdf')
	
	return (inviters)
}
