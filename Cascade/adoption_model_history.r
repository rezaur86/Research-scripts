options( java.parameters = "-Xmx32g" )
library( "RWeka" )
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
library('zoo', quietly = TRUE, warn.conflicts = FALSE) 
#For calculating area under a curve using trapezium figures (rollmean function is used for that)
library('RWeka', quietly = TRUE, warn.conflicts = FALSE)
library('leaps', quietly = TRUE, warn.conflicts = FALSE)
library('glmulti', quietly = TRUE, warn.conflicts = FALSE)
library('car', quietly = TRUE, warn.conflicts = FALSE)

NaiveBayes <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")
IMP_5 <- c('id', 'recep_burst_ihe', 'inv_elapsed_hr_ihe', 'gift_veriety_ihe',
		'inviters_avg_inv_count_ihe',
		'inviters_avg_sent_ARs_ihe',
		'inviters_avg_active_children_ihe',
		'inviters_avg_success_ratio_ihe'
)
IMP_5_history <- c(
		'id', 'adopted_hug', 'adopted_ism',
		'inv_count_hug', 'inv_count_ism', 'inv_count_ihe',
		'recep_burst_hug', 'recep_burst_ism', 'recep_burst_ihe', 
		'inv_elapsed_hr_hug', 'inv_elapsed_hr_ism', 'inv_elapsed_hr_ihe',
		'gift_veriety_hug', 'gift_veriety_ism', 'gift_veriety_ihe',
		'inviters_avg_inv_count_hug', 'inviters_avg_inv_count_ism', 'inviters_avg_inv_count_ihe',
		'inviters_avg_sent_ARs_hug', 'inviters_avg_sent_ARs_ism', 'inviters_avg_sent_ARs_ihe',
		'inviters_avg_active_children_hug', 'inviters_avg_active_children_ism', 'inviters_avg_active_children_ihe',
		'inviters_avg_success_ratio_hug', 'inviters_avg_success_ratio_ism', 'inviters_avg_success_ratio_ihe'
)

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
	print(tr_m)
	############################### Training set performance, along with maximum f-cutoff on training set
	if (is_weka){
		tr_pred <- predict(tr_m, newdata = tr_data, type='probability')
		if (!is.null(ncol(tr_pred)) && ncol(tr_pred) > 1) tr_pred = tr_pred[, 2]
	}else{ #R Logistic Regression
		tr_pred = tr_m$fitted.values
	}
	
	tr_pred = prediction(tr_pred,tr_data$adopted_ihe)
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
	pred = prediction(te_pred,te_data$adopted_ihe)
	
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
	
	ret = list(model=tr_m, auc=auc, te_50_perf=te_50_perf, tr_cutoff_perf=tr_cutoff_perf, te_cutoff_perf=te_cutoff_perf)
	
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
		weka_params = Weka_control(#K = list("weka.classifiers.functions.supportVector.RBFKernel", G = .1),
				M = TRUE) #Fits Logistic Regression model for better probability estimate
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
	colnames(adoption_feat) <- c('id', 'gender',
			'adopted_hug', 'adopted_ism', 'adopted_ihe',
			'inv_count_hug', 'inv_count_ism', 'inv_count_ihe',
			'inviter_count_hug', 'inviter_count_ism', 'inviter_count_ihe',
			'recep_burst_hug', 'recep_burst_ism', 'recep_burst_ihe', 
			'inv_elapsed_hr_hug', 'inv_elapsed_hr_ism', 'inv_elapsed_hr_ihe',
			'gift_veriety_hug', 'gift_veriety_ism', 'gift_veriety_ihe',
			'chosen_inviter_inv_count_hug', 'chosen_inviter_inv_count_ism', 'chosen_inviter_inv_count_ihe',
			'chosen_inviter_sent_ARs_hug', 'chosen_inviter_sent_ARs_ism', 'chosen_inviter_sent_ARs_ihe',
			'chosen_inviter_active_children_hug', 'chosen_inviter_active_children_ism', 'chosen_inviter_active_children_ihe',
			'chosen_inviter_succ_ratio_hug', 'chosen_inviter_succ_ratio_ism', 'chosen_inviter_succ_ratio_ihe',
			'inviters_avg_inv_count_hug', 'inviters_avg_inv_count_ism', 'inviters_avg_inv_count_ihe',
			'inviters_avg_sent_ARs_hug', 'inviters_avg_sent_ARs_ism', 'inviters_avg_sent_ARs_ihe',
			'inviters_avg_active_children_hug', 'inviters_avg_active_children_ism', 'inviters_avg_active_children_ihe',
			'inviters_avg_success_ratio_hug', 'inviters_avg_success_ratio_ism', 'inviters_avg_success_ratio_ihe'
			)
#	adoption_feat <- adoption_feat[adoption_feat$inv_count_hug > 0 & adoption_feat$inv_count_ism > 0 & adoption_feat$inv_count_ihe > 0, ]
#	# Invitee's properties
#	NR <- c('inv_count', 'inviter_count', 'recep_burst', 'inv_elapsed_hr', 'gift_veriety')
#	# Inviter's properties
#	NS <- c('succ_ratio', 'chosen_inv_sent_ARs', 'chosen_inv_children_count', 'chosen_inv_active_children')
#	# Average inviters' properties
#	NAS <- c('inviters_avg_sent_ARs', 'inviters_avg_active_children',
#			'inviters_avg_children_count', 'avg_inviter_succ_ratio')
#	RD <- c('gender', 'locale')
#	# Inviter's properties
#	SD <- c('chosen_inv_gender', 'chosen_inv_locale')
#	# Average inviters' properties
#	ASD <- c('male_inviters', 'female_inviters', 'inviters_locale_popularity')
#	
#	#Link based
#	# Demographics
#	LD <- c('chosen_int_gender', 'chosen_int_locale')
#	LAD <- c('int_gender', 'int_locale')
#	
#	LF <- c('chosen_inv_invitation_count', 'chosen_inv_fav_gift')
#	LAF <- c('inviters_avg_invitation_count', 'grand_parent_avg_succ', 'grand_parent_max_succ')
#
	# Invitee's properties
	NR <- c('inv_count_ihe', 'inviter_count_ihe', 'recep_burst_ihe', 'inv_elapsed_hr_ihe', 'gift_veriety_ihe')
	# Inviter's properties
	NS <- c('chosen_inviter_succ_ratio_ihe', 'chosen_inviter_sent_ARs_ihe', 'chosen_inviter_active_children_ihe')
	# Average inviters' properties
	NAS <- c('inviters_avg_sent_ARs_ihe', 'inviters_avg_active_children_ihe', 'inviters_avg_success_ratio_ihe')
	RD <- c('gender')
	
	LF <- c('chosen_inviter_inv_count_ihe')
	LAF <- c('inviters_avg_inv_count_ihe')
	
	ADH <- c('adopted_hug', 'adopted_ism')
	NHR <- c('inv_count_hug', 'inviter_count_hug', 'recep_burst_hug', 'inv_elapsed_hr_hug', 'gift_veriety_hug',
			'inv_count_ism', 'inviter_count_ism', 'recep_burst_ism', 'inv_elapsed_hr_ism', 'gift_veriety_ism')
	NHS <- c('chosen_inviter_succ_ratio_hug', 'chosen_inviter_sent_ARs_hug', 'chosen_inviter_active_children_hug',
			'chosen_inviter_succ_ratio_ism', 'chosen_inviter_sent_ARs_ism', 'chosen_inviter_active_children_ism')
	NHAS <- c('inviters_avg_sent_ARs_hug', 'inviters_avg_active_children_hug', 'inviters_avg_success_ratio_hug',
			'inviters_avg_sent_ARs_ism', 'inviters_avg_active_children_ism', 'inviters_avg_success_ratio_ism')
	LHF <- c('chosen_inviter_inv_count_hug', 'chosen_inviter_inv_count_ism')
	LHAF <- c('inviters_avg_inv_count_hug', 'inviters_avg_inv_count_ism')
	
#	chi_test <- feature_selction(adoption_feat, 'adopted_ihe', c(NR, NS, NAS, RD, LF, LAF, ADH, NHR, NHS, NHAS, LHF, LHAF))
#	adoption_feat$adopted_ihe <- factor(adoption_feat$adopted_ihe)
#	categories <- levels(adoption_feat$adopted_ihe)
#	adoption_feat$cat_label <- factor(adoption_feat$adopted_ihe, levels = categories, labels = c('No', 'Yes'))
#
#	adoption_feat <- feature_scaling(adoption_feat,
#			c('inv_count_ihe', 'inviter_count_ihe', 'inv_elapsed_hr_ihe', 'gift_veriety_ihe',
#					'chosen_inviter_sent_ARs_ihe', 'chosen_inviter_active_children_ihe',
#					'inviters_avg_sent_ARs_ihe', 'inviters_avg_active_children_ihe', 
#					'chosen_inviter_inv_count_ihe', 'inviters_avg_inv_count_ihe', 
#					'inv_count_hug', 'inviter_count_hug', 'inv_elapsed_hr_hug', 'gift_veriety_hug',
#					'inv_count_ism', 'inviter_count_ism', 'inv_elapsed_hr_ism', 'gift_veriety_ism',
#					'chosen_inviter_sent_ARs_hug', 'chosen_inviter_active_children_hug',
#					'chosen_inviter_sent_ARs_ism', 'chosen_inviter_active_children_ism',
#					'inviters_avg_sent_ARs_hug', 'inviters_avg_active_children_hug',
#					'inviters_avg_sent_ARs_ism', 'inviters_avg_active_children_ism',
#					LHF, LHAF))
	splitted_data <- split(adoption_feat, sample(1:2, nrow(adoption_feat), replace=TRUE, prob=c(1,2)))
	training <- splitted_data[[2]]
	test <- splitted_data[[1]]
#	training$adopted_ihe <- factor(training$adopted_ihe)
#	test$adopted_ihe <- factor(test$adopted_ihe)
	return(list(training = training, test = test, chi=chi_test))
}

feature_scaling <- function(feat, feat_col_list){
	for (a_feat_col in feat_col_list){
		feat[[a_feat_col]] <- scale(feat[[a_feat_col]], center = TRUE, scale = TRUE)
	}
	return(feat)
}

model_names <- c(
		'NR', 'NS', 'NAS', 
		'NR+RD', 'NS+SD', 'NAS+ASD', 
		'NS+NR', 'NAS+NR',
		'NS+SD+NR+RD+LD', 'NAS+ASD+NR+RD+LAD'
		)

adoption_logit_model <- function(feat, model_id){
	# Invitee's properties
	NR <- c('inv_count_ihe', 'recep_burst_ihe', 'inv_elapsed_hr_ihe', 'gift_veriety_ihe')#, 'inviter_count_ihe'
	# Inviter's properties
	NS <- c('chosen_inviter_succ_ratio_ihe', 'chosen_inviter_sent_ARs_ihe', 'chosen_inviter_active_children_ihe')
	# Average inviters' properties
	NAS <- c('inviters_avg_sent_ARs_ihe', 'inviters_avg_active_children_ihe', 'inviters_avg_success_ratio_ihe')
	RD <- c('gender')
	# Inviter's properties
#	SD <- c('chosen_inv_gender', 'chosen_inv_locale')
	# Average inviters' properties
#	ASD <- c('male_inviters', 'female_inviters', 'inviters_locale_popularity')
	
	#Link based
	# Demographics
#	LD <- c('chosen_int_gender', 'chosen_int_locale')
#	LAD <- c('int_gender', 'int_locale')
	
	LF <- c('chosen_inviter_inv_count_ihe')
	LAF <- c('inviters_avg_inv_count_ihe')
	
	ADH <- c('adopted_hug', 'adopted_ism')
	NHR <- c('inv_count_hug', 'recep_burst_hug', 'inv_elapsed_hr_hug', 'gift_veriety_hug',#, 'inviter_count_hug'
			'inv_count_ism', 'recep_burst_ism', 'inv_elapsed_hr_ism', 'gift_veriety_ism')#, 'inviter_count_ism'
	NHS <- c('chosen_inviter_succ_ratio_hug', 'chosen_inviter_sent_ARs_hug', 'chosen_inviter_active_children_hug',
			'chosen_inviter_succ_ratio_ism', 'chosen_inviter_sent_ARs_ism', 'chosen_inviter_active_children_ism')
	NHAS <- c('inviters_avg_sent_ARs_hug', 'inviters_avg_active_children_hug', 'inviters_avg_success_ratio_hug',
			'inviters_avg_sent_ARs_ism', 'inviters_avg_active_children_ism', 'inviters_avg_success_ratio_ism')
	LHF <- c('chosen_inviter_inv_count_hug', 'chosen_inviter_inv_count_ism')
	LHAF <- c('inviters_avg_inv_count_hug', 'inviters_avg_inv_count_ism')
	
	fmla <- c(
#			paste("adopted_ihe~", paste(NR, collapse= "+")),
#			paste("adopted_ihe~", paste(NS, collapse= "+")),
#			paste("adopted_ihe~", paste(NAS, collapse= "+")),
#	
##			paste("adopted_ihe~", paste(c(NR, RD), collapse= "+")),
##			paste("adopted_ihe~", paste(c(NS, SD), collapse= "+")),
##			paste("adopted_ihe~", paste(c(NAS, ASD), collapse= "+")),
#	
##			paste("adopted_ihe~", paste(LF, collapse= "+")),
##			paste("adopted_ihe~", paste(LAF, collapse= "+")),
##			
#			paste("adopted_ihe~", paste(c(NS, NR, LF), collapse= "+")),
#			paste("adopted_ihe~", paste(c(NAS, NR, LAF), collapse= "+")),
#
#			paste("adopted_ihe~", paste(c(NS, NR, LF, ADH, NHS, NHR, LHF), collapse= "+")),
#			paste("adopted_ihe~", paste(c(NAS, NR, LAF, ADH, NHAS, NHR, LHAF), collapse= "+"))
			paste("adopted_ihe~", paste(IMP_5, collapse= "+")),
			paste("adopted_ihe~", paste(IMP_5_history, collapse= "+"))
#			paste("adopted_ihe~", paste(c(NS, SD, NR, RD, LF, LD), collapse= "+")),
#			paste("adopted_ihe~", paste(c(NAS, ASD, NR, RD, LAF, LAD), collapse= "+"))
	)
	models <- c()
	for (i in 1:length(fmla)){
		print(fmla[i])
		models[[i]] <- buildClassifier(model_id, feat$training, feat$test, as.formula(fmla[i]), NULL)
	}
	return(models)
}

feature_selction <- function(df, cat, features){
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

inviter_details <- function(file){
	inviters <- as.data.frame(read.csv(file, header=FALSE))
	colnames(inviters) <- c('gender', 'locale', 'sent_ARs', 'active_children', 'children_count')
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
	pie <- ggplot(mydf, aes(x = factor(1), y = count, fill = gender)) + geom_bar(width = 1, colour = "gray55") + 
			coord_polar(theta = "y") +
			geom_text(aes(label = sprintf("%1.2f%%", 100*ratio), y = position)) + 
			
	save_ggplot(pie, 'iheart_gift/inv_gender.pdf')
	return (inviters)
}

latex_result <- function(result){
	models <- as.data.frame(c('Nodes only', '','', 'Nodes with dem.','','', 
					'Linked nodes', '', 'Linked nodes with dem.',''))
	colnames(models) <- c('Features')
#	models$measures <- c('Precision','TPR','FPR','AUC')
	prec <- c()
	TPR <- c()
	FPR <- c()
	AUC <- c()
	for (i in 1:length(result)){
		AUC <- c(AUC, result[[i]]$auc)
		prec <- c(prec, result[[i]]$te_50_perf[2])
		TPR <- c(TPR, result[[i]]$te_50_perf[3])
		FPR <- c(FPR, result[[i]]$te_50_perf[4])
		f1 <- result[[i]]$te_50_perf[5]
	}
	models$Models <- model_names
	models$Precision <- prec
	models$TPR <- TPR
	models$FPR <- FPR
	models$AUC <- AUC
	print(xtable(models,digits=c(3)), include.rownames=FALSE)
	return (models)
}

#feat <- load_features('iheart_gift/adoption_features_test.csv')
#logistic_model <- adoption_logit_model(feat,0)
#J48_model <- adoption_logit_model(feat, 2)
#svm_model <- adoption_logit_model(feat, 3)
#NB_model <- adoption_logit_model(feat, 4)