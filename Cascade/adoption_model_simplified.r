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
library('FSelector')
library(xtable)

# Invitees' properties
NR <- c('id', 'inv_count', 'recep_burst', 'inv_elapsed_hr', 'gift_veriety', 'inviters_avg_invitation_count')
# Average inviters' properties
NAS <- c('inviters_avg_sent_ARs', 'inviters_avg_active_children', 'avg_inviter_succ_ratio')

NaiveBayes <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")

getPerformanceAt <- function(pred, prec, rec, fpr, f, acc, cutoff_idx){
	return(c(f@x.values[[1]][cutoff_idx], prec@y.values[[1]][cutoff_idx], rec@y.values[[1]][cutoff_idx], 
					fpr@y.values[[1]][cutoff_idx], f@y.values[[1]][cutoff_idx], acc@y.values[[1]][cutoff_idx]))
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
	acc = performance(pred,measure="acc")
	prec = performance(pred,measure="prec")
	rec = performance(pred,measure="rec")
	fpr = performance(pred,measure="fpr")
	f = performance(pred,measure="f")

	tr_cutoff <- tr_max_cutoff
	
	tr_max_f_idx = which.min(abs(f@x.values[[1]] - tr_cutoff))
	tr_cutoff_perf = getPerformanceAt(pred, prec, rec, fpr, f, acc, tr_max_f_idx)
	
	te_max_f_idx = which.max(f@y.values[[1]]) #We are only returning metrics values at MAXIMUM F-score
	te_cutoff_perf = getPerformanceAt(pred, prec, rec, fpr, f, acc, te_max_f_idx)
	
	te_50_idx = which.min(abs(f@x.values[[1]] - 0.5))
	te_50_perf = getPerformanceAt(pred, prec, rec, fpr, f, acc, te_50_idx)
	
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

feature_scaling <- function(feat, feat_col_list){
	for (a_feat_col in feat_col_list){
		feat[[a_feat_col]] <- scale(feat[[a_feat_col]], center = TRUE, scale = TRUE)
	}
	return(feat)
}

load_features <- function(file){
	adoption_feat <- as.data.frame(read.csv(file, header=FALSE))
	colnames(adoption_feat) <- c('id', 'adopted', 'inv_count', 'recep_burst',
			'inv_elapsed_hr', 'gift_veriety',
			'inviters_avg_invitation_count','inviters_avg_sent_ARs',
			'inviters_avg_active_children', 'avg_inviter_succ_ratio'
	)
	adoption_feat <- adoption_feat[adoption_feat$inv_count > 0, ] # & adoption_feat$avg_inviter_succ_ratio >= 0
#	adoption_feat <- feature_scaling(adoption_feat, c(NR, NAS))
	return(adoption_feat)
}

process_features <- function(app1, app2=NA){
	training <- load_features(app1)	
	chi_test <- 0# feature_selction(adoption_feat, 'adopted', c(NR, NS, NAS, RD, SD, ASD, LD, LAD, LF, LAF))
	if (is.na(app2)){
		splitted_data <- split(training, sample(1:2, nrow(training), replace=TRUE, prob=c(1,2)))
		training <- splitted_data[[2]]
		test <- splitted_data[[1]]
	}
	else test <- load_features(app2)
	return(list(training = training, test = test, chi_test = chi_test))
}

IMP_5 <- c('avg_inviter_succ_ratio', 'inviters_avg_active_children', 'inv_elapsed_hr', 'recep_burst', 'id')
comb_all <- combn(IMP_5, 2)
comb_feat <- c()
for (i in 1:(length(comb_all)/2)){
	comb_feat <- c(comb_feat, paste(c(comb_all[i], comb_all[2*i]), collapse= ":"))
}	

adoption_logit_model <- function(feat){
	fmla <- c(
#			paste("adopted~", paste(NR, collapse= "+")),
#			paste("adopted~", paste(NS, collapse= "+")),
#			paste("adopted~", paste(NAS, collapse= "+")),
	
#			paste("adopted~", paste(c(NR, RD), collapse= "+")),
#			paste("adopted~", paste(c(NS, SD), collapse= "+")),
#			paste("adopted~", paste(c(NAS, ASD), collapse= "+")),
#	
##			paste("adopted~", paste(LF, collapse= "+")),
##			paste("adopted~", paste(LAF, collapse= "+")),
##			
#			paste("adopted~", paste(c(NS, NR, LF), collapse= "+")),
#			paste("adopted~", paste(c(NAS, NR, LAF), collapse= "+")),
#			
#			paste("adopted~", paste(c(NS, SD, NR, RD, LF, LD), collapse= "+")),
#			paste("adopted~", paste(c(NAS, ASD, NR, RD, LAF, LAD), collapse= "+"))
#			paste("adopted~", paste(c(NS, NR, LSR), collapse= "+")),
			paste("adopted~", paste(c(NAS, NR), collapse= "+"))
#			paste("adopted~", paste(IMP_5, collapse= "+"))
#			paste("adopted~", paste(c(IMP_5, comb_feat), collapse= "+"))
	)
	models <- c()
	for (i in 1:length(fmla)){
		models[[i]] <- buildClassifier(0, feat$training, feat$test, as.formula(fmla[i]), NULL)
	}
	return(models)
}

evaluator <- function(subset) {
	m <- buildClassifier(0, feat_ihe_3M$training, feat_ihe_3M$test, as.simple.formula(subset, "adopted"), NULL)
	return(m$te_50_perf[5])
}

#library('FSelector')
#subset <- best.first.search(c(NAS, NR, LASR), evaluator)

#feat <- load_features('iheart_gift/succ_adoption_features.csv')
#logistic_model <- adoption_logit_model(feat)
#m_IMP_5 <- buildClassifier(0, feat_ihe_3M$training, feat_ihe_3M$test,
#		as.formula(paste("adopted~", paste(IMP_5, collapse= "+"))), NULL)
#m_IMP_8 <- buildClassifier(0, feat_ihe_3M$training, feat_ihe_3M$test,
#		as.formula(paste("adopted~", paste(IMP_8, collapse= "+"))), NULL)

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

model_names = c('Invitee', 'Inviters', 'All', 'Top 5', 'Top 5 and their interactions')
latex_result <- function(result, model_names){
	models <- as.data.frame(model_names)
	colnames(models) <- c('Features')
	prec <- c()
	TPR <- c()
	FPR <- c()
	ACC <- c()
	F1 <- c()
	AUC <- c()
	print(models)
	for (i in 1:length(result)){
		AUC <- c(AUC, result[[i]]$auc)
		prec <- c(prec, result[[i]]$te_50_perf[2])
		TPR <- c(TPR, result[[i]]$te_50_perf[3])
		FPR <- c(FPR, result[[i]]$te_50_perf[4])
		F1 <- c(F1, result[[i]]$te_50_perf[5])
		ACC <- c(ACC, result[[i]]$te_50_perf[6])
	}
	models$Precision <- prec
	models$TPR <- TPR
	models$FPR <- FPR
	models$ACC <- ACC
	models$AUC <- AUC
	print(xtable(models,digits=c(3)), include.rownames=FALSE)
	return (models)
}