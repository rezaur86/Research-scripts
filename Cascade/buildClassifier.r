options( java.parameters = "-Xmx32g" )
library( "RWeka" )
library('ROCR', quietly = TRUE, warn.conflicts = FALSE)
library('zoo', quietly = TRUE, warn.conflicts = FALSE) 
#For calculating area under a curve using trapezium figures (rollmean function is used for that)
library('RWeka', quietly = TRUE, warn.conflicts = FALSE)
library('leaps', quietly = TRUE, warn.conflicts = FALSE)
library('glmulti', quietly = TRUE, warn.conflicts = FALSE)
library('car', quietly = TRUE, warn.conflicts = FALSE)

NaiveBayes <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")

getPerformanceAt <- function(pred, prec, rec, fpr, f, acc, cutoff_idx){
	return(c(f@x.values[[1]][cutoff_idx], prec@y.values[[1]][cutoff_idx], rec@y.values[[1]][cutoff_idx], 
					fpr@y.values[[1]][cutoff_idx], f@y.values[[1]][cutoff_idx], acc@y.values[[1]][cutoff_idx]))
}

myRLogisticRegression <- function(fmla, data, control = NULL){ #control is to make our function signature compatible with WEKA
	tr_m=glm(fmla,data=data,family="binomial")
	return(tr_m)
}

buildUniversalModel <- function(classifier_func, cat, tr_data, te_data, fmla, is_weka, weka_params, eval_metrics){
	tr_m = classifier_func(fmla, data=tr_data, control = weka_params)#, options=c('model' = TRUE, 'instances' = TRUE))
	
	############################### Training set performance, along with maximum f-cutoff on training set
	if (is_weka){
		tr_pred <- predict(tr_m, newdata = tr_data, type='probability')
		if (!is.null(ncol(tr_pred)) && ncol(tr_pred) > 1) tr_pred = tr_pred[, 2]
	}else{ #R Logistic Regression
		tr_pred = tr_m$fitted.values
	}
	
	tr_pred = prediction(tr_pred,tr_data[[cat]])
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
	pred = prediction(te_pred,te_data[[cat]])
	
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
	
	ret = list(#model=tr_m, 
			auc=auc, te_50_perf=te_50_perf, tr_cutoff_perf=tr_cutoff_perf, te_cutoff_perf=te_cutoff_perf)
	
	return(ret)	
}

buildClassifier <- function(classifier, cat, tr_data, te_data, fmla, eval_metrics){
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
	return(buildUniversalModel(classifier_func, cat, tr_data, te_data, fmla, classifier != 0, weka_params, eval_metrics))
}
