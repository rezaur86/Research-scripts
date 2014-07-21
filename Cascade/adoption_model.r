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
			'chosen_inviter','chosen_inv_gender', 'chosen_inv_locale',
			'chosen_inv_invitation_count', 'chosen_inv_fav_gift', 
			'chosen_inv_sent_ARs', 'chosen_inv_children_count', 'chosen_inv_active_children',
			'neighboring',
			'male_inviters', 'female_inviters', 'inviters_locale_popularity',
			'inviters_avg_invitation_count','inviters_avg_sent_ARs',
			'inviters_avg_active_children', 'inviters_avg_children_count', 'avg_inviter_succ_ratio'
#			'excep_avg_invitation_count','excep_avg_sent_ARs',
#			'excep_avg_active_children', 'excep_avg_children_count', 'avg_excep_succ_ratio'
			)
	adoption_feat <- adoption_feat[adoption_feat$inv_count > 0, ] # & adoption_feat$avg_inviter_succ_ratio >= 0
	adoption_feat$int_gender <- 0
	adoption_feat$int_gender[which(adoption_feat$gender == 1 & adoption_feat$female_inviters > adoption_feat$male_inviters)] <- 1
	adoption_feat$int_gender[which(adoption_feat$gender == 0 & adoption_feat$male_inviters > adoption_feat$female_inviters)] <- -1
	adoption_feat$int_locale <- 1
	adoption_feat$int_locale[which(adoption_feat$locale == adoption_feat$inviters_locale_popularity)] <- 0
	adoption_feat$chosen_int_gender <- 0
	adoption_feat$chosen_int_gender[which(adoption_feat$gender == 1 & adoption_feat$chosen_inv_gender == 0)] <- 1
	adoption_feat$chosen_int_gender[which(adoption_feat$gender == 0 & adoption_feat$chosen_inv_gender == 1)] <- -1
	adoption_feat$chosen_int_locale <- 1
	adoption_feat$chosen_int_locale[which(adoption_feat$locale == adoption_feat$chosen_inv_locale)] <- 0
	adoption_feat$succ_ratio <- adoption_feat$chosen_inv_active_children / adoption_feat$chosen_inv_children_count
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
	
	feature_boxplot(adoption_feat, 'chosen_inv_invitation_count', 
			'Adopted by invitee', 'Number of invitations received from the inviter', 'iheart_gift/inv_invitation_count.pdf')
	feature_boxplot(adoption_feat, 'chosen_inv_sent_ARs', 
			'Adopted by invitee', 'Inviters\' sent AR count', 'iheart_gift/inv_sent_ARs.pdf')
	feature_boxplot(adoption_feat, 'chosen_inv_children_count', 
			'Adopted by invitee', 'Inviters\' children count', 'iheart_gift/inv_children_count.pdf')
	feature_boxplot(adoption_feat, 'chosen_inv_active_children', 
			'Adopted by invitee', 'Inviters\' active children count', 'iheart_gift/inv_active_children.pdf')
	feature_boxplot(adoption_feat, 'succ_ratio', 
			'Adopted by invitee', 'Inviters\' success ratio', 'iheart_gift/inv_succ_ratio.pdf')	
	feature_boxplot(adoption_feat, 'neighboring', 
			'Adopted by invitee', 'Jaccard coefficient of neighbor similarity', 'iheart_gift/inv_jaccard_neighbor.pdf')

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
	
#	feature_boxplot(adoption_feat, 'excep_avg_invitation_count', 
#			'Adopted by invitee', 'Inviters\' avg. exceptional invitation count', 'iheart_gift/avg_excep_invitation.pdf')
#	feature_boxplot(adoption_feat, 'excep_avg_sent_ARs', 
#			'Adopted by invitee', 'Inviters\' avg. exceptional sent AR count', 'iheart_gift/avg_excep_sent_ARs.pdf')
#	feature_boxplot(adoption_feat, 'excep_avg_active_children', 
#			'Adopted by invitee', 'Inviters\' avg. exceptional active children count', 'iheart_gift/avg_excep_active_children.pdf')
#	feature_boxplot(adoption_feat, 'excep_avg_children_count', 
#			'Adopted by invitee', 'Inviters\' avg. exceptional children count', 'iheart_gift/avg_excep_children_count.pdf')
#	feature_boxplot(adoption_feat, 'avg_excep_succ_ratio', 
#			'Adopted by invitee', 'Inviters\' avg. exceptional success ratio', 'iheart_gift/avg_excep_succ_ratio.pdf')
	
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

model_names <- c(
		'NR', 'NS', 'NAS', 
		'NR+RD', 'NS+SD', 'NAS+ASD', 
		'NS+NR', 'NAS+NR',
		'NS+SD+NR+RD+LD', 'NAS+ASD+NR+RD+LAD'
		)

adoption_logit_model <- function(feat){
	# Invitee's properties
	NR <- c('inv_count', 'inviter_count', 'recep_burst', 'inv_elapsed_hr', 'gift_veriety')
	# Inviter's properties
	NS <- c('succ_ratio', 'chosen_inv_sent_ARs', 'chosen_inv_children_count', 'chosen_inv_active_children')
	# Average inviters' properties
	NAS <- c('inviters_avg_sent_ARs', 'inviters_avg_active_children',
			'inviters_avg_children_count', 'avg_inviter_succ_ratio')
	RD <- c('gender', 'locale')
	# Inviter's properties
	SD <- c('chosen_inv_gender', 'chosen_inv_locale')
	# Average inviters' properties
	ASD <- c('male_inviters', 'female_inviters', 'inviters_locale_popularity')
	
	#Link based
	# Demographics
	LD <- c('chosen_int_gender', 'chosen_int_locale', )
	LAD <- c('int_gender', 'int_locale')
	
	LF <- c('chosen_inv_invitation_count', 'chosen_inv_fav_gift', 'neighboring')
	LAF <- c('inviters_avg_invitation_count')
	
	fmla <- c(
			paste("adopted~", paste(NR, collapse= "+")),
			paste("adopted~", paste(NS, collapse= "+")),
			paste("adopted~", paste(NAS, collapse= "+")),
	
			paste("adopted~", paste(c(NR, RD), collapse= "+")),
			paste("adopted~", paste(c(NS, SD), collapse= "+")),
			paste("adopted~", paste(c(NAS, ASD), collapse= "+")),
	
#			paste("adopted~", paste(LF, collapse= "+")),
#			paste("adopted~", paste(LAF, collapse= "+")),
#			
			paste("adopted~", paste(c(NS, NR, LF), collapse= "+")),
			paste("adopted~", paste(c(NAS, NR, LAF), collapse= "+")),
			
			paste("adopted~", paste(c(NS, SD, NR, RD, LF, LD), collapse= "+")),
			paste("adopted~", paste(c(NAS, ASD, NR, RD, LAF, LAD), collapse= "+"))
	)
	models <- c()
	for (i in 1:length(fmla)){
		models[[i]] <- buildClassifier(0, feat$training, feat$test, as.formula(fmla[i]), NULL)
	}
	return(models)
}

#feat <- load_features('iheart_gift/succ_adoption_features.csv')
#logistic_model <- adoption_logit_model(feat)

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