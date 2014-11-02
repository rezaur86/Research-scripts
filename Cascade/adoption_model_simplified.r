source('~/scripts/Cascade/buildClassifier.r')
source('~/scripts/Cascade/tools.r')
library(ggplot2)
library(gtable)
library(reshape)
library(grid)
library(plyr)
library(Hmisc)
library(gridExtra) 
library(nnet)
library(pROC)
library('FSelector')
library(xtable)

# Invitees' properties
NR <- c('inv_count', 'recep_burst', 'inv_elapsed_hr', 'gift_veriety', 'inviters_avg_invitation_count')
# Average inviters' properties
NAS <- c('inviters_avg_sent_ARs', 'inviters_avg_active_children', 'avg_inviter_succ_ratio')

IMP <- c('avg_inviter_succ_ratio', 'inviters_avg_active_children', 'inv_elapsed_hr', 'recep_burst', 'id',
		'gift_veriety', 'inv_count', 'inviters_avg_invitation_count', 'inviters_avg_sent_ARs')

#training <- c()
#testing <- c()

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

load_training_sets <- function(training_sets, names){
	for (i in 1:length(names)){
		training[[names[i]]] <<- load_features(training_sets[i])
	}
}

load_testing_sets <- function(testing_sets, names){
	for (i in 1:length(names)){
		testing[[names[i]]] <<- load_features(testing_sets[i])
	}
}

feature_selection <- function(){
	feat_imp <- data.frame(row.names=c(NR,NAS))
	for (each_training in names(training)){
		each_chi_imp <- chi.squared(as.simple.formula(c(NR,NAS), "adopted"), training[[each_training]])
		feat_imp[[each_training]] <- each_chi_imp
	}
	return(feat_imp)
}

#feat_imp <- feature_selection()
get_top_k_features <- function(training_name, k){
	return(rownames(feat_imp[order(-feat_imp[[training_name]]),][1:k,]))
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

get_2nd_level_interactions <- function(feature_set){
	comb_all <- combn(feature_set, 2)
	comb_feat <- c()
	for (i in 1:(length(comb_all)/2)){
		comb_feat <- c(comb_feat, paste(c(comb_all[i], comb_all[2*i]), collapse= ":"))
	}
	return(comb_feat)
}

build_adoption_model <- function(training_name, testing_names, top_k_feat, interaction_terms = FALSE){
	fmla <- c(
#			paste("adopted~", paste(c(NAS,NR,'id'), collapse= "+"))
#			paste("adopted~", paste(NR, collapse= "+")),
#			paste("adopted~", paste(NAS, collapse= "+")),
#			paste("adopted~", paste(c(NAS, NR), collapse= "+"))
#			paste("adopted~", paste(c(IMP_5, comb_feat), collapse= "+"))
	)
	for (k in top_k_feat){
		top_k <- get_top_k_features(training_name, k)
		fmla <- c(fmla, paste("adopted~", paste(top_k, collapse= "+")))
		if(interaction_terms)
			fmla <- c(fmla, paste("adopted~", paste(c(top_k, get_2nd_level_interactions(top_k)), collapse= "+")))
	}
	models <- c()
	for (i in 1:length(fmla)){
		print(fmla[i])
		models[[i]] <- list()
		for(testing_name in testing_names){
			print(testing_name)
			models[[i]][[testing_name]] <- buildClassifier(0, 'adopted', 
					training[[training_name]], testing[[testing_name]], 
					as.formula(fmla[i]), NULL)
		}
	}
#	if (length(testing_names) == 1){
#		for (i in 1:length(fmla)){
#			print(fmla[i])
#			models[[i]] <- buildClassifier(0, 'adopted', 
#					training[[training_name]], testing[[testing_names[1]]], 
#					as.formula(fmla[i]), NULL)
#		}
#	}
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

#feature_selction <- function(df, cat, features){
#	chi_vals <- c()
#	for (feature in features){
#		chi_table <- table(df[[cat]], round(df[[feature]],2))
#		chi <- chisq.test(chi_table)
#		chi_vals <- c(chi_vals, chi$statistic)
#	}
#	normalized_chi_vals <- (chi_vals)/(max(chi_vals))
#	chi_result <- as.data.frame(features)
#	colnames(chi_result) <- c('Features')
#	chi_result$chi_vals <- chi_vals
#	chi_result$norm_chi_vals <- normalized_chi_vals
#	return(chi_result)
#}

model_names = c('Growth', 'Peak', 'Decline', 'Any')
latex_result <- function(result, model_names){
	models <- as.data.frame(model_names)
	colnames(models) <- c('Phases')
	prec <- c()
	TPR <- c()
	FPR <- c()
	ACC <- c()
	F1 <- c()
	AUC <- c()
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

draw_performance <- function(models){
	prec <- c()
	TPR <- c()
	FPR <- c()
	ACC <- c()
	F1 <- c()
	AUC <- c()
	for (i in 1:length(models)){
		AUC <- c(AUC, models[[i]]$auc)
		prec <- c(prec, models[[i]]$te_50_perf[2])
		TPR <- c(TPR, models[[i]]$te_50_perf[3])
		FPR <- c(FPR, models[[i]]$te_50_perf[4])
		F1 <- c(F1, models[[i]]$te_50_perf[5])
		ACC <- c(ACC, models[[i]]$te_50_perf[6])
	}
	models.perform <- data.frame(value = c(prec, TPR, FPR, ACC, AUC),
			PM=c(rep('Prec', length(prec)),
					rep('TPR', length(TPR)),
					rep('FPR', length(FPR)),
					rep('ACC', length(ACC)),
					rep('AUC', length(AUC))),
			top=c(rep(c(1,2,3,4,5,6,7,8,9),5))
	)
	print(models.perform)
	models.perform$PM <- factor(models.perform$PM)
	plot <- ggplot(data=models.perform, aes(x=top, y=value)) +
			geom_line(aes(linetype=PM)) +
			scale_x_discrete(breaks=1:9)+
			scale_y_continuous(breaks=seq(0,1,0.1))+
			xlab('# of top features') + ylab('Performance value')
	save_ggplot(plot, 'iheart_gift/model_perf.pdf', 24, opts(legend.position=c(.7, .2)))
	return (models.perform)
}
