source('~/scripts/Cascade/buildClassifier.r')
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

IMP_4 <- c(
		'inviters_avg_success_ratio_ihe', 'inviters_avg_active_children_ihe', 'recep_burst_ihe', 'inv_elapsed_hr_ihe'
)
IMP_4_history <- c(
		'inviters_avg_success_ratio_hug', 'inviters_avg_success_ratio_ism',
		'inviters_avg_active_children_hug', 'inviters_avg_active_children_ism',
		'inv_elapsed_hr_hug', 'inv_elapsed_hr_ism',
		'recep_burst_hug', 'recep_burst_ism'
)

# Invitee's properties
NR <- c('inv_count_ihe', 'recep_burst_ihe', 'inv_elapsed_hr_ihe', 'gift_veriety_ihe', 'inviters_avg_inv_count_ihe')
# Average inviters' properties
NAS <- c('inviters_avg_sent_ARs_ihe', 'inviters_avg_active_children_ihe', 'inviters_avg_success_ratio_ihe')

ADH <- c('adopted_hug', 'adopted_ism')
NHR <- c('inv_count_hug', 'recep_burst_hug', 'inv_elapsed_hr_hug', 'gift_veriety_hug',
		'inv_count_ism', 'recep_burst_ism', 'inv_elapsed_hr_ism', 'gift_veriety_ism',
		'inviters_avg_inv_count_hug', 'inviters_avg_inv_count_ism')#, 'inviter_count_ism'
NHAS <- c('inviters_avg_sent_ARs_hug', 'inviters_avg_active_children_hug', 'inviters_avg_success_ratio_hug',
		'inviters_avg_sent_ARs_ism', 'inviters_avg_active_children_ism', 'inviters_avg_success_ratio_ism')

feature_scaling <- function(feat, feat_col_list){
	for (a_feat_col in feat_col_list){
		feat[[a_feat_col]] <- scale(feat[[a_feat_col]], center = TRUE, scale = TRUE)
	}
	return(feat)
}

load_historical_features <- function(file){
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
	adoption_feat <- feature_scaling(adoption_feat, c(NAS, NR, ADH, NHR, NHAS))
	return(adoption_feat)
}

process_features <- function(app1, app2=NA){
	training <- load_historical_features(app1)	
	chi_test <- 0# feature_selction(adoption_feat, 'adopted', c(NR, NS, NAS, RD, SD, ASD, LD, LAD, LF, LAF))
	if (is.na(app2)){
		splitted_data <- split(training, sample(1:2, nrow(training), replace=TRUE, prob=c(1,2)))
		training <- splitted_data[[2]]
		test <- splitted_data[[1]]
	}
	else test <- load_historical_features(app2)
	return(list(training = training, test = test, chi_test = chi_test))
}


adoption_historical_model <- function(feat, training_name, testing_name, model_id=0){
	fmla <- c(
#			paste("adopted_ihe~", paste(IMP_4, collapse= "+")),
#			paste("adopted_ihe~", paste(c(IMP_4, ADH), collapse= "+")),
#			paste("adopted_ihe~", paste(c(IMP_4, ADH, IMP_4_history), collapse= "+"))
#			paste("adopted_ihe~", paste(c(NAS, NR), collapse= "+")),
#			paste("adopted_ihe~", paste(c(NAS, NR,'adopted_hug'), collapse= "+")),
#			paste("adopted_ihe~", paste(c(NAS, NR, ADH), collapse= "+")),
			paste("adopted_ihe~", paste(c(NAS, NR, ADH, NHR, NHAS), collapse= "+"))
	)
	models <- c()
	for (i in 1:length(fmla)){
		print(fmla[i])
		models[[i]] <- buildClassifier(model_id, 'adopted_ihe', feat[[training_name]], feat[[testing_name]], as.formula(fmla[i]), NULL)
	}
	return(models)
}

#m_his_2mo_G <- adoption_historical_model(f_his_2mo_G, 0)
#m_his_2mo_P <- adoption_historical_model(f_his_2mo_P, 0)
#m_his_2mo_D <- adoption_historical_model(f_his_2mo_D, 0)
#m_his_2mo_A <- adoption_historical_model(f_his_2mo_A, 0)

model_names = c('W/o history', 'W/ history (labels)', 'W/ history')
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
	models$Prec <- prec
	models$TPR <- TPR
	models$FPR <- FPR
	models$ACC <- ACC
	models$AUC <- AUC
	print(xtable(models,digits=c(3)), include.rownames=FALSE)
	return (models)
}

#feat <- load_features('iheart_gift/adoption_features_test.csv')
#logistic_model <- adoption_logit_model(feat,0)
#J48_model <- adoption_logit_model(feat, 2)
#svm_model <- adoption_logit_model(feat, 3)
#NB_model <- adoption_logit_model(feat, 4)