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
	adoption_feat$invitation_rate <- adoption_feat$inv_count * 24 / adoption_feat$inv_elapsed_hr
	adoption_feat$inviter_rate <- adoption_feat$inviter_count * 24 / adoption_feat$inv_elapsed_hr
	adoption_feat$gender_popularity <- 0
	adoption_feat$gender_popularity[which(adoption_feat$male_inviters > adoption_feat$female_inviters)] <- 1
	adoption_feat$int_gender <- 0
	adoption_feat$int_gender[which(adoption_feat$gender == 1 & adoption_feat$gender_popularity == 0)] <- 1
	adoption_feat$int_gender[which(adoption_feat$gender == 0 & adoption_feat$gender_popularity == 1)] <- 2
	adoption_feat$int_gender[which(adoption_feat$gender == 1 & adoption_feat$gender_popularity == 1)] <- 3
	adoption_feat$int_locale <- 1
	adoption_feat$int_locale[which(adoption_feat$locale == adoption_feat$inviters_locale_popularity)] <- 0
	adoption_feat$chosen_int_gender <- 0
	adoption_feat$chosen_int_gender[which(adoption_feat$gender == 1 & adoption_feat$chosen_inv_gender == 0)] <- 1
	adoption_feat$chosen_int_gender[which(adoption_feat$gender == 0 & adoption_feat$chosen_inv_gender == 1)] <- 2
	adoption_feat$chosen_int_gender[which(adoption_feat$gender == 1 & adoption_feat$chosen_inv_gender == 1)] <- 3
	adoption_feat$chosen_int_locale <- 1
	adoption_feat$chosen_int_locale[which(adoption_feat$locale == adoption_feat$chosen_inv_locale)] <- 0
	adoption_feat$succ_ratio <- adoption_feat$chosen_inv_active_children / adoption_feat$chosen_inv_children_count
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
	LD <- c('chosen_int_gender', 'chosen_int_locale')
	LAD <- c('int_gender', 'int_locale')
	
	LF <- c('chosen_inv_invitation_count', 'chosen_inv_fav_gift', 'neighboring')
	LAF <- c('inviters_avg_invitation_count')

	gender.df <- gender_stat(adoption_feat, c('gender', 'chosen_inv_gender', 'gender_popularity'))
	gender.df$gender <- factor(gender.df$gender)
	gender.df$type <- factor(gender.df$type)
	plot <- ggplot(gender.df, aes(x=gender, y=adop_prob, fill=type)) + 
			geom_bar(stat="identity", position=position_dodge()) +
			scale_fill_manual(values=c("gray40", "gray65", "gray85"), name = '', breaks=0:2,
					labels=c('Invitee','Chosen inviter','Popularity\namong inviters'))+
			xlab('Gender') + ylab('Adoption probability') 
	save_ggplot(plot, 'iheart_gift/users_gender.pdf', 24, opts(legend.position="bottom"))
	
	int_gender.df <- int_gender_stat(adoption_feat, c('chosen_int_gender', 'int_gender'))
	int_gender.df$gender <- factor(int_gender.df$gender)
	int_gender.df$type <- factor(int_gender.df$type)
	plot <- ggplot(int_gender.df, aes(x=gender, y=adop_prob, fill=type)) + 
			geom_bar(stat="identity", position=position_dodge()) +
			scale_fill_manual(values=c("gray40", "gray65"), name = '', breaks=0:1,
					labels=c('Invitee vs. chosen inviter','Invitee vs. popularity\namong all inviters'))+
			xlab('Invitee gender vs. inviter gender') + ylab('Adoption probability') 
	save_ggplot(plot, 'iheart_gift/link_gender.pdf', 24, opts(legend.position="bottom"))
	
	fav_gifts <- as.data.frame(unique(adoption_feat$chosen_inv_fav_gift))
	colnames(fav_gifts) <- c('gifts')
	temp <- c()
	for (gift in fav_gifts$gifts){
		gf <- adoption_feat$adopted[which(adoption_feat$chosen_inv_fav_gift == gift)]
		temp <- c(temp, sum(gf)/length(gf))
	}
	fav_gifts$adop_prob <- temp
	fav_gifts <- fav_gifts[order(-fav_gifts$adop_prob),]
	fav_gifts_to_plot <- fav_gifts[1:10,]
	fav_gifts_to_plot$gifts <- factor(fav_gifts_to_plot$gifts)
	plot <- ggplot(fav_gifts_to_plot, aes(x=gifts, y=adop_prob)) + 
			geom_bar(stat="identity") +
			xlab('Gift ID') + ylab('Adoption probability') 
	save_ggplot(plot, 'iheart_gift/inv_fav_gift.pdf')
	
	chi_test <- feature_selction(adoption_feat, 'adopted', c(NR, NS, NAS, RD, SD, ASD, LD, LAD, LF, LAF))
	adoption_feat$adopted <- factor(adoption_feat$adopted)
	categories <- levels(adoption_feat$adopted)
	adoption_feat$cat_label <- factor(adoption_feat$adopted, levels = categories, labels = c('No', 'Yes'))

	feature_summary <- list(
		users_inv_count = feature_boxplot(adoption_feat, 'invitation_rate', 
			'Adopted by invitee', '# invitations recevied per day', 'iheart_gift/users_inv_count.pdf'),
		users_inviter_count = feature_boxplot(adoption_feat, 'inviter_rate', 
			'Adopted by invitee', '# inviters per day', 'iheart_gift/users_inviter_count.pdf'),
		users_recep_burst = feature_boxplot(adoption_feat, 'recep_burst', 
			'Adopted by invitee', 'Reception burstiness', 'iheart_gift/users_recep_burst.pdf'),
		users_inv_elapsed_hr = feature_boxplot(adoption_feat, 'inv_elapsed_hr', 
			'Adopted by invitee', 'Invitation elapsed hour', 'iheart_gift/users_inv_elapsed_hr.pdf'),
		users_gift_veriety= feature_boxplot(adoption_feat, 'gift_veriety', 
			'Adopted by invitee', 'Gift variation count in invitations', 'iheart_gift/users_gift_veriety.pdf'),

		inv_jaccard_neighbor = feature_boxplot(adoption_feat, 'neighboring', 
			'Adopted by invitee', 'Jaccard coefficient of neighbor similarity', 'iheart_gift/inv_jaccard_neighbor.pdf'),

		comp_invitation_gender = feature_boxplot_comp(adoption_feat, 'female_inviters', 'male_inviters',
				'Adopted by invitee', 'Population among the inviters', 'iheart_gift/comp_gender_population.pdf', c('Female','Male')),

		comp_invitation_count = feature_boxplot_comp(adoption_feat, 'chosen_inv_invitation_count', 'inviters_avg_invitation_count',
				'Adopted by invitee', 'Number of invitations received from the inviter', 'iheart_gift/comp_invitation_count.pdf'),
		comp_sent_ARs = feature_boxplot_comp(adoption_feat, 'chosen_inv_sent_ARs', 'inviters_avg_sent_ARs',
				'Adopted by invitee', 'Inviters\' sent AR count', 'iheart_gift/comp_sent_ARs.pdf'),
		comp_children_count = feature_boxplot_comp(adoption_feat, 'chosen_inv_children_count', 'inviters_avg_children_count',
				'Adopted by invitee', 'Inviters\' children count', 'iheart_gift/comp_children_count.pdf'),
		comp_active_children = feature_boxplot_comp(adoption_feat, 'chosen_inv_active_children', 'inviters_avg_active_children',
				'Adopted by invitee', 'Inviters\' active children count', 'iheart_gift/comp_active_children.pdf'),
		comp_succ_ratio = feature_boxplot_comp(adoption_feat, 'succ_ratio', 'avg_inviter_succ_ratio', 
				'Adopted by invitee', 'Inviters\' success ratio', 'iheart_gift/comp_succ_ratio.pdf')
	)
	splitted_data <- split(adoption_feat, sample(1:3, nrow(adoption_feat), replace=TRUE, prob=c(1,2,7)))
	training <- splitted_data[[2]]
	test <- splitted_data[[1]]
	training$adopted <- factor(training$adopted)
	test$adopted <- factor(test$adopted)
	return(list(training = training, test = test, feat_summary = feature_summary,
					gender = gender.df, int_gender = int_gender.df, fav_gifts = fav_gifts, chi_test = chi_test))
}

gender_stat <- function(df, genders){
	df_n <- 0
	for (a_gender in genders){
		gender.df_temp <- as.data.frame(c('Female','Male'))
		colnames(gender.df_temp) <- c('gender')
		df0 <- df$adopted[which(df[[a_gender]] == 0)]
		df1 <- df$adopted[which(df[[a_gender]] == 1)]
		gender.df_temp$adop_prob <- c(sum(df0)/length(df0),
				sum(df1)/length(df1))
		gender.df_temp$type <- df_n
		if(df_n == 0)
			gender.df <- gender.df_temp
		else
			gender.df <- rbind(gender.df, gender.df_temp)
		df_n <- df_n + 1
	}
	return(gender.df)	
}

int_gender_stat <- function(df, int_genders){
	df_n <- 0
	for (a_gender in int_genders){
		gender.df_temp <- as.data.frame(c('Female-\nfemale','Male-\nfemale', 'Female-\nmale', 'Male-\nmale'))
		colnames(gender.df_temp) <- c('gender')
		df0 <- df$adopted[which(df[[a_gender]] == 0)]
		df1 <- df$adopted[which(df[[a_gender]] == 1)]
		df2 <- df$adopted[which(df[[a_gender]] == 2)]
		df3 <- df$adopted[which(df[[a_gender]] == 3)]
		gender.df_temp$adop_prob <- c(
				sum(df0)/length(df0),
				sum(df1)/length(df1),
				sum(df2)/length(df2),
				sum(df3)/length(df3)
		)
		gender.df_temp$type <- df_n
		if(df_n == 0)
			gender.df <- gender.df_temp
		else
			gender.df <- rbind(gender.df, gender.df_temp)
		df_n <- df_n + 1
	}
	return(gender.df)	
}

facet_boxplot <- function(df, features, label_x, label_y, figure_name){
	df_n <- 0
	for (a_feature in features){
		df_temp <- as.data.frame(df$cat_label)
		colnames(df_temp) <- c('cat_label')
		df_temp$a_feature <- df[[a_feature]]
		df_temp$comp_type <- df_n
		if (df_n == 0)
			df.all <- df_temp
		else
			df.all <- rbind(df.all, df_temp)
		df_n <- df_n + 1
	}
	features.a_feature <- ddply(df.all, c('comp_type', 'cat_label'), .drop=TRUE,
			.fun = function(one_partition){
				stats = boxplot.stats(one_partition$a_feature)$stats
				c(ymin=stats[1],
						lower=stats[2],
						middle=stats[3],
						upper=stats[4],
						ymax=stats[5],
						mean = mean(one_partition$a_feature))
			})
	features.a_feature$comp_type <- factor(features.a_feature$comp_type)
	features.a_feature$cat_label <- factor(features.a_feature$cat_label)
	
	plot <- ggplot(features.a_feature, aes(x=cat_label, lower=lower, upper=upper, middle=middle, ymin=ymin, ymax=ymax)) + 
			geom_boxplot(stat="identity") +
			geom_point(data = features.a_feature, aes(x=cat_label, y=mean), shape = 8, size = 3)+
			facet_wrap(. ~ comp_type, scales="free_y")+
			xlab(label_x) + ylab(label_y) 
	save_ggplot(plot, figure_name, width=14)
	return(features.a_feature)
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
	return(features.a_feature)
}

grid_boxplots <- function(feature_summary_list, feature_set, label_x_set, label_y_set, figure_name){
	plist <- list()
	i <- 1
	for (a_feature in feature_set){
		plist[[a_feature]] <- ggplot(feature_summary_list[[a_feature]], aes(x=cat_label, lower=lower, upper=upper, middle=middle, ymin=ymin, ymax=ymax)) + 
				geom_boxplot(stat="identity") +
				geom_point(data = feature_summary_list[[a_feature]], aes(x=cat_label, y=mean), shape = 8, size = 3)+
				xlab(label_x_set[i]) + ylab(label_y_set[i]) +
				myPlotTheme(19)	
		i <- i + 1
	}
	pdf(figure_name, width = 15, height = 3.5)
	print(do.call("grid.arrange", c(plist, ncol=length(plist))))
	dev.off()
#	save_ggplot(do.call("grid.arrange", c(plist, ncol=length(plist))), figure_name, width=14)
}

#grid_boxplots(feat$feat_summary, c('users_inv_count','users_inviter_count', 
#				'users_recep_burst', 'users_inv_elapsed_hr', 'users_gift_veriety'),
#		c('Adopted by invitee\n(a) AR count', 'Adopted by invitee\n(b) Invitee count',
#				'Adopted by invitee\n(c) AR recep. burstiness',
#				'Adopted by invitee\n(d) AR elapsed hr', 'Adopted by invitee\n(e) Gift variety'),
#		c('# AR recevied per day', '# inviters per day', 'Reception burstiness', 
#				'Invitation elapsed hr', 'Gift variations in ARs'),
#		'iheart_gift/users_all_feat.pdf')

feature_boxplot_comp <- function(features, feature_1, feature_2, label_x, label_y, figure_name,
		comp_label=c('Chosen inviter', 'Average of all the inviters')){
	comp_features_1 <- as.data.frame(features$cat_label)
	colnames(comp_features_1) <- c('cat_label')
	comp_features_1$a_feature <- features[[feature_1]]
	comp_features_1$comp_type <- 0
	comp_features_2 <- as.data.frame(features$cat_label)
	colnames(comp_features_2) <- c('cat_label')
	comp_features_2$a_feature <- features[[feature_2]]
	comp_features_2$comp_type <- 1
	comp_features <- rbind(comp_features_1, comp_features_2)
	features.a_feature <- ddply(comp_features, c('comp_type', 'cat_label'), .drop=TRUE,
			.fun = function(one_partition){
				stats = boxplot.stats(one_partition$a_feature)$stats
				c(ymin=stats[1],
						lower=stats[2],
						middle=stats[3],
						upper=stats[4],
						ymax=stats[5],
						mean = mean(one_partition$a_feature))
			})
	features.a_feature$comp_type <- factor(features.a_feature$comp_type)
	features.a_feature$cat_label <- factor(features.a_feature$cat_label)
	plot <- ggplot(features.a_feature, aes(x=cat_label, y=mean, fill = comp_type, lower=lower, upper=upper, middle=middle, ymin=ymin, ymax=ymax)) + 
			geom_boxplot(stat="identity") +
			stat_summary(fun.y = "mean", geom = "point", shape= 8, size= 3, position=position_dodge(width=.9)) +
#			geom_point(data = features.a_feature, aes(x=cat_label, y=mean), shape = 8, size = 3)+
			scale_fill_manual(values=c("gray40", "gray65"), name = '', breaks=0:1, labels=comp_label)+
			xlab(label_x) + ylab(label_y) 
	save_ggplot(plot, figure_name,  24, opts(legend.position="bottom"))

#	roles.box$comp_type <- factor(roles.box$comp_type)
#	roles.box$rank.x <- factor(roles.box$rank.x)
#	print(roles.box)
#	plot <- ggplot(roles.box, aes(rank.x, mean, fill = comp_type, lower=lower, upper=upper, middle=middle, ymin=ymin, ymax=ymax)) + 
#			geom_boxplot(stat="identity", fatten = 4)+ #scale_fill_grey(start = .5, end = .9, name = '') +
#			stat_summary(fun.y = "mean", geom = "point", shape= 8, size= 3, position=position_dodge(width=.9)) +
#			scale_x_discrete(breaks=1:4, labels= c('1st', '2nd', '3rd', '4th'))+
#			scale_fill_manual(values=c("gray40", "gray65", "gray90"), name = '', breaks=0:2,
#					labels=c('Hugged vs.   \n   iSmile', 'Hugged vs.   \n   iHeart', 'iSmile vs.   \n  iHeart'))+
#			xlab('Class') + ylab('Role distance')
#	save_ggplot(plot, paste(c('raw_stat_apps/cross_app_', figure_name, '.pdf'), collapse = ''),
	
	return(features.a_feature)
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
	LD <- c('chosen_int_gender', 'chosen_int_locale')
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