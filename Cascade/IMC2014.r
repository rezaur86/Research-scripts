source('~/scripts/Cascade/raw_stat_analysis.r')
source('~/scripts/Cascade/size_depth_analysis.r')
source('~/scripts/Cascade/temporal_dyn.r')

#####################################################################################################
#Indegree and Outdegree distribution in same plot
raw_outdeg_analysis(raw_outdeg_file_name = 'raw_stat_v2/raw_outdeg_stat.csv',
		raw_indeg_file_name='raw_stat_v2/raw_indeg_stat.csv')
#Outputs
#raw_stat_v2/degree.pdf

#Number of ARs vs probability of adoption
influence_threshold_analysis(parent_count_file_name = 'raw_stat_v2/parent_count_before_act.csv',
		indeg_before_act_file= 'raw_stat_v2/indeg_before_act.csv')
#Outputs
#raw_stat_v2/adop_prob.pdf

#Success ratio CDF
success_ratio_analysis(success_ratio_file='raw_stat_v2/act_proportion_count.csv')
#Outputs
#raw_stat_v2/success_ratio.pdf

#Average success ratio vs Number of sent ARs
sender_success <- sender_success_ratio_analysis(success_ratio_file='raw_stat_v2/parent_children_act.csv')
#Outputs
#raw_stat_v2/success_avg_vs_ARs.pdf

#Invitation burstiness; Recieved ARs before getting activated
invitation_burstiness(file='raw_stat_v2/invitation_burstiness_stat.csv')
#Outputs
#raw_stat_v2/invitation_burstiness_count.pdf
#raw_stat_v2/invitation_burstiness_cdf.pdf
#raw_stat_v2/invitation_burstiness_avg_elapse_time.pdf
#raw_stat_v2/invitation_burstiness_median_elapsed_time.pdf

#Elapsed time (Hour) CDF of recieved ARs
invitation_elapsed_time_analysis(invitation_elapsed_time_file='raw_stat_v2/invitation_elapsed_time_stat.csv')
#Outputs
#raw_stat_v2/invitation_elased_time.pdf

#####################################################################################################
#Cascade tree size distribution
size_distribution(file_name='iheart_cascade/size.csv')
#Outputs
#iheart_cascade/size.pdf

#Cascade tree depth distribution
depth_distribution(file_name='iheart_cascade/depth.csv')
#Outputs
#iheart_cascade/depth.pdf

#Cascade width distribution
width_distribution(file_name='top_size.csv_all__max_width.csv')
#Outputs
#iheart_cascade/width.pdf

#Cascade size vs depth and width
size_prop <- size_vs_properties(file='iheart_cascade/top_size.csv_all_evolution.csv')
#Outputs
#iheart_cascade/size_vs_depth.pdf
#iheart_cascade/size_vs_width.pdf

#Seeds's contributions
size_vs_root_contribution(file='iheart_cascade/top_size.csv_all_size_vs_root_odeg.csv')
#Outputs
#iheart_cascade/seed_contribution_ratio.pdf
#iheart_cascade/seed_contribution_vs_size.pdf
#iheart_cascade/seed_ARs_vs_size.pdf
#iheart_cascade/seed_success_ratio_vs_size.pdf

#####################################################################################################
#Daily and Weekly activities
temporal_analysis(daily_born = 'raw_stat_v2/daily_born.csv',
		daily_activation = 'daily_activation.csv',
		daily_last_act = 'daily_last_act.csv',
		daily_last_seen = 'daily_last_seen.csv')
#Outputs
#raw_stat_v2/overall_activities.pdf
#raw_stat_v2/daily_activities.pdf

#Cascade growth evolution; Evotion is the growth across time
evolution<-burstiness_analysis(file='iheart_cascade/top_size.csv_all_evolution.csv')
#Outputs
#iheart_cascade/burstiness_count.pdf
#iheart_cascade/burstiness_cdf.pdf
#iheart_cascade/lifetime_cdf.pdf
#iheart_cascade/burstiness_vs_size.pdf

#Analyze inter-adoption time of all the users
#Inter-adoption time is the time between the activation of parent and child
analyze_inter_adoption_time(file = 'iheart_cascade/inter_adoption_time_stat.txt')
#iheart_cascade/inter_adoption_time.pdf

#Cascade size vs inter-adoption time between the parents and children
analyze_size_vs_inter_adoption_time(file = 'iheart_cascade/size_vs_inter_adoption_time.txt')
#Outputs
#iheart_cascade/size_vs_inter_adoption_time.pdf

#Inter-generation time; defined as the minimum time of the activations in any generation
analyze_inter_generation_time(file = 'iheart_cascade/inter_generation_time_stat.txt')
#Outputs
#iheart_cascade/inter_generation_time.pdf

