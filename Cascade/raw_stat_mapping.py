import sys, gc
import array
import csv

sent_AR_users_ihe =  array.array('l')
recved_AR_users_ihe = array.array('l')
succ_ratio_users_ihe = array.array('f')
act_child_users_ihe = array.array('l')
act_lifespan_users_ihe = array.array('i')
adopt_delay_users_ihe = array.array('i')

sent_AR_users_hug =  array.array('l')
recved_AR_users_hug = array.array('l')
succ_ratio_users_hug = array.array('f')
act_child_users_hug = array.array('l')
act_lifespan_users_hug = array.array('i')
adopt_delay_users_hug = array.array('i')

sent_AR_users_ism =  array.array('l')
recved_AR_users_ism = array.array('l')
succ_ratio_users_ism = array.array('f')
act_child_users_ism = array.array('l')
act_lifespan_users_ism = array.array('i')
adopt_delay_users_ism = array.array('i')

iheart_user_features_file = '/home/rezaur/output_cascade/iheart_users/user_features.csv'
print 'reading iheart user features'
for sent_ar, rec_ar, act_life, adopt_delay, success_ratio, act_child in csv.reader(open(iheart_user_features_file)):
    sent_AR_users_ihe.append(int(sent_ar))
    recved_AR_users_ihe.append(int(rec_ar))
    act_lifespan_users_ihe.append(int(act_life))
    adopt_delay_users_ihe.append(int(adopt_delay))
    succ_ratio_users_ihe.append(float(success_ratio))
    act_child_users_ihe.append(int(act_child))

hugged_user_features_file = '/home/rezaur/output_cascade/hugged_users/user_features.csv'
print 'reading hugged user features'
for sent_ar, rec_ar, act_life, adopt_delay, success_ratio, act_child in csv.reader(open(hugged_user_features_file)):
    sent_AR_users_hug.append(int(sent_ar))
    recved_AR_users_hug.append(int(rec_ar))
    act_lifespan_users_hug.append(int(act_life))
    adopt_delay_users_hug.append(int(adopt_delay))
    succ_ratio_users_hug.append(float(success_ratio))
    act_child_users_hug.append(int(act_child))

ismile_user_features_file = '/home/rezaur/output_cascade/ismile_users/user_features.csv'
print 'reading ismile user features'
for sent_ar, rec_ar, act_life, adopt_delay, success_ratio, act_child in csv.reader(open(ismile_user_features_file)):
    sent_AR_users_ism.append(int(sent_ar))
    recved_AR_users_ism.append(int(rec_ar))
    act_lifespan_users_ism.append(int(act_life))
    adopt_delay_users_ism.append(int(adopt_delay))
    succ_ratio_users_ism.append(float(success_ratio))
    act_child_users_ism.append(int(act_child))


all_users_file = "/home/rezaur/data/user_seq/all_user_seq.txt"
sent_ar_combined_file = open("/home/rezaur/output_cascade/raw_stat_apps/comb_sent_ar.csv", "w")
recved_ar_combined_file = open("/home/rezaur/output_cascade/raw_stat_apps/comb_rec_ar.csv", "w")
act_life_combined_file = open("/home/rezaur/output_cascade/raw_stat_apps/comb_act_life.csv", "w")
adopt_delay_combined_file = open("/home/rezaur/output_cascade/raw_stat_apps/comb_adopt_delay.csv", "w")
succ_ratio_combined_file = open("/home/rezaur/output_cascade/raw_stat_apps/comb_succ_ratio.csv", "w")
act_child_combined_file = open("/home/rezaur/output_cascade/raw_stat_apps/comb_act_child.csv", "w")
user_seq = 1
for user_id, hug_seq, ism_seq, ihe_seq in csv.reader(open(all_users_file)):
    hug_seq = int(hug_seq)
    ism_seq = int(ism_seq)
    ihe_seq = int(ihe_seq)
    if ihe_seq > 189989307:
        continue
    sent_ar_combined_file.write('%s,%s,%s,%s\n'%(user_seq, sent_AR_users_hug[hug_seq] if hug_seq != -1 else -1,
                                                 sent_AR_users_ihe[ihe_seq] if ihe_seq != -1 else -1,
                                                 sent_AR_users_ism[ism_seq] if ism_seq != -1 else -1))
    recved_ar_combined_file.write('%s,%s,%s,%s\n'%(user_seq, recved_AR_users_hug[hug_seq] if hug_seq != -1 else -1,
                                                   recved_AR_users_ihe[ihe_seq] if ihe_seq != -1 else -1,
                                                   recved_AR_users_ism[ism_seq] if ism_seq != -1 else -1))
    act_life_combined_file.write('%s,%s,%s,%s\n'%(user_seq, act_lifespan_users_hug[hug_seq] if hug_seq != -1 else -1,
                                                  act_lifespan_users_ihe[ihe_seq] if ihe_seq != -1 else -1,
                                                  act_lifespan_users_ism[ism_seq] if ism_seq != -1 else -1))
    adopt_delay_combined_file.write('%s,%s,%s,%s\n'%(user_seq, adopt_delay_users_hug[hug_seq] if hug_seq != -1 else -1,
                                                     adopt_delay_users_ihe[ihe_seq] if ihe_seq != -1 else -1,
                                                     adopt_delay_users_ism[ism_seq] if ism_seq != -1 else -1))
    succ_ratio_combined_file.write('%s,%s,%s,%s\n'%(user_seq, round(succ_ratio_users_hug[hug_seq], 3) if hug_seq != -1 else -1,
                                                    round(succ_ratio_users_ihe[ihe_seq], 3) if ihe_seq != -1 else -1,
                                                    round(succ_ratio_users_ism[ism_seq], 3) if ism_seq != -1 else -1))
    act_child_combined_file.write('%s,%s,%s,%s\n'%(user_seq, act_child_users_hug[hug_seq] if hug_seq != -1 else -1,
                                                    act_child_users_ihe[ihe_seq] if ihe_seq != -1 else -1,
                                                    act_child_users_ism[ism_seq] if ism_seq != -1 else -1))
    user_seq += 1
    
sent_ar_combined_file.close()
recved_ar_combined_file.close()
act_life_combined_file.close()
adopt_delay_combined_file.close()
succ_ratio_combined_file.close()
