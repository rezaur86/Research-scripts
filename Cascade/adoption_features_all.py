import sys, gc
import array
from bitarray import bitarray
import operator
from sets import Set
from random import choice
import numpy as np

NEVER = 2**16 - 1
NEVER_S = 2**31 - 1
MAX_USERS = 189989307 #1492480#
INVALID_BURSTINESS = 2
POPULAR_INVITER = 0
SUCCESSFUL_INVITER = 1

iheart_seq = array.array('i', (-1,)*(3*MAX_USERS))
genders = bitarray((3*MAX_USERS))
genders.setall(True) #True/1 is for Male
has_adopted = bitarray((3*MAX_USERS))
has_adopted.setall(False)
inv_count = array.array('I', (0,)*(3*MAX_USERS))
inviter_count = array.array('I', (0,)*(3*MAX_USERS))
recep_burst = array.array('f', (INVALID_BURSTINESS,)*(3*MAX_USERS))
inv_elapsed_hr = array.array('H', (NEVER,)*(3*MAX_USERS))
gift_veriety = array.array('H', (0,)*(3*MAX_USERS))

# chosen_inviter_gender = bitarray((3*MAX_USERS))
# chosen_inviter_gender.setall(True) #True/1 is for Male
# chosen_inviter_locale = array.array('B', (0,)*(3*MAX_USERS))
# chosen_inviter_for_users = array.array('I', (0,)*(3*MAX_USERS))
chosen_inviter_inv_count = array.array('I', (0,)*(3*MAX_USERS))
# chosen_inviter_fav_gift = array.array('H', (0,)*(3*MAX_USERS))
chosen_inviter_sent_ARs = array.array('I', (0,)*(3*MAX_USERS))
# chosen_inviter_children_count = array.array('I', (0,)*(3*MAX_USERS))
chosen_inviter_active_children = array.array('I', (0,)*(3*MAX_USERS))
chosen_inviter_succ_ratio = array.array('f', (-1,)*(3*MAX_USERS))

# inviters_gender_popularity = bitarray((3*MAX_USERS))
# inviters_gender_popularity.setall(True) #True/1 is for Male
# inviters_male_count = array.array('I', (0,)*(3*MAX_USERS))
# inviters_female_count = array.array('I', (0,)*(3*MAX_USERS))
inviters_avg_inv_count = array.array('f', (0,)*(3*MAX_USERS))
inviters_avg_sent_ARs = array.array('f', (0,)*(3*MAX_USERS))
inviters_avg_active_children = array.array('f', (0,)*(3*MAX_USERS))
# inviters_avg_children_count = array.array('f', (0,)*(3*MAX_USERS))
inviters_avg_success_ratio = array.array('f', (-1,)*(3*MAX_USERS))
    

CLR_THRESHOLD = 500000
for i in (0,1,2):
    f = open(sys.argv[i+1], "r")
    count = 0
    for line in f:
        element = line.split(',')
        user_id = int(element[0].strip())
        iheart_seq[3*user_id + i] = user_id
        has_adopted[3*user_id + i] = bool(int(element[1].strip()))
        genders[3*user_id + i] = bool(int(element[2].strip()))
        inv_count[3*user_id + i] = int(element[4].strip())
        inviter_count[3*user_id + i] = int(element[5].strip())
        recep_burst[3*user_id + i] = float(element[6].strip())
        inv_elapsed_hr[3*user_id + i] = int(element[7].strip())
        gift_veriety[3*user_id + i] = int(element[8].strip())

        chosen_inviter_inv_count[3*user_id + i] = int(element[12].strip())
        chosen_inviter_sent_ARs[3*user_id + i] = int(element[14].strip())
        chosen_inviter_active_children[3*user_id + i] = int(element[16].strip())
        chosen_inviter_succ_ratio[3*user_id + i] = int(element[16].strip())*1.0/int(element[15].strip())
        
        inviters_avg_inv_count[3*user_id + i] = float(element[22].strip())
        inviters_avg_sent_ARs[3*user_id + i] = float(element[23].strip())
        inviters_avg_active_children[3*user_id + i] = float(element[24].strip())
        inviters_avg_success_ratio[3*user_id + i] = float(element[26].strip())

        count = count+1
        if (count % (CLR_THRESHOLD/10)) == 0:
            print i
            print count

adoption_features_all_file = open(sys.argv[4]+"adoption_features_all.csv", "w")
for i in range(0,MAX_USERS):
    if iheart_seq[3*i] == -1 or iheart_seq[3*i + 1] == -1 or iheart_seq[3*i + 2] == -1:
        continue
    adoption_features_all_file.write(
        '%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' %(
        iheart_seq[3*i + 2], int(genders[3*i + 2]),
        int(has_adopted[3*i]), int(has_adopted[3*i+1]), int(has_adopted[3*i+2]),
        inv_count[3*i], inv_count[3*i+1], inv_count[3*i+2],
        inviter_count[3*i], inviter_count[3*i+1], inviter_count[3*i+2],
        round(recep_burst[3*i],3), round(recep_burst[3*i+1],3), round(recep_burst[3*i+2],3), 
        inv_elapsed_hr[3*i], inv_elapsed_hr[3*i+1], inv_elapsed_hr[3*i+2],
        gift_veriety[3*i], gift_veriety[3*i+1], gift_veriety[3*i+2],
        chosen_inviter_inv_count[3*i],chosen_inviter_inv_count[3*i+1],chosen_inviter_inv_count[3*i+2],
        chosen_inviter_sent_ARs[3*i], chosen_inviter_sent_ARs[3*i+1], chosen_inviter_sent_ARs[3*i+2],
        chosen_inviter_active_children[3*i], chosen_inviter_active_children[3*i+1], chosen_inviter_active_children[3*i+2],
        round(chosen_inviter_succ_ratio[3*i],3), round(chosen_inviter_succ_ratio[3*i+1],3), round(chosen_inviter_succ_ratio[3*i+2],3),
        round(inviters_avg_inv_count[3*i],3), round(inviters_avg_inv_count[3*i+1],3), round(inviters_avg_inv_count[3*i+2],3),
        round(inviters_avg_sent_ARs[3*i],3), round(inviters_avg_sent_ARs[3*i+1],3), round(inviters_avg_sent_ARs[3*i+2],3),
        round(inviters_avg_active_children[3*i],3), round(inviters_avg_active_children[3*i+1],3),round(inviters_avg_active_children[3*i+2],3),
        round(inviters_avg_success_ratio[3*i],3), round(inviters_avg_success_ratio[3*i+1],3),round(inviters_avg_success_ratio[3*i+2],3) 
    ))
adoption_features_all_file.close()
