import sys, gc
import array
from bitarray import bitarray
import operator
from sets import Set
from random import choice
import numpy as np

NEVER = 2**16 - 1
MAX_USERS = 189989307 #1492480#
INVALID_BURSTINESS = 2

users = bitarray(MAX_USERS)
users.setall(False)
locale_ids = {}
locale_idx = 0
genders = bitarray(MAX_USERS)
genders.setall(True) #True/1 is for Male
locales = array.array('B', (0,)*MAX_USERS)
has_adopted = bitarray(MAX_USERS)
has_adopted.setall(False)
inv_count = array.array('I', (0,)*MAX_USERS)
inviter_count = array.array('I', (0,)*MAX_USERS)
recep_burst = array.array('h', (INVALID_BURSTINESS,)*MAX_USERS)
inv_elapsed_hr = array.array('H', (NEVER,)*MAX_USERS)
hr_delay_from_first_inv = array.array('H', (NEVER,)*MAX_USERS)
hr_delay_from_last_inv = array.array('H', (NEVER,)*MAX_USERS)
gift_veriety = array.array('H', (0,)*MAX_USERS)

############################# Inviter feature #########################
sent_ARs = array.array('i', (-1,)*MAX_USERS)
act_lifespan = array.array('H', (NEVER,)*MAX_USERS)
children_count = array.array('i', (-1,)*MAX_USERS)
active_children = array.array('i', (-1,)*MAX_USERS)

inviters_avg_success_ratio = array.array('f', (-1,)*MAX_USERS)
popular_inviter_gender = bitarray(MAX_USERS)
popular_inviter_gender.setall(True) #True/1 is for Male
popular_inviter_locale = array.array('B', (0,)*MAX_USERS)
popular_inviter_inv_count = array.array('I', (0,)*MAX_USERS)
popular_inviter_fav_gift = array.array('H', (0,)*MAX_USERS)
popular_inviter_sent_ARs = array.array('I', (0,)*MAX_USERS)
popular_inviter_children_count = array.array('I', (0,)*MAX_USERS)
popular_inviter_active_children = array.array('I', (0,)*MAX_USERS)


def reg_children(user_id, parent_list):
    l = len(parent_list)
    invitations = {}
    gifts = {}
    inviters = {}
    inviters_gift_id = {}
    for i in range(l-1,-1,-1):
        u_p = parent_list[i].strip().split(',')
        if u_p[0] == '-1':
            return
        parent = int(u_p[0])
        if users[parent] == False:
            continue
        hid = int(u_p[2])
        if parent in inviters:
            inviters[parent] += 1
            if hid in inviters_gift_id[parent]:
                inviters_gift_id[parent][hid] += 1
            else:
                inviters_gift_id[parent][hid] = 1
        else:
            inviters[parent] = 1
            inviters_gift_id[parent] = {}
            inviters_gift_id[parent][hid] = 1
    if len(inviters) == 0:
        return
    popular_inviter = max(inviters.iteritems(), key=operator.itemgetter(1))[0]
    inviters_succ_ratio = []
    for each_inviter in inviters:
        succ_ratio = (active_children[each_inviter]*1.0)/children_count[each_inviter]
        inviters_succ_ratio.append(succ_ratio)
    inviters_avg_success_ratio[user_id] = sum(inviters_succ_ratio)/inviter_count[user_id]
    popular_inviter_gender[user_id] = genders[popular_inviter]
    popular_inviter_locale[user_id] = locales[popular_inviter]
    popular_inviter_inv_count[user_id] = inviters[popular_inviter]
    popular_inviter_fav_gift[user_id] = max(inviters_gift_id[popular_inviter].iteritems(), key=operator.itemgetter(1))[0]
    popular_inviter_sent_ARs[user_id] = sent_ARs[popular_inviter]
    popular_inviter_children_count[user_id] = children_count[popular_inviter]
    popular_inviter_active_children[user_id] = active_children[popular_inviter]

CLR_THRESHOLD = 500000
# app_id_pos = int(sys.argv[4])
try:
    BURSTINESS_ON = int(sys.argv[5])
except:
    BURSTINESS_ON = 1

f = open(sys.argv[2], "r")
for line in f:
    element = line.split(',')
    user_id = int(element[0].strip())
    if user_id != -1 and user_id < MAX_USERS:
        users[user_id] = True
    has_adopted[user_id] = bool(int(element[1].strip()))
    genders[user_id] = bool(int(element[2].strip()))
    locales[user_id] = int(element[3].strip())
    inv_count[user_id] = int(element[4].strip())
    inviter_count[user_id] = int(element[5].strip())
    recep_burst[user_id] = int(element[6].strip())
    inv_elapsed_hr[user_id] = int(element[7].strip())
    hr_delay_from_first_inv[user_id] = int(element[8].strip())
    hr_delay_from_last_inv[user_id] = int(element[9].strip())
    gift_veriety[user_id] = int(element[10].strip())
    sent_ARs[user_id] = int(element[11].strip())
    act_lifespan[user_id] = int(element[12].strip())
    children_count[user_id] = int(element[13].strip())
    active_children[user_id] = int(element[14].strip())

count = 0
f = open(sys.argv[1], "r")
for line in f:
    element = line.split(' ')
    node_id = int(element[0].strip())
    if users[node_id] == False:
        continue
    parent_list = element[7:len(element)]
    reg_children(node_id, parent_list)
    count = count+1
    if (count % (CLR_THRESHOLD/10)) == 0:
        print count
f.close()

user_features_file = open(sys.argv[3]+"adoption_features.csv", "w")
for i in range(0,MAX_USERS):
    if users[i] == False:
        continue
    user_features_file.write('%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' %(
                            i, int(has_adopted[i]), int(genders[i]), locales[i], inv_count[i], inviter_count[i], recep_burst[i],
                            inv_elapsed_hr[i], hr_delay_from_first_inv[i], hr_delay_from_last_inv[i], gift_veriety[i],
                            round(inviters_avg_success_ratio[i],3),
                            int(popular_inviter_gender[i]), popular_inviter_locale[i],
                            popular_inviter_inv_count[i], popular_inviter_fav_gift[i],
                            popular_inviter_sent_ARs[i], popular_inviter_children_count[i], popular_inviter_active_children[i]))
user_features_file.close()
