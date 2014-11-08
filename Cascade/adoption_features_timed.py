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

MAX_USERS = int(sys.argv[3])
observation_time = int(sys.argv[4])

has_adopted = bitarray(MAX_USERS)
has_adopted.setall(False)
inv_count = array.array('I', (0,)*MAX_USERS)
inviter_count = array.array('I', (0,)*MAX_USERS)
recep_burst = array.array('f', (INVALID_BURSTINESS,)*MAX_USERS)
inv_elapsed_hr = array.array('H', (NEVER,)*MAX_USERS)
hr_delay_from_first_inv = array.array('H', (NEVER,)*MAX_USERS)
gift_veriety = array.array('H', (0,)*MAX_USERS)

############################# Inviter feature #########################
sent_ARs = array.array('i', (-1,)*MAX_USERS)
act_lifespan = array.array('H', (NEVER,)*MAX_USERS)
children_count = array.array('i', (-1,)*MAX_USERS)
active_children = array.array('i', (-1,)*MAX_USERS)


inviters_avg_invitation_count = array.array('f', (0,)*MAX_USERS)
inviters_avg_sent_ARs = array.array('f', (0,)*MAX_USERS)
inviters_avg_active_children = array.array('f', (0,)*MAX_USERS)
inviters_avg_children_count = array.array('f', (0,)*MAX_USERS)
inviters_avg_success_ratio = array.array('f', (-1,)*MAX_USERS)

node_parents = {}

def reg_children(user_id, parent_list):
    l = len(parent_list)
    inviters = {}
    inviters_gift_id = {}
    inviters_succ_ratio = {}
    inviters_sent_AR = []
    inviters_active_children = []
    inviters_children_count = []
    for i in range(l-1,-1,-1):
        u_p = parent_list[i].strip().split(',')
        if u_p[0] == '-1':
            return
        parent = int(u_p[0])
        if parent >= MAX_USERS:
            continue
        invitation_time = int(u_p[1])
        if invitation_time > observation_time:
            continue
        hid = int(u_p[2])
        if parent in inviters:
            inviters[parent] += 1
            if hid in inviters_gift_id[parent]:
                inviters_gift_id[parent][hid] += 1
            else:
                inviters_gift_id[parent][hid] = 1
        else:
            if user_id in node_parents:
                node_parents[user_id].append(parent)
            else:
                node_parents[user_id] = array.array('I')
                node_parents[user_id].append(parent)
            inviters[parent] = 1
            inviters_gift_id[parent] = {}
            inviters_gift_id[parent][hid] = 1
            inviters_succ_ratio[parent] = (active_children[parent]*1.0)/children_count[parent]
            inviters_sent_AR.append(sent_ARs[parent])
            inviters_active_children.append(active_children[parent])
            inviters_children_count.append(children_count[parent])
            
    if len(inviters) == 0:
        return
    if has_adopted[user_id] == False:
        del node_parents[user_id]

    inviters_avg_invitation_count[user_id] = (1.0*sum(inviters.itervalues()))/inviter_count[user_id]
    inviters_avg_sent_ARs[user_id] = (1.0*sum(inviters_sent_AR))/inviter_count[user_id]
    inviters_avg_active_children[user_id] = (1.0*sum(inviters_active_children))/inviter_count[user_id]
    inviters_avg_children_count[user_id] = (1.0*sum(inviters_children_count))/inviter_count[user_id]
    inviters_avg_success_ratio[user_id] = (1.0*sum(inviters_succ_ratio.itervalues()))/inviter_count[user_id]
    

CLR_THRESHOLD = 500000

f = open(sys.argv[2], "r")
for line in f:
    element = line.split(',')
    user_id = int(element[0].strip())
    has_adopted[user_id] = bool(int(element[1].strip()))
    inv_count[user_id] = int(element[2].strip())
    inviter_count[user_id] = int(element[3].strip())
    recep_burst[user_id] = float(element[4].strip())
    inv_elapsed_hr[user_id] = int(element[5].strip())
    hr_delay_from_first_inv[user_id] = int(element[6].strip())
    gift_veriety[user_id] = int(element[7].strip())
    sent_ARs[user_id] = int(element[8].strip())
    children_count[user_id] = int(element[9].strip())
    active_children[user_id] = int(element[10].strip())

count = 0
f = open(sys.argv[1], "r")
for line in f:
    count = count+1
    if (count % (CLR_THRESHOLD/10)) == 0:
        print count
    element = line.split(' ')
    node_id = int(element[0].strip())
    if node_id >= MAX_USERS:
        continue
    parent_list = element[7:len(element)]
    reg_children(node_id, parent_list)
f.close()

adoption_features_file = open(sys.argv[5]+"adoption_timed.csv", "w")
for i in range(0,MAX_USERS):
    if inv_count[i] < 1:
        continue
    if inviters_avg_success_ratio[i] < 0:
        continue
    adoption_features_file.write('%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' %(
                            i, int(has_adopted[i]), inv_count[i], inviter_count[i],
                            round(recep_burst[i],3), inv_elapsed_hr[i], hr_delay_from_first_inv[i], gift_veriety[i],
                            round(inviters_avg_invitation_count[i],3), round(inviters_avg_sent_ARs[i],3),
                            round(inviters_avg_active_children[i],3), round(inviters_avg_children_count[i],3),
                            round(inviters_avg_success_ratio[i],3)
                            ))
adoption_features_file.close()
