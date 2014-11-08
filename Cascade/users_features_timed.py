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
CLR_THRESHOLD = 500000
BURSTINESS_ON = 0

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
children_count = array.array('i', (-1,)*MAX_USERS)
active_children = array.array('i', (-1,)*MAX_USERS)

def reg_children(user_id, parent_list, activation_time):
    l = len(parent_list)
    invitations = {}
    gifts = {}
    inviters = {}
    for i in range(l-1,-1,-1):
        u_p = parent_list[i].strip().split(',')
        if u_p[0] == '-1':
            return
        invitation_time = int(u_p[1])
        if invitation_time > observation_time:
            continue
        invitation_hour = invitation_time/3600 #Hourly
        if invitation_hour in invitations:
            invitations[invitation_hour] += 1
        else:
            invitations[invitation_hour] = 1
        parent = int(u_p[0])
        if parent >= MAX_USERS:
            continue
        if sent_ARs[parent] == -1:
            sent_ARs[parent] = 0
        sent_ARs[parent] += 1
        if parent in inviters:
            inviters[parent] += 1
        else:
            inviters[parent] = 1
            if children_count[parent] == -1:
                children_count[parent] = 1
                active_children[parent] = 0
            else:
                children_count[parent] += 1
            if has_adopted[user_id] == True:
                active_children[parent] += 1
        hid = int(u_p[2])
        if hid in gifts:
            gifts[hid] += 1
        else:
            gifts[hid] = 1
    if invitations == {}:
        return
    inv_count[user_id] = l
    inviter_count[user_id] = len(inviters)
    activation_hr = min(activation_time, observation_time)/3600 #Hourly
    hr_delay_from_first_inv[user_id] = activation_hr - min(invitations)
    inv_elapsed_hr[user_id] = max(invitations) - min(invitations) + 1
    gift_veriety[user_id] = len(gifts)
    if BURSTINESS_ON == 1:
        first_invitation_hour = min(invitations)
        last_invitation_hour = max(invitations)
        elapsed_hour = last_invitation_hour - first_invitation_hour + 1
        if elapsed_hour == 1:
            recep_burst[user_id] = -1 # Burstiness of one time slot is -1
            return
        temp_invitations = []
        for i in range(first_invitation_hour, last_invitation_hour+1):
            temp_invitations.append(invitations[i] if i in invitations else 0)
        sd = np.sqrt(np.var(temp_invitations))
        avg = np.average(temp_invitations)
        recep_burst[user_id] = ((sd - avg) / (sd + avg))
    
if BURSTINESS_ON == 0:
    burstiness_file = open(sys.argv[2], "r")
    for line in burstiness_file:
        element = line.split(',')
        node_id = int(element[0].strip())
        recep_burst[node_id] = float(element[1].strip())

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
    activation_time = int(element[2].strip())
    is_leaf = bool(int(element[5].strip()))
    parent_list = element[7:len(element)]
    if is_leaf == False and activation_time <= observation_time:
        has_adopted[node_id] = True
    reg_children(node_id, parent_list, activation_time)
f.close()

user_features_file = open(sys.argv[5]+"user_features_timed.csv", "w")
for i in range(0,MAX_USERS):
    user_features_file.write('%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' %(
                            i, int(has_adopted[i]), inv_count[i], inviter_count[i], 
                            round(recep_burst[i], 3), inv_elapsed_hr[i],
                            hr_delay_from_first_inv[i], gift_veriety[i],
                            sent_ARs[i], children_count[i], active_children[i]))
user_features_file.close()
