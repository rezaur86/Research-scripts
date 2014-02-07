import sys, os
import csv
from random import sample
import heapq
import math
import random
import array

def WeightedSelectionWithoutReplacement(weights, m):
    elt = [(math.log(random.random()) / weights[i], i) for i in range(len(weights))]
    return [x[1] for x in heapq.nlargest(m, elt)]

total_users = 189989307
total_activities = 2027488267

each_app = sys.argv[1]
# sampling method RN = 0
# sampling method RPN = 1
# sampling method RDN = 2
# sampling method RE = 3
# sampling method RNE = 4
# sampling method HYB = 5
# sampling method RNN = 6
# sampling method RJ = 7
# sampling method RW = 8
# sampling method FF = 9
sampling_method = int(sys.argv[2])
sampled_users = set([])
sampled_act = set([])
sampled_users_act = {}
sampled_senders = set([])

if sampling_method == 0:
    each_app_anonym = each_app + '_sampled_RN'
    try:
        for user_id in csv.reader(open(sys.argv[3])):
            sampled_users.add(int(user_id))
    except:
        # Random Node Sampling
        sampled_users = set(sample(xrange(total_users), int(total_users*.1)))
        w = csv.writer(open("/home/rezaur/data/sampled_users/RN.txt", "w"))
        for user_id in sampled_users:
            w.writerow([user_id]) 

if sampling_method == 2:
    each_app_anonym = each_app + '_sampled_RDN'
    try:
        for user_id in csv.reader(open(sys.argv[3])):
            sampled_users.add(int(user_id))
    except:
        weights = array.array('l',(0,)*total_users)
        for user_id,odeg,indeg,deg in csv.reader(open('/home/rezaur/data/iheart_ext_preprocessed_basic.txt')):
            weights[int(user_id)] = int(deg)
        sampled_users = set(WeightedSelectionWithoutReplacement(weights, int(total_users*.1)))

if sampling_method == 3:
    each_app_anonym = each_app + '_sampled_RE'
    try:
        for activity_line in csv.reader(open(sys.argv[3])): #activity lines are edges
            sampled_act.add(activity_line)
    except:
        # Random Edge Sampling
        sampled_act = set(sample(xrange(total_activities), int(total_users*.1)))
        w = csv.writer(open("/home/rezaur/data/sampled_users/RE.txt", "w"))
        for user_id in sampled_users:
            w.writerow([user_id]) 

if sampling_method == 4:
    each_app_anonym = each_app + '_sampled_RNE'
    # Random Node-Edge Sampling
    sampled_users = set(sample(xrange(total_users), int(total_users*.1)))
    for user_id,odeg,indeg,deg in csv.reader(open(sys.argv[3])):
        if user_id in sampled_users:
            sampled_users_act[user_id] = sample(xrange(deg), 1)
    sampled_users = set([])
    w = csv.writer(open("/home/rezaur/data/sampled_users/RNE.txt", "w"))
    for user_id in sampled_users_act:
        w.writerow([user_id,sampled_users_act[user_id]]) 

if sampling_method == 6:
    each_app_anonym = each_app + '_sampled_RNN'
    for user_id in csv.reader(open(sys.argv[3])):
        sampled_senders.add(user_id)

users = {}
user_count = 0

try:
    os.stat(each_app_anonym)
except:
    os.makedirs(each_app_anonym)
file_list = sorted(os.listdir(each_app))
activity_line = 0
for each_file in file_list:
    f = open(each_app+'/'+each_file, "r")
    lines = f.readlines()
    f.close()
    anonymized_data = []
    for line in lines:
        activity_line += 1
        take_this_act = False
        splits = line.split()
        sender_org_id = long(splits[0].strip())
        recv_org_id = long(splits[1].strip())
        if len(sampled_users) != 0:
            if sender_org_id in sampled_users and recv_org_id in sampled_users:
                take_this_act = True
        if len(sampled_users_act) != 0:
            if sender_org_id in sampled_users_act:
                if sampled_users_act[sender_org_id] != 0:
                    sampled_users_act[sender_org_id] -= 1
                else:
                    take_this_act = True
            if recv_org_id in sampled_users:
                if sampled_users_act[recv_org_id] != 0:
                    sampled_users_act[recv_org_id] -= 1
                else:
                    take_this_act = True
        if len(sampled_act) != 0:
            if (activity_line-1) in sampled_act:
                take_this_act = True
        if len(sampled_senders) != 0:
            if sender_org_id in sampled_senders:
                take_this_act = True

        if take_this_act == False:
            continue        
        if users.has_key(sender_org_id) == False:
            users[sender_org_id] = user_count
            user_count += 1
        sender = str(users[sender_org_id])
        if users.has_key(recv_org_id) == False:
            users[recv_org_id] = user_count
            user_count += 1
        recv = str(users[recv_org_id])            
        timestamp = splits[2].strip()
        symid = splits[3].strip()
        anonymized_data.append((sender, recv, timestamp, symid))
    anonymized_file = open(each_app_anonym+'/'+each_file, "wb")
    writer = csv.writer(anonymized_file, quoting=csv.QUOTE_MINIMAL, delimiter=' ')
    writer.writerows(anonymized_data)
    anonymized_file.close()
    print each_file
    
w = csv.writer(open('/home/rezaur/data/user_seq/'+each_app_anonym+"_user_seq.txt", "w"))
for user_id, seq in users.items():
    w.writerow([user_id, seq])