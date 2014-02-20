import sys, os
import csv
from random import sample
import heapq
import math
import random
import array
import copy

total_users = 189989307
total_activities = 2027488267

SAMPLE_SIZE = int(total_users*.1)
sampled_graph_nodes = set() #Final size of this set has to be equal to the SAMPLE_SIZE

def WeightedSelectionWithoutReplacement(weights, m):
    elt = [(math.log(random.random()) / weights[i], i) for i in range(len(weights))]
    return [x[1] for x in heapq.nlargest(m, elt)]

each_app = sys.argv[1]
# BFS with random seeds from non zero odeg users, seed_choice = 0
# BFS with WEIGHTED random seeds from non zero odeg users, seed_choice = 1
seed_choice = int(sys.argv[2])
nodes_to_visit_next = set([])

each_app_anonym = each_app + '_sampled_BFS' + sys.argv[2]
try:
    for user_id in csv.reader(open(sys.argv[3])):
        nodes_to_visit_next.add(int(user_id))
except:
    non_zero_odeg_users = []
    weights = []
    for user_id,odeg,indeg,deg in csv.reader(open('/home/rezaur/data/iheart_ext_preprocessed_basic.txt')):
        if int(odeg) > 0:
            non_zero_odeg_users.append(int(user_id))
            weights.append(int(odeg))
    total_senders = len(non_zero_odeg_users)
    print "Sampling Seeds..."
    if seed_choice == 0:
        nodes_to_visit_next = set(sample(non_zero_odeg_users, int(total_senders*.25)))
    else:
        idx = WeightedSelectionWithoutReplacement(weights, int(total_senders*.25))
        nodes_to_visit_next = set([non_zero_odeg_users[i] for i in idx])
    w = csv.writer(open("/home/rezaur/data/sampled_users/BFS.txt", "w"))
    for user_id in nodes_to_visit_next:
        w.writerow([user_id]) 
    print "Sampling Done"

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
        sender_org_id = int(splits[0].strip())
        recv_org_id = int(splits[1].strip())
        if sender_org_id in nodes_to_visit_next:
            nodes_to_visit_next.add(recv_org_id)
            take_this_act = True

        # If desired sample size is already met then we don't want to
        # visit any new nodes, but we can explore the edges
        if len(sampled_graph_nodes) >= SAMPLE_SIZE:
            if sender_org_id not in sampled_graph_nodes or recv_org_id not in sampled_graph_nodes:
                take_this_act = False

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
        if users[sender_org_id] != -1:
            sampled_graph_nodes.add(sender_org_id)
        if users[recv_org_id] != -1:
            sampled_graph_nodes.add(recv_org_id)
    anonymized_file = open(each_app_anonym+'/'+each_file, "wb")
    writer = csv.writer(anonymized_file, quoting=csv.QUOTE_MINIMAL, delimiter=' ')
    writer.writerows(anonymized_data)
    anonymized_file.close()
    print each_file
    
w = csv.writer(open('/home/rezaur/data/user_seq/'+each_app_anonym+"_user_seq.txt", "w"))
for user_id, seq in users.items():
    if seq != -1:
        w.writerow([user_id, seq])