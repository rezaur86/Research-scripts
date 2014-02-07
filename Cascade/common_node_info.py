import sys, gc, csv
import array
import operator
from sets import Set
from random import choice

CLR_THRESHOLD = 500000

hugged_users = {}
ismile_users = {}
iheart_users = {}

for user_id, hugged_user_seq, ismile_user_seq, iheart_user_seq in csv.reader(open("all_user_seq.txt")):
    if int(hugged_user_seq) != -1:
        hugged_users[int(hugged_user_seq)] = None    
    if int(ismile_user_seq) != -1:
        ismile_users[int(ismile_user_seq)] = None
    if int(iheart_user_seq) != -1:
        iheart_users[int(iheart_user_seq)] = None

count = 0
f = open("hugged_preprocessed_sorted.txt", "r")
for line in f:
    count = count+1
    if (count % (CLR_THRESHOLD/10)) == 0:
        print 'hugged', count
    element = line.split(' ')
    node_id = int(element[0].strip())
    if node_id not in hugged_users:
        continue
    born_time = int(element[1].strip())
    activation_time = int(element[2].strip())
    last_act_time = int(element[3].strip())
    last_seen_time = int(element[4].strip())
    is_leaf = bool(int(element[5].strip()))
    odeg = int((element[6].strip()))
    parent_list = element[7:len(element)]
    if parent_list[0].strip() == '-1':
        indeg = 0
    else:
        indeg = len(parent_list)
    if is_leaf == False:
        lifespan = last_act_time - born_time + 1
        active_lifespan = last_act_time - activation_time + 1
    else:
        lifespan = 0
        active_lifespan = 0
    hugged_users[node_id] = (odeg, indeg, lifespan, active_lifespan)
f.close()

count = 0
f = open("ismile_preprocessed_sorted.txt", "r")
for line in f:
    count = count+1
    if (count % (CLR_THRESHOLD/10)) == 0:
        print 'ismile', count
    element = line.split(' ')
    node_id = int(element[0].strip())
    if node_id not in ismile_users:
        continue
    born_time = int(element[1].strip())
    activation_time = int(element[2].strip())
    last_act_time = int(element[3].strip())
    last_seen_time = int(element[4].strip())
    is_leaf = bool(int(element[5].strip()))
    odeg = int((element[6].strip()))
    parent_list = element[7:len(element)]
    if parent_list[0].strip() == '-1':
        indeg = 0
    else:
        indeg = len(parent_list)
    if is_leaf == False:
        lifespan = last_act_time - born_time + 1
        active_lifespan = last_act_time - activation_time + 1
    else:
        lifespan = 0
        active_lifespan = 0
    ismile_users[node_id] = (odeg, indeg, lifespan, active_lifespan)
f.close()

count = 0
f = open("iheart_preprocessed_sorted.txt", "r")
for line in f:
    count = count+1
    if (count % (CLR_THRESHOLD/10)) == 0:
        print "iheart", count
    element = line.split(' ')
    node_id = int(element[0].strip())
    if node_id not in iheart_users:
        continue
    born_time = int(element[1].strip())
    activation_time = int(element[2].strip())
    last_act_time = int(element[3].strip())
    last_seen_time = int(element[4].strip())
    is_leaf = bool(int(element[5].strip()))
    odeg = int((element[6].strip()))
    parent_list = element[7:len(element)]
    if parent_list[0].strip() == '-1':
        indeg = 0
    else:
        indeg = len(parent_list)
    if is_leaf == False:
        lifespan = last_act_time - born_time + 1
        active_lifespan = last_act_time - activation_time + 1
    else:
        lifespan = 0
        active_lifespan = 0
    iheart_users[node_id] = (odeg, indeg, lifespan, active_lifespan)
f.close()

odeg_file = csv.writer(open("common_outdegree.txt", "w"))
indeg_file = csv.writer(open("common_indegree.txt", "w"))
lifespan_file = csv.writer(open("common_lifespan.txt", "w"))
active_lifespan_file = csv.writer(open("common_active_lifespan.txt", "w"))
for user_id, hugged_user_seq, ismile_user_seq, iheart_user_seq in csv.reader(open("all_user_seq.txt")):
    if int(hugged_user_seq) in hugged_users:
        hugged_user_info = hugged_users[int(hugged_user_seq)]
    else:
        hugged_user_info = (None, None, None, None)
    if int(ismile_user_seq) in ismile_users:
        ismile_user_info = ismile_users[int(ismile_user_seq)]
    else:
        ismile_user_info = (None, None, None, None)
    iheart_user_info = (None, None, None, None)
    if int(iheart_user_seq) in iheart_users:
        if iheart_users[int(iheart_user_seq)] != None:
            iheart_user_info = iheart_users[int(iheart_user_seq)]
    odeg_file.writerow([hugged_user_info[0], ismile_user_info[0], iheart_user_info[0]])
    indeg_file.writerow([hugged_user_info[1], ismile_user_info[1], iheart_user_info[1]])
    lifespan_file.writerow([hugged_user_info[2], ismile_user_info[2], iheart_user_info[2]])
    active_lifespan_file.writerow([hugged_user_info[3], ismile_user_info[3], iheart_user_info[3]])
     