import sys, gc, csv
import array
import operator
from sets import Set
from random import choice

CLR_THRESHOLD = 500000

hugged_users = {}
ismile_users = {}
iheart_users = {}

for user_id, hugged_user_seq, ismile_user_seq, iheart_user_seq in csv.reader(open("user_seq/all_user_seq.txt")):
    if int(iheart_user_seq) != -1:
        iheart_users[int(iheart_user_seq)] = None
    else:
        continue
    if int(hugged_user_seq) != -1:
        hugged_users[int(hugged_user_seq)] = None    
    if int(ismile_user_seq) != -1:
        ismile_users[int(ismile_user_seq)] = None

count = 0
for node_id, odeg, indeg, indeg_until_act, active_lifespan in csv.reader(open("iheart_preprocessed_basic.txt", "r")):
    count = count+1
    if (count % (CLR_THRESHOLD/10)) == 0:
        print "iheart", count
    if int(node_id) not in iheart_users:
        continue
    if int(indeg_until_act) != -1:
        indeg_after_act = int(indeg) - int(indeg_until_act)
        iheart_users[int(node_id)] = (int(indeg_until_act), indeg_after_act, int(odeg), int(active_lifespan))
    else:
        continue

count = 0
for node_id, odeg, indeg, indeg_until_act, active_lifespan in csv.reader(open("hugged_preprocessed_basic.txt", "r")):
    count = count+1
    if (count % (CLR_THRESHOLD/10)) == 0:
        print "hugged", count
    if int(node_id) not in hugged_users:
        continue
    if int(indeg_until_act) != -1:
        indeg_after_act = int(indeg) - int(indeg_until_act)
        hugged_users[int(node_id)] = (int(indeg_until_act), indeg_after_act, int(odeg), int(active_lifespan))
    else:
        continue

count = 0
for node_id, odeg, indeg, indeg_until_act, active_lifespan in csv.reader(open("ismile_preprocessed_basic.txt", "r")):
    count = count+1
    if (count % (CLR_THRESHOLD/10)) == 0:
        print "ismile", count
    if int(node_id) not in ismile_users:
        continue
    if int(indeg_until_act) != -1:
        indeg_after_act = int(indeg) - int(indeg_until_act)
        ismile_users[int(node_id)] = (int(indeg_until_act), indeg_after_act, int(odeg), int(active_lifespan))
    else:
        continue

cross_app_user_act_file = csv.writer(open("cross_app_user_act.txt", "w"))
cross_app_user_act = []
for user_id, hugged_user_seq, ismile_user_seq, iheart_user_seq in csv.reader(open("user_seq/all_user_seq.txt")):
    if int(iheart_user_seq) == -1:
        continue
    hugged_user_seq = int(hugged_user_seq)
    ismile_user_seq = int(ismile_user_seq)
    iheart_user_seq = int(iheart_user_seq)
    if iheart_users[iheart_user_seq] == None:
        continue
    hugged_act = None
    if hugged_user_seq in hugged_users:
        hugged_act = hugged_users[hugged_user_seq]
    ismile_act = None
    if ismile_user_seq in ismile_users:
        ismile_act = ismile_users[ismile_user_seq]
    if hugged_act == None and ismile_act == None:
        continue
    if hugged_act != None and ismile_act != None:
        cross_app_user_act.append((iheart_users[iheart_user_seq][0], iheart_users[iheart_user_seq][1],
                                   iheart_users[iheart_user_seq][2], iheart_users[iheart_user_seq][3],
                                   hugged_users[hugged_user_seq][0], hugged_users[hugged_user_seq][1],
                                   hugged_users[hugged_user_seq][2], hugged_users[hugged_user_seq][3],
                                   ismile_users[ismile_user_seq][0], ismile_users[ismile_user_seq][1],
                                   ismile_users[ismile_user_seq][2], ismile_users[ismile_user_seq][3]))
    else:
        if hugged_act == None:
            cross_app_user_act.append((iheart_users[iheart_user_seq][0], iheart_users[iheart_user_seq][1],
                                       iheart_users[iheart_user_seq][2], iheart_users[iheart_user_seq][3],
                                       None, None, None, None,
                                       ismile_users[ismile_user_seq][0], ismile_users[ismile_user_seq][1],
                                       ismile_users[ismile_user_seq][2], ismile_users[ismile_user_seq][3]))
        if ismile_act == None:
            cross_app_user_act.append((iheart_users[iheart_user_seq][0], iheart_users[iheart_user_seq][1],
                                       iheart_users[iheart_user_seq][2], iheart_users[iheart_user_seq][3],
                                       hugged_users[hugged_user_seq][0], hugged_users[hugged_user_seq][1],
                                       hugged_users[hugged_user_seq][2], hugged_users[hugged_user_seq][3],
                                       None, None, None, None))
    
cross_app_user_act_file.writerows(cross_app_user_act)
