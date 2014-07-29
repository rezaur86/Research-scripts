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

root_sent_ARs_1 = {}
root_children_count_1 = {}
root_active_children_1 = {}

root_sent_ARs_2 = {}
root_children_count_2 = {}
root_active_children_2 = {}

root_sent_ARs_3 = {}
root_children_count_3 = {}
root_active_children_3 = {}

root_sent_ARs_4 = {}
root_children_count_4 = {}
root_active_children_4 = {}

def reg_children(user_id, parent_list, is_leaf, activation_time):
    l = len(parent_list)
    invitations = {}
    gifts = {}
    inviters = {}
    for i in range(l-1,-1,-1):
        u_p = parent_list[i].strip().split(',')
        if u_p[0] == '-1':
            return
        invitation_time = int(u_p[1])/86400 - 14413 + 1
        activation_time = activation_time/86400 - 14413 + 1
        parent = int(u_p[0])
        if parent in roots:
            if invitation_time <= roots[parent] + 7:
                root_sent_ARs_1[parent] = (root_sent_ARs_1[parent] + 1) if parent in root_sent_ARs_1 else 1
            if invitation_time <= roots[parent] + 14:
                root_sent_ARs_2[parent] = (root_sent_ARs_2[parent] + 1) if parent in root_sent_ARs_2 else 1
            if invitation_time <= roots[parent] + 21:
                root_sent_ARs_3[parent] = (root_sent_ARs_3[parent] + 1) if parent in root_sent_ARs_3 else 1
            if invitation_time <= roots[parent] + 28:
                root_sent_ARs_4[parent] = (root_sent_ARs_4[parent] + 1) if parent in root_sent_ARs_4 else 1
        if parent in inviters:
            inviters[parent] += 1
        else:
            inviters[parent] = 1
            if parent in roots:
                if invitation_time <= roots[parent] + 7:
                    if parent in root_children_count_1:
                        root_children_count_1[parent] += 1
                    else:
                        root_children_count_1[parent] = 1
                        root_active_children_1[parent] = 0
                if invitation_time <= roots[parent] + 14:
                    if parent in root_children_count_2:
                        root_children_count_2[parent] += 1
                    else:
                        root_children_count_2[parent] = 1
                        root_active_children_2[parent] = 0
                if invitation_time <= roots[parent] + 21:
                    if parent in root_children_count_3:
                        root_children_count_3[parent] += 1
                    else:
                        root_children_count_3[parent] = 1
                        root_active_children_3[parent] = 0
                if invitation_time <= roots[parent] + 28:
                    if parent in root_children_count_4:
                        root_children_count_4[parent] += 1
                    else:
                        root_children_count_4[parent] = 1
                        root_active_children_4[parent] = 0
                if is_leaf == False:
                    if activation_time <= roots[parent] + 7:
                        root_active_children_1[parent] = (root_active_children_1[parent] + 1) if parent in root_active_children_1 else 1
                    if activation_time <= roots[parent] + 14:
                        root_active_children_2[parent] = (root_active_children_2[parent] + 1) if parent in root_active_children_2 else 1
                    if activation_time <= roots[parent] + 21:
                        root_active_children_3[parent] = (root_active_children_3[parent] + 1) if parent in root_active_children_3 else 1
                    if activation_time <= roots[parent] + 28:
                        root_active_children_4[parent] = (root_active_children_4[parent] + 1) if parent in root_active_children_4 else 1
    
CLR_THRESHOLD = 500000

roots = {}
f = open(sys.argv[2], "r")
for line in f:
    element = line.split(',')
    root = int(element[0].strip())
#    'root', 'size', 'depth', 'width', 'first_day', 'last_day', 'burstiness'
    first_day = int(element[4].strip())
    roots[root] = first_day

count = 0
f = open(sys.argv[1], "r")
for line in f:
    element = line.split(' ')
    node_id = int(element[0].strip())
    born_time = int(element[1].strip())
    activation_time = int(element[2].strip())
    last_act_time = int(element[3].strip())
    last_seen_time = int(element[4].strip())
    is_leaf = bool(int(element[5].strip()))
    odeg = int((element[6].strip()))
    parent_list = element[7:len(element)]
    reg_children(node_id, parent_list, is_leaf, activation_time)
    count = count+1
    if (count % (CLR_THRESHOLD/10)) == 0:
        print count
f.close()

root_weekly_evolution_file = open(sys.argv[3]+"root_weekly_evolution.csv", "w")
for each_root in roots:
    try:
        root_weekly_evolution_file.write('%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' %(
                            each_root, root_sent_ARs_1[each_root], root_active_children_1[each_root], root_children_count_1[each_root],
                            root_sent_ARs_2[each_root], root_active_children_2[each_root], root_children_count_2[each_root],
                            root_sent_ARs_3[each_root], root_active_children_3[each_root], root_children_count_3[each_root],
                            root_sent_ARs_4[each_root], root_active_children_4[each_root], root_children_count_4[each_root]))
    except:
        continue
root_weekly_evolution_file.close()
