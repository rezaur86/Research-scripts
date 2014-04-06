import sys, gc
import array
import operator
from sets import Set
from random import choice
import numpy as np

def reg_children(parent_list, is_leaf):
    l = len(parent_list)
    invitations = {}
    for i in range(l-1,-1,-1):
        u_p = parent_list[i].strip().split(',')
        if u_p[0] == '-1':
            return
        invitation_day = int(u_p[1])/86400
        if invitation_day in invitations:
            invitations[invitation_day] += 1
        else:
            invitations[invitation_day] = 1
        if u_p[0] not in children_count:
            children_count[u_p[0]] = 1
        else:
            children_count[u_p[0]] += 1
        if is_leaf == False:
            if u_p[0] not in active_children:
                active_children[u_p[0]] = 1
            else:
                active_children[u_p[0]] += 1
    first_invitation_day = min(invitations)
    last_invitation_day = max(invitations)
    temp_invitations = []
    for i in range(first_invitation_day, last_invitation_day+1): # start,end time for week = range(2059, 2117): #start,end time for day 14413*86400, 14819*86400
        temp_invitations.append(invitations[i] if i in invitations else 0)
    sd = np.sqrt(np.var(temp_invitations))
    avg = np.average(temp_invitations)
    burstiness = round(((sd - avg) / (sd + avg)), 3)
    if burstiness in invitation_burstiness_stat:
        invitation_burstiness_stat[burstiness] += 1
    else:
        invitation_burstiness_stat[burstiness] = 1

    
CLR_THRESHOLD = 500000
f = open(sys.argv[1], "r")
lifespan_bin_size = int(sys.argv[4])
lifespan_stat = {}
raw_outdeg = {}
parent_count = {}
critical_parent_count = {}
parent_count[0] = 0
critical_parent_count[0] = 0
children_count = {}
active_children = {}
invitation_burstiness_stat = {}
count = 0
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
    reg_children(parent_list, is_leaf)
    if parent_list[0].strip() == '-1':
        parent_count[0] += 1
        critical_parent_count[0] += 1    
    else:
        p_count = len(parent_list)
        if is_leaf == False:
            if p_count in parent_count:
                parent_count[p_count] += 1
            else:
                parent_count[p_count] = 1
        if p_count in critical_parent_count:
            critical_parent_count[p_count] += 1
        else:
            critical_parent_count[p_count] = 1    
    count = count+1
    if (count % (CLR_THRESHOLD/10)) == 0:
        print count
    if odeg in raw_outdeg:
        raw_outdeg[odeg] += 1
    else:
        raw_outdeg[odeg] = 1
    if is_leaf == False:
        lifespan = last_act_time - activation_time + 1
    else:
        lifespan = 0
        continue
    lifespan_bin = lifespan/lifespan_bin_size
    if(lifespan_bin in lifespan_stat):
        lifespan_stat[lifespan_bin] += 1
    else:
        lifespan_stat[lifespan_bin] = 1
f.close()

f = open(sys.argv[2], "r")
count = 0
indeg_before_act = {}
critical_indeg = {}
raw_indeg = {}
for line in f:
    element = line.split(',')
    node_id = int(element[0].strip())
    odeg = int(element[1].strip())
    indeg = int(element[2].strip())
    indeg_until_active = int(element[3].strip())
    act_time = int(element[4].strip())
    count = count+1
    if (count % (CLR_THRESHOLD/10)) == 0:
        print count
    if indeg in raw_indeg:
        raw_indeg[indeg] += 1
    else:
        raw_indeg[indeg] = 1
    if indeg_until_active == -1:
        if indeg in critical_indeg:
            critical_indeg[indeg] += 1
        else:
            critical_indeg[indeg] = 1    
        continue
    if indeg_until_active in indeg_before_act:
        indeg_before_act[indeg_until_active] += 1
    else:
        indeg_before_act[indeg_until_active] = 1
    if indeg_until_active in critical_indeg:
        critical_indeg[indeg_until_active] += 1
    else:
        critical_indeg[indeg_until_active] = 1    
f.close()

lifespan_stat_file = open(sys.argv[3]+"lifespan_stat.csv", "w")
raw_outdeg_stat_file = open(sys.argv[3]+"raw_outdeg_stat.csv", "w")
raw_indeg_stat_file = open(sys.argv[3]+"raw_indeg_stat.csv", "w")
parent_count_before_act_file = open(sys.argv[3]+"parent_count_before_act.csv", "w")
indeg_before_act_file = open(sys.argv[3]+"indeg_before_act.csv", "w")
act_proportion_count_file = open(sys.argv[3]+"act_proportion_count.csv", "w")
parent_proportion_file = open(sys.argv[3]+"parent_proportion.csv", "w")
invitation_burstiness_stat_file = open(sys.argv[3]+"invitation_burstiness_stat.csv", "w")

temp = sorted(lifespan_stat.iteritems(), key=operator.itemgetter(0), reverse=True)
for tuple in temp:
    lifespan_stat_file.write('%s,%s\n'%(tuple[0],tuple[1]))
temp = sorted(raw_outdeg.iteritems(), key=operator.itemgetter(0), reverse=True)
for tuple in temp:
    raw_outdeg_stat_file.write('%s,%s\n'%(tuple[0],tuple[1]))
temp = sorted(raw_indeg.iteritems(), key=operator.itemgetter(0), reverse=True)
for tuple in temp:
    raw_indeg_stat_file.write('%s,%s\n'%(tuple[0],tuple[1]))
temp = sorted(critical_parent_count.iteritems(), key=operator.itemgetter(0), reverse=True)
for tuple in temp:
    parent_count_before_act_file.write('%s,%s,%s\n'%(tuple[0], parent_count[tuple[0]] if tuple[0] in parent_count else 0, tuple[1]))
temp = sorted(critical_indeg.iteritems(), key=operator.itemgetter(0), reverse=True)
for tuple in temp:
    indeg_before_act_file.write('%s,%s,%s\n'%(tuple[0], indeg_before_act[tuple[0]] if tuple[0] in indeg_before_act else 0, tuple[1]))
temp = sorted(invitation_burstiness_stat.iteritems(), key=operator.itemgetter(0), reverse=True)
for tuple in temp:
    invitation_burstiness_stat_file.write('%s,%s\n'%(tuple[0],tuple[1]))

alpha_account = {}
for each_parent in children_count:
    alpha =  (1.0*active_children[each_parent] if each_parent in active_children else 0) / children_count[each_parent]
    rounded_alpha = round(100*alpha)
    if rounded_alpha not in alpha_account:
        alpha_account[rounded_alpha] = 1
    else:
        alpha_account[rounded_alpha] += 1
    parent_proportion_file.write('%s,%s\n'%(active_children[each_parent] if each_parent in active_children else 0,
                                            children_count[each_parent]))

temp = sorted(alpha_account.iteritems(), key=operator.itemgetter(0), reverse=True)
for tuple in temp:
    act_proportion_count_file.write('%s,%s\n'%(tuple[0],tuple[1]))

lifespan_stat_file.close()
raw_outdeg_stat_file.close()
raw_indeg_stat_file.close()
parent_count_before_act_file.close()
indeg_before_act_file.close()
act_proportion_count_file.close()
parent_proportion_file.close()
invitation_burstiness_stat_file.close()