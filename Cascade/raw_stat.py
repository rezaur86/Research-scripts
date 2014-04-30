import sys, gc
import array
import operator
from sets import Set
from random import choice
import numpy as np

NEVER = 2**31 - 1

def reg_children(parent_list, is_leaf):
    l = len(parent_list)
    invitations = {}
    for i in range(l-1,-1,-1):
        u_p = parent_list[i].strip().split(',')
        if u_p[0] == '-1':
            return
        invitation_hour = int(u_p[1])/3600 #Hourly
        if invitation_hour in invitations:
            invitations[invitation_hour] += 1
        else:
            invitations[invitation_hour] = 1
        parent = int(u_p[0])
        if parent not in children_count:
            children_count[parent] = 1
        else:
            children_count[parent] += 1
        if is_leaf == False:
            if parent not in active_children:
                active_children[parent] = 1
            else:
                active_children[parent] += 1
    first_invitation_hour = min(invitations)
    last_invitation_hour = max(invitations)
    elapsed_hour = last_invitation_hour - first_invitation_hour + 1
    if elapsed_hour in invitation_elapsed_time:
        invitation_elapsed_time[elapsed_hour] += 1
    else:
        invitation_elapsed_time[elapsed_hour] = 1
    if elapsed_hour == 1:
        return
    temp_invitations = []
    for i in range(first_invitation_hour, last_invitation_hour+1):
        temp_invitations.append(invitations[i] if i in invitations else 0)
    sd = np.sqrt(np.var(temp_invitations))
    avg = np.average(temp_invitations)
    burstiness = round(((sd - avg) / (sd + avg)), 3)
    if burstiness in invitation_burstiness_stat:
        invitation_burstiness_stat[burstiness].append(elapsed_hour)
    else:
        invitation_burstiness_stat[burstiness] = [elapsed_hour]

    
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
invitation_elapsed_time = {}
delay_recv_AR_parent_stat = {}
delay_recv_AR_stat = {}
act_lifespan_sent_AR_stat = {}
act_lifespan_sent_AR_children_stat = {}
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
        adoption_delay = (activation_time - born_time) if is_leaf == False else NEVER
        delay_bin = adoption_delay/lifespan_bin_size
        if(delay_bin in delay_recv_AR_parent_stat):
            delay_recv_AR_parent_stat[delay_bin].append(p_count)
        else:
            delay_recv_AR_parent_stat[delay_bin] = [p_count]
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
raw_sent_ARs = {}
for line in f:
    element = line.split(',')
    node_id = int(element[0].strip())
    odeg = int(element[1].strip())
    indeg = int(element[2].strip())
    indeg_until_active = int(element[3].strip())
    act_lifespan = int(element[4].strip())
    adoption_delay = int(element[5].strip())
    sent_ARs = int(element[6].strip())
    delay_bin = ((adoption_delay - 1) if adoption_delay > 0 else NEVER)/lifespan_bin_size
    act_lifespan_bin = (act_lifespan if act_lifespan > 0 else NEVER)/lifespan_bin_size
    count = count+1
    if (count % (CLR_THRESHOLD/10)) == 0:
        print count
    if indeg in raw_indeg:
        raw_indeg[indeg] += 1
    else:
        raw_indeg[indeg] = 1
    if sent_ARs in raw_sent_ARs:
        raw_sent_ARs[sent_ARs] += 1
    else:
        raw_sent_ARs[sent_ARs] = 1
    if(act_lifespan_bin in act_lifespan_sent_AR_stat):
        act_lifespan_sent_AR_stat[act_lifespan_bin].append(sent_ARs)
        act_lifespan_sent_AR_children_stat[act_lifespan_bin].append(children_count[node_id] if node_id in children_count else 0)
    else:
        act_lifespan_sent_AR_stat[act_lifespan_bin] = [sent_ARs]
        act_lifespan_sent_AR_children_stat[act_lifespan_bin] = [children_count[node_id] if node_id in children_count else 0]
    if indeg_until_active == -1:
        if indeg in critical_indeg:
            critical_indeg[indeg] += 1
        else:
            critical_indeg[indeg] = 1    
        if(delay_bin in delay_recv_AR_stat):
            delay_recv_AR_stat[delay_bin].append(indeg)
        else:
            delay_recv_AR_stat[delay_bin] = [indeg]
        continue
    if indeg_until_active in indeg_before_act:
        indeg_before_act[indeg_until_active] += 1
    else:
        indeg_before_act[indeg_until_active] = 1
    if indeg_until_active in critical_indeg:
        critical_indeg[indeg_until_active] += 1
    else:
        critical_indeg[indeg_until_active] = 1    
    if(delay_bin in delay_recv_AR_stat):
        delay_recv_AR_stat[delay_bin].append(indeg_until_active)
    else:
        delay_recv_AR_stat[delay_bin] = [indeg_until_active]
f.close()

lifespan_stat_file = open(sys.argv[3]+"lifespan_stat.csv", "w")
raw_outdeg_stat_file = open(sys.argv[3]+"raw_outdeg_stat.csv", "w")
raw_indeg_stat_file = open(sys.argv[3]+"raw_indeg_stat.csv", "w")
raw_sent_AR_stat_file = open(sys.argv[3]+"raw_sent_AR_stat.csv", "w")
raw_sent_AR_children_stat_file = open(sys.argv[3]+"raw_sent_AR_children_stat.csv", "w")
raw_rec_AR_stat_file = open(sys.argv[3]+"raw_rec_AR_stat.csv", "w")
raw_rec_AR_parent_stat_file = open(sys.argv[3]+"raw_rec_AR_parent_stat.csv", "w")
parent_count_before_act_file = open(sys.argv[3]+"parent_count_before_act.csv", "w")
indeg_before_act_file = open(sys.argv[3]+"indeg_before_act.csv", "w")
act_proportion_count_file = open(sys.argv[3]+"act_proportion_count.csv", "w")
parent_children_act_file = open(sys.argv[3]+"parent_children_act.csv", "w")
parent_proportion_file = open(sys.argv[3]+"parent_proportion.csv", "w")
invitation_burstiness_stat_file = open(sys.argv[3]+"invitation_burstiness_stat.csv", "w")
invitation_elapsed_time_stat_file = open(sys.argv[3]+"invitation_elapsed_time_stat.csv", "w")

temp = sorted(lifespan_stat.iteritems(), key=operator.itemgetter(0), reverse=True)
for tuple in temp:
    lifespan_stat_file.write('%s,%s\n'%(tuple[0],tuple[1]))
temp = sorted(raw_outdeg.iteritems(), key=operator.itemgetter(0), reverse=True)
for tuple in temp:
    raw_outdeg_stat_file.write('%s,%s\n'%(tuple[0],tuple[1]))
temp = sorted(raw_sent_ARs.iteritems(), key=operator.itemgetter(0), reverse=True)
for tuple in temp:
    raw_sent_AR_stat_file.write('%s,%s\n'%(tuple[0],tuple[1]))
temp = sorted(raw_indeg.iteritems(), key=operator.itemgetter(0), reverse=True)
for tuple in temp:
    raw_indeg_stat_file.write('%s,%s\n'%(tuple[0],tuple[1]))
temp = sorted(critical_parent_count.iteritems(), key=operator.itemgetter(0), reverse=True)
for tuple in temp:
    parent_count_before_act_file.write('%s,%s,%s\n'%(tuple[0], parent_count[tuple[0]] if tuple[0] in parent_count else 0, tuple[1]))
    raw_rec_AR_parent_stat_file.write('%s,%s\n'%(tuple[0], tuple[1]))
temp = sorted(critical_indeg.iteritems(), key=operator.itemgetter(0), reverse=True)
for tuple in temp:
    indeg_before_act_file.write('%s,%s,%s\n'%(tuple[0], indeg_before_act[tuple[0]] if tuple[0] in indeg_before_act else 0, tuple[1]))
    raw_rec_AR_stat_file.write('%s,%s\n'%(tuple[0], tuple[1]))
temp = sorted(invitation_burstiness_stat.iteritems(), key=operator.itemgetter(0), reverse=True)
for tuple in temp:
    invitation_burstiness_stat_file.write('%s,%s,%s,%s\n'%(tuple[0], len(tuple[1]),
                                                           round(np.average(tuple[1]),3),
                                                           round(np.median(tuple[1]),3)))
temp = sorted(invitation_elapsed_time.iteritems(), key=operator.itemgetter(0), reverse=True)
for tuple in temp:
    invitation_elapsed_time_stat_file.write('%s,%s\n'%(tuple[0],tuple[1]))

alpha_account = {}
raw_sent_AR_children = {}
raw_sent_AR_children[0] = raw_sent_ARs[0]
for each_parent in children_count:
    alpha =  (1.0*active_children[each_parent] if each_parent in active_children else 0) / children_count[each_parent]
    rounded_alpha = round(100*alpha)
    if rounded_alpha not in alpha_account:
        alpha_account[rounded_alpha] = 1
    else:
        alpha_account[rounded_alpha] += 1
    parent_children_act_file.write('%s,%s\n'%(active_children[each_parent] if each_parent in active_children else 0,
                                            children_count[each_parent]))
    parent_proportion_file.write('%s,%s\n'%(each_parent, alpha))
    if children_count[each_parent] in raw_sent_AR_children:
        raw_sent_AR_children[children_count[each_parent]] += 1
    else:
        raw_sent_AR_children[children_count[each_parent]] = 1

temp = sorted(alpha_account.iteritems(), key=operator.itemgetter(0), reverse=True)
for tuple in temp:
    act_proportion_count_file.write('%s,%s\n'%(tuple[0],tuple[1]))
temp = sorted(raw_sent_AR_children.iteritems(), key=operator.itemgetter(0), reverse=True)
for tuple in temp:
    raw_sent_AR_children_stat_file.write('%s,%s\n'%(tuple[0],tuple[1]))

delay_recv_AR_stat_file = open(sys.argv[3]+"delay_recv_AR_stat.csv", "w")
for time in delay_recv_AR_stat:
    delay_recv_AR_stat_file.write('%s,%s,%s,%s\n'%(time, len(delay_recv_AR_stat[time]),
                                           round(np.average(delay_recv_AR_stat[time]), 3),
                                           round(np.average(delay_recv_AR_parent_stat[time]),3)))
delay_recv_AR_stat_file.close()

act_lifespan_sent_AR_stat_file = open(sys.argv[3]+"act_lifespan_sent_AR_stat.csv", "w")
for time in act_lifespan_sent_AR_stat:
    act_lifespan_sent_AR_stat_file.write('%s,%s,%s,%s\n'%(time, len(act_lifespan_sent_AR_stat[time]),
                                            round(np.average(act_lifespan_sent_AR_stat[time]), 3),
                                            round(np.average(act_lifespan_sent_AR_children_stat[time]), 3)))
act_lifespan_sent_AR_stat_file.close()

lifespan_stat_file.close()
raw_outdeg_stat_file.close()
raw_indeg_stat_file.close()
raw_sent_AR_stat_file.close()
raw_sent_AR_children_stat_file.close()
raw_rec_AR_stat_file.close()
raw_rec_AR_parent_stat_file.close()
parent_count_before_act_file.close()
indeg_before_act_file.close()
act_proportion_count_file.close()
parent_children_act_file.close()
parent_proportion_file.close()
invitation_burstiness_stat_file.close()
invitation_elapsed_time_stat_file.close()