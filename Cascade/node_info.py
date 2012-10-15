import sys, os
from sets import Set
import csv
import array
from bitarray import bitarray
import operator

MAX_USERS = 2**29 - 1
NO_PARENT = MAX_USERS + 1
NEVER = 2**31 - 1

if __name__ == '__main__':
    is_leaf = bitarray(MAX_USERS)
    is_leaf.setall(True)
    
    out_degree = [] #array.array('I')
    potential_parents = []
    born_time = [] #array.array('l')
    activation_time = [] #array.array('l')
    
    f = open(sys.argv[1], "r")
    user_last_seen_act = array.array('l')
    activity_line = 0
    for line in f:
        activity_line += 1
        splits = line.split()
        sender = long(splits[0].strip())
        recv = long(splits[1].strip())
        timestamp = long(splits[2].strip())
        if len(user_last_seen_act) > sender:
            user_last_seen_act[sender] = max(user_last_seen_act[sender], activity_line)
        else:
            user_last_seen_act.append(activity_line)          
        if len(user_last_seen_act) > recv:
            user_last_seen_act[recv] = max(user_last_seen_act[recv], activity_line)
        else:
            user_last_seen_act.append(activity_line)
    f.close()
    total_line = activity_line

    f = open(sys.argv[1], "r")
    o_f = open(sys.argv[2]+'.txt', "w")
    vertices_count = 0
    users_done = Set()
    activity_line = 0
    for line in f:
        activity_line += 1
        splits = line.split()
        sender = long(splits[0].strip())
        recv = long(splits[1].strip())
        timestamp = long(splits[2].strip())
        
        if(sender > vertices_count - 1):
            vertices_count += 1
            potential_parents.append(NO_PARENT)
            born_time.append(timestamp)
            activation_time.append(timestamp)
            out_degree.append(1)
        else:
            out_degree[sender] += 1 # Raising out degree even if multiple sending to same node ??
            if(out_degree[sender]) == 1:
                activation_time[sender] = timestamp # Activation time is the earliest sending time
        
        is_leaf[sender] = False
        
        if(recv > vertices_count - 1):
            vertices_count += 1
            potential_parents.append([])
            potential_parents[recv].append((sender,timestamp))
            born_time.append(timestamp)
            activation_time.append(NEVER) # Not yet Activated
            out_degree.append(0)
        else:
            if out_degree[recv] < 1:
                sender_is_already_parent = False
                for (p,t) in potential_parents[recv]:
                    if sender == p:
                        sender_is_already_parent = True
                        break
                if sender_is_already_parent == False:
                    potential_parents[recv].append((sender,timestamp))
        
        if sender == recv:
            print sender
        if user_last_seen_act[sender]==activity_line:
            users_done.add(sender)
        if user_last_seen_act[recv]==activity_line:
            users_done.add(recv)
        if len(users_done) >= 2 or activity_line==total_line:
            for i in users_done:
                o_f.write('%s %s %s %s %s'%(i, born_time[i], activation_time[i], int(is_leaf[i]), out_degree[i]))
                born_time[i] = None
                activation_time[i] = None
                out_degree[i] = None
                if potential_parents[i] == NO_PARENT:
                    o_f.write(' -1')
                else:
                    for (p,t) in potential_parents[i]:
                        o_f.write(' %s,%s'%(p,t))
                potential_parents[i] = None
                o_f.write('\n')
            users_done = Set()
    f.close()
    o_f.close()
    
    out_degree = None
    potential_parents = None
    born_time = None
    activation_time = None
    
#Sorting
    count = 0
    f = open(sys.argv[2]+'.txt', "r")
    f_sorted = open(sys.argv[2]+'_sorted.txt', "w")
    all_info = []
    for line in f:
        element = line.split(' ')
        element[2] = long(element[2].strip())
        all_info.append(element)
        count = count+1
        if (count % 10000) == 0:
            print 'still reading at %s'%count
    sorted_all_info = sorted(all_info, key = operator.itemgetter(2))
    for i in range(count):
        sorted_all_info[i][2] = str(sorted_all_info[i][2])
        f_sorted.writelines(' '.join(sorted_all_info[i]))
    f.close()
    f_sorted.close()
