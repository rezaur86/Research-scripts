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
    
    out_degree = array.array('I')
    potential_parents = []
    born_time = array.array('l') # Born by chosen parent or the earliest time at which received from the chosen parent
    activation_time = array.array('l')
    
    vertices_count = 0
    f = open(sys.argv[1], "r")
    for line in f:
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
                for (p,t) in potential_parents[recv]:
                    if sender != p:
                        potential_parents[recv].append((sender,timestamp))
                        break

    f = open(sys.argv[2], "w")
    for i in range(vertices_count):
        f.write('%s %s %s %s %s'%(i, born_time[i], activation_time[i], int(is_leaf[i]), out_degree[i])) #there is a difference between out_degree = 0 and leaf. Leaves are those who do not bring any new node to the graph
        if potential_parents[i] == NO_PARENT:
            f.write(' -1')
        else:
            for (p,t) in potential_parents[i]:
                f.write(' %s,%s'%(p,t))
        f.write('\n')
    f.close()

    out_degree = None
    potential_parents = None
    born_time = None
    activation_time = None
    
#Sorting
    count = 0
    f = open(sys.argv[2], "r")
    f_sorted = open('sorted_'+sys.argv[2], "w")
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
