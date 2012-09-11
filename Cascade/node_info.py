import sys, os
from sets import Set
import csv
import array
from bitarray import bitarray
import operator

MAX_USERS = 2**29 - 1
NO_PARENT = MAX_USERS + 1    

if __name__ == '__main__':
    is_leaf = bitarray(MAX_USERS)
    is_leaf.setall(True)
    
    potential_parents = []
    out_degree = array.array('L')
    
    born = array.array('l')    
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
            born.append(timestamp)
            activation_time.append(0)
            out_degree.append(0)

        out_degree[sender] += 1 # Raising out degree even if multiple sending to same node ??
        if(out_degree[sender]) == 1:
            activation_time[sender] = timestamp #Activation time is the latest receiving time if the node never sends or earliest sending time
        
        if(recv > vertices_count - 1):
            vertices_count += 1
            is_leaf[sender] = False
            potential_parents.append(array.array('L'))
            potential_parents[recv].append(sender)
            born.append(timestamp)
            activation_time.append(timestamp) #Activation time is the latest receiving time if the node never sends or earliest sending time
            out_degree.append(0)
        else:
            if out_degree[recv] < 1:
                if sender not in potential_parents[recv]:
                    potential_parents[recv].append(sender)
                    activation_time[recv] = timestamp #Activation time is the latest receiving time if the node never sends or earliest sending time

    f = open(sys.argv[2], "w")
    for i in range(vertices_count):
        f.write('%s %s %s %s %s'%(i, born[i], activation_time[i], int(is_leaf[i]), out_degree[i])) #there is a difference between out_degree = 0 and leaf. Leaves are those who do not bring any new node to the graph
        if potential_parents[i] == NO_PARENT:
            f.write(' -1')
        else:
            for j in potential_parents[i]:
                f.write(' %s'%j)
        f.write('\n')
    f.close()
    
#Sorting
    count = 0
    f = open(sys.argv[2], "r")
    f_sorted = open('sorted_'+sys.argv[2], "w")
    all_info = []
    for line in f:
        element = line.split(' ')
        node_id = long(element[0].strip())
        born_time = int(element[1].strip())
        activation_time = int(element[2].strip())
        is_leaf = int(element[3].strip())
        odeg = int((element[4].strip()))
        potential_parents = array.array('l')
        for p_index in range(5,len(element)):
            potential_parents.append(long(element[p_index].strip()))
        all_info.append(element)
        count = count+1
        if (count % 10000) == 0:
            print 'still reading at %s'%count
    sorted_all_info = sorted(all_info, key = operator.itemgetter(2))
    for i in range(vertices_count):
        f_sorted.writelines(' '.join(sorted_all_info[i]))
    f.close()
    f_sorted.close()
