import sys, os
from sets import Set
import csv
import array
from bitarray import bitarray

MAX_USERS = 2**29 - 1
NO_PARENT = MAX_USERS + 1    

if __name__ == '__main__':
    is_seed = bitarray(MAX_USERS)
    is_seed.setall(False)
    is_leaf = bitarray(MAX_USERS)
    is_leaf.setall(True)
    
    potential_parents = []
    out_degree = array.array('L')
    
    born = array.array('l')    
    activation_time = array.array('l') # First sending time     
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
            activation_time.append(-1)
            is_seed[sender] = True
            out_degree.append(0)
        out_degree[sender] += 1 # Raising out degree even if multiple sending to same node ??
        if(out_degree[sender]) == 1:
            activation_time[sender] = timestamp
        
        if(recv > vertices_count - 1):
            vertices_count += 1
            is_leaf[sender] = False
            potential_parents.append(array.array('L'))
            potential_parents[recv].append(sender)
            born.append(timestamp)
            activation_time.append(-1)
            out_degree.append(0)
        else:
            if out_degree[recv] < 1:
                if sender not in potential_parents[recv]:
                    potential_parents[recv].append(sender)

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