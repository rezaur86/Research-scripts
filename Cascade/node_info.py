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
    
    parent_born = array.array('L')    
    vertices_count = 0
    f = open(sys.argv[1], "r")
    for line in f:
        splits = line.split()
        sender = long(splits[0].strip())
        recv = long(splits[1].strip())
        timestamp = long(splits[2].strip())
        
        if(sender > vertices_count - 1):
            vertices_count += 1
            potential_parents.append(Set([]))
            parent_born.append(NO_PARENT)
            parent_born.append(timestamp)
            is_seed[sender] = True
            out_degree.append(0)
        out_degree[sender] += 1 # Raising out degree even if multiple sending to same node ??
        
        if(recv > vertices_count - 1):
            vertices_count += 1
            is_leaf[sender] = False
            potential_parents.append(Set([]))
            potential_parents[recv].add(sender)
            parent_born.append(sender)
            parent_born.append(timestamp)
            out_degree.append(0)
        else:
            if out_degree[recv] < 1:
                potential_parents[recv].add(sender)

    f = open(sys.argv[2], "w")
    for i in range(vertices_count):
        f.write('%s %s %s %s\n'%(i, -1 if parent_born[2*i+0] == NO_PARENT else parent_born[2*i+0] ,parent_born[2*i+1],int(is_leaf[i])))
    f.close()

    f = open(sys.argv[3], "w")
    for i in range(vertices_count):
        f.write('%s'%out_degree[i])
        for j in potential_parents[i]:
            f.write(' %s'%j)
        f.write('\n')
    f.close()
