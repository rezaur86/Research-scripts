import sys, os
from sets import Set
import csv
import array
from bitarray import bitarray
if __name__ == '__main__':
    activities = array.array('L')
    seen_users = bitarray(2**29-1)
    seen_users.setall(False)
    f = open(sys.argv[1], "r")
    activities_count = 0
    parent_born = {}
    leaf = bitarray(2**29-1)
    leaf.setall(True)
    
    for line in f:
        splits = line.split()
        sender = long(splits[0].strip())
        recv = long(splits[1].strip())
        timestamp = long(splits[2].strip())
        if ((sender < recv) and (seen_users[recv] == False)):
            activities.append(sender)
            activities.append(recv)
            activities.append(timestamp)
            activities_count += 1
            if (seen_users[sender] == False):
                parent_born[sender] = (0,timestamp)
            parent_born[recv] = (sender,timestamp)
            leaf[sender] = False

            seen_users[sender] = True
            seen_users[recv] = True
            
    f.close()
    f = open(sys.argv[2], "w")
    print activities_count
    for i in range(activities_count):
        f.write('%s %s %s\n'%(activities[3*i+0],activities[3*i+1],activities[3*i+2]))
    f.close()
    f = open(sys.argv[3], "w")
    for node in parent_born:
        f.write('%s %s %s %s\n'%(node,parent_born[node][0],parent_born[node][1],int(leaf[node])))
    f.close()
