import sys, os
import copy
from sets import Set
import csv
import array
from bitarray import bitarray
import operator
import resource

MAX_USERS = 2**29 - 1
NO_PARENT = MAX_USERS + 1
NEVER = 2**31 - 1
IS_LEAF = 1
IS_NOT_LEAF = 0

class Node:
    def __init__(self, born_time, activation_time, user_last_act_time,
                 out_degree, sent_AR,
                 in_degree, in_degree_until_active,
                 is_leaf, parent_list):
        self.basic = array.array('i',[born_time, activation_time, user_last_act_time,
                                      out_degree, sent_AR,
                                      in_degree, in_degree_until_active, is_leaf])
        self.potential_parents = parent_list

    def getBornTime(self):
        return self.basic[0]
    def getActivationTime(self):
        return self.basic[1]
    def getLastActivityTime(self):
        return self.basic[2]
    def getOutDegree(self):
        return self.basic[3]
    def getSentAR(self):
        return self.basic[4]
    def getInDegree(self):
        return self.basic[5]
    def getInDegreeUntilActive(self):
        return self.basic[6]
    def getLeaf(self):
        return self.basic[7]
    def getParents(self):
        return self.potential_parents

    def setActivationTime(self, time):
        self.basic[1] = time
    def setLastActivityTime(self, time):
        self.basic[2] = time
    def setOutDegree(self, odeg):
        self.basic[3] = odeg
    def setSentAR(self, sent_ar):
        self.basic[4] = sent_ar
    def setInDegree(self, indeg):
        self.basic[5] = indeg
    def setInDegreeUntilActive(self, indeg):
        self.basic[6] = indeg
    def setLeaf(self, is_leaf):
        self.basic[7] = is_leaf
    def setParents(self, parent_list):
        self.potential_parents = parent_list
    def insertParent(self, a_parent):
        self.potential_parents.append(a_parent)

if __name__ == '__main__':
    nodes = {}
    
    file_list = sorted(os.listdir(sys.argv[1]))
    user_last_seen_time = array.array('l')
    activity_line = 0
    for each_file in file_list:
        f = open(sys.argv[1]+'/'+each_file, "r")
        for line in f:
            activity_line += 1
            splits = line.split()
            sender = long(splits[0].strip())
            recv = long(splits[1].strip())
            timestamp = long(splits[2].strip())
            if sender != -1:
                if len(user_last_seen_time) > sender:
                    user_last_seen_time[sender] = max(user_last_seen_time[sender], activity_line)
                else:
                    user_last_seen_time.append(activity_line)
            if recv != -1:
                if len(user_last_seen_time) > recv:
                    user_last_seen_time[recv] = max(user_last_seen_time[recv], activity_line)
                else:
                    user_last_seen_time.append(activity_line)
        f.close()
        print each_file
    total_line = activity_line
    print 'Total line read: ', total_line

    o_f = open(sys.argv[2]+'.txt', "w")
    node_basic_f = open(sys.argv[2]+'_basic.txt', "w")
    vertices_count = 0
    users_done = []
    activity_line = 0
    for each_file in file_list:
        f = open(sys.argv[1]+'/'+each_file, "r")
        for line in f:
            activity_line += 1
            splits = line.split()
            sender = long(splits[0].strip())
            recv = long(splits[1].strip())
            timestamp = long(splits[2].strip())
            hid = int(splits[3].strip())
            if sender != -1 and recv != -1:
                if(sender > vertices_count - 1):
                    vertices_count += 1
                    new_node = Node(timestamp, timestamp, timestamp, 1, 0, 0, 0, IS_NOT_LEAF, NO_PARENT)
                    nodes[sender] = new_node
                else:
                    nodes[sender].setOutDegree(nodes[sender].getOutDegree() + 1)
                    # Raising out degree even if multiple sending to same node ??
                    if (nodes[sender].getOutDegree()) == 1:
                        nodes[sender].setInDegreeUntilActive(nodes[sender].getInDegree())
                        nodes[sender].setActivationTime(timestamp) # Activation time is the earliest sending time
                        nodes[sender].setLastActivityTime(timestamp)
                    else:
                        nodes[sender].setLastActivityTime(max(nodes[sender].getLastActivityTime(), timestamp))
                nodes[sender].setLeaf(IS_NOT_LEAF)
                if(recv > vertices_count - 1):
                    vertices_count += 1
                    new_node = Node(timestamp, NEVER, NEVER, 0, 0, 1, -1, IS_LEAF, [(sender,timestamp,hid)])
                    nodes[recv] = new_node
                    nodes[sender].setSentAR(nodes[sender].getSentAR() + 1)
                else:
                    nodes[recv].setInDegree(nodes[recv].getInDegree() + 1)
                    # Raising in degree even if multiple receiving at same node ??
                    if nodes[recv].getOutDegree() < 1:
                        if nodes[recv].getParents() == NO_PARENT:
                            nodes[recv].setParents([])
                        nodes[sender].setSentAR(nodes[sender].getSentAR() + 1)
                        nodes[recv].insertParent((sender,timestamp,hid))
            
                if user_last_seen_time[sender]==activity_line:
                    users_done.append(sender)
                    user_last_seen_time[sender] = timestamp #Replacing the last seen line number with last seen time
                if sender!=recv and user_last_seen_time[recv]==activity_line:
                    users_done.append(recv)
                    user_last_seen_time[recv] = timestamp #Replacing the last seen line number with last seen time
            
            if recv == -1:
                if(sender > vertices_count - 1):
                    vertices_count += 1
                    new_node = Node(timestamp, NEVER, NEVER, 0, 0, 0, 0, IS_LEAF, NO_PARENT)
                    nodes[sender] = new_node
            if sender == -1:
                if(recv > vertices_count - 1):
                    vertices_count += 1
                    new_node = Node(timestamp, NEVER, NEVER, 0, 0, 1, -1, IS_LEAF, NO_PARENT)
                    nodes[sender] = new_node

            if len(users_done) >= 10000 or activity_line==total_line:
                for i in users_done:
                    o_f.write('%s %s %s %s %s %s %s'%(
                        i, nodes[i].getBornTime(),
                        nodes[i].getActivationTime() if nodes[i].getLeaf() == IS_NOT_LEAF else (user_last_seen_time[i]+1),
                        nodes[i].getLastActivityTime() if nodes[i].getLeaf() == IS_NOT_LEAF else (user_last_seen_time[i]+1),
                        user_last_seen_time[i], nodes[i].getLeaf(), nodes[i].getOutDegree()))
                    node_basic_f.write('%s,%s,%s,%s,%s,%s,%s'%(
                        i, nodes[i].getOutDegree(), nodes[i].getInDegree(), nodes[i].getInDegreeUntilActive(),
                        (nodes[i].getLastActivityTime() - nodes[i].getActivationTime() + 1) if nodes[i].getLeaf() == IS_NOT_LEAF else 0,
                        (nodes[i].getActivationTime() - nodes[i].getBornTime() + 1) if nodes[i].getLeaf() == IS_NOT_LEAF else 0,
                        nodes[i].getSentAR()))
                    parent_list = nodes[i].getParents()
                    if parent_list == NO_PARENT:
                        o_f.write(' -1')
                    else:
                        for (p,t,h) in parent_list:
                            o_f.write(' %s,%s,%s'%(p,t,h))
                    del nodes[i]
                    o_f.write('\n')
                    node_basic_f.write('\n')
                users_done = []
#                 print('*******************************************************')
#                 print(resource.getrusage(resource.RUSAGE_SELF).ru_maxrss / 1000)
#                 print('*******************************************************')
            if activity_line%10000 == 0:
                print activity_line*100/total_line, '% of', total_line
        f.close()
    o_f.close()
    node_basic_f.close()

    nodes = None
#Sorting
    count = 0
    f = open(sys.argv[2]+'.txt', "r")
    f_sorted = open(sys.argv[2]+'_sorted.txt', "w")
    act_time = []#array.array('l')
    for line in f:
        element = line.split(' ')
        element[0] = int(element[0].strip())
        element[2] = int(element[2].strip())
        act_time.append((element[2],element[0]))
        count = count+1
        if (count % 10000) == 0:
            print 'still reading at %s'%count
    f.close()
    sorted_line_idx = sorted(range(len(act_time)), key = act_time.__getitem__)
    act_time = None
    f = open(sys.argv[2]+'.txt', "r")
    lines=f.readlines()
    f.close()

    for i in sorted_line_idx:
        f_sorted.writelines(lines[i])
    f_sorted.close()
