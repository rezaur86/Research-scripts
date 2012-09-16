import sys
import array
import operator
from random import choice

PARENT_TYPE_FIRST_PARENT = 0
PARENT_TYPE_HIGHEST_ODEG = 1
PARENT_TYPE_LAST_PARENT = 2
PARENT_TYPE_RANDOM_PARENT = 3

TOP_N = 100

CLR_THRESHOLD = 500000

MAX_USERS = 2**29 - 1
NO_PARENT = MAX_USERS + 1

class Node:
    def __init__(self):
        self.size = [1]*len(timeThrsh)
        self.depth = [0]*len(timeThrsh)
    def setBornTime(self, bornTime):
        self.bornTime = bornTime
    def setActTime(self, actTime):
        self.actTime = actTime
    def setPotentialParent(self, p_list):
        self.parent_list = p_list
    def setOutDeg(self, odeg):
        self.odeg = odeg
    def print_node(self):
        print self.size,self.depth

def parent_chooser (parent_list, choice_type):
    potential_parents = []
    
    if choice_type == PARENT_TYPE_FIRST_PARENT:
        first_parent = parent_list[0].strip().split(',')
        if first_parent[0] == '-1':
            return potential_parents
        potential_parents.append((long(first_parent[0]), long(first_parent[1])))
        
    if choice_type == PARENT_TYPE_LAST_PARENT:
        last_parent = parent_list[len(parent_list)-1].strip().split(',')
        if last_parent[0] == '-1':
            return potential_parents
        potential_parents.append((long(last_parent[0]), long(last_parent[1])))
        
    if choice_type == PARENT_TYPE_RANDOM_PARENT:
        random_parent = choice(parent_list).strip().split(',')
        if random_parent[0] == '-1':
            return potential_parents
        potential_parents.append((long(random_parent[0]), long(random_parent[1])))

    if choice_type == PARENT_TYPE_HIGHEST_ODEG:
        chosen_pid_odeg = -1
        chosen_pid = -1
        chosen_receiving_time = -1
        for element in parent_list:
            a_parent = element.strip().split(',')
            if a_parent == '-1':
                return potential_parents
            else:
                pID = long(a_parent[0])
                if pID in graph:
                    if graph[pID].odeg > chosen_pid_odeg :
                        chosen_pid = pID
                        chosen_pid_odeg = graph[pID].odeg
                        chosen_receiving_time = long(a_parent[1])
        if chosen_pid != -1:
            potential_parents.append((chosen_pid, chosen_receiving_time))

    return potential_parents

def process(child, end_time, thrsh_index):
    for (pID,receiving_time) in child.parent_list:
        if end_time == -1:
            end_time = receiving_time
        if pID not in graph:
            return
#        if (child.bornTime-graph[pID].bornTime) <= timeThrsh[thrsh_index]:
        if (end_time-graph[pID].actTime) <= timeThrsh[thrsh_index]:
            graph[pID].size[thrsh_index] += 1
            if graph[pID].depth[thrsh_index] <= child.depth[thrsh_index]:
                graph[pID].depth[thrsh_index] = child.depth[thrsh_index]+1
            process(graph[pID], end_time, thrsh_index)
        else:
            end_time = -1 # For considering next parent of this child, a provision if we consider multiple parents
    
def clearHashTable(current_node):
    global result_size
    global result_depth
    record = []
    for node in graph:
        if current_node.bornTime-graph[node].bornTime > max(timeThrsh):
            record.append(node)
    for node in record:
        for i in range(len(timeThrsh)):    
            if graph[node].size[i] in result_size[i]:
                result_size[i][graph[node].size[i]] += 1
            else:
                result_size[i][graph[node].size[i]] = 1
            if graph[node].depth[i] in result_depth[i]:
                result_depth[i][graph[node].depth[i]] += 1
            else:
                result_depth[i][graph[node].depth[i]] = 1
        del graph[node]

f = open(sys.argv[1], "r")
timeThrsh = [] #[86400,172800,259200,345600,432000,518400,604800,691200,777600,864000]
graph = {}
result_size = []
result_depth = []
for a_thrsh in sys.argv[2].split(','):
    timeThrsh.append(int(a_thrsh.strip()))
    result_size.append({})
    result_depth.append({})
size_file = open(sys.argv[3]+"size.csv", "w")
depth_file = open(sys.argv[3]+"depth.csv", "w")
top_n_size_file = open(sys.argv[3]+"top_size.csv", "w")
top_n_depth_file = open(sys.argv[3]+"top_depth.csv", "w")

parent_type = int(raw_input(
'''PARENT_TYPE_FIRST_PARENT = 0
PARENT_TYPE_HIGHEST_ODEG = 1
PARENT_TYPE_LAST_PARENT = 2
PARENT_TYPE_RANDOM_PARENT = 3
Your choice? '''))
count = 0
for line in f:
    element = line.split(' ')
    node_id = long(element[0].strip())
#    born_time = int(element[1].strip())
    activation_time = int(element[2].strip())
    is_leaf = bool(int(element[3].strip()))
    odeg = int((element[4].strip()))
    newNode = Node()
#    newNode.setBornTime(born_time)
    newNode.setActTime(activation_time)
    newNode.setPotentialParent(parent_chooser(element[5:len(element)],parent_type))
    newNode.setOutDeg(odeg)
    if is_leaf == False:
        graph[node_id] = newNode
    for i in range(len(timeThrsh)):
        process(newNode, -1, i)
    count = count+1
    if (count % 1000) == 0:
        print count
#    if (count % CLR_THRESHOLD) == 0:
#        print "Clearing"
#        clearHashTable(newNode)
#        print graph
                        
f.close()

for node in graph:
    for i in range(len(timeThrsh)):    
        if graph[node].size[i] in result_size[i]:
            result_size[i][graph[node].size[i]] += 1
        else:
            result_size[i][graph[node].size[i]] = 1
        if graph[node].depth[i] in result_depth[i]:
            result_depth[i][graph[node].depth[i]] += 1
        else:
            result_depth[i][graph[node].depth[i]] = 1
print result_size
print result_depth

top_n_sizes = []
top_n_depths = []
for i in range(len(timeThrsh)):
    temp = sorted(result_size[i].iteritems(), key=operator.itemgetter(0), reverse=True)
    top_n_sizes.append((temp[0:TOP_N]))
    for tuple in temp:
        size_file.write('%s,%s,%s\n'%(tuple[0],tuple[1],timeThrsh[i]))
size_file.close()
for i in range(len(timeThrsh)):
    temp = sorted(result_depth[i].iteritems(), key=operator.itemgetter(0), reverse=True)
    top_n_depths.append((temp[0:TOP_N]))
    for tuple in temp:
        depth_file.write('%s,%s,%s\n'%(tuple[0],tuple[1],timeThrsh[i]))
depth_file.close()

top_n_size_users = []
top_n_depth_users = []
for i in range(len(timeThrsh)):
    top_n_size_users.append({})
    top_n_depth_users.append({})
    for j in range(min(TOP_N,len(result_size[i]))):
        top_n_size_users[i][top_n_sizes[i][j][0]] = []
    for j in range(min(TOP_N,len(result_depth[i]))):
        top_n_depth_users[i][top_n_depths[i][j][0]] = []

for node_id in graph:
    for i in range(len(timeThrsh)):
        if graph[node_id].size[i] in top_n_size_users[i]:
            top_n_size_users[i][graph[node_id].size[i]].append(node_id)
        if graph[node_id].depth[i] in top_n_depth_users[i]:
            top_n_depth_users[i][graph[node_id].depth[i]].append(node_id)
print top_n_size_users
print top_n_depth_users

for i in range(len(timeThrsh)):
    temp = sorted(top_n_size_users[i].iteritems(), key=operator.itemgetter(0), reverse=True)
    for tuple in temp:
        size = tuple[0]
        users = tuple[1]
        for each_user in users:
            top_n_size_file.write('%s,%s,%s\n'%(size,each_user,timeThrsh[i]))
top_n_size_file.close()
for i in range(len(timeThrsh)):
    temp = sorted(top_n_depth_users[i].iteritems(), key=operator.itemgetter(0), reverse=True)
    for tuple in temp:
        depth = tuple[0]
        users = tuple[1]
        for each_user in users:
            top_n_depth_file.write('%s,%s,%s\n'%(depth,each_user,timeThrsh[i]))
top_n_depth_file.close()
