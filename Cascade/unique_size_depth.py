import sys, gc
import array
import operator
from sets import Set
from random import choice
from bitarray import bitarray

PARENT_TYPE_FIRST_PARENT = 0
PARENT_TYPE_HIGHEST_ODEG = 1
PARENT_TYPE_LAST_PARENT = 2
PARENT_TYPE_RANDOM_PARENT = 3

CLR_THRESHOLD = 500000

MAX_USERS = 2**29 - 1
NO_PARENT = MAX_USERS + 1

class Node:
    def __init__(self, bornTime, actTime, odeg, lifespan):
        self.cascade_att = array.array('l',[1,0,-1,0]*len(timeThrsh))
        self.node_att = (actTime, odeg, lifespan)
    def setSize(self, idx, value):
        self.cascade_att[idx*4+0] = value
    def getSize(self, idx):
        return self.cascade_att[idx*4+0]
    def setDepth(self, idx, value):
        self.cascade_att[idx*4+1] = value
    def getDepth(self, idx):
        return self.cascade_att[idx*4+1]
    def setRoot(self, idx, root_and_distance):
        self.cascade_att[idx*4+2] = root_and_distance[0]
        self.cascade_att[idx*4+3] = root_and_distance[1]
    def getRoot(self, idx):
        return (self.cascade_att[idx*4+2], self.cascade_att[idx*4+3])
    def getBornTime(self):
        return self.node_att[3]
    def getActTime(self):
        return self.node_att[0]
    def getOutDeg(self):
        return self.node_att[1]
    def getLifespan(self):
        return self.node_att[2]
    def setPotentialParent(self, p_list):
        self.parent_list = p_list
#    def print_node(self):
#        print self.size,self.depth, self.root_id

def record_a_child(child_id, parent_list):
    global children_of_parent
    for (each_parent,receiving_time) in parent_list:
        if each_parent in children_of_parent:
            children_of_parent[each_parent].append((child_id,receiving_time))
        else:
            children_of_parent[each_parent] = []
            children_of_parent[each_parent].append((child_id,receiving_time))

def is_fertile_parent (parent_id):
    if parent_id in graph:
        if influence_type == 5 or influence_type == 6: # Discarding long-lived parents
            if graph[parent_id].getLifespan() <= lifespan_threshold:
                return True
            else:
                infertile_parents[parent_id] = True
                return False
        if influence_type == 3 or influence_type == 4: # Discarding short-lived parents
            if graph[parent_id].getLifespan() >= lifespan_threshold:
                return True
            else:
                infertile_parents[parent_id] = True
                return False
        if influence_type == 1 or influence_type == 2: # Discarding heavy parents
            if parent_id in heavy_users:
                infertile_parents[parent_id] = True
                return False            
            else:
                return True
        if influence_type == 0:
            return True
    else:
        return False

def parent_chooser (node_id, parent_list, choice_type, activation_time):
    potential_parents = []
    l = len(parent_list)
    for i in range(l-1,-1,-1):
        u_p = parent_list[i].strip().split(',')
        if u_p[0] == '-1':
            return potential_parents
        if int(u_p[1]) == activation_time:
            parent_list.pop()
        else:
            break
    if len(parent_list) < 1:
        return potential_parents
    new_list_l = len(parent_list)
    if choice_type == PARENT_TYPE_FIRST_PARENT:
        for i in range(new_list_l):
            first_parent = parent_list[i].strip().split(',')
            if is_fertile_parent(int(first_parent[0]))== False:
                if infertile_parents[int(first_parent[0])] == True:
                    nonleaves_with_infertile_parents[node_id] = True
                if second_chance_allowed:  
                    continue
                else:
                    break
            potential_parents.append((int(first_parent[0]), int(first_parent[1])))
            nonleaves_with_infertile_parents[node_id] = False
            return potential_parents
        
    if choice_type == PARENT_TYPE_LAST_PARENT:
        for i in range(new_list_l-1,-1,-1):
            last_parent = parent_list[i].strip().split(',')
            if is_fertile_parent(int(last_parent[0]))== False:
                continue
            potential_parents.append((int(last_parent[0]), int(last_parent[1])))
            return potential_parents
        
    if choice_type == PARENT_TYPE_RANDOM_PARENT:
        for i in range(new_list_l):
            random_parent = choice(parent_list).strip().split(',')
            if is_fertile_parent(int(random_parent[0]))== False:
                continue
            potential_parents.append((int(random_parent[0]), int(random_parent[1])))
            return potential_parents

    if choice_type == PARENT_TYPE_HIGHEST_ODEG:
        chosen_pid_odeg = -1
        chosen_pid = -1
        chosen_receiving_time = -1
        for element in parent_list:
            a_parent = element.strip().split(',')
            pID = int(a_parent[0])
            if is_fertile_parent(pID)== False:
                continue
            if pID in graph:
                if graph[pID].getOutDeg() > chosen_pid_odeg :
                    chosen_pid = pID
                    chosen_pid_odeg = graph[pID].getOutDeg()
                    chosen_receiving_time = int(a_parent[1])
        if chosen_pid != -1:
            potential_parents.append((chosen_pid, chosen_receiving_time))
            return potential_parents
    if len(potential_parents) == 0:
        return -1
        
def process(child, end_time, thrsh_index):
    cascade_root = (-1,0)
    for (pID,receiving_time) in child.parent_list:
        if end_time == -1:
            end_time = receiving_time
        if pID not in graph:
            log_file = open('size_depth.log','a')
            print 'Out of graph', pID
            log_file.write('Out of graph, %s' %pID)
            log_file.close()
            return cascade_root
#        if (child.bornTime-graph[pID].bornTime) <= timeThrsh[thrsh_index]:
        if (end_time-graph[pID].getActTime()) <= timeThrsh[thrsh_index]:
            graph[pID].setSize(thrsh_index, graph[pID].getSize(thrsh_index)+1)
            if graph[pID].getDepth(thrsh_index) <= child.getDepth(thrsh_index):
                graph[pID].setDepth(thrsh_index, child.getDepth(thrsh_index)+1)
            cascade_root = process(graph[pID], end_time, thrsh_index)
            if cascade_root[0] == -1:
                root_id = pID
            else:
                root_id = cascade_root[0]
            root_distance = cascade_root[1] + 1
            cascade_root = (root_id, root_distance)
            child.setRoot(thrsh_index, cascade_root)
        else:
            end_time = -1 # For considering next parent of this child, a provision if we consider multiple parents
    return cascade_root
    
def dump_garbage():
    """
    show us what's the garbage about
    """        
    # force collection
    print "\nGARBAGE:"
    gc.collect()
    print "\nGARBAGE OBJECTS:"
    for x in gc.garbage:
        s = str(x)
        if len(s) > 80: s = s[:80]
        print type(x),"\n  ", s

def clearMem():
    global result_size, result_depth
    global children_of_parent
    for each_parent in children_of_parent:
        for (each_child,receiving_time) in children_of_parent[each_parent]:
            children_of_parent_file.write('%s %s %s\n'%(each_parent,each_child,receiving_time))
    children_of_parent = {}

#python size_depth.py iheart_preprocessed_sorted.txt 86400,172800,259200,345600,432000,518400,604800,691200,777600,864000,1209600,1814400 First_parent/
f = open(sys.argv[1], "r")
timeThrsh = [] #[86400,172800,259200,345600,432000,518400,604800,691200,777600,864000,1209600,1814400]
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
size_vs_root_lifespan_file = open(sys.argv[3]+"size_vs_root_lifespan.csv", "w")
rooted_top_users_file = open(sys.argv[3]+"rooted_top_users.csv", "w")
nonroot_top_users_file = open(sys.argv[3]+"nonroot_top_users.csv", "w")
children_of_parent = {} # To hold children of all parents
children_of_parent_file = open(sys.argv[3]+"children_of_parent.txt", "w")
infertile_parents = bitarray(MAX_USERS)
infertile_parents.setall(False)
nonleaves_with_infertile_parents = bitarray(MAX_USERS)
nonleaves_with_infertile_parents.setall(False)
leaves_with_infertile_parents = bitarray(MAX_USERS)
leaves_with_infertile_parents.setall(False)
nonleaves_leaves_with_infertile_parents_file = open(sys.argv[3]+"nonleaves_leaves_with_infertile_parents.txt", "w")
parent_type = int(sys.argv[4])#int(raw_input(
print '''PARENT_TYPE_FIRST_PARENT = 0
PARENT_TYPE_HIGHEST_ODEG = 1
PARENT_TYPE_LAST_PARENT = 2
PARENT_TYPE_RANDOM_PARENT = 3
Your choice %s''' %parent_type 
TOP_N = int(sys.argv[5]) # Required number of top users
print '''Top nodes' sizes/depths you want to see is %s''' %TOP_N 
influence_type = int(sys.argv[6])
# Normal parent analysis: influence_type = 0
# Discard heavy parents w/o second chance: influence_type = 1
# Discard heavy parents with second chance to the children: influence_type = 2
# Discard children of short-lived parents analysis w/o second chance: influence_type = 3
# Discard children of short-lived parents analysis with second chance: influence_type = 4
# Discard children of long-lived parents analysis w/o second chance: influence_type = 5
# Discard children of long-lived parents analysis with second chance: influence_type = 6

heavy_users = {}
if influence_type>=1 and influence_type<=2:
    heavy_users_file = open(sys.argv[7], "r")
    for line in heavy_users_file:
        a_heavy_user = int(line.strip())
        heavy_users[a_heavy_user] = None
    print "Heavy users' file loaded"
if influence_type>=3 and influence_type<=6:
    lifespan_threshold = int(sys.argv[7])
    print "Lifespan of parents will be considered"
else:
    lifespan_threshold = 0
if influence_type == 1 or influence_type == 3 or influence_type == 5:
    second_chance_allowed = False
else:
    second_chance_allowed = True
    print "Second chance allowed"
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
    newNode = Node(born_time,activation_time,odeg,last_act_time-activation_time+1 if is_leaf==False else 0)
    potential_parent_list = parent_chooser(node_id, element[7:len(element)],parent_type,activation_time)
    count = count+1 # Read status
    if (count % (CLR_THRESHOLD/10)) == 0:
        print count
    if type(potential_parent_list) != list:
        if is_leaf == True and nonleaves_with_infertile_parents[node_id] == True:
            nonleaves_with_infertile_parents[node_id] = False
            leaves_with_infertile_parents[node_id] = True
        continue
    newNode.setPotentialParent(potential_parent_list)
    record_a_child(node_id, newNode.parent_list)
    if is_leaf == False:
        graph[node_id] = newNode
    for i in range(len(timeThrsh)):
        process(newNode, -1, i)
    if is_leaf == True:
        newNode.parent_list = None
        newNode.cascade_att = None
        newNode.node_att = None
        newNode = None
    if (count % CLR_THRESHOLD) == 0:
        print "Clearing"
        clearMem()
#    if (count % (10*CLR_THRESHOLD)) == 0:
#        break
#        dump_garbage()      
f.close()
clearMem()
children_of_parent_file.close()

nonleaves_leaves_with_infertile_parents_file.write('%s,%s,%s'%(nonleaves_with_infertile_parents.count(),leaves_with_infertile_parents.count(),infertile_parents.count()))
nonleaves_leaves_with_infertile_parents_file.close()

for node_id in graph:
#    print node_id
#    graph[node_id].print_node()
    for i in range(len(timeThrsh)):    
        if graph[node_id].getRoot(i)[0] == -1:
            if graph[node_id].getSize(i) in result_size[i]:
                result_size[i][graph[node_id].getSize(i)] += 1
            else:
                result_size[i][graph[node_id].getSize(i)] = 1
            if graph[node_id].getDepth(i) in result_depth[i]:
                result_depth[i][graph[node_id].getDepth(i)] += 1
            else:
                result_depth[i][graph[node_id].getDepth(i)] = 1
print result_size
print result_depth

top_n_sizes = []
top_n_depths = []
for i in range(len(timeThrsh)):
    temp = sorted(result_size[i].iteritems(), key=operator.itemgetter(0), reverse=True)
    top_counter = 0
    top_n_sizes.append(([]))
    for tuple in temp:
        if top_counter < TOP_N:
            top_n_sizes[i].append((tuple))
            top_counter += tuple[1]
        size_file.write('%s,%s,%s\n'%(tuple[0],tuple[1],timeThrsh[i]))
size_file.close()
for i in range(len(timeThrsh)):
    temp = sorted(result_depth[i].iteritems(), key=operator.itemgetter(0), reverse=True)
    top_counter = 0
    top_n_depths.append(([]))
    for tuple in temp:
        if top_counter < TOP_N:
            top_n_depths[i].append((tuple))
            top_counter += tuple[1]
        depth_file.write('%s,%s,%s\n'%(tuple[0],tuple[1],timeThrsh[i]))
depth_file.close()
top_n_size_users = []
top_n_depth_users = []
for i in range(len(timeThrsh)):
    top_n_size_users.append({})
    top_n_depth_users.append({})
    for j in range(len(top_n_sizes[i])):
        top_n_size_users[i][top_n_sizes[i][j][0]] = []
    for j in range(len(top_n_depths[i])):
        top_n_depth_users[i][top_n_depths[i][j][0]] = []

root_contains = []
for i in range(len(timeThrsh)):
    root_contains.append({})
    for node_id in graph:
        if graph[node_id].getRoot(i)[0] == -1:
            if graph[node_id].getSize(i) in top_n_size_users[i]:
                top_n_size_users[i][graph[node_id].getSize(i)].append(node_id)
                root_contains[i][node_id] = []
            if graph[node_id].getDepth(i) in top_n_depth_users[i]:
                top_n_depth_users[i][graph[node_id].getDepth(i)].append(node_id)
for i in range(len(timeThrsh)):
    root_contains.append({})
    for node_id in graph:
        if graph[node_id].getRoot(i)[0] in root_contains[i]:
            root_id = graph[node_id].getRoot(i)[0]
            if graph[node_id].getSize(i) > 0.5*graph[root_id].getSize(i):
                root_contains[i][root_id].append(node_id)
print top_n_size_users
print top_n_depth_users

for i in range(len(timeThrsh)):
    for a_root in root_contains[i]:
        root_size = graph[a_root].getSize(i)
        root_depth = graph[a_root].getDepth(i)
        for a_top_user in root_contains[i][a_root]:
            top_of_size = graph[a_top_user].getSize(i)
            top_of_depth = graph[a_top_user].getDepth(i)
            top_at_depth = graph[a_top_user].getRoot(i)[1]
            rooted_top_users_file.write('%s,%s,%s,%s,%s,%s,%s\n'%(a_root,root_size,root_depth,a_top_user,top_at_depth,top_of_size,top_of_depth))
            nonroot_top_users_file.write('%s,%s,%s\n'%(top_of_size,a_top_user,timeThrsh[i]))
rooted_top_users_file.close()
nonroot_top_users_file.close()

for i in range(len(timeThrsh)):
    temp = sorted(top_n_size_users[i].iteritems(), key=operator.itemgetter(0), reverse=True)
    for tuple in temp:
        size = tuple[0]
        users = tuple[1]
        for each_user in users:
            top_n_size_file.write('%s,%s,%s\n'%(size,each_user,timeThrsh[i]))
            size_vs_root_lifespan_file.write('%s,%s\n'%(size,graph[each_user].getLifespan()))
top_n_size_file.close()
size_vs_root_lifespan_file.close()
for i in range(len(timeThrsh)):
    temp = sorted(top_n_depth_users[i].iteritems(), key=operator.itemgetter(0), reverse=True)
    for tuple in temp:
        depth = tuple[0]
        users = tuple[1]
        for each_user in users:
            top_n_depth_file.write('%s,%s,%s\n'%(depth,each_user,timeThrsh[i]))
top_n_depth_file.close()

#size evolution of top users
if len(sys.argv) > 10:
    top_file = open(sys.argv[3]+"top_size.csv", "r")
    top_user_growth_file = open(sys.argv[3]+"top_user_growth.txt", "w")
    top_user_set = Set()
    for line in top_file:
        top_user_set.add(int(line.split(',')[1].strip()))
    for each_top_user in top_user_set:
        top_user_growth_file.write("id_%d =[" %each_top_user )
        for i in range(len(timeThrsh)):
            top_user_growth_file.write("%d," %graph[each_top_user].getSize(i))
        top_user_growth_file.write("];\n")
    top_user_growth_file.close()