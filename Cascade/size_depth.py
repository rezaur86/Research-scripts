import sys
import array
import operator

CLR_THRESHOLD = 500000

MAX_USERS = 2**29 - 1
NO_PARENT = MAX_USERS + 1    

class Node:
    def __init__(self):
        self.size = [1]*timeThrs_count
        self.depth = [0]*timeThrs_count
    def setBornTime(self, bornTime):
        self.bornTime = bornTime
    def setActTime(self, actTime):
        self.actTime = actTime
    def setParent(self, pID):
        self.pID = pID
    def print_node(self):
        print self.size,self.depth
            
def process(child, thrsh_index):
#    for pID in child.parent_list[0:1]:
    pID = child.pID
    while pID != -1:
        if pID not in graph:
            break;
        if (child.bornTime-graph[pID].bornTime) <= timeThrs[thrsh_index]:
            graph[pID].size[thrsh_index] += 1
            if graph[pID].depth[thrsh_index] <= child.depth[thrsh_index]:
                graph[pID].depth[thrsh_index] = child.depth[thrsh_index]+1
#            process(graph[pID], thrsh_index)
            child = graph[pID]
            pID = child.pID
        else:
            break
    
def clearHashTable(current_node):
    global result_size
    global result_depth
    record = []
    for node in graph.keys():
        if current_node.bornTime-graph[node].bornTime > max(timeThrs):
            record.append(node)
    for node in record:
        for i in range(timeThrs_count):    
            if graph[node].size[i] in result_size[i]:
                result_size[i][graph[node].size[i]] += 1
            else:
                result_size[i][graph[node].size[i]] = 1
            if graph[node].depth[i] in result_depth[i]:
                result_depth[i][graph[node].depth[i]] += 1
            else:
                result_depth[i][graph[node].depth[i]] = 1
        del graph[node]
                
graph = {}

f = open(sys.argv[1], "r")
timeThrs = [] #[86400,172800,259200,345600,432000,518400,604800,691200,777600,864000]
result_size = []
result_depth = []
for a_thrsh in sys.argv[2].split(','):
    timeThrs.append(int(a_thrsh.strip()))
    result_size.append({})
    result_depth.append({})
timeThrs_count = len(timeThrs)   

count = 0
for line in f:
    element = line.split(' ')
    node_id = long(element[0].strip())
    born_time = int(element[1].strip())
    activation_time = int(element[2].strip())
    is_leaf = bool(int(element[3].strip()))
    odeg = int((element[4].strip()))
    potential_parents = array.array('L')
    pID = -1
#    for p_index in range(5,len(element)):
#        if element[p_index].strip() == '-1':
#            break
#        else:
#            potential_parents.append(long(element[p_index].strip()))
    if element[5].strip() != '-1':
        pID = long(element[5].strip())
    newNode = Node()
    newNode.setBornTime(born_time)
    newNode.setActTime(activation_time)
    newNode.setParent(pID)
    if is_leaf == False:
        graph[node_id] = newNode
    for i in range(timeThrs_count):
        process(newNode, i)
    count = count+1
    if (count % 1000) == 0:
        print count
    if (count % CLR_THRESHOLD) == 0:
        print "Clearing"
        clearHashTable(newNode)
f.close()

for node in graph.keys():
    for i in range(timeThrs_count):    
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

size_file = open("size.csv", "w")
for i in range(timeThrs_count):
    temp = sorted(result_size[i].iteritems(), key=operator.itemgetter(1), reverse=True)
    for tuple in temp:
        size_file.write('%s,%s,%s\n'%(tuple[0],tuple[1],timeThrs[i]))
size_file.close()

depth_file = open("depth.csv", "w")
for i in range(timeThrs_count):
    temp = sorted(result_depth[i].iteritems(), key=operator.itemgetter(1), reverse=True)
    for tuple in temp:
        depth_file.write('%s,%s,%s\n'%(tuple[0],tuple[1],timeThrs[i])) #there is a difference between out_degree = 0 and leaf. Leaves are those who do not bring any new node to the graph
depth_file.close()
#for i in range(10):
#    sys.stdout.write("size%d =[" %timeThrs )
#    for j in range(max(result[i])):
#        if j+1 in result[i]:
#            sys.stdout.write("%d " %result[i][j+1])
#        else:
#            sys.stdout.write("0 ")
#    sys.stdout.write("];\n")
#    sys.stdout.write("depth%d = [" %timeThrs)
#    for j in range(max(result[10+i])):
#        if j+1 in result[10+i]:
#            sys.stdout.write("%d " %result[10+i][j+1])
#        else:
#            sys.stdout.write("0 ")
#    sys.stdout.write("];\n")
