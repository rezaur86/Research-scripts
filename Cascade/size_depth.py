import array

timeThrs = 20 #[86400, 172800, 259200, 345600, 432000, 518400, 604800, 691200, 777600, 864000]
graph = {}
node = {}
threshold = 500000
count = 0

MAX_USERS = 2**29 - 1
NO_PARENT = MAX_USERS + 1    

class Node:
    def __init__(self):
        self.size = 1
        self.depth = 0
    def setbTime(self, bTime):
        self.bTime = bTime
    def setParentList(self, p_list):
        self.parent_list = p_list
#    def initialize(self):
#        self.size = 1 #[1]*10
#        self.depth = 0 #[0]*10

def process(a_node):
#    if element[3]!=1:
#        newNode = node()
#        newNode.setpID(element[1])
#        newNode.setbTime(element[2])
#        newNode.initialize()
#    graph[element[0]] = newNode
#    print "debugging",newNode.pID, newNode.bTime
#    for i in range(10):
    child = a_node
    for pID in child.parent_list:
        if pID not in graph:
            break;
        if (child.bTime-graph[pID].bTime) <= timeThrs:
            graph[pID].size += 1
            if graph[pID].depth <= child.depth:
                graph[pID].depth = child.depth+1
            process(graph[pID])
#            pID = child.pID
        else:
            break
    
def clearHashTable(element, result):
    record = []
    for node in graph.iterkeys():
        if graph[element[0]].bTime-graph[node].bTime > timeThrs[9]:
            record+=[node]
    for node in record:
        #print("delete %s" %node)
        for i in range(10):
            if graph[node].size[i] not in result[i]:
                result[i][graph[node].size[i]] = 1
            else:
                result[i][graph[node].size[i]] += 1
            if graph[node].depth[i] not in result[10+i]:
                result[10+i][graph[node].depth[i]] = 1
            else:
                result[10+i][graph[node].depth[i]] += 1
        del graph[node]

                
import sys
result = [{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{}] # first 10 for size, last 10 for depth
f = open(sys.argv[1], "r")

for line in f:
    element = line.split(' ')
    node_id = long(element[0].strip())
    born_time = int(element[1].strip())
    is_leaf = bool(int(element[2].strip()))
    odeg = int((element[3].strip()))
    potential_parents = array.array('L')
    for p_index in range(4,len(element)):
        if element[p_index].strip() == '-1':
            break
        else:
            potential_parents.append(long(element[p_index]))
    newNode = Node()
    newNode.setbTime(born_time)
    newNode.setParentList(potential_parents)
    if is_leaf == False:
        graph[node_id] = newNode
    process(newNode)
    count = count+1
    if count==threshold:
        clearHashTable(element, result)
        count = 0
f.close()

for node in graph.iterkeys():
#    for i in range(10):    
    if graph[node].size in result[0]:
        result[0][graph[node].size] += 1
    else:
        result[0][graph[node].size] = 1
    if graph[node].depth in result[10+0]:
        result[10+0][graph[node].depth] += 1
    else:
        result[10+0][graph[node].depth] = 1

print result[0], result[10]
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
