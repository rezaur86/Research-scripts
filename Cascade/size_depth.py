import sys
import array
import operator

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
    def setParentList(self, p_list):
        self.parent_list = p_list
    def print_node(self):
        print self.size,self.depth
            
def process(child, thrsh_index):
    for pID in child.parent_list:
        if pID not in graph:
            break;
        if (child.bornTime-graph[pID].bornTime) <= timeThrsh[thrsh_index]:
            graph[pID].size[thrsh_index] += 1
            if graph[pID].depth[thrsh_index] <= child.depth[thrsh_index]:
                graph[pID].depth[thrsh_index] = child.depth[thrsh_index]+1
            process(graph[pID], thrsh_index)
        else:
            break
    
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
                
graph = {}

f = open(sys.argv[1], "r")
timeThrsh = [] #[86400,172800,259200,345600,432000,518400,604800,691200,777600,864000]
result_size = []
result_depth = []
for a_thrsh in sys.argv[2].split(','):
    timeThrsh.append(int(a_thrsh.strip()))
    result_size.append({})
    result_depth.append({})
size_file = open(argv[3]+"_size.csv", "w")
depth_file = open(argv[3]+"_depth.csv", "w")

count = 0
for line in f:
    element = line.split(' ')
    node_id = long(element[0].strip())
    born_time = int(element[1].strip())
    activation_time = int(element[2].strip())
    is_leaf = bool(int(element[3].strip()))
    odeg = int((element[4].strip()))
    potential_parents = array.array('L')
    for p_index in range(5,len(element)):
        if element[p_index].strip() == '-1':
            break
        else:
            potential_parents.append(long(element[p_index].strip()))
    newNode = Node()
    newNode.setBornTime(born_time)
    newNode.setActTime(activation_time)
    newNode.setParentList(potential_parents)
    if is_leaf == False:
        graph[node_id] = newNode
    for i in range(len(timeThrsh)):
        process(newNode, i)
    count = count+1
    if (count % 1000) == 0:
        print count
    if (count % CLR_THRESHOLD) == 0:
        print "Clearing"
        clearHashTable(newNode)
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

for i in range(len(timeThrsh)):
    temp = sorted(result_size[i].iteritems(), key=operator.itemgetter(1), reverse=True)
    for tuple in temp:
        size_file.write('%s,%s,%s\n'%(tuple[0],tuple[1],timeThrsh[i]))
size_file.close()

for i in range(len(timeThrsh)):
    temp = sorted(result_depth[i].iteritems(), key=operator.itemgetter(1), reverse=True)
    for tuple in temp:
        depth_file.write('%s,%s,%s\n'%(tuple[0],tuple[1],timeThrsh[i]))
depth_file.close()
#for i in range(10):
#    sys.stdout.write("size%d =[" %timeThrsh )
#    for j in range(max(result[i])):
#        if j+1 in result[i]:
#            sys.stdout.write("%d " %result[i][j+1])
#        else:
#            sys.stdout.write("0 ")
#    sys.stdout.write("];\n")
#    sys.stdout.write("depth%d = [" %timeThrsh)
#    for j in range(max(result[10+i])):
#        if j+1 in result[10+i]:
#            sys.stdout.write("%d " %result[10+i][j+1])
#        else:
#            sys.stdout.write("0 ")
#    sys.stdout.write("];\n")
