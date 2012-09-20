import sys, os
from sets import Set
import csv
import array
from bitarray import bitarray
import operator

MAX_USERS = 2**29 - 1
NO_PARENT = MAX_USERS + 1
NEVER = 2**31 - 1

def manage_depth_expansion(depth,expansion):
    global depth_expansion
    if depth not in depth_expansion:
        depth_expansion[depth] = expansion
    else:
        depth_expansion[depth] += expansion

def initialize_traverse ():
    global graph, graph_node_count, top_activities, depth_expansion
    graph = {}
    graph_node_count = 0
    top_activities = []
    depth_expansion = {}
    
def cascade_traverse (parent,depth):
    global graph_node_count
    if parent not in graph:
        graph[parent] = 0
        manage_depth_expansion(MAX_DEPTH-depth, 1)
        graph_node_count += 1
        if graph_node_count%10000 == 0:
            print 'graph size:',graph_node_count
        if parent in children_of_parent:
            graph[parent] = len(children_of_parent[parent])
        else:
            return True
        if depth > 0:
            for each_child in children_of_parent[parent]:
                if cascade_traverse(each_child, depth-1) == True:
                    top_activities.append((parent,each_child))
        return True
    else:
        if depth > 0 and graph[parent] > 0:
            for each_child in children_of_parent[parent]:
                if cascade_traverse(each_child, depth-1) == True:
                    top_activities.append((parent,each_child))
        return False

def visulization (infl_file_name, user_list, max_depth):
    global top_activities
    if max_depth > 4:
        print "Unnecessary graph, skipping visualization, please provide value less than 5 as the last argument"
        return
    for i in range(1,max_depth+1):
        initialize_traverse()
        for a_top_user in user_list:
            cascade_traverse(a_top_user, i)
        top_activities_file = open(infl_file_name+'_top_'+str(TOP_N)+'_'+str(i)+"graph.dot", "w")
        top_activities_file.write('digraph G {\n node [shape=circle,label="",width=0.1,height=0.1]\n')#color=orange,style=filled,
        for j in range(len(top_activities)):
            top_activities_file.write('%s -> %s;\n'%(top_activities[j][0],top_activities[j][1])) # [color=black] 
        top_activities_file.write('}')
        top_activities_file.close()    
        os.popen("neato -Ksfdp -Tsvg "+infl_file_name+'_top_'+str(TOP_N)+'_'+str(i)+"graph.dot"+">"+infl_file_name+'_top_'+str(TOP_N)+'_'+str(i)+"graph.svg")

graph = {}
graph_node_count = 0
children_of_parent = {}
top_activities = []
depth_expansion = {}

TOP_N = int(raw_input('''Do you want to see subset of top users?
then input your value: '''))
MAX_DEPTH = int(raw_input('Graph traversal depth? (1~50)?'))

CLR_MEM_THRESH = 10000
if __name__ == '__main__': 
    children_of_parent_file = open(sys.argv[1], "r")
    for line in children_of_parent_file:
        splits = line.split(' ')
        parent = long(splits[0].strip())
        children_of_parent[parent] = []
        for each_child in splits[1:len(splits)]:
            children_of_parent[parent].append(long(each_child))
    print 'children_of_parent_file is read'
    for each_infl_file in sys.argv[2].split(','):
        top_users = {}
        seen_time_window = {}
        top_infl_file = open(each_infl_file, "r")
        for line in top_infl_file:
            splits = line.split(',')
            time_window = long(splits[2].strip())
            if time_window in seen_time_window:
                if len(seen_time_window[time_window]) <  TOP_N:
                    if long(splits[1].strip()) in top_users:
                        top_users[long(splits[1].strip())] = max(top_users[long(splits[1].strip())],long(splits[0].strip()))
                    else:
                        top_users[long(splits[1].strip())] = long(splits[0].strip())
                    seen_time_window[time_window].add(int(splits[0].strip()))
            else:
                seen_time_window[time_window] = Set()
                if long(splits[1].strip()) in top_users:
                    top_users[long(splits[1].strip())] = max(top_users[long(splits[1].strip())],long(splits[0].strip()))
                else:
                    top_users[long(splits[1].strip())] = long(splits[0].strip())
                seen_time_window[time_window].add(int(splits[0].strip()))
        top_infl_file.close()
        top_user_vs_child_odeg_file  = open(each_infl_file+'_top_'+str(TOP_N)+'user_vs_child_odeg.csv', "w")
        depth_vs_expansion_file  = open(each_infl_file+'_top_'+str(TOP_N)+'_'+str(MAX_DEPTH)+'_depth_vs_expansion.csv', "w")
        for a_top_user in top_users:
            initialize_traverse()
            cascade_traverse(a_top_user, MAX_DEPTH)
            for d in depth_expansion:
                depth_vs_expansion_file.write('%s,%s,%s\n'%(d,depth_expansion[d],a_top_user))
            for i in range(len(top_activities)):
                if top_activities[i][0] == a_top_user:
                    top_user_vs_child_odeg_file.write('%s,%s\n'%(graph[top_activities[i][0]],graph[top_activities[i][1]]))
        depth_vs_expansion_file.close()
        top_user_vs_child_odeg_file.close()
        
        if len(sys.argv) > 3:
            visulization(each_infl_file, top_users, int(sys.argv[3]))
