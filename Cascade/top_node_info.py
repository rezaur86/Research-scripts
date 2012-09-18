import sys, os
from sets import Set
import csv
import array
from bitarray import bitarray
import operator

MAX_USERS = 2**29 - 1
NO_PARENT = MAX_USERS + 1
NEVER = 2**31 - 1

def infl_activities (parent,depth):
    global graph_node_count
    if parent not in graph:
        graph[parent] = 0
        graph_node_count += 1
        if graph_node_count%1000 == 0:
            print 'graph size:',graph_node_count
        if parent in children_of_parent:
            graph[parent] = len(children_of_parent[parent])
        else:
            return
        if depth > 0:
            for each_child in children_of_parent[parent]:
                top_activities.append((parent,each_child,depth))
                infl_activities(each_child, depth-1)
        else:
            return
                
graph = {}
graph_node_count = 0
children_of_parent = {}
top_activities = []

CLR_MEM_THRESH = 10000
if __name__ == '__main__':
    TOP_N = int(raw_input('''Do you want to see subset of top users?
    then input your value: '''))
    depth = int(raw_input('Graph depth? (1~4)?'))
    
    children_of_parent_file = open(sys.argv[1], "r")
    for line in children_of_parent_file:
        splits = line.split(' ')
        parent = long(splits[0].strip())
        children_of_parent[parent] = []
        for each_child in splits[1:len(splits)]:
            children_of_parent[parent].append(long(each_child))
    print 'children_of_parent_file is read'
    for each_infl_file in sys.argv[2].split(','):
        graph = {}
        graph_node_count = 0
        top_activities = []
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
        top_activities_file = open(each_infl_file+str(depth)+"graph.dot", "w")
        top_actors_odeg_file  = open(each_infl_file+'top_'+str(TOP_N)+'_actors_odeg.csv', "w")
        for a_top_user in top_users:
            infl_activities(a_top_user, depth)
        top_activities_file.write('''digraph G {
node [shape=circle,label="",width=0.1,height=0.1]
''')#color=orange,style=filled,
        for i in range(len(top_activities)):
            if top_activities[i][0] in top_users:
                top_actors_odeg_file.write('%s,%s\n'%(graph[top_activities[i][0]],graph[top_activities[i][1]]))
            top_activities_file.write('%s -> %s;\n'%(top_activities[i][0],top_activities[i][1])) # [color=black] 
        top_activities_file.write('}')
        top_activities_file.close()
        top_actors_odeg_file.close()
        