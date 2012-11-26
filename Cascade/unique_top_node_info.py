import sys, os
from sets import Set
import csv
import array
from bitarray import bitarray
import operator
from logging import root

MAX_USERS = 2**29 - 1
NO_PARENT = MAX_USERS + 1
NEVER = 2**31 - 1

def manage_depth_expansion_per_root(depth,expansion):
    global depth_expansion_per_root
    if depth not in depth_expansion_per_root:
        depth_expansion_per_root[depth] = expansion
    else:
        depth_expansion_per_root[depth] += expansion
def manage_depth_time_per_root(depth,time):
    global depth_min_time_per_root, depth_max_time_per_root
    if depth not in depth_min_time_per_root:
        depth_min_time_per_root[depth] = time
    else:
        depth_min_time_per_root[depth] = min(depth_min_time_per_root[depth], time)
    if depth not in depth_max_time_per_root:
        depth_max_time_per_root[depth] = time
    else:
        depth_max_time_per_root[depth] = max(depth_max_time_per_root[depth], time)

def manage_outdegree_per_root(outdegree, depth):
    global branching_dist_per_root
    if (outdegree, depth) in branching_dist_per_root:
        branching_dist_per_root[(outdegree, depth)] += 1
    else:
        branching_dist_per_root[(outdegree, depth)] = 1        

def initialize_traverse ():
    global graph, graph_node_count, activities_per_root, depth_expansion_per_root, depth_min_time_per_root, depth_max_time_per_root, branching_dist_per_root
    graph = {}
    graph_node_count = 0
    activities_per_root = []
    depth_expansion_per_root = {}
    depth_min_time_per_root = {}
    depth_max_time_per_root = {}
    branching_dist_per_root = {}
    
def cascade_traverse (parent,depth):
    global graph_node_count
    if parent not in graph:
        graph[parent] = 0
        manage_depth_expansion_per_root(MAX_DEPTH-depth, 1)
        graph_node_count += 1
        if parent in children_of_parent:
            graph[parent] = len(children_of_parent[parent])
            manage_outdegree_per_root(graph[parent],MAX_DEPTH-depth)
        else:
            manage_outdegree_per_root(0,MAX_DEPTH-depth)
            return True
        if depth > 0:
            for (each_child,receiving_time) in children_of_parent[parent]:
                if cascade_traverse(each_child, depth-1) == True:
                    activities_per_root.append((parent,each_child,MAX_DEPTH-depth))
                    manage_depth_time_per_root(MAX_DEPTH-depth+1, receiving_time)
        return True

def visulization (infl_file_name, user_list, max_depth):
    global activities_per_root
    VIZ_CUT_OFF = 100
    if max_depth > 4:
        print "Unnecessary graph, visualizing only one cascade"
        VIZ_CUT_OFF = 1
    for i in range(1,max_depth+1):
        cut_off = 0
        activities_per_root_file = open(infl_file_name+'_'+str(VIZ_CUT_OFF)+'_'+str(i)+"graph.dot", "w")
        activities_per_root_file.write('graph G {\n node [shape=circle,label="",width=0.1,height=0.1,style=filled,fillcolor=white];\n')#color=orange,style=filled,
        initialize_traverse()
        for a_top_user in user_list:
            if cut_off >= VIZ_CUT_OFF:
                break
            activities_per_root_file.write('%s [fillcolor = red];\n'%a_top_user)
            cascade_traverse(a_top_user, i)
            cut_off += 1
        for j in range(len(activities_per_root)):
            activities_per_root_file.write('%s -- %s;\n'%(activities_per_root[j][0],activities_per_root[j][1])) # [color=black] 
        activities_per_root_file.write('}')
        activities_per_root_file.close()    
        os.popen("neato -Ksfdp -Tsvg "+infl_file_name+'_'+str(VIZ_CUT_OFF)+'_'+str(i)+"graph.dot"+">"+infl_file_name+'_'+str(VIZ_CUT_OFF)+'_'+str(i)+"_graph.svg")
        os.popen("rm "+infl_file_name+'_'+str(VIZ_CUT_OFF)+'_'+str(i)+"graph.dot")

def resolve_cascades (user_list):
    global depth_expansion, depth_expansion_per_root, top_users_correlated_info, branching_dist
    not_root_users = Set()
    root_contains_users = {}
    depth_expansion = []
    top_users_correlated_info = []
    top_users_info = {}
    for a_root in user_list:
        print 'Init cascade %s' %a_root
        initialize_traverse()
        cascade_traverse(a_root, MAX_DEPTH)
        cascade_size = len(graph)
        cascade_depth = max(depth_expansion_per_root)
        for d in depth_expansion_per_root:
            depth_expansion.append((d,depth_expansion_per_root[d],a_root,1,depth_min_time_per_root[d] if d in depth_min_time_per_root else None,depth_max_time_per_root[d] if d in depth_max_time_per_root else None)) # 1 for distinct root
        for (degree,depth) in branching_dist_per_root: #Collecting out degree distribution for branching process.
            if (degree,depth) in branching_dist:
                branching_dist[(degree,depth)] += branching_dist_per_root[(degree,depth)]
            else:
                branching_dist[(degree,depth)] = branching_dist_per_root[(degree,depth)]
        for i in range(len(activities_per_root)):
            if activities_per_root[i][0] == a_root:
                top_users_correlated_info.append((a_root,graph[a_root],graph[activities_per_root[i][1]],cascade_size,cascade_depth))
    return

graph = {}
graph_node_count = 0
activities_per_root = []
depth_expansion_per_root = {}
depth_expansion = []
top_users_correlated_info = []
branching_dist_per_root = {}
branching_dist = {}
children_of_parent = {}
#rezaur@rahman:~/Documents/Code/Cascade$ python top_node_info.py test_case/children_of_parent.txt test_case/top_size.csv,test_case/top_depth.csv 20
#rezaur@rahman:~/Documents/Code/Cascade$ python top_node_info.py First_parent/children_of_parent.txt First_parent/top_size.csv,First_parent/top_depth.csv 1814400
TOP_N = int(sys.argv[4])#raw_input('''Do you want to see subset of top users?then input your value: '''))
MAX_DEPTH = int(sys.argv[5])#raw_input('Graph traversal depth? (1~100)?'))
print 'New analysis of top %s cascades and traverse until %s depth' %(TOP_N,MAX_DEPTH)

if len(sys.argv) > 3:
    Special_time_window = int(sys.argv[3])
else:
    Special_time_window = -1

CLR_MEM_THRESH = 10000
if __name__ == '__main__': 
    children_of_parent_file = open(sys.argv[1], "r")
    lines = children_of_parent_file.readlines()
    children_of_parent_file.close()
    for line in lines:
        splits = line.split(' ')
        parent = int(splits[0].strip())
        a_child = int(splits[1].strip())
        receiving_time = int(splits[2].strip())
        if parent not in children_of_parent:
            children_of_parent[parent] = []
        children_of_parent[parent].append((a_child,receiving_time))
    print 'children_of_parent_file is read'
    for each_infl_file in sys.argv[2].split(','):
        top_users = {}
        seen_time_window = {}
        top_infl_file = open(each_infl_file, "r")
        for line in top_infl_file:
            splits = line.split(',')
            time_window = long(splits[2].strip())
            if time_window < Special_time_window:
                continue
            if time_window not in seen_time_window:
                seen_time_window[time_window] = Set()
            if len(seen_time_window[time_window]) <  TOP_N:
                if long(splits[1].strip()) in top_users:
                    top_users[long(splits[1].strip())] = max(top_users[long(splits[1].strip())],long(splits[0].strip()))
                else:
                    top_users[long(splits[1].strip())] = long(splits[0].strip())
                seen_time_window[time_window].add(int(splits[0].strip()))
        top_infl_file.close()
        if TOP_N <= len(top_users):
            o_file_prefix = each_infl_file+'_top_'+str(TOP_N)
        else:
            o_file_prefix = each_infl_file+'_all_'
        top_users_correlated_info_file  = open(o_file_prefix+'roots_correlated_info.csv', "w")
        depth_vs_expansion_file  = open(o_file_prefix+str(MAX_DEPTH)+'_depth_vs_expansion.csv', "w")
        branching_dist_file  = open(o_file_prefix+'branching_dist.csv', "w")
        rooted_top_users = resolve_cascades(top_users)
        writer = csv.writer(depth_vs_expansion_file, quoting=csv.QUOTE_MINIMAL)
        writer.writerows(depth_expansion)
        depth_vs_expansion_file.close()
        writer = csv.writer(top_users_correlated_info_file, quoting=csv.QUOTE_MINIMAL)
        writer.writerows(top_users_correlated_info)        
        top_users_correlated_info_file.close()
        for (outdeg,depth) in branching_dist:
            branching_dist_file.write('%s,%s,%s\n' %(outdeg,branching_dist[(outdeg,depth)],depth))
        branching_dist_file.close()
        
        if len(sys.argv) > 6:
            print 'Visualizing until %s depth' %sys.argv[6]
            visulization(each_infl_file, top_users, int(sys.argv[6]))
