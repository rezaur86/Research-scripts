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
#        if graph_node_count%10000 == 0:
#            print 'graph size:',graph_node_count
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
    if max_depth > 4:
        print "Unnecessary graph, skipping visualization, please provide value less than 5 as the last argument"
        return
    for i in range(1,max_depth+1):
        initialize_traverse()
        for a_top_user in user_list:
            cascade_traverse(a_top_user, i)
        activities_per_root_file = open(infl_file_name+'_top_'+str(TOP_N)+'_'+str(i)+"graph.dot", "w")
        activities_per_root_file.write('digraph G {\n node [shape=circle,label="",width=0.1,height=0.1]\n')#color=orange,style=filled,
        for j in range(len(activities_per_root)):
            activities_per_root_file.write('%s -> %s;\n'%(activities_per_root[j][0],activities_per_root[j][1])) # [color=black] 
        activities_per_root_file.write('}')
        activities_per_root_file.close()    
        os.popen("neato -Ksfdp -Tsvg "+infl_file_name+'_top_'+str(TOP_N)+'_'+str(i)+"graph.dot"+">"+infl_file_name+'_top_'+str(TOP_N)+'_'+str(i)+"graph.svg")
        os.popen("rm "+infl_file_name+'_top_'+str(TOP_N)+'_'+str(i)+"graph.dot")

def resolve_cascades (user_list):
    global depth_expansion, depth_expansion_per_root, top_users_correlated_info, branching_dist
#    sorted_user_list = sorted(user_list.iteritems(), key=operator.itemgetter(1), reverse=True)
    not_root_users = Set()
    root_contains = {}
    root_contains_users = {}
    depth_expansion = []
    top_users_correlated_info = []
    top_users_info = {}
    print user_list
    for this_user in user_list:
#        if this_user in not_root_users:
#            continue
        initialize_traverse()
        cascade_traverse(this_user, MAX_DEPTH)
        top_users_info[this_user] = (len(graph), max(depth_expansion_per_root))
        root_contains_users[this_user] = Set()
        for other_user in user_list:
            if (other_user != this_user) and (other_user in graph):
                not_root_users.add(other_user)
                root_contains_users[this_user].add(other_user)
    for a_root in user_list:
#        if a_root in not_root_users:
#            if a_root in root_contains_users:
#                del root_contains_users[a_root]
#            continue
        initialize_traverse()
        cascade_traverse(a_root, MAX_DEPTH)
        root_contains[(a_root,top_users_info[a_root][0],top_users_info[a_root][1])] = Set()
        if a_root in not_root_users:
            for d in depth_expansion_per_root:
                depth_expansion.append((d,depth_expansion_per_root[d],a_root,0,depth_min_time_per_root[d] if d in depth_min_time_per_root else None,depth_max_time_per_root[d] if d in depth_max_time_per_root else None)) # 0 for sub-root
        else:
            for d in depth_expansion_per_root:
                depth_expansion.append((d,depth_expansion_per_root[d],a_root,1,depth_min_time_per_root[d] if d in depth_min_time_per_root else None,depth_max_time_per_root[d] if d in depth_max_time_per_root else None)) # 1 for distinct root
            for (degree,depth) in branching_dist_per_root: #Collecting out degree distribution for branching process.
                if (degree,depth) in branching_dist:
                    branching_dist[(degree,depth)] += branching_dist_per_root[(degree,depth)]
                else:
                    branching_dist[(degree,depth)] = branching_dist_per_root[(degree,depth)]
                
        for i in range(len(activities_per_root)):
            if activities_per_root[i][0] == a_root:
                top_users_correlated_info.append((a_root,graph[a_root],graph[activities_per_root[i][1]],top_users_info[a_root][0],top_users_info[a_root][1]))
            if activities_per_root[i][0] in root_contains_users[a_root]:
                root_contains[(a_root,top_users_info[a_root][0],top_users_info[a_root][1])].add((activities_per_root[i][0],activities_per_root[i][2],top_users_info[activities_per_root[i][0]][0],top_users_info[activities_per_root[i][0]][1]))
    return root_contains

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
TOP_N = int(raw_input('''Do you want to see subset of top users?
then input your value: '''))
MAX_DEPTH = int(raw_input('Graph traversal depth? (1~100)?'))

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
        top_users_correlated_info_file  = open(each_infl_file+'_top_'+str(TOP_N)+'users_correlated_info.csv', "w")
        depth_vs_expansion_file  = open(each_infl_file+'_top_'+str(TOP_N)+'_'+str(MAX_DEPTH)+'_depth_vs_expansion.csv', "w")
        rooted_top_users_file  = open(each_infl_file+'_top_'+str(TOP_N)+'_rooted_top_users.csv', "w")
        branching_dist_file  = open(each_infl_file+'_top_'+str(TOP_N)+'_branching_dist.csv', "w")
        rooted_top_users = resolve_cascades(top_users)
        writer = csv.writer(depth_vs_expansion_file, quoting=csv.QUOTE_MINIMAL)
        writer.writerows(depth_expansion)
        depth_vs_expansion_file.close()
        writer = csv.writer(top_users_correlated_info_file, quoting=csv.QUOTE_MINIMAL)
        writer.writerows(top_users_correlated_info)        
        top_users_correlated_info_file.close()
        for (a_root,root_size,root_depth) in rooted_top_users:
            for (a_top_user, at_depth, of_size, of_depth) in rooted_top_users[(a_root,root_size,root_depth)]:
                rooted_top_users_file.write('%s,%s,%s,%s,%s,%s,%s\n'%(a_root,root_size,root_depth,a_top_user,at_depth, of_size, of_depth))
        rooted_top_users_file.close()
        for (outdeg,depth) in branching_dist:
            branching_dist_file.write('%s,%s,%s\n' %(outdeg,branching_dist[(outdeg,depth)],depth))
        branching_dist_file.close()
#        for a_top_user in top_users:
#            cascade_traverse(a_top_user, MAX_DEPTH)
#            for d in depth_expansion_per_root:
#                depth_vs_expansion_file.write('%s,%s,%s\n'%(d,depth_expansion_per_root[d],a_top_user))
#            for i in range(len(activities_per_root)):
#                if activities_per_root[i][0] == a_top_user:
#                    top_users_correlated_info_file.write('%s,%s\n'%(graph[activities_per_root[i][0]],graph[activities_per_root[i][1]]))
#        top_users_correlated_info_file.close()
        
        if len(sys.argv) > 4:
            visulization(each_infl_file, top_users, int(sys.argv[4]))
