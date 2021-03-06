import sys, os
from sets import Set
import csv
import array
from bitarray import bitarray
import operator
import numpy as np
from numpy.lib.scimath import sqrt

MAX_USERS = 2**29 - 1
NO_PARENT = MAX_USERS + 1
NEVER = 2**31 - 1
BEGINNING_TIME = 1245304105
END_TIME = 1280645999

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

def evolution_size_per_root(time, depth):
    global evolution_per_root, depth_evolution_per_root, width_evolution_per_root
    day = time/ 86400 #604800
    if day not in evolution_per_root:
        evolution_per_root[day] = 1
        depth_evolution_per_root[day] = depth
        width_evolution_per_root[day] = [depth]
    else:
        evolution_per_root[day] += 1
        depth_evolution_per_root[day] = max(depth_evolution_per_root[day], depth)
        width_evolution_per_root[day].append(depth)
                        
def manage_outdegree_per_root(outdegree, depth):
    global branching_dist_per_root
    if (outdegree, depth) in branching_dist_per_root:
        branching_dist_per_root[(outdegree, depth)] += 1
    else:
        branching_dist_per_root[(outdegree, depth)] = 1        

def manage_time_to_next_generation(parent_time, time_diff):
    global time_to_next_generation
    parent_time_week = parent_time / 604800
    time_diff_day = time_diff / 86400
    if (parent_time_week, time_diff_day) not in time_to_next_generation:
        time_to_next_generation[(parent_time_week, time_diff_day)] = 1
    else:
        time_to_next_generation[(parent_time_week, time_diff_day)] += 1

def initialize_traverse ():
    global graph, graph_node_count, activities_per_root, depth_expansion_per_root, depth_min_time_per_root, depth_max_time_per_root, branching_dist_per_root
    global evolution_per_root, depth_evolution_per_root, width_evolution_per_root
    graph = {}
    graph_node_count = 0
    evolution_per_root = {}
    depth_evolution_per_root = {}
    width_evolution_per_root = {}
    activities_per_root = []
    depth_expansion_per_root = {}
    depth_min_time_per_root = {}
    depth_max_time_per_root = {}
    branching_dist_per_root = {}
    
def cascade_traverse (parent,depth,time):
    global graph_node_count
    if parent not in graph:
        graph[parent] = 0
        manage_depth_expansion_per_root(MAX_DEPTH-depth, 1)
        evolution_size_per_root(time, MAX_DEPTH-depth)
        graph_node_count += 1
        if parent in children_of_parent:
            graph[parent] = len(children_of_parent[parent])
            manage_outdegree_per_root(graph[parent],MAX_DEPTH-depth)
        else:
            manage_outdegree_per_root(0,MAX_DEPTH-depth)
            return True
        if depth > 0:
            for (each_child,receiving_time) in children_of_parent[parent]:
                if cascade_traverse(each_child, depth-1, receiving_time) == True:
                    activities_per_root.append((parent,each_child,MAX_DEPTH-depth))
                    manage_depth_time_per_root(MAX_DEPTH-depth+1, receiving_time)
                    time_diff = receiving_time-time
                    manage_time_to_next_generation(time, time_diff)
        return True

def visulization (infl_file_name, user_list, max_depth):
    global activities_per_root
    VIZ_CUT_OFF = 100
    if max_depth > 4:
        print "Unnecessary graph, visualizing only one cascade"
        VIZ_CUT_OFF = 1
    for i in range(1,max_depth+1):
        if i != max_depth:
            continue
        cut_off = 0
        activities_per_root_file = open(infl_file_name+'_'+str(VIZ_CUT_OFF)+'_'+str(i)+"graph.dot", "w")
        activities_per_root_file.write('graph G {\n node [shape=circle,label="",width=0.1,height=0.1,style=filled,fillcolor=white];\n')#color=orange,style=filled,
        initialize_traverse()
        for a_top_user in user_list:
            if cut_off >= VIZ_CUT_OFF:
                break
            if a_top_user != 19845:
                continue
            activities_per_root_file.write('%s [fillcolor = red];\n'%a_top_user)
#             cascade_traverse(a_top_user, i, BEGINNING_TIME)
            cut_off += 1
        for j in range(len(activities_per_root)):
            activities_per_root_file.write('%s -- %s;\n'%(activities_per_root[j][0],activities_per_root[j][1])) # [color=black] 
        activities_per_root_file.write('}')
        activities_per_root_file.close()    
        os.popen("neato -Ksfdp -Tsvg "+infl_file_name+'_'+str(VIZ_CUT_OFF)+'_'+str(i)+"graph.dot"+">"+infl_file_name+'_'+str(VIZ_CUT_OFF)+'_'+str(i)+"_graph.svg")
        os.popen("rm "+infl_file_name+'_'+str(VIZ_CUT_OFF)+'_'+str(i)+"graph.dot")

def resolve_cascades (user_list):
    global depth_expansion, depth_expansion_per_root, top_users_correlated_info, branching_dist, cascade_evolution, parent_alpha
    global cascade_count, cascade_width, evolution_per_root, depth_evolution_per_root, width_evolution_per_root
    not_root_users = Set()
    root_contains_users = {}
    depth_expansion = []
    top_users_correlated_info = []
    top_users_info = {}
    for a_root in user_list:
        print 'Init cascade %s' %a_root
        initialize_traverse()
        root_fake_born_time = END_TIME
        if a_root in children_of_parent:
            for (each_child,receiving_time) in children_of_parent[a_root]:
                root_fake_born_time = min(root_fake_born_time, receiving_time)
            cascade_traverse(a_root, MAX_DEPTH, root_fake_born_time)
            ############################## A major change here ##############################
            # 0 depth 0 degree (i.e. root has to have out degree > 0) is no longer acceptable
            #################################################################################
        else:
            continue
        cascade_count += 1
        print 'Cascade Count %s' %cascade_count
        cascade_size = len(graph)
        cascade_depth = max(depth_expansion_per_root)
        if cascade_depth == 0:
            size_vs_root_odeg.append((cascade_size, 0, round(parent_alpha[a_root], 3)))
        else:
            size_vs_root_odeg.append((cascade_size, depth_expansion_per_root[1], round(parent_alpha[a_root], 3)))
#         cascade_evolution.append((a_root,evolution_per_root))
        first_day = min(evolution_per_root)
        last_day = max(evolution_per_root)
        temp_evolution = []
        weekly_cum_evolution = [a_root]
        this_week_evolution = 0
        this_week_depth = 0
        this_week_width = [0]*(cascade_depth+1)
        for i in range(first_day, last_day+1): # start,end time for week = range(2059, 2117): #start,end time for day 14413*86400, 14819*86400
            temp_evolution.append(evolution_per_root[i] if i in evolution_per_root else 0)
            if (i - first_day + 1) <= 4*7:
                this_week_evolution += (evolution_per_root[i] if i in evolution_per_root else 0)
                this_week_depth = max(this_week_depth, depth_evolution_per_root[i] if i in depth_evolution_per_root else 0)
                for each_depth in range(1, this_week_depth+1):
                    this_week_width[each_depth] += width_evolution_per_root[i].count(each_depth) if i in width_evolution_per_root else 0
                if (((i - first_day + 1) % 7) == 0 and (i - first_day + 1) <= 4*7) or (
                    ((last_day - first_day + 1) < 4*7) and (i == last_day)):
                    sd = np.sqrt(np.var(temp_evolution))
                    avg = np.average(temp_evolution)
                    burstiness = round(((sd - avg) / (sd + avg)), 3)
                    weekly_cum_evolution.append(this_week_width[1]) #Root's contribution
                    weekly_cum_evolution.append(this_week_width[2] if this_week_depth > 1 else 0) #Root's neighbor's contribution
                    weekly_cum_evolution.append(burstiness)
                    weekly_cum_evolution.append(this_week_depth)
                    weekly_cum_evolution.append(max(this_week_width))
                    weekly_cum_evolution.append(this_week_evolution)
                    this_week_evolution = 0

        sd = np.sqrt(np.var(temp_evolution))
        avg = np.average(temp_evolution)
        burstiness = round(((sd - avg) / (sd + avg)), 3)
        for d in depth_expansion_per_root:
            depth_expansion.append((d,depth_expansion_per_root[d],a_root, 1,
                                    depth_min_time_per_root[d] if d in depth_min_time_per_root else None,
                                    depth_max_time_per_root[d] if d in depth_max_time_per_root else None)) # 1 for distinct root
        max_width = max(depth_expansion_per_root.iteritems(), key=operator.itemgetter(1))[1]
        cascade_evolution.append((a_root, cascade_size, cascade_depth, max_width, first_day-14413+1, last_day-14413+1, burstiness))
        cascade_weekly_evolution.append((weekly_cum_evolution))
        for d in depth_min_time_per_root:
            if d > 1:
                inter_generation_time = depth_min_time_per_root[d] - depth_min_time_per_root[d-1]
                if d in inter_generation_time_stat:
                    inter_generation_time_stat[d].append(inter_generation_time)
                else:
                    inter_generation_time_stat[d] = [inter_generation_time]
        if max_width not in cascade_width:
            cascade_width[max_width] = 1
        else:
            cascade_width[max_width] += 1
        for (degree,depth) in branching_dist_per_root: #Collecting out degree distribution for branching process.
            if (degree,depth) in branching_dist:
                branching_dist[(degree,depth)] += branching_dist_per_root[(degree,depth)]
            else:
                branching_dist[(degree,depth)] = branching_dist_per_root[(degree,depth)]
        for i in range(len(activities_per_root)):
            if activities_per_root[i][0] == a_root:
                top_users_correlated_info.append((a_root,graph[a_root],graph[activities_per_root[i][1]],cascade_size,cascade_depth))
        parent_infulence_proportion = []
        for each_parent in graph:
            if graph[each_parent] > 0:
                parent_infulence_proportion.append(round(parent_alpha[each_parent], 3))
        influence_proportion_stat.append((a_root, cascade_size, cascade_depth,
                                          np.sum(parent_infulence_proportion),
                                          np.average(parent_infulence_proportion),
                                          np.std(parent_infulence_proportion)))
    return

graph = {}
graph_node_count = 0
cascade_count = 0
activities_per_root = []
evolution_per_root = {}
depth_evolution_per_root = {}
width_evolution_per_root = {}
cascade_evolution = []
cascade_weekly_evolution = []
time_to_next_generation = {}
depth_expansion_per_root = {}
inter_generation_time_stat = {}
cascade_width = {}
depth_expansion = []
top_users_correlated_info = []
size_vs_root_odeg = []
influence_proportion_stat = []
branching_dist_per_root = {}
branching_dist = {}
children_of_parent = {}
#rezaur@rahman:~/Documents/Code/Cascade$ python top_node_info.py test_case/children_of_parent.txt test_case/top_size.csv,test_case/top_depth.csv 20
#rezaur@rahman:~/Documents/Code/Cascade$ python top_node_info.py First_parent/children_of_parent.txt First_parent/top_size.csv,First_parent/top_depth.csv 1814400
TOP_N = int(sys.argv[4])#raw_input('''Do you want to see subset of top users?then input your value: '''))
MAX_DEPTH = int(sys.argv[5])
#raw_input('Graph traversal depth? (1~100)?'))
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
    parent_alpha = array.array('f',(0,)*MAX_USERS)
    for parent_id,alpha in csv.reader(open('/home/rezaur/output_cascade/raw_stat_v2/parent_proportion.csv')):
        parent_alpha[int(parent_id)] = float(alpha)
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
        if TOP_N < len(top_users):
            o_file_prefix = each_infl_file+'_top_'+str(TOP_N)
        else:
            o_file_prefix = each_infl_file+'_all_'
        rooted_top_users = resolve_cascades(top_users)
        top_users_correlated_info_file  = open(o_file_prefix+'roots_correlated_info.csv', "w")
        size_vs_root_odeg_file  = open(o_file_prefix+'size_vs_root_odeg.csv', "w")
        depth_vs_expansion_file  = open(o_file_prefix+str(MAX_DEPTH)+'_depth_vs_expansion.csv', "w")
        inter_generation_time_stat_file = open(o_file_prefix+'inter_generation_time_stat.csv', "w")
        width_file  = open(o_file_prefix+'_max_width.csv', "w")
        branching_dist_file  = open(o_file_prefix+'branching_dist.csv', "w")
        evolution_file  = open(o_file_prefix+'evolution.csv', "w")
        weekly_evolution_file  = open(o_file_prefix+'weekly_evolution.csv', "w")
        time_to_next_generation_file  = open(o_file_prefix+'time_to_next_generation.csv', "w")
        influence_proportion_stat_file  = open(o_file_prefix+'influence_proportion_stat.csv', "w")
        writer = csv.writer(depth_vs_expansion_file, quoting=csv.QUOTE_MINIMAL)
        writer.writerows(depth_expansion)
        depth_vs_expansion_file.close()
        temp = sorted(cascade_width.iteritems(), key=operator.itemgetter(0), reverse=True)
        for tuple in temp:
            width_file.write('%s,%s\n'%(tuple[0],tuple[1]))
        width_file.close()
        writer = csv.writer(size_vs_root_odeg_file, quoting=csv.QUOTE_MINIMAL)
        writer.writerows(size_vs_root_odeg)        
        size_vs_root_odeg_file.close()
        writer = csv.writer(influence_proportion_stat_file, quoting=csv.QUOTE_MINIMAL)
        writer.writerows(influence_proportion_stat)        
        influence_proportion_stat_file.close()
        writer = csv.writer(top_users_correlated_info_file, quoting=csv.QUOTE_MINIMAL)
        writer.writerows(top_users_correlated_info)        
        top_users_correlated_info_file.close()
        for (outdeg,depth) in branching_dist:
            branching_dist_file.write('%s,%s,%s\n' %(outdeg,branching_dist[(outdeg,depth)],depth))
        branching_dist_file.close()
        writer = csv.writer(evolution_file, quoting=csv.QUOTE_MINIMAL)
        writer.writerows(cascade_evolution)
        evolution_file.close()        
        writer = csv.writer(weekly_evolution_file, quoting=csv.QUOTE_MINIMAL)
        writer.writerows(cascade_weekly_evolution)
        weekly_evolution_file.close()        
        for (each_week, each_time_diff) in time_to_next_generation:
            time_to_next_generation_file.write('%s,%s,%s\n' %(each_week, each_time_diff,
                                                              time_to_next_generation[(each_week, each_time_diff)]))
        time_to_next_generation_file.close()
        for each_gen in inter_generation_time_stat:
            inter_generation_time_stat_file.write('%s,%s,%s\n' %(each_gen,
                                                                 round(np.average(inter_generation_time_stat[each_gen]),3),
                                                                 np.median(inter_generation_time_stat[each_gen])))
        inter_generation_time_stat_file.close()
        if len(sys.argv) > 6:
            print 'Visualizing until %s depth' %sys.argv[6]
            visulization(each_infl_file, top_users, int(sys.argv[6]))
