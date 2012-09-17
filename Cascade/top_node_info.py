import sys, os
from sets import Set
import csv
import array
from bitarray import bitarray
import operator

MAX_USERS = 2**29 - 1
NO_PARENT = MAX_USERS + 1
NEVER = 2**31 - 1

def infl_activities (parent):
    global graph_node_count
    if parent not in children_of_parent:
        return
    else:
        if parent not in graph:
            graph[parent] = None
            graph_node_count += 1
            if graph_node_count%1000 == 0:
                print graph_node_count
            for each_child in children_of_parent[parent]:
                top_activities.append((parent,each_child))
                infl_activities(each_child)
                
graph = {}
graph_node_count = 0
children_of_parent = {}
top_activities = []

CLR_MEM_THRESH = 10000
if __name__ == '__main__':
    TOP_N = int(raw_input('''Do you want to see subset of top users?
    then input your value: '''))
    build_graph = int(raw_input('Build graph? (1 for yes)?'))
            
    top_influencers = {}
    top_actors = {}
    seen_time_window = {}
    for each_infl_file in sys.argv[2].split(','):
        top_infl_file = open(each_infl_file, "r")
        for line in top_infl_file:
            splits = line.split(',')
            time_window = long(splits[2].strip())
            if time_window in seen_time_window:
                if len(seen_time_window[time_window]) <  TOP_N:
                    top_actors[long(splits[1].strip())] = 0 #We will save out degree here
                    top_influencers[long(splits[1].strip())] = [] # to save 1st hop neighbors
                    seen_time_window[time_window].add(int(splits[0].strip()))
            else:
                seen_time_window[time_window] = Set()
                top_actors[long(splits[1].strip())] = 0 #We will save out degree here
                top_influencers[long(splits[1].strip())] = [] # to save 1st hop neighbors
                seen_time_window[time_window].add(int(splits[0].strip()))
        top_infl_file.close()
    print seen_time_window
    print top_actors
    top_activities_file = open(sys.argv[4]+"top_activities.dot", "w")
    if build_graph == 1:
        children_of_parent_file = open(sys.argv[3], "r")
        for line in children_of_parent_file:
            splits = line.split(' ')
            parent = long(splits[0].strip())
            children_of_parent[parent] = []
            for each_child in splits[1:len(splits)]:
                children_of_parent[parent].append(long(each_child))
        print 'children_of_parent_file is read'
        for an_influencer in top_influencers:
            infl_activities(an_influencer)
        top_activities_file.write('''digraph G {
node [shape=circle,color=orange,style=filled,label="",width=0.15,height=0.15]
''')
        for i in range(len(top_activities)):
            top_activities_file.write('%s -> %s [color=black] ;\n'%(top_activities[i][0],top_activities[i][1]))
        top_activities_file.write('}')
        top_activities_file.close()

    else:                    
        count = 0
        top_activities_file = open(sys.argv[2]+'TOP_'+str(TOP_N)+'_activities.txt', "w")
        top_actors_odeg_file  = open(sys.argv[2]+'TOP_'+str(TOP_N)+'_actors_odeg.csv', "w")
        top_1st_hop_nighbors_file = open(sys.argv[2]+'TOP_'+str(TOP_N)+'_1st_hop_neighbor.txt', "w")
        f = open(sys.argv[1], "r")
        for line in f:
            splits = line.split()
            sender = long(splits[0].strip())
            recv = long(splits[1].strip())
            timestamp = long(splits[2].strip())
            
            if sender in top_actors:
                if recv not in top_actors:
                    top_actors[recv] = 0
                top_actors[sender] += 1
                top_activities.append((sender, recv, timestamp))
                if sender in top_influencers:
                    top_influencers[sender].append(recv)
            count += 1
            if count%CLR_MEM_THRESH == 0:
                writer = csv.writer(top_activities_file,  delimiter=' ', quoting=csv.QUOTE_MINIMAL)
                writer.writerows(top_activities)
                top_activities = []
        
        writer = csv.writer(top_activities_file,  delimiter=' ', quoting=csv.QUOTE_MINIMAL)
        writer.writerows(top_activities)
        top_activities_file.close()
    
        for an_actor in top_actors:
            top_actors_odeg_file.write('%s,%s\n'%(an_actor,top_actors[an_actor]))
        top_actors_odeg_file.close()
    
        for an_infl in top_influencers:
            top_1st_hop_nighbors_file.write('%s'%(an_infl))
            for a_1st_hop in top_influencers[an_infl]:
                top_1st_hop_nighbors_file.write(' %s'%(a_1st_hop))
            top_1st_hop_nighbors_file.write('\n')
        top_1st_hop_nighbors_file.close()
#infl_activities(each_user)
#size_i = i
#for i in range(size_i+1,len(top_activities)):
#    top_activities_file.write('%s -> %s [color=red] ;\n'%(top_activities[i][0],top_activities[i][1]))
