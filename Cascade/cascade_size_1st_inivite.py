import sys, os
from sets import Set
import csv
import array
import operator
import threading
import time
import math
from bitarray import bitarray
from multiprocessing import Pool, Process, Value, Array
from multiprocessing.dummy import Manager

def CascadeWorker(activities, pool_of_seeds, vertices, process_id):
    cascade_sizes = []
    proc_seeds_count = len(pool_of_seeds)
    pool_last_index = 0
    batch_size = 10000
    while pool_last_index < proc_seeds_count:
        print pool_last_index*100/proc_seeds_count,'% of process',process_id.value
        seeds_batch = array.array('L')
        target_seeds = {}
        for i in range(batch_size):
            if pool_last_index + i == proc_seeds_count:
                break
            new_seed_act_id = pool_of_seeds[pool_last_index + i]
            if i == 0:
                min_seed_act_time = new_seed_act_id
            seeds_batch.append(activities[2*new_seed_act_id+0])
            target_seeds[new_seed_act_id] = i
        pool_last_index += batch_size
        cascade_sizes.extend(CascadeBuilder(activities, vertices, min_seed_act_time, seeds_batch, target_seeds))
    o_seeds_sizes =  open ('o_p'+str(process_id.value), "w")
    writer = csv.writer(o_seeds_sizes, quoting=csv.QUOTE_MINIMAL)
#    cascade_sizes.sort(key=operator.itemgetter(1), reverse=True)
    writer.writerows(cascade_sizes)
    o_seeds_sizes.close()

def CascadeBuilder(activities, vertices, min_seed_act_time, seeds_batch, target_seeds):
    participation = {}
    participation_at_depth = {}
    seeds_reg = target_seeds.copy()
    target_seeds = {}
    target_seeds_depth = {}
    cascade_sizes = []
    activities_count = len(activities)/2

    for i in range(min_seed_act_time, activities_count):
        if seeds_reg.has_key(i) == True:
            if participation.has_key(activities[2*i+0]) == False:
                participation[activities[2*i+0]] = Set([seeds_reg[i]])
                participation_at_depth[activities[2*i+0]] = {}
                participation_at_depth[activities[2*i+0]][seeds_reg[i]] = 0
                target_seeds[activities[2*i+0]] = 0
                target_seeds_depth[activities[2*i+0]] = 0
                
            if vertices.has_key(activities[2*i+1]):
                if participation.has_key(activities[2*i+1]) == False:
                    participation[activities[2*i+1]] = participation[activities[2*i+0]].copy()
                    participation_at_depth[activities[2*i+1]] = participation_at_depth[activities[2*i+0]].copy()
                    for each_seed in participation_at_depth[activities[2*i+1]].keys():
                        participation_at_depth[activities[2*i+1]][each_seed] += 1
                else:
                    participation[activities[2*i+1]] = participation[activities[2*i+1]].union(participation[activities[2*i+0]])
                    for each_seed in participation[activities[2*i+1]]:
                        if participation_at_depth[activities[2*i+1]].has_key(each_seed) and participation_at_depth[activities[2*i+0]].has_key(each_seed):
                            participation_at_depth[activities[2*i+1]][each_seed] = max(participation_at_depth[activities[2*i+1]][each_seed], participation_at_depth[activities[2*i+0]][each_seed]) + 1
                        else:
                            if participation_at_depth[activities[2*i+0]].has_key(each_seed):
                                participation_at_depth[activities[2*i+1]][each_seed] = participation_at_depth[activities[2*i+0]][each_seed] + 1
            else:
                for seed_v in participation[activities[2*i+0]]:
                    target_seeds[seeds_batch[seed_v]] += 1
                    target_seeds_depth[seeds_batch[seed_v]] = max(target_seeds_depth[seeds_batch[seed_v]],participation_at_depth[activities[2*i+0]][seed_v]+1)

        else:
            if participation.has_key(activities[2*i+0]) == True:
                if vertices.has_key(activities[2*i+1]):
                    if participation.has_key(activities[2*i+1]) == False:
                        participation[activities[2*i+1]] = participation[activities[2*i+0]].copy()
                        participation_at_depth[activities[2*i+1]] = participation_at_depth[activities[2*i+0]].copy()
                        for each_seed in participation_at_depth[activities[2*i+1]].keys():
                            participation_at_depth[activities[2*i+1]][each_seed] += 1
                    else:
                        participation[activities[2*i+1]] = participation[activities[2*i+1]].union(participation[activities[2*i+0]])
                        for each_seed in participation[activities[2*i+1]]:
                            if participation_at_depth[activities[2*i+1]].has_key(each_seed) and participation_at_depth[activities[2*i+0]].has_key(each_seed):
                                participation_at_depth[activities[2*i+1]][each_seed] = max(participation_at_depth[activities[2*i+1]][each_seed], participation_at_depth[activities[2*i+0]][each_seed]) + 1
                            else:
                                if participation_at_depth[activities[2*i+0]].has_key(each_seed):
                                    participation_at_depth[activities[2*i+1]][each_seed] = participation_at_depth[activities[2*i+0]][each_seed] + 1
                else:
                    for seed_v in participation[activities[2*i+0]]:
                        target_seeds[seeds_batch[seed_v]] += 1
                        target_seeds_depth[seeds_batch[seed_v]] = max(target_seeds_depth[seeds_batch[seed_v]],participation_at_depth[activities[2*i+0]][seed_v]+1)
                    
    for v_participates in participation.keys():
        for seed_v in participation[v_participates]:
            target_seeds[seeds_batch[seed_v]] += 1
        for seed_v in participation_at_depth[v_participates].keys():
            target_seeds_depth[seeds_batch[seed_v]] = max(target_seeds_depth[seeds_batch[seed_v]],participation_at_depth[v_participates][seed_v])
    for a_seed in target_seeds.keys():
        cascade_sizes.append([a_seed, target_seeds[a_seed], target_seeds_depth[a_seed]])
    participation = None
    participation_at_depth = None
    target_seeds = None
    target_seeds_depth = None
    return cascade_sizes

process_count = 12
vertices = {}
shared_activities = None
if __name__ == '__main__':
    activities = array.array('L')
    seen_users = bitarray(2**29-1)
    seen_users.setall(False)
    manager = Manager()
    vertices = manager.dict()
    pool_of_seeds = array.array('L')
    ODEG = 0
    INDEG = 1
    f = open(sys.argv[1], "r")
    seeds_count = 0
    activities_count = 0
    for line in f:
        splits = line.split()
        sender = long(splits[0].strip())
        recv = long(splits[1].strip())
        timestamp = long(splits[2].strip())
        if (seen_users[sender] == False or seen_users[recv] == False):
            activities.append(sender)
            activities.append(recv)
            activities.append(timestamp)
            activities_count += 1
            seen_users[sender] = True
            seen_users[recv] = True
            
#        if vertices.has_key(sender):
#            vertices[sender][ODEG] += 1
#        else:
#            vertices[sender] = [1,0] #outdeg = 1, indeg = 0
#            pool_of_seeds.append(activities_count)
#            seeds_count += 1
#        if vertices.has_key(recv):
#            vertices[recv][INDEG] += 1
#        else:
#            vertices[recv] = [0,1] #outdeg = 0, indeg = 1
#        
#        activities.append(sender)#[sender, recv]) # time of this activity is activities_count 
#        activities.append(recv)
#        activities_count += 1
#    print len(vertices.keys())
#    for key in vertices.keys():
#        if vertices[key][ODEG] == 0 and vertices[key][INDEG] == 1:
#            del vertices[key]
#    print len(vertices.keys())
    f.close()
    f = open("1st_activity.txt", "w")
    print activities_count
    for i in range(activities_count):
        f.write('%s %s %s\n'%(activities[3*i+0],activities[3*i+1],activities[3*i+2]))
    f.close()
#    print seeds_count
#    shared_activities = manager.Array('L', activities, lock=False)
##    activities = None
#
#    process_list = []
#    output_files = []
#    for k in range(process_count):
#        target_seeds = array.array('L')
#        for s_i in range(seeds_count):
#            if s_i%process_count == k:
#                target_seeds.append(pool_of_seeds[s_i])
#        p = Process(target=CascadeWorker, args=(shared_activities, Array('L', target_seeds, lock = False), vertices, Value('i', k)))
#        target_seeds = None
#        process_list.append(p)
#        p.start()
#        output_files.append('o_p'+str(k))
##    vertices = None
#    pool_of_seeds = None
#    for p in process_list:
#        p.join()
#print "Exiting Main Thread"
#cascade_sizes = []
#o_seeds_sizes =  open (sys.argv[2], "w")
#for o_file in output_files:
#    file = open(o_file, 'r')
#    data = file.read()
#    file.close()
#    os.remove(o_file)
#    o_seeds_sizes.write(data)
#    #'\n'???
##writer = csv.writer(o_seeds_sizes, quoting=csv.QUOTE_MINIMAL)
##cascade_sizes.sort(key=operator.itemgetter(1), reverse=True)
##writer.writerows(cascade_sizes)
#o_seeds_sizes.close()