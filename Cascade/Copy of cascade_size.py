import sys
from sets import Set
import csv
import array
import operator
import threading
import time
import math
from multiprocessing import Pool, Process, Value, Array

f = open(sys.argv[1], "r")

vertices = {}
v_ids = array.array('L')
vertex_count = 0
activities = array.array('L')
activities_count = 0
seeds = []
seeds_count = 0

pool_of_seeds = array.array('L')
pool_of_seeds_lock = threading.RLock()

cascade_sizes = []
cascade_sizes_lock = threading.RLock()

batch_size = 10000
thread_count = 24
thread_ids = []

ODEG = 0
INDEG = 1
# pre-processing: make lists of sender_ids and recipient_ids in the original graph
for line in f:
    splits = line.split()
    sender = long(splits[0].strip())
    recv = long(splits[1].strip())
    
    if vertices.has_key(sender):
        vertices[sender][ODEG] += 1
    else:
        vertices[sender] = [1,0] #outdeg = 1, indeg = 0
        pool_of_seeds.append(activities_count)
        seeds_count += 1
    if vertices.has_key(recv):
        vertices[recv][INDEG] += 1
    else:
        vertices[recv] = [0,1] #outdeg = 0, indeg = 1
    
    activities.append(sender)#[sender, recv]) # time of this activity is activities_count 
    activities.append(recv)
    activities_count += 1

#    if vertices.has_key(sender) == False:
#        vertices[sender] = vertex_count
#        sender_id = vertex_count
#        v_ids.append(sender)
#        seeds.append([sender_id, activities_count]) # activities_count is the time of it's self activation
#        pool_of_seeds.append(seeds_count)
#        seeds_count += 1
#        vertex_count += 1
#    else:
#        sender_id = vertices[sender]
#        vertices
#        
#    if vertices.has_key(recv) == False:
#        vertices[recv] = vertex_count
#        recv_id = vertex_count
#        v_ids.append(recv)
#        vertex_count = vertex_count + 1
#    else:
#        recv_id = vertices[recv]

#    if activities_count > 100:
#        break

#    activities.append([sender_id, recv_id]) # time of this activity is activities_count 
#    activities_count += 1
f.close()

print activities_count
print seeds_count

class CascadeThread (threading.Thread):
    def __init__(self, threadID, name):
        super(CascadeThread, self).__init__()
        self.threadID = threadID
        self.name = name
    def run(self):
        print "Starting " + self.name
        dummy(self.name)#CascadeWorker(self.name)
        print "Exiting " + self.name

def dummy(activities, activities_count):
    while(True):
        for i in range(activities_count):
            if activities[2*i+0] % 1001 == 100:
                A = math.exp(10)

def CascadeWorker(thread_name):
    to_do = True
    while to_do:
        seeds_batch = []
        with pool_of_seeds_lock:
            if len(pool_of_seeds) > 0:
                new_seed_act_id = pool_of_seeds.pop(0)
            else:
                to_do = False
        if to_do == False:
            break
        print '1 seed', thread_name 
#        seeds_batch[seeds[new_seed_id][0]] = 0
        seeds_batch.append(new_seed_act_id)
        batch_counter = batch_size - 1
#        min_seed_act_time = seeds[new_seed_id][1]
        min_seed_act_time = new_seed_act_id
        while batch_counter > 0:
            with pool_of_seeds_lock:
                if len(pool_of_seeds) > 0:
                    new_seed_act_id = pool_of_seeds.pop(0)
                else:
                    to_do = False
                    batch_counter = 0
            if batch_counter == 0:
                break
#            seeds_batch[seeds[new_seed_act_id][0]] = 0
            seeds_batch.append(new_seed_act_id)
            batch_counter -= 1
            print batch_counter,thread_name
        print 'CascadeWorker', thread_name  
        CascadeBuilder(min_seed_act_time, seeds_batch, thread_name)

def CascadeBuilder(min_seed_act_time, seeds_batch, thread_name):
    print 'CascadeBuilder', thread_name
    participation = {}
    participation_at_depth = {}
    target_seeds = {}
    target_seeds_depth = {}
    for i in range(min_seed_act_time, activities_count):
#        if target_seeds.has_key(activities[i][0]) == True:
        if i in seeds_batch:
            if participation.has_key(activities[i][0]) == False:
                participation[activities[i][0]] = Set([int(seeds_batch.index(i))])#activities[i][0]])
                participation_at_depth[activities[i][0]] = {}
                participation_at_depth[activities[i][0]][int(seeds_batch.index(i))] = 0 #activities[i][0]] = 0
                target_seeds[activities[i][0]] = 0
                target_seeds_depth[activities[i][0]] = 0
            if participation.has_key(activities[i][1]) == False:
                participation[activities[i][1]] = participation[activities[i][0]].copy()
                participation_at_depth[activities[i][1]] = participation_at_depth[activities[i][0]].copy()
                for each_seed in participation_at_depth[activities[i][1]].keys():
                    participation_at_depth[activities[i][1]][each_seed] += 1
            else:
                participation[activities[i][1]] = participation[activities[i][1]].union(participation[activities[i][0]])
                for each_seed in participation[activities[i][1]]:
                    if participation_at_depth[activities[i][1]].has_key(each_seed) and participation_at_depth[activities[i][0]].has_key(each_seed):
                        participation_at_depth[activities[i][1]][each_seed] = max(participation_at_depth[activities[i][1]][each_seed], participation_at_depth[activities[i][0]][each_seed]) + 1
                    else:
                        if participation_at_depth[activities[i][0]].has_key(each_seed):
                            participation_at_depth[activities[i][1]][each_seed] = participation_at_depth[activities[i][0]][each_seed] + 1
        else:
            if participation.has_key(activities[i][0]) == True:
                if participation.has_key(activities[i][1]) == False:
                    participation[activities[i][1]] = participation[activities[i][0]].copy()
                    participation_at_depth[activities[i][1]] = participation_at_depth[activities[i][0]].copy()
                    for each_seed in participation_at_depth[activities[i][1]].keys():
                        participation_at_depth[activities[i][1]][each_seed] += 1
                else:
                    participation[activities[i][1]] = participation[activities[i][1]].union(participation[activities[i][0]])
                    for each_seed in participation[activities[i][1]]:
                        if participation_at_depth[activities[i][1]].has_key(each_seed) and participation_at_depth[activities[i][0]].has_key(each_seed):
                            participation_at_depth[activities[i][1]][each_seed] = max(participation_at_depth[activities[i][1]][each_seed], participation_at_depth[activities[i][0]][each_seed]) + 1
                        else:
                            if participation_at_depth[activities[i][0]].has_key(each_seed):
                                participation_at_depth[activities[i][1]][each_seed] = participation_at_depth[activities[i][0]][each_seed] + 1
    for v_participates in participation.keys():
        for seed_v in participation[v_participates]:
            target_seeds[activities[seeds_batch[seed_v]][0]] += 1
        for seed_v in participation_at_depth[v_participates].keys():
            target_seeds_depth[activities[seeds_batch[seed_v]][0]] = max(target_seeds_depth[activities[seeds_batch[seed_v]][0]],participation_at_depth[v_participates][seed_v])
    for a_seed in target_seeds.keys():
        with cascade_sizes_lock:
            cascade_sizes.append([a_seed, target_seeds[a_seed], target_seeds_depth[a_seed]])
    print 'CascadeBuilder_Exit', thread_name

if __name__ == '__main__':
    process_list = []
    arr = activities
    for k in range(12):
        p = Process(target=dummy, args=(arr, activities_count))
        process_list.append(p)
        p.start()
    for p in process_list:
        p.join()

# Create threads
for i in range(thread_count):
    thread = CascadeThread(i, "Thread-"+str(i))
    thread_ids.append(thread)
    thread.start()    # Start new Threads

while True:
    if len(pool_of_seeds) > 0:
        print len(pool_of_seeds)*100/seeds_count,'%'#, pool_of_seeds[0:5]
        time.sleep(20)
    else:
        break
for a_thread in thread_ids:
    a_thread.join()
        
#pool_of_seeds.join()
print "Exiting Main Thread"
o_seeds_sizes =  open (sys.argv[2], "w")
writer = csv.writer(o_seeds_sizes, quoting=csv.QUOTE_MINIMAL)
cascade_sizes.sort(key=operator.itemgetter(1), reverse=True)
writer.writerows(cascade_sizes)
o_seeds_sizes.close()
