import sys, os
import csv
import operator

users = {}
adoptions = {}

def register_activity(sender_id, recv_id):
    global users, adoptions
    if sender_id not in users:
        adoption_seed = sender_id
        sender_depth = 0
        users[sender_id] = (adoption_seed, sender_depth)
        cascade_size = 1
        seed_height = 0
        adoptions[adoption_seed] = (cascade_size, seed_height)
    else:
        adoption_seed = users[sender_id][0]
        sender_depth = users[sender_id][1]
        cascade_size = adoptions[adoption_seed][0]
        seed_height = adoptions[adoption_seed][1]
    if recv_id not in users:
        users[recv_id] = (adoption_seed, sender_depth+1)
        adoptions[adoption_seed] = (cascade_size+1, max(seed_height,
                                                         sender_depth+1))

each_app = sys.argv[1]
file_list = sorted(os.listdir(each_app))
activity_line = 0
for each_file in file_list:
    f = open(each_app+'/'+each_file, "r")
    lines = f.readlines()
    f.close()
    anonymized_data = []
    for line in lines:
        activity_line += 1
        splits = line.split()
        sender_id = long(splits[0].strip())
        recv_id = long(splits[1].strip())
        register_activity(sender_id, recv_id)
    print each_file
    print activity_line

adoption_size = {}
adoption_depth = {}
for each_seed in adoptions:
    each_cascade = adoptions[each_seed]
    if each_cascade[0] in adoption_size:
        adoption_size[each_cascade[0]] += 1
    else:
        adoption_size[each_cascade[0]] = 1
    if each_cascade[1] in adoption_depth:
        adoption_depth[each_cascade[1]] += 1
    else:
        adoption_depth[each_cascade[1]] = 1
w = csv.writer(open("size.csv", "w"))
temp_size = sorted(adoption_size.iteritems(), key=operator.itemgetter(0), reverse=True)
for (size, count) in temp_size:
    w.writerow([size, count, 63072000])
w = csv.writer(open("depth.csv", "w"))
temp_depth = sorted(adoption_depth.iteritems(), key=operator.itemgetter(0), reverse=True)
for (depth, count) in temp_depth:
    w.writerow([depth, count, 63072000])