import sys, gc
import array
import operator
from sets import Set
from random import choice

CLR_THRESHOLD = 500000
f = open(sys.argv[1], "r")
lifespan_bin_size = int(sys.argv[4])
lifespan_stat = {}
raw_outdeg = {}
parent_count = {}
parent_count[0] = 0
count = 0
for line in f:
    element = line.split(' ')
    node_id = int(element[0].strip())
    born_time = int(element[1].strip())
    activation_time = int(element[2].strip())
    last_act_time = int(element[3].strip())
    last_seen_time = int(element[4].strip())
    is_leaf = bool(int(element[5].strip()))
    odeg = int((element[6].strip()))
    parent_list = element[7:len(element)]
    if is_leaf == False:
        if parent_list[0].strip() == '-1':
            parent_count[0] += 1
        else:
            indeg = len(parent_list)
            if indeg in parent_count:
                parent_count[indeg] += 1
            else:
                parent_count[indeg] = 1
    count = count+1
    if (count % (CLR_THRESHOLD/10)) == 0:
        print count
    if odeg in raw_outdeg:
        raw_outdeg[odeg] += 1
    else:
        raw_outdeg[odeg] = 1
    if is_leaf == False:
        lifespan = last_act_time - activation_time + 1
    else:
        lifespan = 0
        continue
    lifespan_bin = lifespan/lifespan_bin_size
    if(lifespan_bin in lifespan_stat):
        lifespan_stat[lifespan_bin] += 1
    else:
        lifespan_stat[lifespan_bin] = 1
f.close()

f = open(sys.argv[2], "r")
count = 0
indeg_before_act = {}
for line in f:
    element = line.split(',')
    node_id = int(element[0].strip())
    odeg = int(element[1].strip())
    indeg = int(element[2].strip())
    indeg_until_active = int(element[3].strip())
    act_time = int(element[4].strip())
    count = count+1
    if (count % (CLR_THRESHOLD/10)) == 0:
        print count
    if indeg_until_active == -1:
        continue
    if indeg_until_active in indeg_before_act:
        indeg_before_act[indeg_until_active] += 1
    else:
        indeg_before_act[indeg_until_active] = 1
f.close()

lifespan_stat_file = open(sys.argv[3]+"lifespan_stat.csv", "w")
raw_outdeg_stat_file = open(sys.argv[3]+"raw_outdeg_stat.csv", "w")
parent_count_before_act_file = open(sys.argv[3]+"parent_count_before_act.csv", "w")
indeg_before_act_file = open(sys.argv[3]+"indeg_before_act.csv", "w")

temp = sorted(lifespan_stat.iteritems(), key=operator.itemgetter(0), reverse=True)
for tuple in temp:
    lifespan_stat_file.write('%s,%s\n'%(tuple[0],tuple[1]))
temp = sorted(raw_outdeg.iteritems(), key=operator.itemgetter(0), reverse=True)
for tuple in temp:
    raw_outdeg_stat_file.write('%s,%s\n'%(tuple[0],tuple[1]))
temp = sorted(parent_count.iteritems(), key=operator.itemgetter(0), reverse=True)
for tuple in temp:
    parent_count_before_act_file.write('%s,%s\n'%(tuple[0],tuple[1]))
temp = sorted(indeg_before_act.iteritems(), key=operator.itemgetter(0), reverse=True)
for tuple in temp:
    indeg_before_act_file.write('%s,%s\n'%(tuple[0],tuple[1]))

lifespan_stat_file.close()
raw_outdeg_stat_file.close()
parent_count_before_act_file.close()
indeg_before_act_file.close()