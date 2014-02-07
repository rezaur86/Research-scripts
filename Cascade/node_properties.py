import sys, gc
import array
import operator
from sets import Set
from random import choice

CLR_THRESHOLD = 500000
f = open(sys.argv[1], "r")
node_properties_file = open(sys.argv[2]+"node_properties.csv", "w")
node_prop = {}
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
    if parent_list[0].strip() == '-1':
        raw_indeg[0] += 1
    else:
        indeg = len(parent_list)
        if indeg in raw_indeg:
            raw_indeg[indeg] += 1
        else:
            raw_indeg[indeg] = 1
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

temp = sorted(lifespan_stat.iteritems(), key=operator.itemgetter(0), reverse=True)
for tuple in temp:
    lifespan_stat_file.write('%s,%s\n'%(tuple[0],tuple[1]))
temp = sorted(raw_outdeg.iteritems(), key=operator.itemgetter(0), reverse=True)
for tuple in temp:
    raw_outdeg_stat_file.write('%s,%s\n'%(tuple[0],tuple[1]))
temp = sorted(raw_indeg.iteritems(), key=operator.itemgetter(0), reverse=True)
for tuple in temp:
    raw_indeg_stat_file.write('%s,%s\n'%(tuple[0],tuple[1]))

lifespan_stat_file.close()
raw_outdeg_stat_file.close()
raw_indeg_stat_file.close()