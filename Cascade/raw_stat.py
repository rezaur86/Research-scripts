import sys, gc
import array
import operator
from sets import Set
from random import choice

CLR_THRESHOLD = 500000
f = open(sys.argv[1], "r")
lifespan_stat_file = open(sys.argv[3]+"lifespan_stat.csv", "w")
raw_outdeg_stat_file = open(sys.argv[3]+"raw_outdeg_stat.csv", "w")
raw_indeg_stat_file = open(sys.argv[3]+"raw_indeg_stat.csv", "w")
lifespan_bin_size = int(sys.argv[2])
lifespan_stat = {}
raw_outdeg = {}
raw_indeg = {}
raw_indeg[0] = 0
count = 0
for line in f:
    element = line.split(' ')
    node_id = int(element[0].strip())
    born_time = int(element[1].strip())
    activation_time = int(element[2].strip())
    last_seen_time = int(element[3].strip())
    is_leaf = bool(int(element[4].strip()))
    odeg = int((element[5].strip()))
    parent_list = element[6:len(element)]
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
        lifespan = last_seen_time - activation_time
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