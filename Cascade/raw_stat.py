import sys, gc
import array
import operator
from sets import Set
from random import choice

CLR_THRESHOLD = 500000
f = open(sys.argv[1], "r")
lifespan_stat_file = open(sys.argv[3]+"lifespan_stat.csv", "w")
lifespan_bin_size = int(sys.argv[2])
lifespan_stat = {}
count = 0
for line in f:
    element = line.split(' ')
    node_id = int(element[0].strip())
    born_time = int(element[1].strip())
    activation_time = int(element[2].strip())
    last_seen_time = int(element[3].strip())
    is_leaf = bool(int(element[4].strip()))
    odeg = int((element[5].strip()))
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
    count = count+1
    if (count % (CLR_THRESHOLD/10)) == 0:
        print count
f.close()

temp = sorted(lifespan_stat.iteritems(), key=operator.itemgetter(0), reverse=True)
for tuple in temp:
    lifespan_stat_file.write('%s,%s\n'%(tuple[0],tuple[1]))

lifespan_stat_file.close()