import sys, gc
import array, csv
import operator
from sets import Set
from random import choice
import numpy as np

NEVER = 2**31 - 1    
CLR_THRESHOLD = 500000
MAX_USERS = 2**29 - 1

lifespan_bin_size = int(sys.argv[4])
f = open(sys.argv[2], "r")
parent_alpha = array.array('f',(0,)*MAX_USERS)
for parent_id,alpha in csv.reader(open('/home/rezaur/output_cascade/raw_stat_v2/parent_proportion.csv')):
    parent_alpha[int(parent_id)] = float(alpha)
count = 0
act_life_vs_succ = {}
for line in f:
    element = line.split(',')
    node_id = int(element[0].strip())
    odeg = int(element[1].strip())
    indeg = int(element[2].strip())
    indeg_until_active = int(element[3].strip())
    act_lifespan = int(element[4].strip())
    adoption_delay = int(element[5].strip())
    sent_ARs = int(element[6].strip())
    act_lifespan_bin = (act_lifespan if act_lifespan > 0 else NEVER)/lifespan_bin_size
    if act_lifespan_bin < (NEVER/lifespan_bin_size):
        if act_lifespan in act_life_vs_succ:
            act_life_vs_succ[act_lifespan_bin].append(parent_alpha[node_id])
        else:
            act_life_vs_succ[act_lifespan_bin] = [parent_alpha[node_id]]
    count = count+1
    if (count % (CLR_THRESHOLD/10)) == 0:
        print count
f.close()

act_life_vs_avg_succ_ratio_file = open(sys.argv[3]+"act_life_vs_avg_succ_ratio.csv", "w")
for time in act_life_vs_succ:
    act_life_vs_avg_succ_ratio_file.write('%s,%s\n'%(time, round(np.average(act_life_vs_succ[time]), 3)))
act_life_vs_avg_succ_ratio_file.close()

