import sys, os
import copy
from sets import Set
import csv
import array
from bitarray import bitarray
import operator
import numpy as np

MAX_USERS = 2**29 - 1
NO_PARENT = MAX_USERS + 1
NEVER = 2**31 - 1

if __name__ == '__main__':
    is_leaf = bitarray(MAX_USERS)
    is_leaf.setall(True)
    
    out_degree = [] #array.array('I')
    in_degree = [] #array.array('I')
    in_degree_until_active = [] #array.array('I')
    potential_parents = []
    born_time = [] #array.array('l')
    activation_time = [] #array.array('l')
    user_last_act_time = []
    gift_type_stat = {}
    sent_gifts = {}
    recved_gifts = {}
    
    file_list = sorted(os.listdir(sys.argv[1]))
    user_last_seen_time = array.array('l')
    activity_line = 0
    for each_file in file_list:
        f = open(sys.argv[1]+'/'+each_file, "r")
        for line in f:
            activity_line += 1
            splits = line.split()
            sender = long(splits[0].strip())
            recv = long(splits[1].strip())
            timestamp = long(splits[2].strip())
            hid = int(splits[3].strip())
            if hid in gift_type_stat:
                gift_type_stat[hid] += 1
            else:
                gift_type_stat[hid] = 1
#             if sender != -1:
#                 if sender in sent_gifts:
#                     if hid in sent_gifts[sender]:
#                         sent_gifts[sender][hid] += 1
#                     else:
#                         sent_gifts[sender][hid] = 1
#                 else:
#                     sent_gifts[sender] = {}
#                     sent_gifts[sender][hid] = 1
#             if recv != -1:
#                 if recv in recved_gifts:
#                     if hid in recved_gifts[recv]:
#                         recved_gifts[recv][hid] += 1
#                     else:
#                         recved_gifts[recv][hid] = 1
#                 else:
#                     recved_gifts[recv] = {}
#                     recved_gifts[recv][hid] = 1
        f.close()
        print each_file
    total_line = activity_line
    print 'Total line read: ', total_line
    #Gift type stat
    f = open(sys.argv[2]+'_gift_type_stat.txt', "w")
    for each_gift in gift_type_stat:
        f.write('%s,%s\n' %(each_gift,gift_type_stat[each_gift]))
    f.close()
#     f_stat = csv.writer(open(sys.argv[2]+'_gift_sent_stat.txt', "w"))
# #     f = csv.writer(open(sys.argv[2]+'_gift_sent.txt', "w"))
#     temp = sorted(sent_gifts.iteritems(), key=operator.itemgetter(0), reverse=False)
#     for tuple in temp:
#         f_stat.writerow([tuple[0], len(tuple[1]), max(tuple[1], key=tuple[1].get)])
# #         f.writerow(tuple[1])
#     f_stat = csv.writer(open(sys.argv[2]+'_gift_received_stat.txt', "w"))
# #     f = csv.writer(open(sys.argv[2]+'_gift_received.txt', "w"))
#     temp = sorted(recved_gifts.iteritems(), key=operator.itemgetter(0), reverse=False)
#     for tuple in temp:
#         f_stat.writerow([tuple[0], len(tuple[1]), max(tuple[1], key=tuple[1].get)])
# #         f.writerow(tuple[1])
