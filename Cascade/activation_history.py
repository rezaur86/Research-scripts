import sys, gc, csv
import array
import operator
from sets import Set
from random import choice

CLR_THRESHOLD = 500000

iheart_users = {}

# for user_id, hugged_user_seq, ismile_user_seq, iheart_user_seq in csv.reader(open("user_seq/all_user_seq.txt")):
for user_id, hugged_user_seq, ismile_user_seq, iheart_user_seq, gender, x, locale in csv.reader(open("users_gender_locale.txt")):
    if int(iheart_user_seq) != -1:
        iheart_users[int(iheart_user_seq)] = (user_id, hugged_user_seq, ismile_user_seq, gender, x, locale, None, None)
    else:
        continue

count = 0
f = open("iheart_prep_all_gift_sorted.txt", "r")
for line in f:
    count = count+1
    if (count % (CLR_THRESHOLD/10)) == 0:
        print "iheart", count
    element = line.split(' ')
    node_id = int(element[0].strip())
    activation_time = int(element[2].strip())
    is_leaf = int(element[5].strip())
    if int(node_id) not in iheart_users:
        continue
    user_id = iheart_users[int(node_id)][0]
    hugged_user_seq = iheart_users[int(node_id)][1]
    ismile_user_seq = iheart_users[int(node_id)][2]
    gender = iheart_users[int(node_id)][3]
    x = iheart_users[int(node_id)][4]
    locale = iheart_users[int(node_id)][5]
    iheart_users[int(node_id)] = (user_id, hugged_user_seq, ismile_user_seq, gender, x, locale, is_leaf, activation_time)

activation_history_file = open("activation_history.txt", "w")
activation_history = []
for each_iheart_seq in iheart_users:
    if iheart_users[each_iheart_seq][6] == None:
        continue
    activation_history_file.write('%s,%s,%s,%s,%s,%s,%s,%s,%s\n' %(
                            iheart_users[each_iheart_seq][0],
                            iheart_users[each_iheart_seq][1],
                            iheart_users[each_iheart_seq][2],
                            each_iheart_seq,
                            iheart_users[each_iheart_seq][3],
                            iheart_users[each_iheart_seq][4],
                            iheart_users[each_iheart_seq][5],
                            iheart_users[each_iheart_seq][6],
                            iheart_users[each_iheart_seq][7]))
activation_history_file.close()
