import sys, os
import csv

#Biggest FB ID = 9223372036854775807
users = {}
for key, val in csv.reader(open("hugged_user_seq.txt")):
    ismile_user_seq = -1
    iheart_user_seq = -1
    users[key] = (val, ismile_user_seq, iheart_user_seq)
print "hugged done"
for key, val in csv.reader(open("ismile_user_seq.txt")):
    hugged_user_seq = -1
    iheart_user_seq = -1
    if key in users:
        hugged_user_seq = users[key][0]
    users[key] = (hugged_user_seq, val, iheart_user_seq)
print "ismile done"
for key, val in csv.reader(open("iheart_user_seq.txt")):
    if key in users:
        hugged_user_seq = users[key][0]
        ismile_user_seq = users[key][1]
    else:
        continue
    users[key] = (hugged_user_seq, ismile_user_seq, val)
print "iheart done"

deleted_keys = {}
for key in users:
    if users[key][1] == -1 and users[key][2] == -1:
        deleted_keys[key] = None
    if users[key][0] == -1 and users[key][2] == -1:
        deleted_keys[key] = None

w = csv.writer(open("all_user_seq.txt", "w"))
for user_id, (hugged_user_seq, ismile_user_seq, iheart_user_seq) in users.items():
    if user_id not in deleted_keys:
        w.writerow([user_id, hugged_user_seq, ismile_user_seq, iheart_user_seq])