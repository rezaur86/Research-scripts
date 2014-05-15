import sys, os
import csv
from tabulate import tabulate

#Biggest FB ID = 9223372036854775807
users = {}
HUG = 0
IHE = 1
ISM = 2
common_user_count = [[0,0,0],[0,0,0],[0,0,0]]
common_in_3_count = 0
for key, val in csv.reader(open("user_seq/hugged_user_seq.txt")):
    common_user_count[HUG][HUG] += 1
    ismile_user_seq = -1
    iheart_user_seq = -1
    users[key] = (val, ismile_user_seq, iheart_user_seq)
print "hugged done"
for key, val in csv.reader(open("user_seq/ismile_user_seq.txt")):
    common_user_count[ISM][ISM] += 1
    hugged_user_seq = -1
    iheart_user_seq = -1
    if key in users:
        common_user_count[HUG][ISM] += 1
        common_user_count[ISM][HUG] += 1
        hugged_user_seq = users[key][0]
    users[key] = (hugged_user_seq, val, iheart_user_seq)
print "ismile done"
for key, val in csv.reader(open("user_seq/iheart_user_seq.txt")):
    common_user_count[IHE][IHE] += 1
    if key in users:
        hugged_user_seq = users[key][0]
        ismile_user_seq = users[key][1]
        if hugged_user_seq != -1:
            common_user_count[IHE][HUG] += 1
            common_user_count[HUG][IHE] += 1
        if ismile_user_seq != -1:
            common_user_count[IHE][ISM] += 1
            common_user_count[ISM][IHE] += 1
        if hugged_user_seq != -1 and ismile_user_seq != -1:
            common_in_3_count += 1
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

print(common_in_3_count)
print(common_user_count)

print(1.0*common_user_count[IHE][HUG]/common_user_count[HUG][HUG]) #P(IHE|HUG)
print(1.0*common_user_count[HUG][IHE]/common_user_count[IHE][IHE]) #P(HUG|IHE)
print(1.0*common_user_count[ISM][HUG]/common_user_count[HUG][HUG]) #P(ISM|HUG)
print(1.0*common_user_count[HUG][ISM]/common_user_count[ISM][ISM]) #P(HUG|ISM)
print(1.0*common_user_count[ISM][IHE]/common_user_count[IHE][IHE]) #P(ISM|IHE)
print(1.0*common_user_count[IHE][ISM]/common_user_count[ISM][ISM]) #P(IHE|ISM)

w = csv.writer(open("common_users.csv", "w"))
w.writerows(common_user_count)

# 16476594
# [[34525693, 26461451, 17559491], [26461451, 199302275, 78044885], [17559491, 78044885, 92749770]]
# 0.766427802043
# 0.132770441281
# 0.508591992636
# 0.189321127158
# 0.391590537539
# 0.841456372345