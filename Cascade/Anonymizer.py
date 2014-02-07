import sys, os
import csv

#Biggest FB ID = 9223372036854775807
users = {}
user_count = 0

for each_app in sys.argv:
    if each_app == sys.argv[0]:
        continue
    each_app_anonym = each_app + '_anonymized_seq'
    try:
        os.stat(each_app_anonym)
    except:
        os.makedirs(each_app_anonym)
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
            sender_fb_id = long(splits[0].strip())
            if users.has_key(sender_fb_id) == False:
                users[sender_fb_id] = user_count
                user_count += 1
            sender = str(users[sender_fb_id])
            recv_fb_id = long(splits[1].strip())
            if users.has_key(recv_fb_id) == False:
                users[recv_fb_id] = user_count
                user_count += 1
            recv = str(users[recv_fb_id])            
            timestamp = splits[2].strip()
            symid = splits[3].strip()
            anonymized_data.append((sender, recv, timestamp, symid))
        anonymized_file = open(each_app_anonym+'/'+each_file, "wb")
        writer = csv.writer(anonymized_file, quoting=csv.QUOTE_MINIMAL, delimiter=' ')
        writer.writerows(anonymized_data)
        anonymized_file.close()
        print each_file
    
    w = csv.writer(open(each_app_anonym+'/'+"user_seq.txt", "w"))
    for user_id, seq in users.items():
        w.writerow([user_id, seq])