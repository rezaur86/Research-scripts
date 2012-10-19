import MySQLdb
import MySQLdb.cursors
import time
import datetime
import csv, math
import operator

conn = MySQLdb.Connect(
    host='localhost', user='root',
    passwd='', db='hearts')
cursor = conn.cursor(cursorclass=MySQLdb.cursors.DictCursor)

batch_number = 0
batch = 100000
batch_count = math.ceil(2128369625/batch)
done = 1

results = {}

last_day = ""

while (batch_number < batch_count):
    cursor.execute("SELECT to_uid,from_uid,hid,performed_at FROM hearts WHERE id >= "+str(batch_number*batch+1)+" AND id < "+str((batch_number+1)*batch+1)+" order by performed_at")
    rows = cursor.fetchall()
    for row in rows:        
        rowtime = time.localtime(row["performed_at"])
        rowtimestr = time.strftime("%Y-%U-%w", rowtime)
        if (last_day != rowtimestr):
            last_day = rowtimestr
        try:
            results[rowtimestr].append((row["from_uid"],row["to_uid"],row["performed_at"],row["hid"]))
        except:
            results[rowtimestr] = [(row["from_uid"],row["to_uid"],row["performed_at"],row["hid"])]
                 
        done += 1
        if (done % 100000 == 0):
            print "::: ", done

    print results.keys()
    for a_day in results.keys():
        if a_day != last_day:
            o_file = open('traces'+a_day+'-hid.txt', 'a')
            writer = csv.writer(o_file, quoting=csv.QUOTE_MINIMAL,delimiter=' ')
            writer.writerows(results[a_day])
            o_file.close()
            del results[a_day]       
    batch_number = batch_number + 1 # on to the next

cursor.close()
conn.close()