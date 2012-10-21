import sys, os
import array
count = 0
file_list = sorted(os.listdir(sys.argv[1]))
for each_file in file_list:
    f = open(sys.argv[1]+'/'+each_file, "r")
    lines=f.readlines()
    f.close()
    act_time = array.array('l')
    for line in lines:
        element = line.split(' ')
        element[2] = int(element[2].strip())
        act_time.append(element[2])
        count = count+1
        if (count % 10000) == 0:
            print 'still reading at %s'%count
    f.close()
    sorted_line_idx = sorted(range(len(act_time)), key = act_time.__getitem__)
    act_time = None
    f_sorted = open(sys.argv[2]+'/'+each_file, "w")
    for i in sorted_line_idx:
        f_sorted.writelines(lines[i])
    f_sorted.close()