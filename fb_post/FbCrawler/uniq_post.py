# find the unique postid in Whitney Houst FB group

infile = open("posts.txt","r")
outfile = open("postid2.txt","w")

postid_list = {}
count = 0
count1 = 0

for line in infile:
    line = line.replace("\n", "")
    count += 1
    
    if count%2 != 0:
        outfile.write(str(line)+"\t")

    if count%2 == 0:
        if line not in postid_list:
            postid_list[line] = 1
            count1 += 1
            outfile.write(str(line)+"\t"+str(count1)+"\n")

"""
for key in postid_list.keys():
    outfile.write(str(key)+"\n")

print count
print count1
print len(postid_list)
"""