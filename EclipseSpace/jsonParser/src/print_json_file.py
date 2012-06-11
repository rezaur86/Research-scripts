import json
import sys

if(len(sys.argv) > 1):    
    open("printed.json", "w").close() #To empty the file
    temp = open("printed.json", "a") 
    FILE = open (sys.argv[1], "r")
    for oneline in FILE:
        print oneline
        if oneline != '\n':
            try:
                onedata = json.loads(oneline)
            except Exception as e:
                print("*********Json Loading at Json File************Error %s\n" % e.args[0]) 
            temp.write(json.dumps(onedata, sort_keys=True, indent = 4))
    FILE.close()
    temp.close()
else:
    print('Give a Json file name')

