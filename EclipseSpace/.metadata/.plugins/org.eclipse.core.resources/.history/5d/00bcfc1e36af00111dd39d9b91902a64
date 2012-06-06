import sys
import os
import fnmatch
import shutil
import tarfile

compress_mode = 'r:gz'
compress_file_format = r'*.tar.gz'
json_file_pattern = r'0*.json'

def recursive_directory_content_move(root, where_to_move):
    for each_entry in os.listdir(root):
        if os.path.isdir(os.path.join(root,each_entry)):
            recursive_directory_content_move(os.path.join(root,each_entry), where_to_move)
        else:
            try:
                if fnmatch.fnmatch(each_entry, json_file_pattern):
                    shutil.move(os.path.join(root,each_entry), os.path.join(where_to_move,each_entry))
            except shutil.Error, e:
                continue
            
if(len(sys.argv) > 2):
    for root, dirs, files in os.walk(sys.argv[1]):
        if sys.argv[2] == '-e':
            for each_file in files:
                if fnmatch.fnmatch(each_file, compress_file_format):
                    print each_file
                    tar = tarfile.open(os.path.join(sys.argv[1],each_file), compress_mode)
                    tar.extractall(os.path.join(sys.argv[1],'ex_temp'))
                    tar.close()
                    os.mkdir(os.path.join(sys.argv[1],'temp'))
                    recursive_directory_content_move(os.path.join(sys.argv[1],'ex_temp'),os.path.join(sys.argv[1],'temp')) 
                    raw_input()
                    shutil.rmtree(os.path.join(sys.argv[1], 'ex_temp'))
                    shutil.rmtree(os.path.join(sys.argv[1],'temp'))
        else:
            os.chdir(sys.argv[1])
            for dir in dirs:
                print dir
                entries = os.listdir(dir)
                tmp_dir = dir
                for each_entry in entries:
                    if os.path.isdir(os.path.join(tmp_dir,each_entry)):
                        os.rename(dir, 'temp')
                        tmp_dir = 'temp'
                        os.mkdir(dir)
                        recursive_directory_content_move(os.path.join(sys.argv[1], 'temp'), os.path.join(sys.argv[1], dir))
                        shutil.rmtree('temp')
                        break
                for each_file in os.listdir(dir):
                    json_strings = open (sys.argv[1]+dir+'/'+each_file)
else:
    print('usage: python path {-e,-d}\n -e:for tar files auto extraction\n -d: already extracted json directories')
