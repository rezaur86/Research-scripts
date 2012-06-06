import fnmatch
import shutil
import tarfile
from time import gmtime, strftime
import sys
import os
import csv
import psycopg2

from json_describer import *
from db_connection import openDb, closeDB
import json_describer

compress_mode = 'r:gz'
compress_file_format = r'*.tar.gz'
json_file_pattern = r'0*.json'

csv_writing_mode = "w"

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

def write_CSV():
    fb_user_file = open (sys.argv[1]+"fb_user.csv", csv_writing_mode)
    message_file = open (sys.argv[1]+"message.csv", csv_writing_mode) 
    message_to_file = open (sys.argv[1]+"message_to.csv", csv_writing_mode) 
    likedby_file = open (sys.argv[1]+"likedby.csv", csv_writing_mode) 
    tag_file = open (sys.argv[1]+"tag.csv", csv_writing_mode)
    link_file =  open (sys.argv[1]+"link.csv", csv_writing_mode)
    
    writer = csv.writer(fb_user_file, quoting=csv.QUOTE_MINIMAL)
    writer.writerows(json_describer.new_fb_users)
    writer = csv.writer(message_file ,quoting=csv.QUOTE_MINIMAL)
    writer.writerows(json_describer.new_messages)
    writer = csv.writer(message_to_file ,quoting=csv.QUOTE_MINIMAL)
    writer.writerows(json_describer.new_message_tos)
    writer = csv.writer(likedby_file ,quoting=csv.QUOTE_MINIMAL)
    writer.writerows(json_describer.new_likedbys)
    writer = csv.writer(tag_file ,quoting=csv.QUOTE_MINIMAL)
    writer.writerows(json_describer.new_tags)
    writer = csv.writer(link_file ,quoting=csv.QUOTE_MINIMAL)
    writer.writerows(json_describer.new_links)
    
    fb_user_file.close()
    message_file.close() 
    message_to_file.close() 
    likedby_file.close() 
    tag_file.close() 
    link_file.close()

# Initialize variables by reading from Database

def data_initializer ():
    try:
        (conn, cursor) = openDb(True, True)
    except psycopg2.Error, e:
        print "Error %d: %s" % (e.pgcode, e.pgerror)
        return -1
    try:
        cursor.execute('select row_id, id from fb_user')
        if cursor.rowcount > 0:
            for record in cursor:
                json_describer.fb_user_ids[record[1]] = record[0]
        else:
            return 0

        cursor.execute('select row_id, id from message')
        if cursor.rowcount > 0:
            for record in cursor:
                json_describer.message_ids[record[1]] = record[0]
        else:
            return 0

        cursor.execute('select max(fb_wall_row_id) from message')
        json_describer.fb_wall_last_value = cursor.fetchone()[0]
        
        cursor.execute('select max(row_id) from fb_user')
        json_describer.fb_user_last_value = cursor.fetchone()[0]
        
        cursor.execute('select max(row_id) from message')
        json_describer.message_last_value = cursor.fetchone()[0]
        
        cursor.execute('select max(row_id) from message_to')
        json_describer.message_to_last_value = cursor.fetchone()[0]

        cursor.execute('select max(row_id) from likedby')
        json_describer.likedby_last_value = cursor.fetchone()[0]
        
        cursor.execute('select max(row_id) from tag')
        json_describer.tag_last_value = cursor.fetchone()[0]

        cursor.execute('select max(row_id) from link')
        json_describer.link_last_value = cursor.fetchone()[0]

        closeDB(conn, cursor)
        return 1
    except psycopg2.Error, e:
        closeDB(conn, cursor)
        ERROR_FILE.write("*********Database************Error %s: %s\n" % (e.pgcode, e.pgerror))
        return -1

if(len(sys.argv) > 2):
    print(strftime("%a, %d %b %Y %H:%M:%S +0000", gmtime()))
    data_initializer()
    json_describer.LOG_FILE = open(sys.argv[1]+"logfile.txt", "w").close() #To empty the file
    json_describer.LOG_FILE = open (sys.argv[1]+"logfile.txt", "a")
    json_describer.ERROR_FILE = open(sys.argv[1]+"errorfile.txt", "w").close() #To empty the file
    json_describer.ERROR_FILE = open (sys.argv[1]+"errorfile.txt", "a")
    for root, dirs, files in os.walk(sys.argv[1]):
        if sys.argv[2] == '-e':
            for each_file in files:
                if fnmatch.fnmatch(each_file, compress_file_format):
                    shutil.rmtree(os.path.join(sys.argv[1],'ex_temp'))
                    shutil.rmtree(os.path.join(sys.argv[1],'temp'))
                    print each_file
                    tar = tarfile.open(os.path.join(sys.argv[1],each_file), compress_mode)
                    tar.extractall(os.path.join(sys.argv[1],'ex_temp'))
                    tar.close()
                    os.mkdir(os.path.join(sys.argv[1],'temp'))
                    recursive_directory_content_move(os.path.join(sys.argv[1],'ex_temp'),os.path.join(sys.argv[1],'temp'))
                    list_of_posts = os.listdir(os.path.join(sys.argv[1],'temp'))
                    for each_post in list_of_posts:
                        json_describer.working_json_file_name = each_file + '/' + each_post
                        json_strings = open (os.path.join(sys.argv[1], 'temp', each_post))
                        parse_this_post(json_strings)
                    shutil.rmtree(os.path.join(sys.argv[1], 'ex_temp'))
                    shutil.rmtree(os.path.join(sys.argv[1],'temp'))
        else:
            # This part doesn't work since I am planning to work with compressed data only
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
    json_describer.LOG_FILE.close()
    json_describer.ERROR_FILE.close()
    write_CSV()
    print(strftime("%a, %d %b %Y %H:%M:%S +0000", gmtime()))
else:
    print('usage: python path {-e,-d}\n -e:for tar files auto extraction\n -d: already extracted json directories')
