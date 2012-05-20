import json
import sys
import os
import dateutil.parser
from collections import deque
from db_connection import *
from psycopg2.extensions import adapt


#Return the row_id of the found user else -1 for not found or exception
def find_user (fb_id):
    try:
        cursor.execute('select row_id from fb_user where id = %s', (fb_id,))
        if cursor.rowcount > 0:
            row_id = cursor.fetchone()[0]
            return row_id
        else:
            return -1
    except psycopg2.Error, e:
        ERROR_FILE.write("*********Database************Error %s: %s\n" % (e.pgcode, e.pgerror))
        return -1
    
# Return row_id if already exists or successful, else return -1 if exception occurs
def register_user (jsonObj):
    print(json.dumps(jsonObj))
    if jsonObj.has_key("id"):
        fb_id = json.dumps(jsonObj["id"]).replace('\"','')
        row_id = find_user(fb_id)
        if row_id != -1:
            return row_id 
    else:
        return -1
    if jsonObj.has_key("name"):
        name = adapt(json.dumps(jsonObj["name"]).replace('\"',''))
    else:
        name = None
    if jsonObj.has_key("category"):
        category = adapt(json.dumps(jsonObj["category"]).replace('\"',''))
    else:
        category = None
    try:
        cursor.execute('insert into fb_user(id, name, category) values (%s, %s, %s)', (fb_id, name, category))
        cursor.execute('select last_value from fb_user_row_id_seq')
        insert_id = cursor.fetchone()[0]
        return insert_id#find_user(fb_id)
    except psycopg2.Error, e:
        ERROR_FILE.write("*********Database************Error %s: %s\n" % (e.pgcode, e.pgerror))
        return -1

def find_message (message_id):
    try:
        cursor.execute('select row_id from message where id = %s', (message_id,))
        if cursor.rowcount > 0:
            row_id = cursor.fetchone()[0]
            return row_id
        else:
            return -1
    except psycopg2.Error, e:
        ERROR_FILE.write("*********Database************Error %s: %s\n" % (e.pgcode, e.pgerror))
        return -1    
    
#parent_message is the Post on which the user commented, else None if this is the Post 
def register_message (onedata, parent_message_row_id):
    message_id = onedata["id"].replace('\"','')
    found_message_row_id = find_message(message_id)
    if found_message_row_id != -1:
        return found_message_row_id
    if onedata.has_key("name"):
        name = adapt(onedata["name"].replace('\"',''))
    else:
        name = None
    if onedata.has_key("message"):
        text = adapt(onedata["message"].replace('\"',''))
    else:
        text = None
    if onedata.has_key("type"):
        type_ = adapt(onedata["type"].replace('\"',''))
    else:
        type_ = None
    if onedata.has_key("description"):
        description = adapt(onedata["description"].replace('\"',''))
    else:
        description = None
    if onedata.has_key("caption"):
        caption = adapt(onedata["caption"].replace('\"',''))
    else:
        caption = None
    if onedata.has_key("created_time"):
        created_time = dateutil.parser.parse(onedata["created_time"].replace('\"',''))
    else:
        created_time = None
    if onedata.has_key("updated_time"):
        updated_time = dateutil.parser.parse(onedata["updated_time"].replace('\"',''))
    else:
        updated_time = None        
    if onedata.has_key("can_remove"):
        can_remove = onedata["can_remove"]
    else:
        can_remove = None
    if onedata.has_key("shares"):
        shares_count = onedata["shares"]["count"]
    else:
        shares_count = None
        
    if onedata.has_key("from"):
        postedBy_row_id = register_user(onedata["from"])
    try:
        cursor.execute('insert into message(id, parent_message_row_id, name, text, type, description, caption, from_user_row_id, created_time, updated_time, can_remove, shares_count) values (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)', 
                       (message_id, parent_message_row_id, name, text, type_, description, caption, postedBy_row_id, created_time, updated_time, can_remove, shares_count))
        cursor.execute('select last_value from message_row_id_seq')
        message_row_id = cursor.fetchone()[0]
    except psycopg2.Error, e:
        ERROR_FILE.write("*********Database************Error %s: %s\n" % (e.pgcode, e.pgerror))
    
    if onedata.has_key("to"):
        for each_to in onedata["to"]["data"]:
            if each_to is None:
                continue
            print json.dumps(each_to)
            postedTo_row_id = register_user(each_to)
            try:
                cursor.execute('insert into message_to(message_row_id, to_user_row_id) values (%s, %s)', 
                               (message_row_id, postedTo_row_id))
            except psycopg2.Error, e:
                ERROR_FILE.write("*********Database************Error %s: %s\n" % (e.pgcode, e.pgerror))

    if onedata.has_key("message_tags"):
        if type(onedata["message_tags"]) is list:
            if type(onedata["message_tags"][0]) is list:
                for each_tag in onedata["message_tags"][0]:
                    if each_tag is None:
                        continue
                    print (json.dumps(each_tag))
                    user_row_id = register_user(each_tag)
                    if each_tag.has_key("type"):
                        type_ = adapt(each_tag["type"].replace('\"',''))
                    else:
                        type_ = None
                    if each_tag.has_key("offset"):
                        offset = each_tag["offset"]
                    else:
                        offset = None
                    if each_tag.has_key("length"):
                        length = each_tag["length"]
                    else:
                        length = None
                    try:
                        cursor.execute('insert into tag(message_row_id, user_row_id, type, starting_offset, length) values (%s, %s, %s, %s, %s)', (message_row_id, user_row_id, type_, offset, length))
                    except psycopg2.Error, e:
                        ERROR_FILE.write("*********Database************Error %s: %s\n" % (e.pgcode, e.pgerror))
            else:
                for each_tag in onedata["message_tags"]:
                    if each_tag is None:
                        continue
                    print (json.dumps(each_tag))
                    user_row_id = register_user(each_tag)
                    if each_tag.has_key("type"):
                        type_ = adapt(each_tag["type"].replace('\"',''))
                    else:
                        type_ = None
                    if each_tag.has_key("offset"):
                        offset = each_tag["offset"]
                    else:
                        offset = None
                    if each_tag.has_key("length"):
                        length = each_tag["length"]
                    else:
                        length = None
                    try:
                        cursor.execute('insert into tag(message_row_id, user_row_id, type, starting_offset, length) values (%s, %s, %s, %s, %s)', (message_row_id, user_row_id, type_, offset, length))
                    except psycopg2.Error, e:
                        ERROR_FILE.write("*********Database************Error %s: %s\n" % (e.pgcode, e.pgerror))
                
        else:
            list_of_tag_keys = onedata["message_tags"].keys()
            print list_of_tag_keys
            for each_tag_key in list_of_tag_keys:
                for each_tag in onedata["message_tags"][each_tag_key]:
                    if each_tag is None:
                        continue
                    user_row_id = register_user(each_tag)
                    if each_tag.has_key("type"):
                        type_ = adapt(each_tag["type"].replace('\"',''))
                    else:
                        type_ = None
                    if each_tag.has_key("offset"):
                        offset = each_tag["offset"]
                    else:
                        offset = None
                    if each_tag.has_key("length"):
                        length = each_tag["length"]
                    else:
                        length = None
                    try:
                        cursor.execute('insert into tag(message_row_id, user_row_id, type, starting_offset, length) values (%s, %s, %s, %s, %s)', (message_row_id, user_row_id, type_, offset, length))
                    except psycopg2.Error, e:
                        ERROR_FILE.write("*********Database************Error %s: %s\n" % (e.pgcode, e.pgerror))
                                      
    return message_row_id
    

def describe_this (json_strings):
    likes_count = 0
    comments_count = 0
    likes_of_comments_count_queue = deque([])
    comment_message_row_id_queue = deque([])
    for oneline in json_strings:
        print oneline
        if oneline != '\n':
            try:
                onedata = json.loads(oneline)
            except Exception as e:
                ERROR_FILE.write("*********Json Loading at Json File %s************Error %s\n" % (working_json_file_name, e.args[0])) 
                return -1        
            FILE.write(json.dumps(onedata, sort_keys=True, indent = 4))
            if onedata is None:
                continue
            if onedata is False:
                continue
            if onedata.has_key("id"):
                message_row_id = register_message(onedata, None)
                if onedata.has_key("likes"):
                    likes_count = onedata["likes"]["count"]
                if onedata.has_key("comments"):
                    comments_count = onedata["comments"]["count"]
                                            
            else:
                if onedata.has_key("ep_likes"):
                    for each_like in onedata["ep_likes"]["data"]:
                        if likes_count > 0:
                            print likes_count
                            likes_count -= 1
                            print ('got a post like')
                            likedby = register_user(each_like)
                            try:
                                cursor.execute('insert into likedby(what_message_row_id, who_user_row_id) values (%s, %s)', 
                                               (message_row_id, likedby))
                            except psycopg2.Error, e:
                                ERROR_FILE.write("*********Database************Error %s: %s\n" % (e.pgcode, e.pgerror))
                    if onedata["ep_likes"].has_key("paging"):
                        if not onedata["ep_likes"]["paging"].has_key("next"):
                            if onedata["ep_likes"]["paging"].has_key("previous"):
                                if likes_count > 0:
                                    LOG_FILE.write('Inconsistency of %s likes at %s \n' %(likes_count, working_json_file_name))
                    else:
                        if likes_count > 0:
                            LOG_FILE.write('Inconsistency of %s likes at %s \n' %(likes_count, working_json_file_name))
                        
                            
                if onedata.has_key("ec_comments"):
                    for each_comment in onedata["ec_comments"]["data"]:
                        if comments_count > 0:
                            print comments_count
                            comments_count -= 1
                            if each_comment.has_key("likes"):
                                likes_of_comments_count_queue.append(each_comment["likes"])
                            else:
                                likes_of_comments_count_queue.append(0)
                            comment_message_row_id_queue.append(register_message(each_comment, message_row_id))
                    if onedata["ec_comments"].has_key("paging"):
                        if not onedata["ec_comments"]["paging"].has_key("next"):
                            if onedata["ec_comments"]["paging"].has_key("previous"):
                                if comments_count > 0:
                                    LOG_FILE.write('Inconsistency of %s comments at %s \n' %(comments_count, working_json_file_name))
                    else:
                        if comments_count > 0:
                            LOG_FILE.write('Inconsistency of %s comments at %s \n' %(comments_count, working_json_file_name))

                if onedata.has_key("ec_likes"):
                    print likes_of_comments_count_queue
                    try:
                        likes_of_a_comment_count = likes_of_comments_count_queue.popleft()
                        comment_message_row_id = comment_message_row_id_queue.popleft()
                        for each_like_on_comment in onedata["ec_likes"]["data"]:
                            if likes_of_a_comment_count > 0:
                                likes_of_a_comment_count -= 1
                                print ('got a comment like')
                                likedby = register_user(each_like_on_comment)
                                try:
                                    cursor.execute('insert into likedby(what_message_row_id, who_user_row_id) values (%s, %s)', 
                                                   (comment_message_row_id, likedby))
                                except psycopg2.Error, e:
                                    ERROR_FILE.write("*********Database************Error %s: %s\n" % (e.pgcode, e.pgerror))
                        if onedata["ec_likes"].has_key("paging"):
                            if onedata["ec_likes"]["paging"].has_key("next"):
                                likes_of_comments_count_queue.appendleft(likes_of_a_comment_count)
                                comment_message_row_id_queue.appendleft(comment_message_row_id)
                            else:
                                if onedata["ec_likes"]["paging"].has_key("previous"):
                                    if likes_of_a_comment_count > 0:
                                        LOG_FILE.write('Inconsistency of %s likes within comments at %s \n' %(likes_of_a_comment_count, working_json_file_name))
                        else:
                            if likes_of_a_comment_count > 0:
                                LOG_FILE.write('Inconsistency of %s likes within comments at %s \n' %(likes_of_a_comment_count, working_json_file_name))
                    except:
                        LOG_FILE.write('Inconsistency of %s likes within comments at %s \n' %(likes_of_a_comment_count, working_json_file_name))

conn = None
cursor = None
FILE = None
LOG_FILE = None
ERROR_FILE = None
working_json_file_name = None
try:
    (conn, cursor) = openDb(True, True)
    #cursor = conn.cursor()
    LOG_FILE = open(sys.argv[1]+"logfile.txt", "w").close() #To empty the file
    LOG_FILE = open (sys.argv[1]+"logfile.txt", "a")
    ERROR_FILE = open(sys.argv[1]+"errorfile.txt", "w").close() #To empty the file
    ERROR_FILE = open (sys.argv[1]+"errorfile.txt", "a")
    if(len(sys.argv) > 1):
        for root, dirs, files in os.walk(sys.argv[1]):
            os.chdir(sys.argv[1])
            for dir in dirs:
                print dir
                for root_next, dirs_next, files_next in os.walk(dir):
                    for each_file in files_next:
                        open(sys.argv[1]+"temp.json", "w").close() #To empty the file
                        FILE = open (sys.argv[1]+"temp.json", "a")
                        print (sys.argv[1]+dir+'/'+each_file)
                        json_strings = open (sys.argv[1]+dir+'/'+each_file)
                        working_json_file_name = dir+'/'+each_file
                        #raw_input('press a key')
                        describe_this(json_strings)
                        FILE.close()
    else:
        print('Give a path for Json file names')
    LOG_FILE.close()
    ERROR_FILE.close()    
    closeDB(conn, cursor)
except psycopg2.Error, e:
    closeDB(conn, cursor)
    print "Error %d: %s" % (e.pgcode, e.pgerror)
