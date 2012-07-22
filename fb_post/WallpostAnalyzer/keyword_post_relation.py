import os, sys
import psycopg2
sys.path.append(os.path.abspath('../jsonParser/'))
from db_connection import openDb, closeDB

def init_keywords():
    try:
        (conn, cursor) = openDb(True, True)
    except psycopg2.Error, e:
        print "Error %d: %s" % (e.pgcode, e.pgerror)
        return -1
    try:
        cursor.execute('select row_id, word from keyword')
        if cursor.rowcount > 0:
            for record in cursor:
                keywords[record[1]] = record[0]
    except psycopg2.Error, e:
        closeDB(conn, cursor)
        print ("*********Database************Error %s: %s\n" % (e.pgcode, e.pgerror))
        return -1

def read_post(group_id):
    try:
        cursor.execute('select row_id, name from fb_user where id = %s', (group_id,))
        if cursor.rowcount > 0:
            group_info = cursor.fetchone()
            group_row_id = group_info[0]
            name = group_info[1]
        else:
            return None

        cursor.execute('select max(row_id) from keyword_post')
        if cursor.rowcount < 1:
            keyword_post_row_id = 0
        else:
            keyword_post_row_id = cursor.fetchone()[0]
        
        for each_word in keywords.keys():
            cursor.execute('select row_id, parent_message_row_id from message where fb_wall_row_id = %s and text ~ %s', (group_id, each_word))
            keyword_freq_in_post = {}
            for record in cursor:
                post_row_id = -1
                if record[1] is None:
                    post_row_id = record[0]
                else:
                    post_row_id = record[1]
                if keyword_freq_in_post.has_key(post_row_id):
                    keyword_freq_in_post[post_row_id] += 1
                else:
                    keyword_freq_in_post[post_row_id] = 1
                    
            for each_post in keyword_freq_in_post.keys():
                keyword_post_row_id += 1
                keyword_post.append((keyword_post_row_id, keywords[each_word], keyword_freq_in_post[each_post])) 
            
        closeDB(conn, cursor)
    except psycopg2.Error, e:
        closeDB(conn, cursor)
        print ("*********Database************Error %s: %s\n" % (e.pgcode, e.pgerror))
        return -1
    

groups = sys.argv[1].split(',')
keywords = {}
keyword_post = []
init_keywords()
for each_group in groups:
    