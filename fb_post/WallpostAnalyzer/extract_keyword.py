import os, sys
import re
from topia.termextract import extract
import psycopg2
sys.path.append(os.path.abspath('../jsonParser/'))
from db_connection import openDb, closeDB


def extract_keyword(text):
    extractor = extract.TermExtractor()
    extractor.filter = extract.DefaultFilter(singleStrengthMinOccur=3, noLimitStrength=2)
    keywords = sorted(extractor(text))
    return keywords

def read_story (group_id, by_post = False):
    try:
        (conn, cursor) = openDb(True, True)
        (conn_1, cursor_1) = openDb(True, True)        
    except psycopg2.Error, e:
        print "Error %d: %s" % (e.pgcode, e.pgerror)
        return -1
    try:
        cursor.execute('select row_id, name from fb_user where id = %s', (group_id,))
        if cursor.rowcount > 0:
            group_info = cursor.fetchone()
            group_row_id = group_info[0]
            name = group_info[1]
        else:
            return None
        global keyword_post_row_id
        if by_post == False:
            story = ''
            cut_off = 100
            cursor.execute('''select string_agg(name||', '||description||', '||caption||' : '||text,E'\n') from message where fb_wall_row_id = %s''', (group_row_id,))
            if cursor.rowcount > 0:
                story = cursor.fetchone()[0]
            else:
                return None
        else:
            stories = []
            post_row_ids = []
            cursor.execute('''select row_id from message where fb_wall_row_id = %s and parent_message_row_id is null''', (group_row_id,))
            keyword_freq_in_post = {}
            for record in cursor:
                print record[0]
                cursor_1.execute('''select string_agg(ARRAY_TO_STRING(ARRAY[name,description,caption,text], ', '),E'\n') from message where row_id = %s or parent_message_row_id = %s''', (record[0],record[0]))
                if cursor_1.rowcount > 0:
                    story = cursor_1.fetchone()[0]
                    print story
                    story_keywords = extract_keyword(story)
                    i = 0
                    for each_keyword in story_keywords:
                        keyword_row_id = register_keywords(" ".join(re.split("[^a-zA-Z]+", each_keyword[0])))
                        keyword_post_row_id += 1
                        new_keyword_post.append((keyword_post_row_id, keyword_row_id, record[0], each_keyword[1]))    
                        i += 1
                    print i        
                else:
                    continue
        closeDB(conn, cursor)
        closeDB(conn_1, cursor_1)
        if (by_post == False):
            return story
        else:
            return stories
    except psycopg2.Error, e:
        closeDB(conn, cursor)
        closeDB(conn_1, cursor_1)
        print ("*********Database************Error %s: %s\n" % (e.pgcode, e.pgerror))
        return -1

def register_keywords(a_keyword):
    global all_keywords
    global keyword_row_id
    if all_keywords.has_key(a_keyword):
        return all_keywords[a_keyword]
    else:
        keyword_row_id += 1
        new_keywords.append((keyword_row_id, a_keyword))
        all_keywords[a_keyword] = keyword_row_id
        return keyword_row_id
 
def init_extraction():
    try:
        (conn, cursor) = openDb(True, True)
    except psycopg2.Error, e:
        print "Error %d: %s" % (e.pgcode, e.pgerror)
        return -1
    try:
        global keyword_row_id
        global all_keywords
        global keyword_post_row_id
        cursor.execute('select max(row_id) from keyword')
        keyword_row_id = cursor.fetchone()[0]
        if keyword_row_id is None:
            keyword_row_id = 0
        cursor.execute('select row_id, word from keyword')
        if cursor.rowcount > 0:
            for record in cursor:
                all_keywords[record[1]] = record[0]

        cursor.execute('select max(row_id) from keyword_post')
        keyword_post_row_id = cursor.fetchone()[0]
        if keyword_post_row_id is None:
            keyword_post_row_id = 0
    except psycopg2.Error, e:
        closeDB(conn, cursor)
        print ("*********Database************Error %s: %s\n" % (e.pgcode, e.pgerror))
        return -1    

def push_new_data():
    try:
        (conn, cursor) = openDb(True, True)
    except psycopg2.Error, e:
        print "Error %d: %s" % (e.pgcode, e.pgerror)
        return -1
    try:
        cursor.executemany('insert into keyword(row_id, word) values (%s, %s)', new_keywords)
        cursor.executemany('insert into keyword_post(row_id, keyword_row_id, post_row_id, freq) values (%s, %s, %s, %s)', new_keyword_post)
        closeDB(conn, cursor)
    except psycopg2.Error, e:
        closeDB(conn, cursor)
        print ("*********Database************Error %s: %s\n" % (e.pgcode, e.pgerror))
        return -1    
    
groups = sys.argv[1].split(',')

new_keywords = []
all_keywords = {}
keyword_row_id = 0
keyword_post_row_id = 0
new_keyword_post = []
init_extraction()

for each_group in groups:
    read_story(each_group, True)

push_new_data()