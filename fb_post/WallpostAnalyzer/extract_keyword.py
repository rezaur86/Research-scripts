import os, sys
from topia.termextract import extract
import psycopg2
sys.path.append(os.path.abspath('../jsonParser/'))
from db_connection import openDb, closeDB


def extract_keyword(text):
    extractor = extract.TermExtractor()
    extractor.filter = extract.DefaultFilter(singleStrengthMinOccur=3, noLimitStrength=2)
    keywords = sorted(extractor(text))
    i = 0
    for each_keyword in keywords:
        i = i + 1
        print each_keyword
    print i 
    return keywords

def read_story (group_id, by_post = False):
    try:
        (conn, cursor) = openDb(True, True)
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
        
        if by_post == False:
            story = ''
            cut_off = 100
            cursor.execute('select row_id, id, parent_message_row_id, name, text, description, caption from message where fb_wall_row_id = %s', (group_row_id,))
            if cursor.rowcount > 0:
                for record_t in cursor:
                    record = list(record_t)
                    for i in range(len(record)):
                        if record[i] is None:
                            record[i] = ''
                    story = story + str(record[3]) + ' ' + str(record[5]) + ' ' + str(record[6]) + ':' + str(record[4]) + '\n'
                    if (cut_off < 0):
                         break
                    else:
                        cut_off -= 1
            else:
                return None
        else:
            stories = []
            post_row_ids = []
            cursor.execute('select row_id, id, parent_message_row_id, name, text, description, caption from message where fb_wall_row_id = %s and parent_message_row_id is null', (group_row_id,))
            post_count = cursor.rowcount
            if post_count > 0:
                for record_t in cursor:
                    record = list(record_t)
                    post_row_ids.append(record_t[0])
                    for i in range(len(record)):
                        if record[i] is None:
                            record[i] = ''
                    stories.append(str(record[3]) + ' ' + str(record[5]) + ' ' + str(record[6]) + ':' + str(record[4]) + '\n')
                for i in range(post_count):
                    cursor.execute('select row_id, id, parent_message_row_id, name, text, description, caption from message where parent_message_row_id = %s', (post_row_ids[i],))
                    for record_t in cursor:
                        record = list(record_t)
                        for i in range(len(record)):
                            if record[i] is None:
                                record[i] = ''
                        stories[i] = stories[i] + str(record[3]) + ' ' + str(record[5]) + ' ' + str(record[6]) + ':' + str(record[4]) + '\n'
            else:
                return None
            
        closeDB(conn, cursor)
        if (by_post == False):
            return story
        else:
            return stories
    except psycopg2.Error, e:
        closeDB(conn, cursor)
        print ("*********Database************Error %s: %s\n" % (e.pgcode, e.pgerror))
        return -1

story = read_story(153774071379194)
print story
raw_input()
keywords = extract_keyword(story)
 