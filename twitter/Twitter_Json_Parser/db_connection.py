import psycopg2

db_name = 'rezaur_db'
db_server = 'localhost'
db_namespace = 'twitter'

def openDb(auto_commit=True, change_schema=True):
    con = psycopg2.connect(database=db_name, user='rezaur', password='acer1986', host=db_server, port=5432)
    con.autocommit = auto_commit
    cur = con.cursor()
    if change_schema:
        cur.execute('set search_path=%s,%s', (db_namespace, 'public'))
    return (con, cur)
