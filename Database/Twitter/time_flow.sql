set search_path = twitter;
drop table status_time;
create table status_time (
	child NUMERIC (40,0),
	parent NUMERIC (40,0),
	time INT
);

drop table flow_table;
create table flow_table (
	flow_num INT,
	child NUMERIC (40,0),
	parent NUMERIC (40,0)
);

\copy flow_table (flow_num,child,parent) from '/home/rezaur/Documents/SoCom/shoeb_flowtable.txt' DELIMITER ',' NULL as 'null';
\copy flow_table (child,parent,time) from '/home/rezaur/Documents/SoCom/shoeb_allstatus.txt' DELIMITER ',' NULL as 'null';

create index idx_status_time on status_time using btree(child, parent);
create index idx_flow_table on flow_table using btree(child, parent);

\copy (select flow_table.flow_num, status_time.time from flow_table INNER JOIN status_time ON flow_table.child = status_time.child and flow_table.parent = status_time.parent) to '/home/rezaur/tmp/rezaur_db/flow_time.txt' with DELIMITER as ',';

drop table flow_time;
create table flow_time (
	flow_num int,
	time int	
);

\copy flow_time (flow_num,time) from '/tmp/flow_time.txt' DELIMITER ',' NULL as 'null';

\copy (select flow_table.flow_num, status_time.time from flow_table INNER JOIN status_time ON flow_table.parent is NULL and status_time.parent is NULL and flow_table.child = status_time.child) to '/tmp/significant_flow.txt' with DELIMITER as ',';

drop table significant_flow;
create table significant_flow(
	flow_num int,
	time int
);

\copy significant_flow (flow_num,time) from '/tmp/significant_flow.txt' DELIMITER ',' NULL as 'null';

\copy (select flow_time.flow_num, flow_time.time from flow_time INNER JOIN significant_flow ON flow_time.flow_num = significant_flow.flow_num order by flow_time.flow_num ASC, flow_time.time ASC) to '/tmp/sorted_flow_time.txt' with DELIMITER as ',';

create table significant_flow_without_null (
	flow_num int,
	time int	
);

\copy significant_flow_without_null (flow_num,time) from '/tmp/sorted_flow_time.txt' DELIMITER ',' NULL as 'null';
insert into significant_flow_without_null select * from significant_flow;

\copy (select * from significant_flow_without_null order by flow_num ASC, time ASC) to '/tmp/sorted_flow_time.txt' with DELIMITER as ',';

drop table uid_mid;
create table uid_mid (
	row_id INT NOT NULL AUTO_INCREMENT,
	uid_mid varchar (50),
	PRIMARY KEY (row_id)
);


insert into uid_mid (uid_mid) select DISTINCT child from status_time;


SELECT flow_num, child, parent INTO OUTFILE '/home/rezaur/tmp/flow_table.csv'
FIELDS TERMINATED BY ','
ESCAPED BY '\\'
LINES TERMINATED BY '\n'
FROM flow_table;

SELECT child, parent, time INTO OUTFILE '/home/rezaur/tmp/status_time.csv'
FIELDS TERMINATED BY ','
ESCAPED BY '\\'
LINES TERMINATED BY '\n'
FROM status_time;

