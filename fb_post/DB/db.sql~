DROP TABLE tag;
DROP TABLE message_to;
DROP TABLE likedby;
DROP TABLE message;
DROP TABLE fb_user;
DROP TABLE fb_page;

DROP SCHEMA fb_wallpost cascade;
create schema fb_wallpost;
set search_path to fb_wallpost;

CREATE TABLE fb_user (
	row_id BIGINT,
	id BIGINT NOT NULL,
	name TEXT,
	category VARCHAR(100),
	PRIMARY KEY (row_id),
	UNIQUE(id)
);

CREATE TABLE message (
	row_id BIGINT,
	id VARCHAR(100) NOT NULL,
	parent_message_row_id INT,
	fb_wall_row_id BIGINT,
	name TEXT,
	text TEXT,
	type VARCHAR(300),
	description TEXT,
	caption TEXT,
	created_time TIMESTAMP WITH TIME ZONE,
	updated_time TIMESTAMP WITH TIME ZONE,
	from_user_row_id INT NOT NULL,
	can_remove BOOL,
	shares_count INT,
	likes_count INT,
	comments_count INT,
	PRIMARY KEY (row_id),
	FOREIGN KEY (parent_message_row_id) REFERENCES message(row_id) ON UPDATE CASCADE ON DELETE RESTRICT,
	FOREIGN KEY (from_user_row_id) REFERENCES fb_user(row_id) ON UPDATE CASCADE ON DELETE RESTRICT,
	FOREIGN KEY (fb_wall_row_id) REFERENCES fb_user(row_id) ON UPDATE CASCADE ON DELETE RESTRICT,
	UNIQUE(id)
);

CREATE TABLE message_to (
	row_id BIGINT,
	message_row_id INT NOT NULL,
	to_user_row_id INT NOT NULL,
	PRIMARY KEY (row_id),
	FOREIGN KEY (to_user_row_id) REFERENCES fb_user(row_id) ON UPDATE CASCADE ON DELETE RESTRICT,
	FOREIGN KEY (message_row_id) REFERENCES message(row_id) ON UPDATE CASCADE ON DELETE RESTRICT
);

CREATE TABLE likedby (
	row_id BIGINT,
	what_message_row_id INT NOT NULL,
	who_user_row_id INT NOT NULL,
	PRIMARY KEY (row_id),
	FOREIGN KEY (who_user_row_id) REFERENCES fb_user(row_id) ON UPDATE CASCADE ON DELETE RESTRICT,
	FOREIGN KEY (what_message_row_id) REFERENCES message(row_id) ON UPDATE CASCADE ON DELETE RESTRICT
);

CREATE TABLE tag (
	row_id BIGINT,
	message_row_id INT NOT NULL,
	user_row_id INT NOT NULL,
	type VARCHAR(30),
	starting_offset INT,
	length INT,	
	PRIMARY KEY (row_id),
	FOREIGN KEY (user_row_id) REFERENCES fb_user(row_id) ON UPDATE CASCADE ON DELETE RESTRICT,
	FOREIGN KEY (message_row_id) REFERENCES message(row_id) ON UPDATE CASCADE ON DELETE RESTRICT
);

CREATE TABLE link (
	row_id BIGINT,
	message_row_id INT NOT NULL,
	address TEXT,
	type varchar(10),
	name TEXT,
	text TEXT,
	FOREIGN KEY (message_row_id) REFERENCES message(row_id) ON UPDATE CASCADE ON DELETE RESTRICT
);

CREATE TABLE keyword (
	row_id BIGINT,
	word TEXT	
);

CREATE TABLE keyword_post (
	row_id BIGINT,
	keyword_row_id BIGINT,
	post_row_id BIGINT,
	freq INT
);

alter table fb_user drop CONSTRAINT fb_user_pkey cascade;
alter table fb_user drop CONSTRAINT fb_user_id_key cascade;
alter table message drop CONSTRAINT message_id_key cascade;
alter table message drop CONSTRAINT message_pkey cascade;
alter table message_to drop CONSTRAINT message_to_pkey cascade;
alter table likedby drop CONSTRAINT likedby_pkey cascade;
drop index likedby_what_message_row_id_idx cascade;
alter table tag drop CONSTRAINT tag_pkey cascade;
alter table link drop CONSTRAINT link_message_row_id_fkey cascade;
drop index link_message_row_id_idx cascade;
drop index message_parent_message_row_id_idx;

alter table keyword drop CONSTRAINT keyword_row_id_pkey cascade;
drop index keyword_post_keyword_row_id_idx;


\COPY fb_user(row_id, id, name, category) from '/home/rezaur/Documents/fb_user.csv' with delimiter ',' CSV quote '"';
\COPY message(row_id, id, parent_message_row_id, fb_wall_row_id, name, text, type, description, caption, from_user_row_id, created_time, updated_time, can_remove, shares_count, likes_count, comments_count) from '/home/rezaur/Documents/message.csv' with delimiter ',' CSV quote '"';
\COPY message_to(row_id, message_row_id, to_user_row_id) from '/home/rezaur/Documents/message_to.csv' with delimiter ',' CSV quote '"';
\COPY likedby(row_id, what_message_row_id, who_user_row_id) from '/home/rezaur/Documents/likedby.csv' with delimiter ',' CSV quote '"';
\COPY tag(row_id, message_row_id, user_row_id, type, starting_offset, length) from '/home/rezaur/Documents/tag.csv' with delimiter ',' CSV quote '"';
\COPY link(row_id, message_row_id, address, type, name, text) from '/home/rezaur/Documents/link.csv' with delimiter ',' CSV quote '"';

\COPY keyword(row_id, word) from '/home/rezaur/Documents/keyword.csv' with delimiter ',' CSV quote '"';
\COPY keyword_post(row_id, keyword_row_id, post_row_id, freq) from '/home/rezaur/Documents/keyword_post.csv' with delimiter ',' CSV quote '"';


alter table fb_user add CONSTRAINT fb_user_pkey PRIMARY KEY (row_id);
alter table fb_user add CONSTRAINT fb_user_id_key UNIQUE(id);

alter table message add CONSTRAINT message_pkey PRIMARY KEY (row_id);
alter table message add CONSTRAINT message_id_key UNIQUE(id);
alter table message add CONSTRAINT message_fb_wall_row_id_fkey FOREIGN KEY (fb_wall_row_id) REFERENCES fb_user(row_id) ON UPDATE CASCADE ON DELETE RESTRICT;
alter table message add CONSTRAINT message_from_user_row_id_fkey FOREIGN KEY (from_user_row_id) REFERENCES fb_user(row_id) ON UPDATE CASCADE ON DELETE RESTRICT;
alter table message add CONSTRAINT message_parent_message_row_id_fkey FOREIGN KEY (parent_message_row_id) REFERENCES message(row_id) ON UPDATE CASCADE ON DELETE RESTRICT;
create index message_parent_message_row_id_idx on message using btree (parent_message_row_id);

alter table message_to add CONSTRAINT message_to_pkey PRIMARY KEY (row_id);
alter table message_to add CONSTRAINT message_to_message_row_id_fkey FOREIGN KEY (message_row_id) REFERENCES message(row_id) ON UPDATE CASCADE ON DELETE RESTRICT;
alter table message_to add CONSTRAINT message_to_to_user_row_id_fkey FOREIGN KEY (to_user_row_id) REFERENCES fb_user(row_id) ON UPDATE CASCADE ON DELETE RESTRICT;

alter table likedby add CONSTRAINT likedby_pkey PRIMARY KEY (row_id);
alter table likedby add CONSTRAINT likedby_what_message_row_id_fkey FOREIGN KEY (what_message_row_id) REFERENCES message(row_id) ON UPDATE CASCADE ON DELETE RESTRICT;
alter table likedby add CONSTRAINT likedby_who_user_row_id_fkey FOREIGN KEY (who_user_row_id) REFERENCES fb_user(row_id) ON UPDATE CASCADE ON DELETE RESTRICT;
create index likedby_what_message_row_id_idx on likedby using btree (what_message_row_id);

alter table tag add CONSTRAINT tag_pkey PRIMARY KEY (row_id);
alter table tag add CONSTRAINT tag_message_row_id_fkey FOREIGN KEY (message_row_id) REFERENCES message(row_id) ON UPDATE CASCADE ON DELETE RESTRICT;
alter table tag add CONSTRAINT tag_user_row_id_fkey FOREIGN KEY (user_row_id) REFERENCES fb_user(row_id) ON UPDATE CASCADE ON DELETE RESTRICT;

alter table link add CONSTRAINT link_row_id_pkey PRIMARY KEY (row_id);
create index link_message_row_id_idx on link using btree (message_row_id);

alter table keyword add CONSTRAINT keyword_row_id_pkey PRIMARY KEY (row_id);
alter table keyword_post add CONSTRAINT keyword_row_id_fkey FOREIGN KEY (keyword_row_id) REFERENCES keyword(row_id) ON UPDATE CASCADE ON DELETE RESTRICT;
create index keyword_post_keyword_row_id_idx on keyword_post using btree (keyword_row_id);

