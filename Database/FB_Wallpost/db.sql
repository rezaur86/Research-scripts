DROP TABLE tag;
DROP TABLE message_to;
DROP TABLE likedby;
DROP TABLE message;
DROP TABLE fb_user;
DROP TABLE fb_page;

DROP SCHEMA fb_wallpost cascade;
create schema fb_wallpost;
set search_path to fb_wallpost;

CREATE TABLE fb_wall(
	row_id BIGINT,
	id BIGINT NOT NULL,
	name varchar(1000),
	link varchar(1000),
	likes INT,
	category varchar(100),
	website varchar(1000)
);

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
	fb_wall_row_id INT,
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
	PRIMARY KEY (row_id),
	FOREIGN KEY (from_user_row_id) REFERENCES fb_user(row_id) ON UPDATE CASCADE ON DELETE RESTRICT,
	FOREIGN KEY (parent_message_row_id) REFERENCES message(row_id) ON UPDATE CASCADE ON DELETE RESTRICT,
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
	name varchar(100),
	text varchar(200),
	FOREIGN KEY (message_row_id) REFERENCES message(row_id) ON UPDATE CASCADE ON DELETE RESTRICT
);


\COPY fb_user(row_id, id, name, category) from '/home/rezaur/Documents/fb_user.csv' with delimiter ',' CSV quote '"';
\COPY message(row_id, id, parent_message_row_id, fb_wall_row_id, name, type, description, caption, from_user_row_id, created_time, updated_time, can_remove, shares_count) from '/home/rezaur/Documents/message.csv' with delimiter ',' CSV quote '"';
\COPY message_to(row_id, message_row_id, to_user_row_id) from '/home/rezaur/Documents/message_to.csv' with delimiter ',' CSV quote '"';
\COPY likedby(row_id, what_message_row_id, who_user_row_id) from '/home/rezaur/Documents/likedby.csv' with delimiter ',' CSV quote '"';
\COPY tag(row_id, message_row_id, user_row_id, type, starting_offset, length) from '/home/rezaur/Documents/tag.csv' with delimiter ',' CSV quote '"';
\COPY link(row_id, message_row_id, address, type, name, text) from '/home/rezaur/Documents/link.csv' with delimiter ',' CSV quote '"';

