DROP TABLE tag;
DROP TABLE message_to;
DROP TABLE likedby;
DROP TABLE message;
DROP TABLE fb_user;
CREATE TABLE fb_user (
	row_id BIGSERIAL,
	id VARCHAR(100) NOT NULL,
	name VARCHAR(1000),
	category VARCHAR(100),
	PRIMARY KEY (row_id),
	UNIQUE(id)
);

CREATE TABLE message (
	row_id BIGSERIAL,
	id VARCHAR(100) NOT NULL,
	parent_message_row_id INT,
	name VARCHAR(3000),
	text TEXT,
	type VARCHAR(300),
	description TEXT,
	caption VARCHAR(3000),
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
	row_id BIGSERIAL,
	message_row_id INT NOT NULL,
	to_user_row_id INT NOT NULL,
	PRIMARY KEY (row_id),
	FOREIGN KEY (to_user_row_id) REFERENCES fb_user(row_id) ON UPDATE CASCADE ON DELETE RESTRICT,
	FOREIGN KEY (message_row_id) REFERENCES message(row_id) ON UPDATE CASCADE ON DELETE RESTRICT
);

CREATE TABLE likedby (
	row_id BIGSERIAL,
	what_message_row_id INT NOT NULL,
	who_user_row_id INT NOT NULL,
	PRIMARY KEY (row_id),
	FOREIGN KEY (who_user_row_id) REFERENCES fb_user(row_id) ON UPDATE CASCADE ON DELETE RESTRICT,
	FOREIGN KEY (what_message_row_id) REFERENCES message(row_id) ON UPDATE CASCADE ON DELETE RESTRICT
);

CREATE TABLE tag (
	row_id BIGSERIAL,
	message_row_id INT NOT NULL,
	user_row_id INT NOT NULL,
	type VARCHAR(30),
	starting_offset INT,
	length INT,	
	PRIMARY KEY (row_id),
	FOREIGN KEY (user_row_id) REFERENCES fb_user(row_id) ON UPDATE CASCADE ON DELETE RESTRICT,
	FOREIGN KEY (message_row_id) REFERENCES message(row_id) ON UPDATE CASCADE ON DELETE RESTRICT
);

