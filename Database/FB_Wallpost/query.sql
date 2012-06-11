select message.id, user.id, user.name from user,message,message_to where user.row_id = message.from_user_row_id and message_to.message_row_id = message.row_id and message_to.to_user_row_id = (select row_id from user where user.id = 89579399804) into outfile "/tmp/who_posts.txt" FIELDS TERMINATED BY '\t' LINES TERMINATED BY '\n';

select post.id, comments.id, user.id, user.name from user, message as post, message as comments where user.row_id = comments.from_user_row_id and post.row_id = comments.parent_message_row_id and post.row_id in (select message_row_id from message_to where to_user_row_id = (select row_id from user where user.id = 89579399804)) 
into outfile "/tmp/who_posts.txt" FIELDS TERMINATED BY '\t' LINES TERMINATED BY '\n';
union select message.id, Null, user.id, user.name from user,message,message_to where user.row_id = message.from_user_row_id and message_to.message_row_id = message.row_id and message_to.to_user_row_id = (select row_id from user where user.id = 89579399804)


create table as select fb_user.id as user_id, message.fb_wall_row_id as group_id from fb_user JOIN message JOIN likedby ON fb_user.row_id = message.from_user_row_id 
	and message.row_id = likedby.what_message_row_id and fb_user.row_id = who_fb_user_row_id where count(group_id) > 1 group by user_id;

create table participate as (
select message.from_user_row_id as fb_user_row_id, message.fb_wall_row_id as group_row_id from message
UNION
select likedby.who_user_row_id as fb_user_row_id, message.fb_wall_row_id as group_row_id from likedby JOIN message on likedby.what_message_row_id = message.row_id
);

create table participate_count as (
select fb_user.id as user_id, count(participate.group_row_id) as participation_count from fb_user JOIN participate ON fb_user.row_id = participate.fb_user_row_id
	group by user_id having count(participate.group_row_id) > 1 order by participation_count desc
);

