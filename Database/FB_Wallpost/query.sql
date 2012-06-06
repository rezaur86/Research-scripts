select message.id, user.id, user.name from user,message,message_to where user.row_id = message.from_user_row_id and message_to.message_row_id = message.row_id and message_to.to_user_row_id = (select row_id from user where user.id = 89579399804) into outfile "/tmp/who_posts.txt" FIELDS TERMINATED BY '\t' LINES TERMINATED BY '\n';

select post.id, comments.id, user.id, user.name from user, message as post, message as comments where user.row_id = comments.from_user_row_id and post.row_id = comments.parent_message_row_id and post.row_id in (select message_row_id from message_to where to_user_row_id = (select row_id from user where user.id = 89579399804)) 
into outfile "/tmp/who_posts.txt" FIELDS TERMINATED BY '\t' LINES TERMINATED BY '\n';
union select message.id, Null, user.id, user.name from user,message,message_to where user.row_id = message.from_user_row_id and message_to.message_row_id = message.row_id and message_to.to_user_row_id = (select row_id from user where user.id = 89579399804)

