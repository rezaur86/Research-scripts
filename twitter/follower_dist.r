library ("rjson")
follower_followee_file <- "/home/rezaur/Downloads/all_followers_followees.txt"
follower <- list()
input <- readLines(follower_followee_file, n=-1)
for (i in 1:length(input)){
	json_data <- fromJSON(input[i])
	for (key in names(json_data)){
		follower[[key]] = json_data[[key]]$'followers'
	}
} 

unique_user_file <- "/home/rezaur/Downloads/final_unique_user.txt"
user <- readLines(unique_user_file, n=-1)

for (i in 1:length(user)){
	follower[user[i]]
}
