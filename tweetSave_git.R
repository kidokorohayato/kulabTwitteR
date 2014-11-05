tweetSave <- function(word){
	setwd("/Users/kidokorohayato/R/twitteR/R/all")
	tweet <- searchTwitter(word, n=1000, lang="ja")
	if(file.exists(word)){
		setwd(word)
	}else{
	dir.create(word)
	setwd(word)
	}
	name_dir <- paste(word, Sys.Date())
	if(!file.exists(name_dir))dir.create(name_dir)
		setwd(name_dir)
	text <- NULL
	for(i in 1:length(tweet)){
		text <- rbind(text, tweet[[i]]$text)
			}
		name_file <- paste(word, Sys.time())
		write.table(text, file=name_file)
		setwd("/Users/kidokorohayato/R/twitteR/R/all")
		setwd(word)	
		write(text, file="all", append=T)
		setwd("/Users/kidokorohayato/R/twitteR/R/all/all_txt")
		name_file <- paste(word, "_all.txt", sep="")
		write(text, file=name_file, append=T)
}

tweetSave("FOREVER21")
tweetSave("ZARA")
tweetSave("GAP")
tweetSave("H&M")
tweetSave("UNIQLO")	