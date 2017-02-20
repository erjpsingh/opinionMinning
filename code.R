# install the basic packages
install.packages("ROAuth")
install.packages("twitteR")
install.packages("stringr")


"Now lets implement a modular approach"

#load necessary libraries
load.libraries <- function(){
  library("ROAuth")
  library("twitteR")
  library("stringr")
}

#function to establish connection with Twitter
establish.connection <- function(){
  consumer_key <-'86HF6yYj3hGwpdGVFOmlnsUeX'
  consumer_secret <-'PYjVFZitzBsrr1wfp1QRzidXVa6OOqEbAET4ri4HBfIu1HIS8Q'
  access_token <-'1111878264-X8NAa8lknRcoVwUcyeiE9RwMAJg6qiJCJ87WOmY'
  access_secret <-'kLd90mJmv2jGYvvRXowVnsz8Sl6p1G7xTZwSUzMLMtWxZ' 
  
  setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
}

#function to fetch tweets and prepare them
fetch.prepare.tweets <- function(keyword,num_tweets){
  tweets <- searchTwitter(keyword, num_tweets, lang = "en")
  #preparing tweets
  tweets_noReTweets<- strip_retweets(tweets, strip_manual = TRUE, strip_mt = TRUE) #eleminating re tweets
  tweets_text <<- sapply(tweets_noReTweets, function(x)x$getText())  # converting to text
  tweets_text <<- gsub("@\\w+", "", tweets_text)       # Removing all twitter handle
  tweets_text <<- gsub("[[:digit:]]", "", tweets_text) # Removing all the digits
  tweets_text <<- gsub("[[:punct:]]", "", tweets_text) # removing All the punctuation
  tweets_text <<- gsub("http\\w+", "", tweets_text)    # removing http link
  tweets_text <<- sapply(tweets_text, tolower)         # to lower chrachters
  tweets_text <<- iconv(tweets_text, 'UTF-8', 'ASCII') # converting to ASCII and hence removing emoji.
  tweets_text <<- tweets_text[!is.na(tweets_text)]     # removing any missing values if they exist
  names(tweets_text) <- NULL                          # removing the names of Array
  
  return(tweets_text)
}

#function to read necessary files
read.files <- function(){
  pos <<- scan('positive-words.txt', what = 'character', comment.char = ';')
  neg <<- scan('negative-words.txt', what = 'character', comment.char = ';')
  
}

#function analyse positive and negative opinions

  positive.negative.opiniion <- function(refined.tweets, pos, neg){
    words.list <<- str_split(refined.tweets, " ")
    w <<- unlist(words.list)
    pos.matches <<- match(w , pos)
    neg.matches <<- match(w, neg)
    
    total.pos <<- sum(table(pos.matches))
    total.neg <<- sum(table(neg.matches))
    barplot(c(total.pos, total.neg), ylim = c(0,500), names.arg = c("Positive" , "Negative"), main = "Opinion Minning")
    return( (total.pos - total.neg)*100 / (total.pos + total.neg) )
    
    
  }
  
  
  
  
  ##################################################################################################
  # trying to make histogram of words.
  tbl <- table(pos.matches)
  hdr <- as.integer(names(tbl))
  barplot(rev(tbl), names.arg = pos[hdr], horiz = TRUE)
  
  #Simple barplot of positive and negative opinions
  barplot(c(total.pos, total.neg), ylim = c(0,500))
  



