# install the basic packages
install.packages("ROAuth")
install.packages("twitteR")

# load the respective packages
library("ROAuth")
library("twitteR")

# Fetching data from twitter
consumer_key <-'86HF6yYj3hGwpdGVFOmlnsUeX'
consumer_secret <-'PYjVFZitzBsrr1wfp1QRzidXVa6OOqEbAET4ri4HBfIu1HIS8Q'
access_token <-'1111878264-X8NAa8lknRcoVwUcyeiE9RwMAJg6qiJCJ87WOmY'
access_secret <-'kLd90mJmv2jGYvvRXowVnsz8Sl6p1G7xTZwSUzMLMtWxZ' 

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
AIB_tweets <- searchTwitter("@AllIndiaBakchod", n=1500, lang = "en")
# removing retweets
Aib_tweets_noReTweets<- strip_retweets(AIB_tweets, strip_manual = TRUE, strip_mt = TRUE)
#Exploring Tweets.
length(Aib_tweets_noReTweets)
class(Aib_tweets_noReTweets)

#Now fetching the text of these tweets.
AIB_tweets_text <- sapply(Aib_tweets_noReTweets, function(x)x$getText())
#We wil  have all of these as "chracter"
class(AIB_tweets_text)

#Removing unnecessary data
AIB_tweets_text <- AIB_tweets_text[!is.na(AIB_tweets_text)] # removing any missing values if they exist
AIB_tweets_text <- gsub("@\\w+", "", AIB_tweets_text)       # Removing all twitter handle
AIB_tweets_text <- gsub("[[:digit:]]", "", AIB_tweets_text) # Removing all the digits
AIB_tweets_text <- gsub("[[:punct:]]", "", AIB_tweets_text) # removing All the punctuation
AIB_tweets_text <- gsub("http\\w+", "", AIB_tweets_text)    # removing http link
AIB_tweets_text <- sapply(AIB_tweets_text, tolower)         # to lower chrachters

# emotion icons pose a challenge in minning tweets , on eplausible solution is filtering tweets on basis of regular expression
# Here is a function for the same.
rmNonAlphabet <- function(str) {
  words <- unlist(strsplit(str, " "))
  in.alphabet <- grep(words, pattern = "[a-z|0-9]", ignore.case = T)
  nice.str <- paste(words[in.alphabet], collapse = " ")
  nice.str
}

AIB_tweets_text <- sapply(AIB_tweets_text, rmNonAlphabet)

names(AIB_tweets_text) <- NULL

#have a look at refined Tweets
head(AIB_tweets_text)


# Finally our Data is ready to perform Analysis

"let us convert the twitter sentences into words so that they can be compared and judged"
# we need stringR package
install.packages("stringr")
library(stringr)

words.list <- str_split(AIB_tweets_text, " ")

pos <- scan('positive-words.txt', what = 'character', comment.char = ';')
neg <- scan('negative-words.txt', what = 'character', comment.char = ';')

words <- unlist(words.list)

pos.matches <- match(words , pos)
neg.matches <- match(words, neg)

pos.matches <- !is.na(pos.matches)
neg.matches <- !is.na(neg.matches)

pos.matches
neg.matches


